library(data.table)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(randomForest)
library(arules)
library(arulesSequences)
library(Matrix)
library(smbinning)
both_customer <- function(data) {
  phone <- data[, .(mobile, contact)]
  mutual <- merge(phone, phone, by.x = "mobile", by.y = "contact", allow.cartesian=TRUE)
  colnames(mutual) <- c('mobile.a', 'contact', 'mobile.b')
  mutual[, mobile.a]
}

share_contact <- function(data) {
  phone <- data[, .(mobile, contact)]
  both <- merge(phone, phone, by.x = "contact", by.y = "contact", allow.cartesian=TRUE)
  colnames(both) <- c("contact", "mobile.a", "mobile.b")
  rm_du <- subset(both, mobile.a!=mobile.b)
  # rm_du <- as.data.frame(rm_du)
  # rm_du[,c("mobile.a","mobile.b")] <- t(apply(rm_du[,c("mobile.a","mobile.b")], 1, sort))
  multi_contact <- unique(rm_du[,contact])
  multi_contact
}

share_client <- function(data) {
  phone <- data[, .(mobile, contact)]
  both <- merge(phone, phone, by.x = "mobile", by.y = "mobile", allow.cartesian=TRUE)
  colnames(both) <- c("mobile", "contact.a", "contact.b")
  rm_du <- subset(both, contact.a!=contact.b)
  # rm_du <- as.data.frame(rm_du)
  # rm_du[,c("mobile.a","mobile.b")] <- t(apply(rm_du[,c("mobile.a","mobile.b")], 1, sort))
  multi_mobile <- unique(rm_du[,mobile])
  multi_mobile
}


preprocess <- function(span) {
  file_name <- paste0("fraud_mutual_", span, ".txt")
  input_file <- paste0("C:/Users/Documents/R Scripts/data/", file_name)
  data <- fread(input_file, sep='\t',integer64="character",header = TRUE)
  online <- c("online1", "online2")
  processed <- unique(data) %>%
    subset(select = -id)
  processed[,date:=as.Date(date)]
  processed[,dpd00:=ifelse(dpd00=="N", 0, 1)]
  processed[, product:=ifelse(product %in% online, "online", "offline")]
  processed[, contact_mobile:=!contact_mobile]
  return(processed)
}

data_constr <- function(orig_data){
  date_min <- min(orig_data[,date])
  date_max <- max(orig_data[,date])
  end_date0 <- date_min + 29
  date_span <- 30
  if(exists("client_contact_tbl"))
    rm(client_contact_tbl)
  loop_len <- date_max - end_date0 + 1
  for (i in seq(loop_len)-1) {
    if(i%%10==0){
      print(paste("the ", i, "th"))
    }
    end_date <- end_date0 + i
    fraud_item <- orig_data[date %between% c(end_date-date_span+1, end_date)]
    pool <- unique(c(both_customer(fraud_item), share_contact(fraud_item)))
    fraud_item <- subset(fraud_item, contact %in% pool)
    
    client_tbl <- subset(fraud_item, select=c("mobile", "date", "dpd00", "product", "contact")) %>%
      unique(by="mobile")
    # client table
    shared_client <- share_client(fraud_item)
    single_client <- setdiff(fraud_item[,mobile], shared_client)
    
    item <- fraud_item[mobile %in% shared_client]
    setorder(item, mobile)
    
    # contact table
    shared_contact <- share_contact(fraud_item)
    contact_tbl <- fraud_item[contact %in% shared_contact]
    
    shared_contact_no <- group_by(contact_tbl, mobile) %>%
      summarise(contact_no_shared = n())
    
    # contact
    contact_info <- copy(contact_tbl)
    colnames(contact_info) <- c("loop1_fpd", "loop1_date", "loop1_mobile", "loop0_contact", "loop0_contact_mobile", "loop1_mobile_product")
    temp11 <- merge(contact_info, item, by.x = "loop1_mobile", by.y = "mobile", all.x = TRUE, allow.cartesian=TRUE)
    temp12 <- temp11[(contact!=loop0_contact & !is.na(contact))|is.na(contact)]
    temp12[,c("loop2_contact", "loop2_contact_mobile"):=list(contact, contact_mobile)]
    temp12[,c("dpd00","date","contact","product","contact_mobile"):=list(NULL,NULL,NULL,NULL,NULL)]
    
    temp21 <- merge(temp12, contact_tbl, by.x = "loop2_contact", by.y = "contact", all.x = TRUE, allow.cartesian = TRUE)
    temp22 <- temp21[((mobile!=loop1_mobile) & !is.na(loop2_contact) & !is.na(mobile))
                     |is.na(loop2_contact)
                     |(!(is.na(loop2_contact)) & is.na(mobile))]
    temp22[,c("loop3_mobile", "loop3_mobile_product", "loop3_fpd", "loop3_date"):=list(NA,NA,NA,NA)]
    temp22[,c("loop3_mobile", "loop3_mobile_product", "loop3_fpd", "loop3_date"):=list(mobile, product, dpd00, date)]
    temp22[,c("dpd00", "date", "mobile", "contact_mobile", "product"):=list(NULL,NULL,NULL,NULL,NULL)]
    
    
    loop1_no <- temp22[, length(unique(loop1_mobile)), by=loop0_contact]
    colnames(loop1_no) <- c("loop0_contact", "loop1_no_client")
    
    # by contact and product
    loop1_product <- temp22[, length(unique(loop1_mobile)), by="loop0_contact,loop1_mobile_product"]
    loop1_product <- dcast(loop1_product, loop0_contact~loop1_mobile_product)
    colnames(loop1_product)[2:ncol(loop1_product)] <- sapply(colnames(loop1_product)[2:ncol(loop1_product)], function(x) paste0("loop1_", x))
    
    # for loop0 contact how many loop1 mobile have loop2 contact
    loop1_have_child <- temp22[!is.na(loop2_contact), length(unique(loop1_mobile)), by="loop0_contact"]
    colnames(loop1_have_child) <- c("loop0_contact", "loop1_have_child")
    
    # for loop0 contact how many loop2 contact
    loop2_no_contact <- temp22[!is.na(loop2_contact), length(unique(loop2_contact)), by="loop0_contact"]
    colnames(loop2_no_contact) <- c("loop0_contact", "loop2_no_contact")
    
    # for loop0 contact how many loop2 contact is mobile
    loop2_no_contact_mobile <- temp22[!is.na(loop2_contact)&loop2_contact_mobile==1, 
                                      length(unique(loop2_contact)), 
                                      by="loop0_contact"]
    colnames(loop2_no_contact_mobile) <- c("loop0_contact", "loop2_no_contact_mobile")
    
    # for loop0 contact how many loop3 client
    loop3_no_client <- temp22[!is.na(loop3_mobile), length(unique(loop3_mobile)), by="loop0_contact"]
    colnames(loop3_no_client) <- c("loop0_contact", "loop3_no_client")
    
    # for loop0 contact how many each product in loop3 client
    loop3_product <- temp22[!is.na(loop3_mobile), length(unique(loop3_mobile)), by="loop0_contact,loop3_mobile_product"]
    loop3_product <- dcast(loop3_product, loop0_contact~loop3_mobile_product)
    colnames(loop3_product)[2:ncol(loop3_product)] <- sapply(colnames(loop3_product)[2:ncol(loop3_product)], function(x) paste0("loop3_", x))
    
    # 
    contact_all <- group_by(contact_tbl, contact) %>%
      summarise(contact_mobile=max(contact_mobile), if_dpd00=max(dpd00), no_dpd00=sum(dpd00)) 
    
    colnames(contact_all) <- c("loop0_contact", "loop0_contact_mobile", "loop0_if_dpd00", "loop0_no_dpd00")
    contact_all <- merge(contact_all, loop1_no, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE) %>%
      merge(loop1_product, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE) %>%
      merge(loop1_have_child, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE) %>%
      merge(loop2_no_contact, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE) %>%
      merge(loop2_no_contact_mobile, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE) %>%
      merge(loop3_no_client, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE) %>%
      merge(loop3_product, by.x = "loop0_contact", by.y = "loop0_contact", all.x = TRUE)
    
    # contact_all[,loop0_def_rate:=loop0_no_dpd00/loop1_no_client]
    contact_all[is.na(contact_all)] <- 0
    
    # just client connected to at least one shared contact
    client_contact <- merge(client_tbl, shared_contact_no, by.x = "mobile", by.y = "mobile", all.x = TRUE) %>%
      merge(contact_all, by.x = "contact", by.y = "loop0_contact")
    client_contact <- as.data.table(client_contact)
    client_contact[,is_max:=loop1_no_client==max(loop1_no_client), by= "mobile"]
    client_contact <- client_contact[date==end_date & is_max==TRUE]
    client_contact[, is_max:=NULL]
    
    if(!exists("client_contact_tbl"))
      client_contact_tbl <- copy(client_contact)
    else
      client_contact_tbl <- rbindlist(list(client_contact_tbl, client_contact))
    # [1] "contact"                 "mobile"                  "date"                    "dpd00"                  
    # [5] "product"                 "loop0_contact_mobile"    "loop0_if_dpd00"          "loop0_no_dpd00"         
    # [9] "loop1_no_client"         "loop1_product7"             "loop1_product1"          "loop1_product2"       
    # [13] "loop1_product3"        "loop1_product5"               "loop1_product4"               "loop1_product6"              
    # [17] "loop1_have_child"        "loop2_no_contact"        "loop2_no_contact_mobile" "loop3_no_client"        
    # [21] "loop3_product7"             "loop3_product1"          "loop3_product2"        "loop3_product3"       
    # [25] "loop3_product5"               "loop3_product4"               "loop3_product6"               "loop0_def_rate"   
    
    # [1] "contact"                 "mobile"                  "date"                    "dpd00"                  
    # [5] "product"                 "contact_no_shared"       "loop0_contact_mobile"    "loop0_if_dpd00"         
    # [9] "loop0_no_dpd00"          "loop1_no_client"         "loop1_offline"           "loop1_online"           
    # [13] "loop1_have_child"        "loop2_no_contact"        "loop2_no_contact_mobile" "loop3_no_client"        
    # [17] "loop3_offline"           "loop3_online"            "loop0_def_rate"       
  }
  return(client_contact_tbl)
}

gen_data <- function(data) {
  preprocessed <- preprocess(data)
  current_data <- data_constr(preprocessed)
  return(current_data)
}

# find compressed pattern
is_cover <- function(x, y, stl, delta) {
  delta_distance <- 1 - length(intersect(stl[[x]], stl[[y]]))/length(union(stl[[x]], stl[[y]]))
  return(delta_distance<delta)
}

compress <- function(fp, stl, delta) {
  super_m <- is.superset(fp, fp)
  removed <- c()
  for (i in seq(ncol(super_m))){
    if(i%%100==0)
      print(paste("the ",i,"th"))
    x <- colnames(super_m)[i]
    ys <- which(super_m[,i]==TRUE)
    ys <- setdiff(ys, c(i))
    flag <- FALSE
    if(length(ys)>0){
      for (y in ys){
        y <- rownames(super_m)[y]
        if(is_cover(x, y, stl, delta)){
          removed <- c(removed, x)
          break
        }
      }
    }
  }
  setdiff(rownames(super_m), removed)
}


gen_discretize <- function(orig_data) {
  client_contact_pattern <- copy(orig_data)
  client_contact_pattern[, c("mobile", "contact", "date"):=list(NULL,NULL,NULL)]
  client_contact_pattern[, product:=as.factor(product)]
  client_contact_pattern[, loop0_contact_mobile:=as.factor(loop0_contact_mobile)]
  client_contact_pattern[, loop1_no_client:=discretize(loop1_no_client, "fixed", 
                                                       categories = c(0, 3, 10, Inf))]
  client_contact_pattern[, loop1_offline:=discretize(loop1_offline, "fixed",
                                                     categories = c(0, 1, 3, 10, Inf))]
  client_contact_pattern[, loop1_online:=discretize(loop1_online, "fixed", 
                                                    categories = c(0, 1, Inf))]
  client_contact_pattern[, loop1_have_child:=discretize(loop1_have_child, "fixed", 
                                                        categories = c(0, 1, 5, Inf))]
  client_contact_pattern[, loop2_no_contact:=discretize(loop2_no_contact, "fixed", 
                                                        categories = c(0, 1, 10, Inf))]
  client_contact_pattern[, loop2_no_contact_mobile:=discretize(loop2_no_contact_mobile, "fixed",
                                                               categories = c(0, 1, 5, Inf))]     
  client_contact_pattern[, loop3_no_client:=discretize(loop3_no_client, "fixed",
                                                       categories = c(0, 1, 8, 50, Inf))]
  client_contact_pattern[, loop3_offline:=discretize(loop3_offline, "fixed", 
                                                     categories = c(0, 1, 8, 50, Inf))]
  client_contact_pattern[, loop3_online:=discretize(loop3_online, "fixed", 
                                                    categories = c(0, 1, Inf))]                      
  client_contact_pattern[, contact_no_shared:=discretize(contact_no_shared, "fixed", 
                                                         categories = c(1, 2, 3, Inf))]
  client_contact_pattern[, c("loop0_if_dpd00","loop0_no_dpd00", "loop0_def_rate"):=list(NULL,NULL,NULL)]
  ####
  client_contact_pattern[, c("loop1_offline", "loop3_offline"):=list(NULL, NULL)]
  return(client_contact_pattern)
  
}

gen_trans <- function(orig_data) {
  client_contact_pattern <- gen_discretize(orig_data)
  client_contact_pattern <- subset(client_contact_pattern, select = -dpd00)
  names(client_contact_pattern) <- paste0("_", names(client_contact_pattern))
  client_trans <- as(client_contact_pattern, "transactions")
  transactionInfo(client_trans)$classID <- orig_data$dpd00
  transactionInfo(client_trans)$date <- orig_data$date
  transactionInfo(client_trans)$product <- orig_data$product
  return(client_trans)
}

gen_tidlist <- function(client_trans) {
  freqset_apriori <- apriori(client_trans, 
                             parameter = list(support = 0.002,  minlen = 6, maxlen=8, target = "closed frequent itemsets"),
                             control = list(sort = -2, verbose = FALSE))
  st <- supportingTransactions(freqset_apriori, client_trans)
  stl <- as(st, "list")
  delta <- 0.1
  left <- compress(freqset_apriori, stl, delta)
  stl_left <- stl[left]
  return(stl_left)
}

gen_sum_tbl <- function(stl_left, trani){
  rate <- sapply(stl_left, function(x) mean(trani[trani$transactionID %in% x,]$classID))
  len <- sapply(stl_left, length)
  len_online <- sapply(stl_left, function(x) nrow(trani[trani$transactionID %in% x & trani$product == "online",]))
  len_offline <- sapply(stl_left, function(x) nrow(trani[trani$transactionID %in% x & trani$product == "offline",]))
  rate_online <- sapply(stl_left, function(x) mean(trani[trani$transactionID %in% x & trani$product == "online",]$classID))
  rate_offline <- sapply(stl_left, function(x) mean(trani[trani$transactionID %in% x & trani$product == "offline",]$classID))      
  print(summary(rate_offline[!is.na(rate_offline)]))
  cri_all <- !is.na(rate_offline)&rate_offline > 0.05
  cri_online <- !is.na(rate_online)&rate_online > 0.40
  freq_tbl <- function(cri) {
    selected <- stl_left[cri]
    count_online <- sapply(selected, function(x) trani[trani$transactionID %in% x & trani$product == "online",]$transactionID %>% unlist())
    names(count_online) <- NULL
    count_on <- unlist(count_online) %>% unique() 
    count_on_def <- sum(trani[trani$transactionID %in% count_on,"classID"])
    print(count_on_def)
    count_offline <- sapply(selected, function(x) trani[trani$transactionID %in% x & trani$product == "offline",]$transactionID %>% unlist())
    names(count_offline) <- NULL
    count_off <- unlist(count_offline) %>% unique() 
    count_off_no <- length(count_off)
    count_off_def <- sum(trani[trani$transactionID %in% count_off,"classID"])
    print(count_off_def)
    freq_cri <- rate[cri]
    len_cri <- len[cri]
    len_online_cri <- len_online[cri]
    rate_online_cri <- rate_online[cri]
    rate_offline_cri <- rate_offline[cri]
    freq_df <- data.frame(pattern = names(freq_cri), all_rate = freq_cri, no = len_cri, no_online = len_online_cri, 
                          rate_online = rate_online_cri, rate_offline = rate_offline_cri)
    
    freq_tbl_left <- group_by(freq_df, pattern) %>% 
      summarise(mean_rate_offline=mean(rate_offline), 
                mean_no = mean(no), 
                mean_no_online = mean(no_online),  
                mean_rate_online = mean(rate_online)) %>% 
      as.data.frame()
    return(freq_tbl_left)
  }
  freq_tbl_left <- freq_tbl(cri_all)
  freq_tbl_left_online <- freq_tbl(cri_online)
  return(list(freq_tbl_left, freq_tbl_left_online))
}

summary_tbls <- function(client_trans) {
  stl_left <- gen_tidlist(client_trans)
  trani <- transactionInfo(client_trans)
  freq_tbls <- gen_sum_tbl(stl_left, trani)
  return(freq_tbls)
}

search_isin <- function(x, pattern_list){
  x <- unlist(x)
  how_many <- 0
  how_many <- sum(sapply(pattern_list, 
                         function(elem) ifelse(sum(!(elem %in% x))==0, 1, 0)))
  if(how_many>0)
    isin <- TRUE
  else
    isin <- FALSE
  return(isin)
}

search_isin_single <- function(x, pattern_list){
  x <- unlist(x)
  how_many <- 0
  isin <- lapply(pattern_list, 
                 function(elem) ifelse(sum(!(elem %in% x))==0, 1, 0))
  return(isin)
}
# product_no <- contact_all[, list(sum(loop1_online), sum(loop1_offline)), by=loop0_contact]
# select subset


sum_tbl_off <- sum_tbls[[1]]
sum_tbl_on <- sum_tbls[[2]]
write.csv(sum_tbl_off, "C:/Users/Documents/work/network/sum_tbl_off.csv")
sum_tbl_off <- read.csv("C:/Users/Documents/R Scripts/data/network/sum_tbl_off.csv")
sum_tbl_off <- sum_tbl_off[sum_tbl_off$mean_rate_offline>0.06,]
pattern_offline <- com_tbl$pattern %>%
  sapply(function(a) {gsub("\\{|\\}", "", a) %>% 
      strsplit(",_") %>% 
      lapply(function(x) {temp <- gsub("^_","",x); paste0("_",temp)})
  })

pattern_offline <- com_tbl$pattern %>%
  sapply(function(a) {gsub("\\{|\\}", "", a) %>% 
      strsplit(", _") %>% 
      lapply(function(x) {temp <- gsub("^_","",x); paste0("_",temp)})
  })
sum_tbls <- summary_tbls(apr_client_trans)



# april
apr_data <- gen_data("march_april")
write.csv(apr_data, file = "C:/Users/Documents/R Scripts/data/network/apr_data.csv", row.names = FALSE)
apr_data <- fread("C:/Users/Documents/R Scripts/data/network/apr_data.csv")

apr_client_trans <- gen_trans(apr_data)
sum_tbls <- summary_tbls(apr_client_trans)
sum_tbl_off <- sum_tbls[[1]]
write.csv(sum_tbl_off, "C:/Users/Documents/R Scripts/data/network/apr_sum_tbl_off.csv")

apr_client_trans_l <- as(apr_client_trans, "list")
apr_is_inpattern <- sapply(apr_client_trans_l, function(x) search_isin_single(x, pattern_offline))
apr_is_inpattern_t <- t(apr_is_inpattern)
a <- cbind(apr_is_inpattern_t, apr_data$dpd00, apr_data$product) %>% as.data.frame()
colnames(a) <- c(seq((ncol(a)-2)), "fpd", "product")
b <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a[,54])[x==1&a$product=="offline"]))
summary(b)

sum(unlist(a[,51])[a$product=="offline"])
a <- data.frame(pattern=apr_is_inpattern, fpd=apr_data$dpd00, product=apr_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())

# may
may_data <- gen_data("apr_may")
may_client_trans <- gen_trans(may_data)
write(may_client_trans, file = "C:/Users/Documents/work/network/may_trans.csv", format = "basket")

may_client_trans <- read.transactions(file = "C:/Users/Documents/work/network/may_trans.csv",
                                      format = "basket")
may_client_trans_l <- as(may_client_trans, "list")
may_is_inpattern <- sapply(may_client_trans_l, function(x) search_isin(x, pattern_offline))
a <- data.frame(pattern=may_is_inpattern, fpd=may_data$dpd00, product=may_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())


# july
july_data <- gen_data("june_july")
write.csv(july_data, file = "C:/Users/Documents/R Scripts/data/network/july_data.csv", row.names = FALSE)
july_data <- fread("C:/Users/Documents/R Scripts/data/network/july_data.csv")
july_client_trans <- gen_trans(july_data)
sum_tbls <- summary_tbls(july_client_trans)
sum_tbl_off <- sum_tbls[[1]]
write.csv(sum_tbl_off, "C:/Users/Documents/R Scripts/data/network/july_sum_tbl_off.csv")

july_client_trans_l <- as(july_client_trans, "list")
july_is_inpattern <- sapply(july_client_trans_l, function(x) search_isin_single(x, pattern_offline))
july_is_inpattern_t <- t(july_is_inpattern)
a <- cbind(july_is_inpattern_t, july_data$dpd00, july_data$product) %>% as.data.frame()
colnames(a) <- c(seq((ncol(a)-2)), "fpd", "product")
b <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a$fpd)[x==1&a$product=="offline"]))
summary(b)

july_is_inpattern <- sapply(july_client_trans_l, function(x) search_isin(x, pattern_offline))
a <- data.frame(pattern=july_is_inpattern, fpd=july_data$dpd00, product=july_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())

# august
aug_data <- gen_data("july_august")
write.csv(aug_data, file = "C:/Users/Documents/R Scripts/data/network/aug_data.csv", row.names = FALSE)
aug_data <- fread("C:/Users/Documents/R Scripts/data/network/aug_data.csv")
aug_client_trans <- gen_trans(aug_data)
sum_tbls <- summary_tbls(aug_client_trans)
sum_tbl_off <- sum_tbls[[1]]
write.csv(sum_tbl_off, "C:/Users/Documents/R Scripts/data/network/aug_sum_tbl_off.csv")

aug_client_trans_l <- as(aug_client_trans, "list")
aug_is_inpattern <- sapply(aug_client_trans_l, function(x) search_isin_single(x, pattern_offline))
aug_is_inpattern_t <- t(aug_is_inpattern)
a <- cbind(aug_is_inpattern_t, aug_data$dpd00, aug_data$product) %>% as.data.frame()
colnames(a) <- c(seq(ncol(a)-2), "fpd", "product")
b_aug <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a$fpd)[x==1&a$product=="offline"]))
summary(b_aug)
a_aug <- a
ss <- apply(as.matrix(subset(a_aug, select=comm_pattern)), 1, function(x) sum(unlist(x)))
sss <- apply(as.matrix(subset(a_aug, select=comm_pattern)), 2, function(x) sum(unlist(x)))

a <- data.frame(pattern=aug_is_inpattern, fpd=aug_data$dpd00, product=aug_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())

# merge pattern
apr_tbl <- read.csv("C:/UsersDocuments/R Scripts/data/network/apr_sum_tbl_off.csv")
july_tbl <- read.csv("C:/Users/Documents/R Scripts/data/network/july_sum_tbl_off.csv")
aug_tbl <- read.csv("C:/Users/Documents/R Scripts/data/network/aug_sum_tbl_off.csv")
com_tbl <- merge(apr_tbl, july_tbl, by.x = "pattern", by.y = "pattern") %>%
  merge(aug_tbl, by.x = "pattern", by.y = "pattern")

# september
sep_data <- gen_data("august_sep")
write.csv(sep_data, file = "C:/Users/Documents/R Scripts/data/network/sep_data.csv", row.names = FALSE)
sep_data <- fread("C:/Users/Documents/R Scripts/data/network/sep_data.csv")
sep_client_trans <- gen_trans(sep_data)
sep_client_trans_l <- as(sep_client_trans, "list")
sep_is_inpattern <- sapply(sep_client_trans_l, function(x) search_isin_single(x, pattern_offline))
sep_is_inpattern_t <- t(sep_is_inpattern)
a <- cbind(sep_is_inpattern_t, sep_data$dpd00, sep_data$product) %>% as.data.frame()
colnames(a) <- c(seq(ncol(a)-2), "fpd", "product")
b_sep <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a$fpd)[x==1&a$product=="offline"]))
summary(b_sep)
a_sep <- a
ss <- apply(as.matrix(subset(a_sep, select=comm_pattern)), 1, function(x) sum(unlist(x)))
sss <- apply(as.matrix(subset(a_sep, select=comm_pattern)), 2, function(x) sum(unlist(x)))
ss_sep <- ss
sss_sep <- sss
sum(ss>0)

a <- data.frame(pattern=sep_is_inpattern, fpd=sep_data$dpd00, product=sep_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())

# october
oct_data <- gen_data("sep_oct")
write.csv(oct_data, file = "C:/Users/Documents/R Scripts/data/network/oct_data.csv", row.names = FALSE)
oct_data <- fread("C:/Users/Documents/R Scripts/data/network/oct_data.csv")

oct_client_trans <- gen_trans(oct_data)
oct_client_trans_l <- as(oct_client_trans, "list")
oct_is_inpattern <- sapply(oct_client_trans_l, function(x) search_isin_single(x, pattern_offline))

oct_is_inpattern_t <- t(oct_is_inpattern)
a <- cbind(oct_is_inpattern_t, oct_data$dpd00, oct_data$product) %>% as.data.frame()
colnames(a) <- c(seq(ncol(a)-2), "fpd", "product")
b <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a$fpd)[x==1&a$product=="offline"]))
summary(b)
b_oct <- b
a_oct <- a
comm_pattern <- intersect(names(b_oct)[b_oct>0.06], names(b_sep)[b_sep>0.06])
ss <- apply(as.matrix(subset(a_oct, select=comm_pattern)), 1, function(x) sum(unlist(x)))
sss <- apply(as.matrix(subset(a_oct, select=comm_pattern)), 2, function(x) sum(unlist(x)))
ss_oct <- ss
sss_oct <- sss
sum(ss>0)
summary(sss)
comm_pattern_n <- as.integer(comm_pattern)
summary(b_sep[comm_pattern_n])
summary(b_oct[comm_pattern_n])
mean(unlist(a_oct$fpd)[ss>0])
stat_tbl <- data.frame(pattern=sapply(pattern_offline[comm_pattern_n], function(x) paste(unlist(x), collapse = ", ")),
                       count_sep=sss_sep[comm_pattern], 
                       bad_rate_sep=b_sep[comm_pattern_n],
                       count_oct=sss_oct[comm_pattern], 
                       bad_rate_oct=b_oct[comm_pattern_n])
write.csv(stat_tbl, "sum_sep_oct.csv", row.names=FALSE)
com_tbl <- read.csv("C:/Users/Documents/R Scripts/sum_pattern_sep_oct.csv")
com_tbl <- com_tbl[1:28,]
mean(unlist(a_sep$fpd)[ss_sep>0])
mean(unlist(a_oct$fpd)[ss_oct>0])
mean(unlist(a_sep[a_sep$product=="offline",]$fpd))
mean(unlist(a_oct[a_oct$product=="offline",]$fpd))

a <- data.frame(pattern=oct_is_inpattern, fpd=oct_data$dpd00, product=oct_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())

# nov 
nov_data <- gen_data("oct_nov")
write.csv(nov_data, file = "C:/Users/Documents/R Scripts/data/network/nov_data.csv", row.names = FALSE)
nov_data <- fread("C:/Users/Documents/R Scripts/data/network/nov_data.csv")

nov_client_trans <- gen_trans(nov_data)
nov_client_trans_l <- as(nov_client_trans, "list")
nov_is_inpattern <- sapply(nov_client_trans_l, function(x) search_isin_single(x, pattern_offline))

nov_is_inpattern_t <- t(nov_is_inpattern)
a <- cbind(nov_is_inpattern_t, nov_data$dpd00, nov_data$product) %>% as.data.frame()
colnames(a) <- c(seq(ncol(a)-2), "fpd", "product")
a <- a[a$product=="offline",]
b <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a$fpd)[x==1&a$product=="offline"]))
summary(b)
b_nov <- b
a_nov <- a
comm_pattern <- intersect(names(b_oct)[b_oct>0.06], names(b_sep)[b_sep>0.06])
ss <- apply(as.matrix(a_nov[1:28]), 1, function(x) sum(unlist(x)))
sss <- apply(as.matrix(a_nov[1:28]), 2, function(x) sum(unlist(x)))
ss_nov <- ss
sss_nov <- sss
sum(ss>0)
mean(a_nov$fpd[ss_nov>0 & a_nov$product=="offline"])
summary(sss)
com_tbl_nov <- com_tbl
com_tbl_nov$count_offline_nov <- sss_nov
com_tbl_nov$bad_rate_offline_nov <- b_nov
write.csv(com_tbl_nov, "sum_sep_nov.csv", row.names = FALSE)
comm_pattern_n <- as.integer(comm_pattern)
summary(b_sep[comm_pattern_n])
summary(b_oct[comm_pattern_n])
mean(unlist(a_oct$fpd)[ss>0])
stat_tbl <- data.frame(pattern=sapply(pattern_offline[comm_pattern_n], function(x) paste(unlist(x), collapse = ", ")),
                       count_sep=sss_sep[comm_pattern], 
                       bad_rate_sep=b_sep[comm_pattern_n],
                       count_oct=sss_oct[comm_pattern], 
                       bad_rate_oct=b_oct[comm_pattern_n])
write.csv(stat_tbl, "sum_sep_oct.csv", row.names=FALSE)
mean(unlist(a_sep$fpd)[ss_sep>0])
mean(unlist(a_oct$fpd)[ss_oct>0])
mean(unlist(a_sep[a_sep$product=="offline",]$fpd))
mean(unlist(a_oct[a_oct$product=="offline",]$fpd))

a <- data.frame(pattern=oct_is_inpattern, fpd=oct_data$dpd00, product=oct_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())


# dec
dec_data <- gen_data("nov_dec")
write.csv(dec_data, file = "C:/Users/Documents/R Scripts/data/network/dec_data.csv", row.names = FALSE)
dec_data <- fread("C:/Users/Documents/R Scripts/data/network/dec_data.csv")

dec_client_trans <- gen_trans(dec_data)
dec_client_trans_l <- as(dec_client_trans, "list")
dec_is_inpattern <- sapply(dec_client_trans_l, function(x) search_isin_single(x, pattern_offline))

dec_is_inpattern_t <- t(dec_is_inpattern)
a <- cbind(dec_is_inpattern_t, dec_data$dpd00, dec_data$product) %>% as.data.frame()
colnames(a) <- c(seq(ncol(a)-2), "fpd", "product")
a <- a[a$product=="offline",]
b <- apply(a[,1:(ncol(a)-2)], 2, function(x) mean(unlist(a$fpd)[x==1&a$product=="offline"]))
summary(b)
b_dec <- b
a_dec <- a
comm_pattern <- intersect(names(b_oct)[b_oct>0.06], names(b_sep)[b_sep>0.06])
ss <- apply(as.matrix(a_dec[1:28]), 1, function(x) sum(unlist(x)))
sss <- apply(as.matrix(a_dec[1:28]), 2, function(x) sum(unlist(x)))
ss_dec <- ss
sss_dec <- sss
sum(ss>0)
mean(a_dec$fpd[ss_dec>0 & a_dec$product=="offline"])
summary(sss)
com_tbl_dec <- com_tbl
com_tbl_dec$count_offline_dec <- sss_dec
com_tbl_dec$bad_rate_offline_dec <- b_dec
write.csv(com_tbl_dec, "sum_nov_dec.csv", row.names = FALSE)
comm_pattern_n <- as.integer(comm_pattern)

summary(b_sep[comm_pattern_n])
summary(b_oct[comm_pattern_n])
mean(unlist(a_oct$fpd)[ss>0])
stat_tbl <- data.frame(pattern=sapply(pattern_offline[comm_pattern_n], function(x) paste(unlist(x), collapse = ", ")),
                       count_sep=sss_sep[comm_pattern], 
                       bad_rate_sep=b_sep[comm_pattern_n],
                       count_oct=sss_oct[comm_pattern], 
                       bad_rate_oct=b_oct[comm_pattern_n])
write.csv(stat_tbl, "sum_sep_oct.csv", row.names=FALSE)
mean(unlist(a_sep$fpd)[ss_sep>0])
mean(unlist(a_oct$fpd)[ss_oct>0])
mean(unlist(a_sep[a_sep$product=="offline",]$fpd))
mean(unlist(a_oct[a_oct$product=="offline",]$fpd))

a <- data.frame(pattern=oct_is_inpattern, fpd=oct_data$dpd00, product=oct_data$product)
group_by(a, pattern) %>% summarise(mean_fpd = mean(fpd))
a[a$pattern==TRUE, ] %>% group_by(product) %>% summarise(mean_fpd = mean(fpd), no_def = sum(fpd), no=n())
########################################################## 
client_contact_pattern <- copy(client_contact)
client_contact_pattern[, c("mobile", "contact", "date"):=list(NULL,NULL,NULL)]
client_contact_pattern[, dpd00:=as.factor(dpd00)]
client_contact_pattern[, product:=as.factor(product)]
client_contact_pattern[, loop0_contact_mobile:=as.factor(loop0_contact_mobile)]
client_contact_pattern[, loop1_no_client:=discretize(loop1_no_client, "fixed", 
                                                     categories = c(0, 3, 10, 30, Inf))]
client_contact_pattern[, loop1_product1:=discretize(loop1_product1, "fixed",
                                                    categories = c(-Inf, 0, Inf))]
client_contact_pattern[, loop1_product2:=discretize(loop1_product2, "fixed",
                                                      categories = c(-Inf, 0, 4, 20, Inf))]
client_contact_pattern[, loop1_product3:=discretize(loop1_product3, "fixed",
                                                      categories = c(-Inf, 0, 3, 10, Inf))]

client_contact_pattern[, loop1_product4:=discretize(loop1_product4, "fixed", 
                                               categories = c(-Inf, 0, Inf))]
client_contact_pattern[, loop1_product5:=discretize(loop1_product5, "fixed", 
                                               categories = c(-Inf, 0, 1, Inf))]
client_contact_pattern[, loop1_have_child:=discretize(loop1_have_child, "fixed", 
                                                      categories = c(-Inf, 0, 3, 15, Inf))]
client_contact_pattern[, loop2_no_contact:=discretize(loop2_no_contact, "fixed", 
                                                      categories = c(-Inf, 0, 2, 10, Inf))]
client_contact_pattern[, loop2_no_contact_mobile:=discretize(loop2_no_contact_mobile, "fixed",
                                                             categories = c(-Inf, 0, Inf))]     
client_contact_pattern[, loop3_no_client:=discretize(loop3_no_client, "fixed",
                                                     categories = c(-Inf, 0, 5, 10, Inf))]
client_contact_pattern[, loop3_product1:=discretize(loop3_product1, "fixed", 
                                                    categories = c(-Inf, 0, Inf))]
client_contact_pattern[, loop3_product2:=discretize(loop3_product2, "fixed",
                                                      categories = c(-Inf, 0, 7, 20, Inf))] 
client_contact_pattern[, loop3_product3:=discretize(loop3_product3, "fixed",
                                                      categories = c(-Inf, 0, 7, 15, Inf))]
client_contact_pattern[, loop3_product5:=discretize(loop3_product5, "fixed",
                                               categories = c(-Inf, 0, 3, Inf))]
client_contact_pattern[, loop3_product4:=discretize(loop3_product4, "fixed", 
                                               categories = c(-Inf, 0, Inf))]                      
client_contact_pattern[, contact_no_shared:=discretize(contact_no_shared, "fixed", 
                                                       categories = c(-Inf, 0, 1, Inf))]
client_contact_pattern[, c("loop0_if_dpd00","loop0_no_dpd00", "loop0_def_rate"):=list(NULL,NULL,NULL)]
client_contact_pattern[, c("loop1_product6", "loop3_product6", "loop1_product7", "loop3_product7"):=list(NULL,NULL,NULL,NULL)]
client_trans <- as(subset(client_contact_pattern, select = -dpd00), "transactions")
transactionInfo(client_trans)$classID <- client_contact_pattern$dpd00

freqset_apriori <- eclat(client_trans, 
                         parameter = list(support = 0.2, minlen = 9, maxlen = 20, target = "closed frequent itemsets"),
                         control = list(sort = -2, verbose = FALSE))
st <- supportingTransactions(freqset_apriori, client_trans)

pattern_ts <- melt(pattern, id="sid", measure.vars = c("pattern_1", "pattern_2", "pattern_3"),
                   variable.name = "eid", value.name = "items")



