
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
# library(keras, warn.conflicts = F, quietly = T)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

mixer <- function(dat, x, howmany){
  dat %>%
    group_by(species_id) %>%
    sample_n(., 1) %>%
    ungroup() %>%
    sample_n(., howmany) %>%
    mutate(iter = x)
}

mixer_v2 <- function(dat, iter, howmany){
  
  iter = iter
  
  tmp_list <- vector('list', iter)
  key_list <- vector('list', iter)
  
  for(i in 1:iter){
    print(paste0("mixer ___ ",i))
    tmp <- mixer(dat, i, howmany) %>%
      mutate(n = 1:n())
    
    tmp_res <- data.frame(unique_id =  as.character(glue::glue_collapse(c(unique(tmp$iter),unique(tmp$species_id)),"_")), matrix(0, 1, ncol=length(unique(dat$species_id))))
    colnames(tmp_res)[-1] <- paste0("s", seq(0,23,1))
    species <- as.numeric(unique(tmp$species_id))
    
    j=1
    repeat{
      tmp_res[1,species[j]+2] <- 1
      if(j==nrow(tmp)){break}
      j=j+1
    }
    
    tmp <- data.table::rbindlist(lapply(1:nrow(tmp), function(x) readRDS(tmp[x,]$file_id) %>% mutate(value = round(value, digits=3)))) 
    tmp <- tmp %>%
      select(FreqHz, zts, value) %>%
      group_by(FreqHz, zts) %>%
      summarise(value = sum(value), .groups = 'drop')
    tmp$unique_id <- tmp_res$unique_id
    
    tmp_list[[i]] <- tmp 
    key_list[[i]] <- tmp_res
  }
  
  return(list(data.table::rbindlist(tmp_list), data.table::rbindlist(key_list)))
}


read_rds_v2 <- function(tmp){
  
  read_rds <- function(files, x){
    print(paste0("reading rds ___ ", x))
    readRDS(files[x]) %>%
      select(unique_id, species_id, FreqHz, zts, value) %>%
      mutate(value = round(value, digits=3))
  }
  
  df1 <- lapply(1:length(tmp), function(x) read_rds(tmp, x))
  data.table::rbindlist(df1)
}


keras_whole <- function(data){
  freq_bin <- data.frame(FreqHz = unique(data$FreqHz), 
                         FreqBin1 = cut(unique(data$FreqHz), 4, labels = c(1,2,3,4)))
  tmp <- data  %>%
    left_join(., freq_bin, by="FreqHz") %>%
    group_by(unique_id, FreqHz) %>%
    mutate(zts = 1:n()) %>%
    ungroup()
  
  f1 <- tmp %>%
    dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + value + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    dplyr::filter(FreqBin1 == 2) %>%
    dcast(unique_id + value + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    dplyr::filter(FreqBin1 == 3) %>%
    dcast(unique_id + value + zts ~ FreqHz) 
  
  f4 <- tmp %>%
    dplyr::filter(FreqBin1 == 4) %>%
    dcast(unique_id + value + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -value, -zts) %>% as.matrix(),
         f2 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -value, -zts) %>% as.matrix(),
         f3 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -value, -zts) %>% as.matrix(),
         f4 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -value, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  # train <- parallel::mclapplylapply(1:length(times), function(x) lister(x))
  
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  
  output_x_lab <- f1 %>% 
    dplyr::filter(zts == 1) %>% 
    select(unique_id)
  
  return(list(output_x, output_x_lab))
}

#### Setup

data_files <- list.files("output/ind_processed", pattern="ind_", full.names = TRUE)
true_files <- data_files[grepl("true", data_files)]
key <- data.frame(do.call(rbind, strsplit(true_files,"/|_|[.]"))[,5:9], file_id = true_files)
colnames(key) <- c('recording_id','unique_id','data_type','species_id','cate', "file_id")
key$unique_id <- paste0(key$recording_id, "_" , key$unique_id)

data_list <- unique(key$data_type)[1:5]

############
status_x <- rep(x = NA, times = length(data_list))
pb <- txtProgressBar(0, length(status_x), style = 3)
############

for(i in 1:length(data_list)){
  
  #########################
  setTxtProgressBar(pb, i)
  status_x[i] 
  #########################
  print("setup")
  dat <- key %>% filter(data_type == data_list[i])
  
  ### Save for evaluation
  data_y <- data.table::rbindlist(lapply(1:10, function(x) mixer(dat, x, 23)))
  data_y <- data_y[!duplicated(data_y$unique_id),]
  table(data_y$species_id)
  
  data_x <- dat[!(dat$unique_id %in% data_y$unique_id),]
  
  ####### randomly split into three setA, B, C
  print("spliting data")
  
  files <- data_x$file_id
  A <- sample(files, length(files)/5)
  B <- sample(files[!(files %in% A)], length(files)/5)
  C <- sample(files[!(files %in% A | files %in% B)], length(files)/5)
  D <- sample(files[!(files %in% A | files %in% B | files %in% C)], length(files)/5)
  E <- files[!(files %in% A | files %in% B | files %in% C | files %in% D)]
  
  # AB <- data_x[data_x$file_id %in% c(A, B),]
  # AC <- data_x[data_x$file_id %in% c(A, C),]
  # AD <- data_x[data_x$file_id %in% c(A, D),]
  # BC <- data_x[data_x$file_id %in% c(B, C),]
  # BD <- data_x[data_x$file_id %in% c(B, D),]
  # CD <- data_x[data_x$file_id %in% c(C, D),]
  
  ### Shortedn fOR TESTING
  # AB <- read_rds_v2(AB[sample(1:nrow(AB), nrow(AB)*.20),])
  keras_whole_v2 <- function(input_data){
    
    ##### Generate mixed files
    print("mixing species")
    
    mixed2 <- mixer_v2(dat, 2, 2)
    mixed3 <- mixer_v2(dat, 2, 3)
    mixed4 <- mixer_v2(dat, 2, 4)
    mixed5 <- mixer_v2(dat, 2, 5)
    mixed6 <- mixer_v2(dat, 2, 6)
    # input_data <- read_rds_v2(input_data[sample(1:nrow(input_data), nrow(input_data)*.05),])
    tmp <- read_rds_v2(input_data)
    
    tmp <- tmp %>%
      select(-species_id) %>%
      # rbind(mixed2[[1]]) 
      rbind(mixed2[[1]],
            mixed3[[1]],
            mixed4[[1]],
            mixed5[[1]],
            mixed6[[1]])
    
    sample_list <- unique(tmp$unique_id)
    x_names <- sample(sample_list, length(sample_list)*0.9)
    y_names <- sample_list[!(sample_list %in% x_names)]
    
    data_x <- tmp[tmp$unique_id %in% x_names,]
    data_y <- tmp[tmp$unique_id %in% y_names,]
    
    print("creating keras array")
    
    data_x <- keras_whole(data_x)
    train_data_x <- data_x[[1]]
    data_y <- keras_whole(data_y)
    train_data_y <- data_y[[1]]
    
    ### making label matrix
    train_lab <- input_data %>%
      select(unique_id, species_id) %>%
      distinct(.) %>%
      mutate(tmp = 1) %>%
      mutate(species_id = paste0("s",species_id)) %>%
      dcast(unique_id ~species_id, value.var = "tmp") %>%
      mutate_all(., function(x) ifelse(is.na(x), 0, x))
    
    train_lab <- rbind(mixed2[[2]], mixed3[[2]], mixed4[[2]], mixed5[[2]], mixed6[[2]],
                       train_lab, fill=TRUE) %>%
      mutate_all(., function(x) ifelse(is.na(x), 0, x))
    
    train_lab_x <- train_lab[train_lab$unique_id %in% x_names,]
    train_lab_y <- train_lab[train_lab$unique_id %in% y_names,]
    
    train_lab_x <- train_lab_x[match(data_x[[2]]$unique_id, train_lab_x$unique_id),]
    train_lab_y <- train_lab_y[match(data_y[[2]]$unique_id, train_lab_y$unique_id),]
    
    return(list(train_data_x, train_lab_x, train_data_y, train_lab_y))
  }
  # print("exporting")
  
  saveRDS(keras_whole_v2(A), paste0("output/ind_keras/",data_list[i],"_A.RDS"))
  saveRDS(keras_whole_v2(B), paste0("output/ind_keras/",data_list[i],"_B.RDS"))
  saveRDS(keras_whole_v2(C), paste0("output/ind_keras/",data_list[i],"_C.RDS"))
  saveRDS(keras_whole_v2(D), paste0("output/ind_keras/",data_list[i],"_D.RDS"))
  saveRDS(keras_whole_v2(E), paste0("output/ind_keras/",data_list[i],"_E.RDS"))
  # saveRDS(keras_whole_v2(F), paste0("output/ind_keras/",data_list[i],"_CD.RDS"))
  
}


