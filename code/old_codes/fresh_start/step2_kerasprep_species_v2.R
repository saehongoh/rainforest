
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
    readRDS(files[x,]$file_id) %>%
      select(unique_id, species_id, FreqHz, zts, value) %>%
      mutate(value = round(value, digits=3)) %>%
      mutate(unique_id = files[x,]$unique_id)
  }

  df1 <- lapply(1:nrow(tmp), function(x) read_rds(tmp, x))
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
    dcast(unique_id + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    dplyr::filter(FreqBin1 == 2) %>%
    dcast(unique_id + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    dplyr::filter(FreqBin1 == 3) %>%
    dcast(unique_id + zts ~ FreqHz) 
  
  f4 <- tmp %>%
    dplyr::filter(FreqBin1 == 4) %>%
    dcast(unique_id + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>% dplyr::select(-unique_id, -zts) %>% as.matrix(),
         f2 %>% dplyr::filter(zts ==times[x])%>% dplyr::select(-unique_id, -zts) %>% as.matrix(),
         f3 %>% dplyr::filter(zts ==times[x])%>% dplyr::select(-unique_id, -zts) %>% as.matrix(),
         f4 %>% dplyr::filter(zts ==times[x])%>% dplyr::select(-unique_id, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- pbmcapply::pbmclapply(1:length(times), function(x) lister(x), mc.cores = 4)
  # train <- lapply(1:length(times), function(x) lister(x))
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  
  output_x_lab <- f1 %>% 
    dplyr::filter(zts == 1) %>% 
    select(unique_id)
  
  return(list(output_x, output_x_lab))
}

keras_whole_v2 <- function(input_data, mix=FALSE){
  
  print("reading in file")
  tmp <- read_rds_v2(input_data)
  
  print("creating keras array")
  
  data_x <- keras_whole(tmp)
  train_data <- data_x[[1]]
  # data_y <- keras_whole(data_y)
  # train_data_y <- data_y[[1]]
  
  ### making label matrix
  train_lab <- input_data %>%
    # mutate(unique_id = paste0(unique_id, "_",data_type)) %>%
    select(unique_id, species_id, cate) %>%
    distinct(.) %>%
    mutate(cate = ifelse(cate == "true", 1, 0)) %>%
    mutate(species_id = paste0("s",species_id)) %>%
    dcast(unique_id ~species_id, value.var = "cate") %>%
    mutate_all(., function(x) ifelse(is.na(x), 0, x))
  
  template <- data.frame(matrix(0, ncol=25, nrow=nrow(train_lab)))
  colnames(template) <- c("unique_id", paste0("s", seq(0,23,1)))
  
  train_lab <- cbind(train_lab, template[,!colnames(template) %in% colnames(train_lab)])
  
  data_labels <- data_x[[2]] 
  
  train_lab <- train_lab[match(data_labels$unique_id, train_lab$unique_id),]
  train_lab <- data.frame(train_lab)[,c(match(colnames(template), colnames(train_lab)))]
  # train_lab_y <- train_lab_y[match(data_y[[2]]$unique_id, train_lab_y$unique_id),]
  print("exporting")
  return(list(train_data, train_lab))
}


#### Setup

data_files <- list.files("output/ind_processed", pattern="ind", full.names = TRUE)
data_tmp <- gsub("[.]4","",data_files)
key <- data.frame(do.call(rbind, strsplit(data_tmp,"/|_|[.]"))[,5:9], file_id = data_files)
colnames(key) <- c('recording_id','unique_id','data_type','species_id','cate', "file_id")
key$unique_id <- paste0(key$recording_id, "_" , key$unique_id)

species_list <- sort(unique(key$species_id))
species_list <- species_list[23:length(species_list)]

############
status_x <- rep(x = NA, times = length(species_list))
pb <- txtProgressBar(0, length(status_x), style = 3)
############

for(i in 1:length(species_list)){
  
  #########################
  setTxtProgressBar(pb, i)
  status_x[i] 
  #########################
  print(paste0("working on --- ", species_list[i]))
  dat <- key %>% filter(species_id == species_list[i]) %>%
    mutate(unique_id = paste0(unique_id, "_", data_type))
  print("spliting data")
  
  data_y <- dat %>%
    group_by(cate) %>%
    mutate(n = n()) %>%
    sample_n(., n*0.20) %>%
    select(-n)
  data_y <- data_y[sample(1:nrow(data_y)),]
  saveRDS(keras_whole_v2(data_y), paste0("output/ind_keras/", "s",species_list[i], "_dataY.RDS"))
  
  data_x <- dat[!(dat$file_id %in% data_y$file_id),]
  data_x <- data_x[sample(1:nrow(data_x)),]
  
  files <- data_x$file_id
  if(unique(data_x$species_id) == 23){
    batches <- split(files, ceiling(seq_along(files)/(length(files)/4)))
  } else {
    batches <- split(files, ceiling(seq_along(files)/(length(files)/3)))
  }

  for(j in 1:length(batches)){
    # print("exporting")
    tmp <- data_x[data_x$file_id %in% batches[[j]],]
    saveRDS(keras_whole_v2(tmp), paste0("output/ind_keras/", "s", species_list[i], "_dataX_B", j, ".RDS"))
  }
  
}

# test <- readRDS("output/ind_keras/s21_dataX_B1.RDS")
# which(is.na(test[[1]]))
# test[[2]]
# dim(test)

# readRDS("output/ind_processed/ind_8e2ff1226_2_org_17_false.rds")
