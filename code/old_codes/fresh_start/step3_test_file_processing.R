library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

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

keras_unmasked_v2 <- function(input_data, mix=FALSE){
  
  print("reading in file")
  tmp <- input_data
  
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

specmaker_v3 <- function(tmp, augment = FALSE){
  
  specmaker_v2 <- function(tmp, x, augment = FALSE){
    print(paste0("reading rds ___ ", x))
    # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
    res <- specmaker(tmp[x,], low_f = 50, high_f = 14000, window_multiplier = 4)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() 
    
    if(augment == TRUE){
      print("augmenting data")
      if(unique(res$t_min) > 30){
        shift_time = -sample(seq(10,25,5),1)
        add_time = 60
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time < 0, time + add_time, time))
      } else if(unique(res$t_min) < 30){
        shift_time = sample(seq(10,25,5),1)
        add_time = -60
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time > 60, time + add_time, time))
      } else {
        res <- res
      }
    }
    
    res <- res %>%
      group_by(FreqHz) %>%
      arrange(time) %>%
      mutate(zts = 1:n()) %>%
      ungroup() %>%
      # mutate(value = (value - min(value)) / (max(value) - min(value)))  %>%
      # mutate(zts = seqt - min(seqt)) %>%
      group_by(zts) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value)) %>%
      mutate(value = value/max(value)) %>%
      mutate(value = round(value, digits=3))
    
    res %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()
  }
  df1 <- lapply(1:nrow(tmp), function(x) specmaker_v2(tmp, x, augment=augment))
  data.table::rbindlist(df1)
}

## True positives
train_tp <- read.csv("data/train_tp.csv") %>% mutate(cate = "true") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  # mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  # mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  # mutate(species_id = as.numeric(as.character(species_id)))  %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup() %>%
  mutate(t_min = ifelse(duration < 1.5,t_min - duration/2, t_min)) %>%
  mutate(t_max = ifelse(duration < 1.5, t_max + duration/2, t_max)) %>%
  mutate(duration = t_max - t_min)

train_batches <- train_tp %>%
  group_by(species_id) %>%
  mutate(group_n = ceiling(seq_along(file_id)/(length(file_id)/8))) %>%
  ungroup() %>%
  group_by(group_n) %>%
  group_split(.)

for(i in 1:length(train_batches)){
  tmp <- train_batches[[i]]
  res <- specmaker_v3(tmp, augment = FALSE)
  res <- keras_unmasked_v2(res)
  file_name = paste0("output/unmasked/train_posonly_B", i, "_org.RDS")
  print(file_name)
  saveRDS(res, file_name)
  j=1
  repeat{
    res <- specmaker_v3(tmp, augment = TRUE)
    res <- keras_unmasked_v2(res)
    file_name = paste0("output/unmasked/train_posonly_B",i,"_aug", j, ".RDS")
    print(file_name)
    saveRDS(res, file_name)
    if(j==5){break}
    j=j+1
  }
}
######
rm(res)
rm(file_name)
#### Species specific neg files

train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  # mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  # mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  # mutate(species_id = as.numeric(as.character(species_id))) %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup() %>%
  mutate(t_min = ifelse(duration < 1.5,t_min - duration/2, t_min)) %>%
  mutate(t_max = ifelse(duration < 1.5, t_max + duration/2, t_max)) %>%
  mutate(duration = t_max - t_min)


j=1
repeat{
  
  train_batch2 <- rbind(train_tp, train_fp) %>%
    group_by(species_id) %>%
    mutate(positive_n = length(which(cate == "true"))) %>%
    group_by(species_id, cate) %>%
    sample_n(., positive_n) %>%
    mutate(group_n = ceiling(seq_along(file_id)/(length(file_id)/12))) %>%
    ungroup() %>%
    group_by(group_n) %>%
    group_split(.)
  
  for(i in 1:length(train_batch2)){
    tmp <- train_batch2[[i]]
    res <- specmaker_v3(tmp, augment = TRUE)
    res <- keras_unmasked_v2(res)
    file_name = paste0("output/unmasked/train_posneg_B", i,"_rep_", j, ".RDS")
    print(file_name)
    saveRDS(res, file_name)
  }
  if(j==5){break}
  j=j+1
  
}

