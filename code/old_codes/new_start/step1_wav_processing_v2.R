library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
require(fields, warn.conflicts = F, quietly = T)
# require(pbmcapply)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

keras_whole <- function(data){
  # freq_bin <- data.frame(FreqHz = unique(data$FreqHz), 
  #                        FreqBin1 = cut(unique(data$FreqHz), 4, labels = c(1,2,3,4)))
  tmp <- data  %>%
    # left_join(., freq_bin, by="FreqHz") %>%
    group_by(unique_id, FreqHz) %>%
    mutate(zts = 1:n()) %>%
    ungroup()
  
  f1 <- tmp %>%
    # dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>% dplyr::select(-unique_id, -zts) %>% as.matrix())
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
  tmp <- input_data %>%
    mutate(unique_id = paste0(unique_id,"_", cate))
  
  print("creating keras array")
  
  data_x <- keras_whole(tmp)
  train_data <- data_x[[1]]
  # data_y <- keras_whole(data_y)
  # train_data_y <- data_y[[1]]
  
  ### making label matrix
  train_lab <- tmp %>%
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

rescale <- function(x, newrange=range(x)){
  xrange <- range(x)
  mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  newrange[1]+(x-xrange[1])*mfac
}

ResizeMat <- function(mat, ndim=dim(mat)){
  # input object
  odim <- dim(mat)
  obj <- list(x= 1:odim[1], y=1:odim[2], z= mat)
  
  # output object
  ans <- matrix(NA, nrow=ndim[1], ncol=ndim[2])
  ndim <- dim(ans)
  
  # rescaling
  ncord <- as.matrix(expand.grid(seq_len(ndim[1]), seq_len(ndim[2])))
  loc <- ncord
  loc[,1] = rescale(ncord[,1], c(1,odim[1]))
  loc[,2] = rescale(ncord[,2], c(1,odim[2]))
  
  # interpolation
  ans[ncord] <- interp.surface(obj, loc)
  
  ans
}

specmaker_v3 <- function(tmp, augment = FALSE){
  
  specmaker_v2 <- function(tmp, x, augment = FALSE){
    print(paste0("reading rds ___ ", x))
    # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
    res <- specmaker(tmp[x,], low_f = 50, high_f = 14000, window_multiplier = 3)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() 
    
    if(augment == TRUE){
      # print("augmenting data")
      if(unique(res$t_min) > 30){
        shift_time = -sample(seq(13,28,5),1)
        add_time = 60
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time < 0, time + add_time, time))
      } else if(unique(res$t_min) < 30){
        shift_time = sample(seq(13,28,5),1)
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
      group_by(zts) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value)) %>%
      mutate(value = value/max(value)) %>%
      mutate(value = round(value, digits=3))
    
    res <- res %>%
      filter(FreqHz > f_min & FreqHz < f_max) %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()
    
    df1 <- res %>%
      dcast(zts ~ FreqHz, value.var = "value")
    df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 50)))
    df1 <- data.frame(ResizeMat(df1, c(nrow(df1)/1.5, ncol(df1)))) %>%
      melt(., id.var="X1") %>%
      rename(zts = X1, FreqHz = variable) %>%
      mutate(zts = round(zts, digits=1)) %>%
      mutate(value = round(value, digits=3)) %>%
      mutate(unique_id = unique(res$unique_id),
             species_id = unique(res$species_id),
             cate = unique(res$cate))
    
    return(df1)
      
  }
  if(augment == TRUE){print("augmenting data")}
  df1 <- lapply(1:nrow(tmp), function(x) specmaker_v2(tmp, x, augment=augment))
  data.table::rbindlist(df1)
}

specmaker_v4 <- function(tmp, augment = FALSE){
  
  specmaker_v2 <- function(tmp, x, augment = FALSE){
    print(paste0("reading rds ___ ", x))
    # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
    res <- specmaker(tmp[x,], low_f = 50, high_f = 14000, window_multiplier = 3)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() 
    
    if(augment == TRUE){
      # print("augmenting data")
      if(unique(res$t_min) > 30){
        shift_time = -sample(seq(13,28,5),1)
        add_time = 60
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time < 0, time + add_time, time))
      } else if(unique(res$t_min) < 30){
        shift_time = sample(seq(13,28,5),1)
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
      group_by(zts) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value)) %>%
      mutate(value = value/max(value)) %>%
      mutate(value = round(value, digits=3))
    
    res <- res %>%
      filter(FreqHz > f_min & FreqHz < f_max) %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()
    
    df1 <- res %>%
      dcast(zts ~ FreqHz, value.var = "value")
    df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 50)))
    
    df1 <- data.frame(ResizeMat(df1, c(nrow(df1)/1.5, ncol(df1)))) %>%
      melt(., id.var="X1") %>%
      rename(zts = X1, FreqHz = variable) %>%
      mutate(zts = round(zts, digits=1)) %>%
      mutate(value = round(value, digits=3)) %>%
      mutate(unique_id = unique(res$unique_id),
             species_id = unique(res$species_id),
             cate = unique(res$cate))
    
    return(df1)
    
  }
  
  if(augment == TRUE){print("augmenting data")}
  df1 <- pbmcapply::pbmclapply(1:nrow(tmp), function(x) specmaker_v2(tmp, x, augment=augment), mc.cores=4)
  data.table::rbindlist(df1)
}

## True positives
# train_tp <- read.csv("data/train_tp.csv") %>% mutate(cate = "true") %>%
#   mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
#   mutate(t_min = round(t_min, digits=4)) %>%
#   mutate(t_max = round(t_max, digits=4)) %>%
#   mutate(duration = round(t_max - t_min, digits=3)) %>%
#   group_by(recording_id) %>%
#   mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
#   ungroup()
# 
# train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false") %>%
#   mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
#   mutate(t_min = round(t_min, digits=4)) %>%
#   mutate(t_max = round(t_max, digits=4)) %>%
#   mutate(duration = round(t_max - t_min, digits=3)) %>%
#   group_by(recording_id) %>%
#   mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
#   ungroup()
# 
# train_key <- rbind(train_tp, train_fp)  %>%
#     group_by(species_id) %>%
#     mutate(f_min = mean(f_min),
#            f_max = mean(f_max)) %>%
#   ungroup()
# 
# train_key <- train_key[sample(1:nrow(train_key)),]
# 
# test_key <- train_key %>%
#   group_by(species_id, cate) %>%
#   sample_n(., 5) %>%
#   mutate(test = "yes")
# 
# train_key <- train_key %>%
#   filter(!(unique_id %in% test_key$unique_id)) %>%
#   mutate(test = "no")
# 
# saveRDS(rbind(train_key, test_key), "output/resized_samplekey.rds")

train_key <- readRDS("output/resized_samplekey.rds")
test_key <- train_key %>% filter(test == "yes") 
train_key <- train_key %>% filter(test == "no")

### test set aside
# tmp <- test_key[sample(1:nrow(test_key)),]
# res <- specmaker_v4(tmp, augment = FALSE)
# if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
# res <- keras_unmasked_v2(res)
# file_name = paste0("output/resized/test_ori.RDS")
# print(file_name)
# saveRDS(res, file_name)
# j=1
# repeat{
#   res <- specmaker_v4(tmp, augment = TRUE)
#   if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
#   res <- keras_unmasked_v2(res)
#   file_name = paste0("output/resized/test_aug", j, ".RDS")
#   print(file_name)
#   saveRDS(res, file_name)
#   if(j==1){break}
#   j=j+1
# }

#############################################################
### All only positives
#############################################################

# tmp <- train_key %>%
#   filter(cate == "true")
# res <- specmaker_v3(tmp, augment = FALSE)
# if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
# res <- keras_unmasked_v2(res)
# file_name = paste0("output/resized/train_posonly_ori.RDS")
# print(file_name)
# saveRDS(res, file_name)
# j=1
# repeat{
#   res <- specmaker_v3(tmp, augment = TRUE)
#   if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
#   res <- keras_unmasked_v2(res)
#   file_name = paste0("output/resized/train_posonly_aug", j+2, ".RDS")
#   print(file_name)
#   saveRDS(res, file_name)
#   if(j==6){break}
#   j=j+1
#   }

#############################################################
#### Species specific or batch wise positives and negatives
#############################################################

specmaker_v4species <- function(tmp, augment = FALSE){

  specmaker_v2species <- function(tmp, x, augment = FALSE){
    print(paste0("reading rds ___ ", x, " ___ out of ___ ", nrow(tmp)))
    # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
    res <- specmaker(tmp[x,], low_f = 50, high_f = 14000, window_multiplier = 3)  %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()

    if(augment == TRUE & unique(res$cate) == "true"){
      
      augmentation <- function(x){
        if(unique(res$t_min) > 30){
          shift_time = -sample(seq(13,28,5),1)
          add_time = 60
          res <- res %>%
            mutate(time = time + shift_time) %>%
            mutate(time = ifelse(time < 0, time + add_time, time)) %>%
            mutate(unique_id = paste0(unique_id, "_a", x))
        } else if(unique(res$t_min) <= 30){
          shift_time = sample(seq(13,28,5),1)
          add_time = -60
          res <- res %>%
            mutate(time = time + shift_time) %>%
            mutate(time = ifelse(time > 60, time + add_time, time)) %>%
            mutate(unique_id = paste0(unique_id, "_a", x))
        }
        return(res)
      }
      
      if(unique(res$species_id) == 23){
        res <- data.table::rbindlist(lapply(1:4, function(x) augmentation(x)))
      } else {
        res <- data.table::rbindlist(lapply(1:6, function(x) augmentation(x)))
      }
    } else if(augment == FALSE){
      res <- res
    }

    res <- res %>%
      group_by(unique_id, FreqHz) %>%
      arrange(time) %>%
      mutate(zts = 1:n()) %>%
      ungroup() %>%
      group_by(unique_id, zts) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value)) %>%
      mutate(value = value/max(value)) %>%
      mutate(value = round(value, digits=3))

    res <- res %>%
      filter(FreqHz > f_min & FreqHz < f_max) %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()

    reSizeMe <- function(res){
      df1 <- res %>%
        dcast(zts ~ FreqHz, value.var = "value")

      df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 50)))
      df1 <- data.frame(ResizeMat(df1, c(nrow(df1)/1.5, ncol(df1)))) %>%
        melt(., id.var="X1") %>%
        rename(zts = X1, FreqHz = variable) %>%
        mutate(zts = round(zts, digits=1)) %>%
        mutate(value = round(value, digits=3)) %>%
        mutate(unique_id = unique(res$unique_id),
               species_id = unique(res$species_id),
               cate = unique(res$cate))
      return(df1)
    }

    df2 <- res %>%
      group_by(unique_id) %>%
      group_split()

    df2 <- data.table::rbindlist(lapply(1:length(df2), function(x) reSizeMe(df2[[x]])))
    return(df2)
  }

  if(augment == TRUE){print("augmenting data")}
  df1 <- pbmcapply::pbmclapply(1:nrow(tmp), function(x) specmaker_v2species(tmp, x, augment=augment), mc.cores=4)
  data.table::rbindlist(df1)
}

#############################################################
#### Species specific positives and negatives
#############################################################

# train_batches <-  train_key %>%
#   group_by(species_id) %>%
#   group_split(.)


# train_batches <- train_batches[24]
# 
# for(i in 1:length(train_batches)){
#   tmp <- train_batches[[i]]
# 
#   # res <- specmaker_v4species(tmp, augment = FALSE)
#   # if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
#   # res <- keras_unmasked_v2(res)
#   # file_name = paste0("output/resized/train_s", unique(tmp$species_id), "_ori.RDS")
#   # print(file_name)
#   # saveRDS(res, file_name)
#   j=1
#   repeat{
#     res <- specmaker_v4species(tmp, augment = TRUE)
#     if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
#     res <- keras_unmasked_v2(res)
#     file_name = paste0("output/resized/train_s", unique(tmp$species_id),"_aug", j+1, ".RDS")
#     print(file_name)
#     saveRDS(res, file_name)
#     if(j==1){break}
#     j=j+1
#   }
# }

#############################################################
#### batchwise positives and negatives
#############################################################

train_batches <- train_key %>%
  group_by(species_id, cate) %>%
  # sample_n(., positive_n) %>%
  mutate(group_n = ceiling(seq_along(file_id)/(length(file_id)/18))) %>%
  group_by(group_n) %>%
  group_split(.)
# lapply(1:length(train_batches), function(x) nrow(train_batches[[x]]))

for(i in 1:length(train_batches)){
  tmp <- train_batches[[i]]
  # table(tmp$species_id, tmp$cate)
  j=1
  repeat{
    res <- specmaker_v4species(tmp, augment = TRUE)
    if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
    res <- keras_unmasked_v2(res)
    file_name = paste0("output/resized/train_posneg_B", i, "_aug", j,".RDS")
    print(file_name)
    saveRDS(res, file_name)
    if(j==2){break}
    j=j+1
  }
  
}

######
# rm(res)
# rm(file_name)
#### Species specific neg files
# 
# train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false") %>%
#   mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
#   mutate(t_min = round(t_min, digits=4)) %>%
#   mutate(t_max = round(t_max, digits=4)) %>%
#   mutate(duration = round(t_max - t_min, digits=3)) %>%
#   # mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
#   # mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
#   # mutate(species_id = as.numeric(as.character(species_id))) %>%
#   group_by(recording_id) %>%
#   mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
#   ungroup() %>%
#   mutate(t_min = ifelse(duration < 1.5,t_min - duration/2, t_min)) %>%
#   mutate(t_max = ifelse(duration < 1.5, t_max + duration/2, t_max)) %>%
#   mutate(duration = t_max - t_min)
# 
# 
# j=1
# repeat{
#   
#   train_batch2 <- rbind(train_tp, train_fp) %>%
#     group_by(species_id) %>%
#     mutate(positive_n = length(which(cate == "true"))) %>%
#     group_by(species_id, cate) %>%
#     sample_n(., positive_n) %>%
#     mutate(group_n = ceiling(seq_along(file_id)/(length(file_id)/12))) %>%
#     ungroup() %>%
#     group_by(group_n) %>%
#     group_split(.)
#   
#   for(i in 1:length(train_batch2)){
#     tmp <- train_batch2[[i]]
#     res <- specmaker_v3(tmp, augment = TRUE)
#     res <- keras_unmasked_v2(res)
#     file_name = paste0("output/unmasked/train_posneg_B", i,"_rep_", j, ".RDS")
#     print(file_name)
#     saveRDS(res, file_name)
#   }
#   if(j==5){break}
#   j=j+1
#   
# }


