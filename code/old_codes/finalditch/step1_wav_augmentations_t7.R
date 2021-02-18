library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
require(fields, warn.conflicts = F, quietly = T)
# require(pbmcapply)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

#############################################################
#### read key
#############################################################

keras_whole <- function(data){
  # freq_bin <- data.frame(FreqHz = unique(data$FreqHz),
  #                        FreqBin1 = cut(unique(data$FreqHz), 4, labels = c(1,2,3,4)))
  tmp <- data  %>%
    # left_join(., freq_bin, by="FreqHz") %>%
    group_by(unique_id, FreqHz) %>%
    mutate(zts = 1:n()) %>%
    ungroup() %>%
    # dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + zts ~ FreqHz)
  
  f1 <- tmp %>%
    group_by(unique_id) %>%
    tidyr::nest()
  
  f1 <- f1[sample(1:nrow(f1)),] %>%
    tidyr::unnest(cols = c(data)) %>%
    ungroup()
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x]) %>% dplyr::select(-unique_id, -zts) %>% as.matrix())
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

#############################################################
#### read key
#############################################################

keras_unmasked_v2 <- function(input_data, mix=FALSE, test_type){
  
  print("reading in file")
  tmp <- input_data %>%
    mutate(unique_id = paste0(unique_id,"_", cate))
  
  print("creating keras array")
  
  data_x <- keras_whole(tmp)
  train_data <- data_x[[1]]
  # data_y <- keras_whole(data_y)
  # train_data_y <- data_y[[1]]
  
  ### making label matrix
  if(test_type == "01true"){
    train_lab <- tmp %>%
      # mutate(unique_id = paste0(unique_id, "_",data_type)) %>%
      select(unique_id, species_id, cate) %>%
      distinct(.) %>%
      mutate(cate = ifelse(cate == "true", 1, 0)) %>%
      mutate(species_id = paste0("s",species_id)) %>%
      dcast(unique_id ~species_id, value.var = "cate") %>%
      mutate_all(., function(x) ifelse(is.na(x), 0, x))
    
    # template <- data.frame(matrix(0, ncol=25, nrow=nrow(train_lab)))
    # colnames(template) <- c("unique_id", paste0("s", seq(0,23,1)))
    # train_lab <- cbind(train_lab, template[,!colnames(template) %in% colnames(train_lab)])
    # train_lab <- data.frame(train_lab)[,c(match(colnames(template), colnames(train_lab)))]
    
  } else if(test_type == "01false") {
    train_lab <- tmp %>%
      # mutate(unique_id = paste0(unique_id, "_",data_type)) %>%
      select(unique_id, species_id, cate) %>%
      distinct(.) %>%
      mutate(species_id = paste0("s",species_id,"_",cate)) %>%
      mutate(cate = 1) %>%
      # mutate(species_id = paste0("s",species_id)) %>%
      dcast(unique_id ~species_id, value.var = "cate") %>%
      mutate_all(., function(x) ifelse(is.na(x), 0, x))
    # template <- data.frame(matrix(0, ncol=25, nrow=nrow(train_lab)))
    # colnames(template) <- c("unique_id", paste0("s", seq(0,23,1)))
    
  } else if(test_type == "101") {
    train_lab <- tmp %>%
      # mutate(unique_id = paste0(unique_id, "_",data_type)) %>%
      select(unique_id, species_id, cate) %>%
      distinct(.) %>%
      mutate(cate = ifelse(cate == "true", 1, -1)) %>%
      mutate(species_id = paste0("s",species_id)) %>%
      dcast(unique_id ~species_id, value.var = "cate") %>%
      mutate_all(., function(x) ifelse(is.na(x), 0, x))
  } else if(test_type == "012") {
    train_lab <- tmp %>%
      # mutate(unique_id = paste0(unique_id, "_",data_type)) %>%
      select(unique_id, species_id, cate) %>%
      distinct(.) %>%
      mutate(cate = ifelse(cate == "true", 2, 1)) %>%
      mutate(species_id = paste0("s",species_id)) %>%
      dcast(unique_id ~species_id, value.var = "cate") %>%
      mutate_all(., function(x) ifelse(is.na(x), 0, x))
  } else if(test_type == "102") {
    train_lab <- tmp %>%
      # mutate(unique_id = paste0(unique_id, "_",data_type)) %>%
      select(unique_id, species_id, cate) %>%
      distinct(.) %>%
      mutate(cate = ifelse(cate == "true", 2, 0)) %>%
      mutate(species_id = paste0("s",species_id)) %>%
      dcast(unique_id ~species_id, value.var = "cate") %>%
      mutate_all(., function(x) ifelse(is.na(x), 1, x))
  }
  
  data_labels <- data_x[[2]]
  train_lab <- train_lab[match(data_labels$unique_id, train_lab$unique_id),]
  # train_lab_y <- train_lab_y[match(data_y[[2]]$unique_id, train_lab_y$unique_id),]
  print("exporting")
  return(list(train_data, train_lab))
}

#############################################################
#### read key
#############################################################

rescale <- function(x, newrange=range(x)){
  xrange <- range(x)
  mfac <- (newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  newrange[1]+(x-xrange[1])*mfac
}

#############################################################
#### read key
#############################################################

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

#############################################################
#### read key
#############################################################

augmentations <- function(df1, x, shift_time){
  cut_off = max(df1$zts)
  df1 <- df1 %>%
    mutate(zts = zts + shift_time) %>%
    mutate(zts = ifelse(zts > cut_off, zts - cut_off, zts)) %>%
    arrange(zts) %>%
    mutate(unique_id = paste0(unique_id, "_aug", x))
  
  return(df1)
}

#############################################################
#### read key
#############################################################

reSizeMe <- function(res, zts_dim, freq_dim){
  df1 <- res %>%
    dcast(zts ~ FreqHz, value.var = "value")
  
  df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), freq_dim)))
  
  if(all(is.na(df1[,ncol(df1)]))){
    df1 <- res %>%
      dcast(zts ~ FreqHz, value.var = "value")
    df1 <- df1[,1:(ncol(df1)-1)]
    df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), freq_dim)))
  }
  
  df2 <- data.frame(ResizeMat(df1, c(zts_dim, ncol(df1)))) 
  
  if(length(which(is.na(df2))) > 0){
    df2 <- df1[1:(nrow(df1)-1),]
    df2 <- data.frame(ResizeMat(df2, c(zts_dim, ncol(df2)))) 
  }
  
  df2 <- df2 %>%
    melt(., id.var="X1") %>%
    dplyr::rename(zts = X1, FreqHz = variable) %>%
    mutate(zts = round(zts, digits=1)) %>%
    mutate(value = round(value, digits=3)) %>%
    mutate(unique_id = unique(res$unique_id),
           species_id = unique(res$species_id),
           cate = unique(res$cate))
  
  return(df2)
}

#############################################################
#### read key
#############################################################

specmaker_lean <- function(tmp, x, augment = FALSE){
  
  print(paste0("reading rds ___ ", x, " ___ out of ___ ", nrow(tmp)))
  # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
  res <- specmaker(tmp[x,], low_f = tmp[x,]$f_min, high_f = tmp[x,]$f_max, window_multiplier = 1) %>%
    filter(time > t_min & time < t_max) %>%
    group_by(FreqHz) %>%
    mutate(value = value - mean(value)) %>%
    ungroup() %>%
    mutate(value = round(value, digits=3)) 
  
  # res %>%
  #   ggplot(aes(x=time, y=FreqHz, fill=value)) +
  #   geom_tile() +
  #   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))
  # 
  if(augment == TRUE){
    
    res <- res %>%
      group_by(unique_id, FreqHz) %>%
      arrange(time) %>%
      mutate(zts = 1:n()) %>%
      ungroup() %>%
      select(unique_id, species_id, cate, FreqHz, frame_shift, zts, value) %>%
      ungroup()
    
    if(unique(res$cate) == "false"){
      res <- augmentations(res, x=1, shift_time = round(max(res$zts)*unique(res$frame_shift), digit=0)) %>%
        select(-frame_shift)
    } else {
      original <- unique(res$frame_shift)
      deviations <- seq(original, original + 0.08, 0.02)
      df1 <- data.table::rbindlist(lapply(1:length(deviations), function(x) augmentations(res, x=x, shift_time = round(max(res$zts)*deviations[x], digit=0)) %>%
               select(-frame_shift)))
      res <- rbind(res%>% select(-frame_shift), df1)
    }
    
  } else {
    
    res <- res %>%
      group_by(unique_id, FreqHz) %>%
      arrange(time) %>%
      mutate(zts = 1:n()) %>%
      ungroup() %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()
  }

  res <- res %>%
    group_by(unique_id) %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value <= 0, 0, value)) %>%
    # mutate(value = value + min(value)) %>%
    mutate(value = value/max(value)) %>%
    mutate(value = round(value, digits=3)) %>%
    select(unique_id, species_id, cate, FreqHz, zts, value) %>%
    ungroup()
  
  df2 <- res %>%
    group_by(unique_id) %>%
    group_split()
  
  df2 <- data.table::rbindlist(lapply(1:length(df2), function(x) reSizeMe(df2[[x]], zts_dim=384, freq_dim=96))) 
  
  # df2 %>%
  #   ggplot(aes(x=zts, y=FreqHz, fill=value)) +
  #   geom_tile() +
  #   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))
  
  if(length(which(is.na(df2$value)))>0){
    if(unique(df2$cate) == "false"){
      df2$value <- ifelse(is.na(df2$value), 0, df2$value)
      print("had to fill in nas for false")
    } else if(unique(df2$cate) == "true"){
      stop(print(paste0("there are NAs in true data! sample ", x)))
    }
  }
  
  return(df2)
}

#############################################################
#### read key
#############################################################

specmaker_v4 <- function(tmp, augment = NA, use_multicore = TRUE){
  
  if(augment == TRUE){print("augmenting data")}
  
  if(use_multicore == TRUE){
    df1 <- pbmcapply::pbmclapply(1:nrow(tmp), function(x) specmaker_lean(tmp, x, augment=augment), mc.cores=4)
  } else {
    df1 <- lapply(1:nrow(tmp), function(x) specmaker_lean(tmp, x, augment=augment))
  }
  data.table::rbindlist(df1)
  
}

#############################################################
#### read key
#############################################################

wav_to_matrix <- function(tmp, augment = FALSE, output_type = "01true", subfix=NULL){
  print("starting cohort data")
  cohort = unique(tmp$cohort)
  test = ifelse(unique(tmp$test) == "yes", "test", "train")
  
  # use_cores <- ifelse(table(tmp$cate)[[1]]*2 <= 5000, TRUE, FALSE)
  res <- specmaker_v4(tmp, augment = augment, use_multicore = TRUE)
  if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
  
  for(i in 1:length(output_type)){
    export <- keras_unmasked_v2(res, test_type = output_type[i])
    if(!is.null(subfix)){
      file_name = paste0("output/finalditch/cohort_t6/", cohort, "_" , test, "_", output_type[i],"_", subfix,".rds")
    } else {
      file_name = paste0("output/finalditch/cohort_t6/", cohort, "_" , test, "_", output_type[i], ".rds")
    }
    print(file_name)
    saveRDS(export, file_name)
  }
  print("############### DONE #################### ")
}

#############################################################
#### read key
#############################################################

cohort1 <- readRDS("output/finalized/cohort1_key.rds") %>% ungroup()
cohort1_test <- cohort1 %>% dplyr::filter(test == "yes")
# cohort1_train <- cohort1 %>% dplyr::filter(test == "no")
# 
cohort2 <- readRDS("output/finalized/cohort2_key.rds") %>% ungroup()
cohort2_test <- cohort2 %>% dplyr::filter(test == "yes")
# cohort2_train <- cohort2 %>% dplyr::filter(test == "no")
# 
cohort3 <- readRDS("output/finalized/cohort3_key.rds") %>% ungroup()
cohort3_test <- cohort3 %>% dplyr::filter(test == "yes")
# cohort3_train <- cohort3 %>% dplyr::filter(test == "no")

#############################################################
#### training and testing by cohort
#############################################################

# train_batch_maker <- function(tmp, x){
#   frame_shifts <- seq(0, 0.9, 0.1)
#   frame_id <- (seq(0, 9, 1))
#   data.frame(tmp, frame_shift = frame_shifts[x], frame_id = frame_id[x]) %>%
#     mutate(tmp_id = unique_id) %>%
#     mutate(unique_id = paste0(unique_id, "_f", frame_id))}
# 
# cohort1_train <- data.table::rbindlist(lapply(1:10, function(x) train_batch_maker(cohort1_train, x))) %>%
#   group_by(tmp_id) %>%
#   arrange(tmp_id) %>%
#   mutate(random_group = sample(1:n())) %>%
#   ungroup() %>%
#   group_by(random_group) %>%
#   group_split(.)
# saveRDS(cohort1_train, "output/finalized/cohort1_key_augmented.rds")
# 
# cohort2_train <- data.table::rbindlist(lapply(1:10, function(x) train_batch_maker(cohort2_train, x))) %>%
#   group_by(tmp_id) %>%
#   arrange(tmp_id) %>%
#   mutate(random_group = sample(1:n())) %>%
#   ungroup() %>%
#   group_by(random_group) %>%
#   group_split(.)
# saveRDS(cohort2_train, "output/finalized/cohort2_key_augmented.rds")
# 
# cohort3_train <- data.table::rbindlist(lapply(1:10, function(x) train_batch_maker(cohort3_train, x))) %>%
#   group_by(tmp_id) %>%
#   arrange(tmp_id) %>%
#   mutate(random_group = sample(1:n())) %>%
#   ungroup() %>%
#   group_by(random_group) %>%
#   group_split(.)
# saveRDS(cohort3_train, "output/finalized/cohort3_key_augmented.rds")

cohort1_train <- readRDS("output/finalized/cohort1_key_augmented.rds")
cohort2_train <- readRDS("output/finalized/cohort2_key_augmented.rds")
cohort3_train <- readRDS("output/finalized/cohort3_key_augmented.rds")

#############################################################
#### training and testing by cohort
#############################################################
args = commandArgs(trailingOnly=TRUE)

#### COHORT2
if(args[[1]] == "cohort2"){
  print("working on cohort 2")
  if(args[[2]] == 1){
    wav_to_matrix(cohort2_test, output_type=c("01false"), augment=FALSE, subfix="noaug")
  } else if(args[[2]] == 2){
    i=1
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 3){
    i=2
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 4){
    i=3
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 5){
    i=4
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 6){
    i=5
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 7){
    i=6
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 8){
    i=7
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 9){
    i=8
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 10){
    i=9
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 11){
    i=10
    tmp <- cohort2_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  }
}


#### COHORT1
if(args[[1]] == "cohort1"){
  print("working on cohort 1")
  if(args[[2]] == 1){
    wav_to_matrix(cohort1_test, output_type=c("01false"), augment=FALSE, subfix="noaug")
  } else if(args[[2]] == 2){
    i=1
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 3){
    i=2
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 4){
    i=3
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 5){
    i=4
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 6){
    i=5
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 7){
    i=6
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 8){
    i=7
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 9){
    i=8
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 10){
    i=9
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 11){
    i=10
    tmp <- cohort1_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  }
}

#### COHORT3
if(args[[1]] == "cohort3"){
  print("working on cohort 3")
  if(args[[2]] == 1){
    wav_to_matrix(cohort3_test, output_type=c("01false"), augment=FALSE, subfix="noaug")
  } else if(args[[2]] == 2){
    i=1
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 3){
    i=2
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 4){
    i=3
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 5){
    i=4
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 6){
    i=5
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 7){
    i=6
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 8){
    i=7
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 9){
    i=8
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 10){
    i=9
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  } else if(args[[2]] == 11){
    i=10
    tmp <- cohort3_train[[i]]
    wav_to_matrix(tmp, output_type=c("01false"), augment=TRUE, subfix=paste0("augset",i))
  }
}
  