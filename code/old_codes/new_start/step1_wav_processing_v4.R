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
#   mutate(species_id = ifelse(species_id == 23 & songtype_id == 4, species_id+0.4, species_id)) %>%
#   group_by(species_id) %>%
#   mutate(f_min = mean(f_min),
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

train_key <- readRDS("output/resized_samplekey.rds") %>%
  group_by(species_id) %>%
  mutate(duration_diff = max(duration) - duration) %>%
  mutate(t_min = ifelse(duration <= max(duration), t_min - (duration_diff/2), t_min))  %>%
  mutate(t_max= ifelse(duration <= max(duration), t_max + (duration_diff/2), t_max)) %>%
  mutate(t_min = floor(t_min)) %>%
  mutate(t_max = ceiling(t_max)) %>%
  mutate(duration = t_max - t_min) %>%
  ungroup() %>%
  select(-duration_diff)
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
#### Species specific or batch wise positives and negatives
#############################################################

specmaker_v4species <- function(tmp, augment = FALSE, use_multicore = TRUE, multi=1){
  
  specmaker_v2species <- function(tmp, x, augment = FALSE, multi=1){
    
    print(paste0("reading rds ___ ", x, " ___ out of ___ ", nrow(tmp)))
    # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
    res <- specmaker(tmp[x,], low_f = 50, high_f = 14000, window_multiplier = 1)  %>%
      filter(time > t_min & time < t_max) %>%
      filter(FreqHz > f_min & FreqHz < f_max) %>%
      group_by(FreqHz) %>%
      mutate(value = value - mean(value)) %>%
      ungroup() %>%
      mutate(value = round(value, digits=3))
    
    # if(unique(res$t_min) < 30 & unique(res$f_max >= 30)){
    #   res %>%
    #     mutate(t_min = t_min - duration) %>%
    #     mutate(t_max = t_max - duration) %>%
    #     mutate(time = time - duration) %>%
    #     mutate(time = ifelse(time > 60, time + duration, time)) 
    # }
    # 
    res %>%
      ggplot(aes(x=time, y=FreqHz, fill=value)) +
      geom_tile() +
      scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))
    
    if(augment == TRUE & unique(res$cate) == "true"){
      
      augmentation <- function(x, shift_time){
          # add_time = res$t_min
          res <- res %>%
            mutate(time = time + shift_time) %>%
            mutate(time = ifelse(time > t_max, time - duration, time)) %>%
            mutate(unique_id = paste0(unique_id, "_a", x))
          
          corrupt <- rbinom(nrow(res), 1, sample(seq(0.25, .40, 0.05), 1))
          corrupt <- as.logical(corrupt)
          noise <- rnorm(sum(corrupt),mean(res$value),sd(res$value)/sample(seq(2, 3, 0.5), 1))
          res$value[corrupt] <- res$value[corrupt] + noise
          
          return(res)
      }
      
      deviations = sample(seq(0, unique(res$duration), unique(res$duration)/40), multi)
      
      res <- data.table::rbindlist(lapply(1:multi, function(x) augmentation(x, deviations[x])))
      
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
      mutate(value = round(value, digits=3)) %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()
    
    res %>%
      # filter(unique_id == "45c356538_1_b1_a7") %>%
      ggplot(aes(x=zts, y=FreqHz, fill=value)) +
      geom_tile() +
      scale_fill_gradient2(low = ("white"), mid = "white", high = ("red")) +
      theme_bw()
    
    reSizeMe <- function(res){
      df1 <- res %>%
        dcast(zts ~ FreqHz, value.var = "value")
      
      df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 45)))
      
      if(all(is.na(df1[,ncol(df1)]))){
        df1 <- res %>%
          dcast(zts ~ FreqHz, value.var = "value")
        df1 <- df1[,1:(ncol(df1)-1)]
        df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 45)))
      }

      # df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 45)))
      
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
    
    # df2 %>%
    #   filter(unique_id == "45c356538_1_b1_a7") %>%
    #   ggplot(aes(x=zts, y=FreqHz, col=value)) +
    #   geom_tile() +
    #   scale_color_gradient2(low = ("white"), mid = "white", high = ("red"))
    if(length(which(is.na(df2$value)))>0){
      if(unique(df2$cate) == "false"){
        df2$value <- ifelse(is.na(df2$value), 0, df2$value)
        print("had to fill in nas for false")
      } else if(unique(df2$cate) == "true"){
      stop(print(paste0("there are NAs in true data! sample ",x)))
      }
      }
    
    return(df2)
  }
  
  if(augment == TRUE){print("augmenting data")}
  
  if(use_multicore == TRUE){
    df1 <- pbmcapply::pbmclapply(1:nrow(tmp), function(x) specmaker_v2species(tmp, x, augment=augment, multi=multi), mc.cores=4)
  } else {
    df1 <- lapply(1:nrow(tmp), function(x) specmaker_v2species(tmp, x, augment=augment, multi=multi))
  }
  data.table::rbindlist(df1)
}

#############################################################
#### Species specific test files
#############################################################

species_list <- unique(test_key$species_id)
species_list <- species_list[18:length(species_list)]

# for(i in 1:length(species_list)){
#   tmp <- test_key %>%
#     filter(species_id == species_list[i])
# 
#   multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
# 
#   res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
#   if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
#   res <- keras_unmasked_v2(res)
#   file_name = paste0("output/january/test_NoAug_s", unique(tmp$species_id), ".RDS")
#   print(file_name)
#   saveRDS(res, file_name)
# }

for(i in 1:length(species_list)){
  tmp <- test_key %>%
    filter(species_id == species_list[i])

  add_tmp <- test_key %>%
    filter((species_id != unique(tmp$species_id))) %>%
    filter(cate == "true") %>%
    group_by(species_id, cate) %>%
    sample_n(., 5) %>%
    mutate(cate = "false")

  tmp <- rbind(tmp, add_tmp)

  multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
  use_cores <- ifelse(table(tmp$cate)[[1]]*2 >= 900, FALSE, TRUE)
  # multiplier = 2

  res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = use_cores)

  if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
  res <- keras_unmasked_v2(res)
  file_name = paste0("output/january/test_withothers_s", species_list[i], ".RDS")
  print(file_name)
  saveRDS(res, file_name)
}

#############################################################
#### Species specific positives and negatives
#############################################################

train_batches <-  train_key %>%
  group_by(species_id) %>%
  group_split(.)

for(i in 1:length(train_batches)){
  tmp <- train_batches[[i]] %>%
    mutate(unique_id = paste0(unique_id, "_b1"))

  species <- unique(tmp$species_id)
  print(paste0("working on ____ ", species))

  add_tmp <- train_key %>%
    filter(!(species_id == species)) %>%
    filter(cate == "true") %>%
    group_by(species_id, cate) %>%
    sample_n(., 20) %>%
    mutate(cate = "false") %>%
    mutate(unique_id = paste0(unique_id, "_b2"))

  tmp <- rbind(tmp, add_tmp)

  multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
  print(multiplier)
  print(table(tmp$cate)[[1]]*2)
  use_cores <- ifelse(table(tmp$cate)[[1]]*2 >= 900, FALSE, TRUE)

  res <- specmaker_v4species(tmp, augment = TRUE, multi=multiplier, use_multicore = use_cores)

  if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}

  res <- keras_unmasked_v2(res)
  if(length(which(is.na(res[[1]])))>0){stop("there are NAs in data!")}

  file_name = paste0("output/january/train_s", species, ".RDS")
  print(file_name)
  saveRDS(res, file_name)
}

# readRDS("output/resized/train_s0.RDS")[[1]]

