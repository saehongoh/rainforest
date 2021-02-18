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

# ## True positives
# train_tp <- read.csv("data/train_tp.csv") %>% mutate(cate = "true") %>%
#   mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
#   mutate(t_min = round(t_min, digits=4)) %>%
#   mutate(t_max = round(t_max, digits=4)) %>%
#   mutate(duration = round(t_max - t_min, digits=3))
# 
# train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false") %>%
#   mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
#   mutate(t_min = round(t_min, digits=4)) %>%
#   mutate(t_max = round(t_max, digits=4)) %>%
#   mutate(duration = round(t_max - t_min, digits=3))
# 
# train_key <- rbind(train_tp, train_fp) %>%
#   group_by(recording_id) %>%
#   mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
#   ungroup() %>%
#   group_by(species_id) %>%
#   mutate(duration_diff = max(duration) - duration) %>%
#   mutate(lower_bound =  (t_min-duration_diff) < 0) %>%
#   mutate(upper_bound =  (t_max-duration_diff) > 60) %>%
#   mutate(t_min = ifelse(duration <= max(duration) & !lower_bound, t_min - (duration_diff), t_min)) %>%
#   mutate(t_max = ifelse(duration <= max(duration) & lower_bound, t_max + (duration_diff), t_max)) %>%
#   mutate(duration = round(t_max - t_min, digits=2))  %>%
#   select(-duration_diff, -lower_bound, -upper_bound)
# 
# windows <- data.frame(t_min=seq(0,50, 10), t_max=seq(10,60,10), window=paste0(rep("window", 6), seq(1,6,1)))
# require(IRanges)
# rangesA <- IRanges::IRanges(start = windows$t_min, end = windows$t_max, names= windows$window)
# rangesB <- IRanges::IRanges(start = train_key$t_min, end= train_key$t_max, names= train_key$unique_id)
# ov <- IRanges::findOverlaps(rangesB, rangesA, type="within")
# 
# tmp <- data.frame(unique_id = train_key$unique_id[queryHits(ov)],
#                   window = windows$window[subjectHits(ov)],
#                   w_start = windows$t_min[subjectHits(ov)],
#                   w_end = windows$t_max[subjectHits(ov)])  %>%
#   group_by(unique_id) %>% 
#   slice_max(., w_start)
# 
# train_key <- train_key %>%
#   left_join(., tmp, by = "unique_id") %>%
#   mutate(to_shift = ifelse(is.na(window), "yes", "no")) %>%
#   group_by(species_id) %>%
#   mutate(f_min = mean(f_min), f_max = mean(f_max)) %>%
#   ungroup()
# 
# #
# # train_key <- train_key %>%
# #   group_by(species_id) %>%
# #   mutate(duration_diff = max(duration) - duration) %>%
# #   mutate(lower_bound =  (t_min-duration_diff) < 0) %>%
# #   mutate(upper_bound =  (t_max-duration_diff) > 60) %>%
# #   mutate(t_min = ifelse(duration <= max(duration) & !lower_bound, t_min - (duration_diff), t_min)) %>%
# #   mutate(t_max = ifelse(duration <= max(duration) & lower_bound, t_max + (duration_diff), t_max)) %>%
# #   mutate(duration = t_max - t_min) %>%
# #   ungroup() %>%
# #   select(-duration_diff, -lower_bound, -upper_bound)
# 
# # train_key %>%
# #   select(species_id, f_min, f_max) %>%
# #   distinct(.) %>%
# #   melt(., id.var = "species_id") %>%
# #   ggplot(aes(x=as.factor(species_id), y=value)) +
# #   geom_hline(yintercept=5700) +
# #   geom_hline(yintercept=4800, col="red") +
# #   geom_line() +
# #   geom_point() +
# #   facet_wrap(~ value < 5900, scale="free_x")
# 
# # train_key %>%
# #   select(species_id, f_min, f_max) %>%
# #   distinct(.) %>%
# #   melt(., id.var = "species_id") %>%
# #   ggplot(aes(x=as.factor(species_id), y=value)) +
# #   geom_hline(yintercept=5900) +
# #   geom_hline(yintercept=4600, col="red") +
# #   geom_line() +
# #   geom_point() +
# #   facet_wrap(~ value < 4600, scale="free_x")
# 
# train_key <- train_key %>%
#   mutate(cohort = ifelse(f_max < 5900, "cohort1", "cohort2")) %>%
#   mutate(f_min = ifelse(cohort == "cohort1", 0, 4600)) %>%
#   mutate(f_max = ifelse(cohort == "cohort1", 5900, 14000))
# 
# ########
# ### Note to self: put species 10 into both cohorts. Currently in cohort 2
# ########
# 
# # res <- specmaker(train_key[5,], low_f = 50, high_f = 14000, window_multiplier = 1)  %>%
# #   filter(time > w_start & time < w_end) %>%
# #   # filter(FreqHz > f_min & FreqHz < f_max) %>%
# #   group_by(FreqHz) %>%
# #   mutate(value = value - mean(value)) %>%
# #   ungroup() %>%
# #   mutate(value = round(value, digits=3))
# #
# # res %>%
# #   ggplot(aes(x=time, y=FreqHz, fill=value)) +
# #   geom_tile() +
# #   geom_hline(yintercept=5700) +
# #   geom_hline(yintercept=4600, col="red") +
# #   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))
# 
# cohort1 <- train_key %>%
#   filter(cohort == "cohort1" | species_id == 10) %>%
#   mutate(f_min = min(f_min), f_max = min(f_max))
# 
# cohort2 <- train_key %>%
#   filter(cohort == "cohort2")
# 
# #### PARSE TESTING SET
# 
# cohort1 <- cohort1[sample(1:nrow(cohort1)),]
# cohort2 <- cohort2[sample(1:nrow(cohort2)),]
# 
# cohort1_test <- cohort1 %>%
#   group_by(species_id, cate) %>%
#   sample_n(., 5) %>%
#   mutate(test = "yes")
# 
# cohort1_train <- cohort1 %>%
#   filter(!(unique_id %in% cohort1_test$unique_id)) %>%
#   mutate(test = "no")
# 
# cohort2_test <- cohort2 %>%
#   group_by(species_id, cate) %>%
#   sample_n(., 5) %>%
#   mutate(test = "yes")
# 
# cohort2_train <- cohort2 %>%
#   filter(!(unique_id %in% cohort2_test$unique_id)) %>%
#   mutate(test = "no")
# 
# saveRDS(rbind(cohort1_test, cohort1_train), "output/lastditch/cohort1_samplekey.rds")
# saveRDS(rbind(cohort2_test, cohort2_train), "output/lastditch/cohort2_samplekey.rds")

cohort1 <- readRDS("output/lastditch/cohort1_samplekey.rds")
cohort1_test <- cohort1 %>% filter(test == "yes") 
cohort1_train <- cohort1 %>% filter(test == "no")

cohort2 <- readRDS("output/lastditch/cohort2_samplekey.rds")
cohort2_test <- cohort2 %>% filter(test == "yes") 
cohort2_train <- cohort2 %>% filter(test == "no")

#############################################################
#### Species specific or batch wise positives and negatives
#############################################################

specmaker_v4species <- function(tmp, augment = FALSE, use_multicore = TRUE, multi=1){
  
  specmaker_v2species <- function(tmp, x, augment = FALSE, multi=1){
    
    print(paste0("reading rds ___ ", x, " ___ out of ___ ", nrow(tmp)))
    # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
    res <- specmaker(tmp[x,], low_f = tmp[x,]$f_min, high_f = tmp[x,]$f_max, window_multiplier = 1)  
    
    if(!is.na(tmp[x,]$window)){
      # print("TRUE")
      res <- res %>%
        filter(time > w_start & time < w_end) %>%
        group_by(FreqHz) %>%
        mutate(value = value - mean(value)) %>%
        ungroup() %>%
        mutate(value = round(value, digits=3))
      
    } else {
      # print("FALSE")
      closest <- seq(10, 50, 10)
      shift_to <- closest[which.min(abs(closest - tmp[x,]$t_min))]
      shift_time = shift_to - tmp[x,]$t_min 
      
      if(shift_time > 0){
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time > 60, time - 60, time)) %>%
          mutate(w_start = shift_to, w_end = shift_to + 10)
      } else if(shift_time < 0){
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time < 0, time + 60, time)) %>%
          mutate(w_start = shift_to, w_end = shift_to + 10)
      }
      res <- res %>%
        filter(time > w_start & time < w_end) %>%
        group_by(FreqHz) %>%
        mutate(value = value - mean(value)) %>%
        ungroup() %>%
        mutate(value = round(value, digits=3))
    }
    
    # res %>%
    #   ggplot(aes(x=time, y=FreqHz, fill=value)) +
    #   geom_tile() +
    #   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))
    
    if(augment == TRUE & unique(res$cate) == "true"){
      
      augmentation <- function(x, shift_time){
        # add_time = res$t_min
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time > t_max, time - duration, time)) %>%
          mutate(unique_id = paste0(unique_id, "_a", x))
        
        corrupt <- rbinom(nrow(res), 1, sample(seq(0.10, .25, 0.05), 1))
        corrupt <- as.logical(corrupt)
        noise <- rnorm(sum(corrupt),mean(res$value),sd(res$value)/sample(seq(5, 10, 0.5), 1))
        res$value[corrupt] <- round(res$value[corrupt] + noise, digits =3)
        
        return(res)
      }
      
      deviations = sample(seq(0, unique(res$duration)*.50, unique(res$duration)/40), multi)
      
      res <- data.table::rbindlist(lapply(1:multi, function(x) augmentation(x, deviations[x])))
      
    } else if(augment == FALSE){
      res <- res
    }
    
    res <- res %>%
      group_by(unique_id, FreqHz) %>%
      arrange(time) %>%
      mutate(zts = 1:n()) %>%
      ungroup() %>%
      # group_by(unique_id, zts) %>%
      # mutate(value = value - mean(value)) %>%
      # ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value)) %>%
      # mutate(value = value + min(value)) %>%
      mutate(value = value/max(value)) %>%
      mutate(value = round(value, digits=3)) %>%
      select(unique_id, species_id, cate, FreqHz, zts, value) %>%
      ungroup()
    
    # res %>%
    # # filter(unique_id == "45c356538_1_b1_a2") %>%
    #   ggplot(aes(x=zts, y=FreqHz, fill=value)) +
    #   geom_tile() +
    #   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red")) +
    #   theme_bw()
    
    reSizeMe <- function(res){
      df1 <- res %>%
        dcast(zts ~ FreqHz, value.var = "value")
      
      df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 56)))
      
      if(all(is.na(df1[,ncol(df1)]))){
        df1 <- res %>%
          dcast(zts ~ FreqHz, value.var = "value")
        df1 <- df1[,1:(ncol(df1)-1)]
        df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 56)))
      }
      
      df2 <- data.frame(ResizeMat(df1, c(560, ncol(df1)))) 
      
      if(length(which(is.na(df2))) > 0){
        df2 <- df1[1:(nrow(df1)-1),]
        df2 <- data.frame(ResizeMat(df2, c(560, ncol(df2)))) 
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
    
    df2 <- res %>%
      group_by(unique_id) %>%
      group_split()
    
    df2 <- data.table::rbindlist(lapply(1:length(df2), function(x) reSizeMe(df2[[x]])))
    
    # df2 %>%
    #   # filter(unique_id == "45c356538_1_b1_a1") %>%
    #   ggplot(aes(x=zts, y=FreqHz, fill=value)) +
    #   geom_tile() +
    #   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))
    
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
#### Test files
#############################################################

##### Cohort 1
print(paste0("starting cohort 1"))
tmp <- cohort1_test
multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}

export <- keras_unmasked_v2(res, test_type = "01true")
file_name = paste0("output/lastditch/cohort1_test_01true.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "01false")
file_name = paste0("output/lastditch/cohort1_test_01false.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "101")
file_name = paste0("output/lastditch/cohort1_test_101.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "012")
file_name = paste0("output/lastditch/cohort1_test_012.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "102")
file_name = paste0("output/lastditch/cohort1_test_102.RDS")
print(file_name)
saveRDS(export, file_name)

##### Cohort 2
print(paste0("starting cohort 2"))
tmp <- cohort2_test
multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}

export <- keras_unmasked_v2(res, test_type = "01true")
file_name = paste0("output/lastditch/cohort2_test_01true.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "01false")
file_name = paste0("output/lastditch/cohort2_test_01false.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "101")
file_name = paste0("output/lastditch/cohort2_test_101.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "012")
file_name = paste0("output/lastditch/cohort2_test_012.RDS")
print(file_name)
saveRDS(export, file_name)

export <- keras_unmasked_v2(res, test_type = "102")
file_name = paste0("output/lastditch/cohort2_test_102.RDS")
print(file_name)
saveRDS(export, file_name)

# readRDS("output/lastditch/cohort2_test.RDS")

# #############################################################
# #### Train files 
# #############################################################
# 
# ##### Cohort 1
# print(paste0("starting cohort 1"))
# tmp <- cohort1_train %>%
#   group_by(species_id) %>%
#   mutate(n_true = length(which(cate == "true"))) %>%
#   group_by(species_id, cate) %>%
#   sample_n(., floor(n_true/2))
# 
# multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
# res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
# if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
# 
# export <- keras_unmasked_v2(res, test_type = "01true")
# file_name = paste0("output/lastditch/cohort1_train_01true_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "01false")
# file_name = paste0("output/lastditch/cohort1_train_01false_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "101")
# file_name = paste0("output/lastditch/cohort1_train_101_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "012")
# file_name = paste0("output/lastditch/cohort1_train_012_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "102")
# file_name = paste0("output/lastditch/cohort1_train_102_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# ##### Cohort 2
# print(paste0("starting cohort 2"))
# tmp <- cohort2_train %>%
#   group_by(species_id, cate) %>%
#   sample_n(., 20)
# 
# multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
# res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
# if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
# 
# export <- keras_unmasked_v2(res, test_type = "01true")
# file_name = paste0("output/lastditch/cohort2_train_01true_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "01false")
# file_name = paste0("output/lastditch/cohort2_train_01false_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "101")
# file_name = paste0("output/lastditch/cohort2_train_101_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "012")
# file_name = paste0("output/lastditch/cohort2_train_012_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)
# 
# export <- keras_unmasked_v2(res, test_type = "102")
# file_name = paste0("output/lastditch/cohort2_train_102_n20.RDS")
# print(file_name)
# saveRDS(export, file_name)


#############################################################
#### Species specific test files
#############################################################

### COHORT 1

species_list <- unique(cohort1_test$species_id)

for(i in 1:length(species_list)){
  print(paste0("starting ____ s", species_list[i]))
  tmp <- cohort1_test %>%
    filter(species_id == species_list[i])

  multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])

  res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
  if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}

  export <- keras_unmasked_v2(res, test_type = "01true")
  file_name = paste0("output/lastditch/species/cohort1_test_", species_list[i], "_01true.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "01false")
  file_name = paste0("output/lastditch/species/cohort1_test_", species_list[i], "_01false.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "101")
  file_name = paste0("output/lastditch/species/cohort1_test_", species_list[i], "_101.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "012")
  file_name = paste0("output/lastditch/species/cohort1_test_", species_list[i], "_012.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "102")
  file_name = paste0("output/lastditch/species/cohort1_test_", species_list[i], "_102.RDS")
  print(file_name)
  saveRDS(export, file_name)
}

### COHORT 2

species_list <- unique(cohort2_test$species_id)
for(i in 1:length(species_list)){
  print(paste0("starting ____ s", species_list[i]))
  tmp <- cohort2_test %>%
    filter(species_id == species_list[i])

  multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])

  res <- specmaker_v4species(tmp, augment = FALSE, multi=multiplier, use_multicore = TRUE)
  if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}

  export <- keras_unmasked_v2(res, test_type = "01true")
  file_name = paste0("output/lastditch/species/cohort2_test_", species_list[i], "_01true.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "01false")
  file_name = paste0("output/lastditch/species/cohort2_test_", species_list[i], "_01false.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "101")
  file_name = paste0("output/lastditch/species/cohort2_test_", species_list[i], "_101.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "012")
  file_name = paste0("output/lastditch/species/cohort2_test_", species_list[i], "_012.RDS")
  print(file_name)
  saveRDS(export, file_name)

  export <- keras_unmasked_v2(res, test_type = "102")
  file_name = paste0("output/lastditch/species/cohort2_test_", species_list[i], "_102.RDS")
  print(file_name)
  saveRDS(export, file_name)
}


#############################################################
#### Species specific positives and negatives and other species
#############################################################
# 
# ### COHORT 1
# 
# train_key <- cohort1_train
# 
# train_batches <-  train_key %>%
#   group_by(species_id) %>%
#   group_split(.)
# 
# for(i in 1:length(train_batches)){
#   tmp <- train_batches[[i]] %>%
#     mutate(unique_id = paste0(unique_id, "_b1"))
# 
#   species <- unique(tmp$species_id)
#   print(paste0("working on ____ s", species))
# 
#   add_tmp <- train_key %>%
#     filter(!(species_id == species)) %>%
#     filter(cate == "true") %>%
#     group_by(species_id, cate) %>%
#     sample_n(., 20) %>%
#     mutate(cate = "false") %>%
#     mutate(unique_id = paste0(unique_id, "_b2"))
# 
#   tmp <- rbind(tmp, add_tmp)
# 
#   multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
#   print(multiplier)
#   print(table(tmp$cate)[[1]]*2)
#   use_cores <- ifelse(table(tmp$cate)[[1]]*2 >= 900, FALSE, TRUE)
# 
#   res <- specmaker_v4species(tmp, augment = TRUE, multi=multiplier, use_multicore = use_cores)
# 
#   if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
# 
#   # res <- keras_unmasked_v2(res)
#   if(length(which(is.na(res[[1]])))>0){stop("there are NAs in data!")}
# 
#   export <- keras_unmasked_v2(res, test_type = "01true")
#   file_name = paste0("output/lastditch/species/cohort1_train_", species, "_01true.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "01false")
#   file_name = paste0("output/lastditch/species/cohort1_train_", species, "_01false.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "101")
#   file_name = paste0("output/lastditch/species/cohort1_train_", species, "_101.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "012")
#   file_name = paste0("output/lastditch/species/cohort1_train_", species, "_012.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "102")
#   file_name = paste0("output/lastditch/species/cohort1_train_", species, "_102.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
# }
# 
# 
# ### COHORT 2
# 
# train_key <- cohort2_train
# 
# train_batches <-  train_key %>%
#   group_by(species_id) %>%
#   group_split(.)
# 
# for(i in 1:length(train_batches)){
#   tmp <- train_batches[[i]] %>%
#     mutate(unique_id = paste0(unique_id, "_b1"))
#   
#   species <- unique(tmp$species_id)
#   print(paste0("working on ____ s", species))
#   
#   add_tmp <- train_key %>%
#     filter(!(species_id == species)) %>%
#     filter(cate == "true") %>%
#     group_by(species_id, cate) %>%
#     sample_n(., 20) %>%
#     mutate(cate = "false") %>%
#     mutate(unique_id = paste0(unique_id, "_b2"))
#   
#   tmp <- rbind(tmp, add_tmp)
#   
#   multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
#   print(multiplier)
#   print(table(tmp$cate)[[1]]*2)
#   use_cores <- ifelse(table(tmp$cate)[[1]]*2 >= 900, FALSE, TRUE)
#   
#   res <- specmaker_v4species(tmp, augment = TRUE, multi=multiplier, use_multicore = use_cores)
#   
#   if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
#   
#   # res <- keras_unmasked_v2(res)
#   if(length(which(is.na(res[[1]])))>0){stop("there are NAs in data!")}
#   
#   export <- keras_unmasked_v2(res, test_type = "01true")
#   file_name = paste0("output/lastditch/species/cohort2_train_", species, "_01true.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "01false")
#   file_name = paste0("output/lastditch/species/cohort2_train_", species, "_01false.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "101")
#   file_name = paste0("output/lastditch/species/cohort2_train_", species, "_101.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "012")
#   file_name = paste0("output/lastditch/species/cohort2_train_", species, "_012.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
#   
#   export <- keras_unmasked_v2(res, test_type = "102")
#   file_name = paste0("output/lastditch/species/cohort2_train_", species, "_102.RDS")
#   print(file_name)
#   saveRDS(export, file_name)
# }
# # readRDS("output/resized/train_s0.RDS")[[1]]
