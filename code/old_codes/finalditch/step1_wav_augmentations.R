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

hoz_shifter <- function(df1, x, shift_time){
  # add_time = res$t_min
  df1 <- df1 %>%
    mutate(time = time + shift_time) %>%
    mutate(time = ifelse(time > t_max, time - duration, time)) %>%
    mutate(unique_id = paste0(unique_id, "_hozshift", x))
  return(df1)
}

rnorm_corruptor <- function(df1, x){
  corrupt <- rbinom(nrow(df1), 1, sample(seq(0.05, .25, 0.05), 1))
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),mean(df1$value),sd(df1$value)*sample(seq(0.05, 0.25, 0.05), 1))
  df1$value[corrupt] <- round(df1$value[corrupt] + noise, digits =3)
  df1$unique_id <- paste0(df1$unique_id, "_corrupt", x)
  return(df1)
}

hoz_cor<- function(df1, x, shift_time){
  corrupt <- rbinom(nrow(df1), 1, sample(seq(0.05, .25, 0.05), 1))
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),mean(df1$value),sd(df1$value)*sample(seq(0.05, 0.25, 0.05), 1))
  df1$value[corrupt] <- round(df1$value[corrupt] + noise, digits =3)
  
  df1 <- df1 %>%
    mutate(time = time + shift_time) %>%
    mutate(time = ifelse(time > t_max, time - duration, time)) %>%
    mutate(unique_id = paste0(unique_id, "_hozcor", x))
  return(df1)
}

ver_flipper_hoz_shifter <- function(df1, x, shift_time){
  df1 <- df1 %>%
    mutate(time = time + shift_time) %>%
    mutate(time = ifelse(time > t_max, time - duration, time))
  df1 <- df1 %>%
    group_by(time) %>%
    arrange(FreqHz) %>%
    mutate(FreqHz = rev(FreqHz))%>%
    mutate(unique_id = paste0(unique_id, "_verfliphoz", x)) %>%
    ungroup()
}

ver_flipper_corruptor <- function(df1, x){
  corrupt <- rbinom(nrow(df1), 1, sample(seq(0.05, .25, 0.05), 1))
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),mean(df1$value),sd(df1$value)*sample(seq(0.05, 0.25, 0.05), 1))
  df1$value[corrupt] <- round(df1$value[corrupt] + noise, digits =3)
  
  df1 <- df1 %>%
    group_by(time) %>%
    mutate(FreqHz = rev(FreqHz))%>%
    mutate(unique_id = paste0(unique_id, "_verflipcor", x)) %>%
    ungroup()
}

specmaker_v4species <- function(tmp, augment = NA, use_multicore = TRUE, use_augment_only = FALSE, multi=1){
  
  specmaker_v2species <- function(tmp, x, augment = NA, use_augment_only = FALSE, multi=1){
    
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
    
    if(augment == "true"){
      
      if(unique(res$cate) == "true"){
        
        howmany = ceiling(multi/5)
        min_shift = 0.05
        max_shift = unique(res$duration)*0.5
        ###1 horizontal shifts
        deviations = sapply(1:howmany, function(x) sample(seq(min_shift, max_shift, 0.05), 1))
        res_hozshifted <- data.table::rbindlist(lapply(1:howmany, function(x) hoz_shifter(res, x, deviations[x])))
        
        ###2 guassian noise
        res_corr <- data.table::rbindlist(lapply(1:howmany, function(x) rnorm_corruptor(res, x)))
        
        ###3 horizontal shift + guassian noise
        deviations = sapply(1:howmany, function(x) sample(seq(min_shift, max_shift, 0.05), 1))
        res_hozcorr <- data.table::rbindlist(lapply(1:howmany, function(x) hoz_cor(res, x, deviations[x])))
        
        ###4 vertical flip  + horzontal shift
        deviations = sapply(1:howmany, function(x) sample(seq(min_shift, max_shift, 0.05), 1))
        res_verfliphoz <- data.table::rbindlist(lapply(1:howmany, function(x) ver_flipper_hoz_shifter(res, x, deviations[x])))
        
        ###5 vertical flip + guassian noise
        res_verflipcor <- data.table::rbindlist(lapply(1:howmany, function(x) ver_flipper_corruptor(res, x)))
        
        if(use_augment_only == TRUE){
          res <- rbind(res_hozshifted, res_corr, res_hozcorr, res_verfliphoz, res_verflipcor)
        } else {
          res <- rbind(res, res_hozshifted, res_corr, res_hozcorr, res_verfliphoz, res_verflipcor)
        }
      }

    } else if(augment == "both"){
      
      if(unique(res$cate) == "false"){
        howmany = 1
      } else {
        howmany = ceiling(multi/5)
      }
      
      min_shift = 0.05
      max_shift = unique(res$duration)*0.5
      ###1 horizontal shifts
      deviations = sapply(1:howmany, function(x) sample(seq(min_shift, max_shift, 0.05), 1))
      res_hozshifted <- data.table::rbindlist(lapply(1:howmany, function(x) hoz_shifter(res, x, deviations[x])))
      
      ###2 guassian noise
      res_corr <- data.table::rbindlist(lapply(1:howmany, function(x) rnorm_corruptor(res, x)))
      
      ###3 horizontal shift + guassian noise
      deviations = sapply(1:howmany, function(x) sample(seq(min_shift, max_shift, 0.05), 1))
      res_hozcorr <- data.table::rbindlist(lapply(1:howmany, function(x) hoz_cor(res, x, deviations[x])))
      
      ###4 vertical flip  + horzontal shift
      deviations = sapply(1:howmany, function(x) sample(seq(min_shift, max_shift, 0.05), 1))
      res_verfliphoz <- data.table::rbindlist(lapply(1:howmany, function(x) ver_flipper_hoz_shifter(res, x, deviations[x])))
      
      ###5 vertical flip + guassian noise
      res_verflipcor <- data.table::rbindlist(lapply(1:howmany, function(x) ver_flipper_corruptor(res, x)))
      
      if(use_augment_only == TRUE){
        res <- rbind(res_hozshifted, res_corr, res_hozcorr, res_verfliphoz, res_verflipcor)
      } else {
        res <- rbind(res, res_hozshifted, res_corr, res_hozcorr, res_verfliphoz, res_verflipcor)
      }
      
    } else {
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
      mutate(value = ifelse(value <= 0, 0, value)) %>%
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
      
      df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 50)))
      
      if(all(is.na(df1[,ncol(df1)]))){
        df1 <- res %>%
          dcast(zts ~ FreqHz, value.var = "value")
        df1 <- df1[,1:(ncol(df1)-1)]
        df1 <- data.frame(zts = df1$zts, ResizeMat(df1[,2:ncol(df1)], c(nrow(df1), 50)))
      }
      
      df2 <- data.frame(ResizeMat(df1, c(250, ncol(df1)))) 
      
      if(length(which(is.na(df2))) > 0){
        df2 <- df1[1:(nrow(df1)-1),]
        df2 <- data.frame(ResizeMat(df2, c(250, ncol(df2)))) 
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
  
  if(augment %in% c('true','both')){print("augmenting data")}
  
  if(use_multicore == TRUE){
    df1 <- pbmcapply::pbmclapply(1:nrow(tmp), function(x) specmaker_v2species(tmp, x, 
                                                                              augment=augment,
                                                                              use_augment_only = use_augment_only,
                                                                              multi=multi), mc.cores=4)
  } else {
    df1 <- lapply(1:nrow(tmp), function(x) specmaker_v2species(tmp, x, 
                                                               augment=augment, 
                                                               use_augment_only = use_augment_only,
                                                               multi=multi))
  }
  data.table::rbindlist(df1)
}

#############################################################
#### read key
#############################################################

cohort1 <- readRDS("output/finalditch/cohort1_key.rds") %>% ungroup()
cohort1_test <- cohort1 %>% filter(test == "yes") 
cohort1_train <- cohort1 %>% filter(test == "no")

cohort2 <- readRDS("output/finalditch/cohort2_key.rds") %>% ungroup()
cohort2_test <- cohort2 %>% filter(test == "yes") 
cohort2_train <- cohort2 %>% filter(test == "no")
  
cohort3 <- readRDS("output/finalditch/cohort3_key.rds") %>% ungroup()
cohort3_test <- cohort3 %>% filter(test == "yes") 
cohort3_train <- cohort3 %>% filter(test == "no")


#############################################################
#### training and testing by cohort
#############################################################

wav_to_matrix <- function(tmp, augment = FALSE, output_type = "01true", use_augment_only = FALSE, subfix=NULL){
  print("starting cohort data")
  cohort = unique(tmp$cohort)
  test = ifelse(unique(tmp$test) == "yes", "test", "train")
  
  if(augment %in% c('true','both')){
    multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
    print(paste0("augmenting additional data ___ multiplier : ", multiplier))
  } else{
    multiplier = 1
  }
  
  # use_cores <- ifelse(table(tmp$cate)[[1]]*2 <= 5000, TRUE, FALSE)
  res <- specmaker_v4species(tmp, augment = augment, use_augment_only = use_augment_only, 
                             multi=multiplier, 
                             use_multicore = TRUE)
  if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
  
  for(i in 1:length(output_type)){
    export <- keras_unmasked_v2(res, test_type = output_type[i])
    if(!is.null(subfix)){
      file_name = paste0("output/finalditch/cohort/", cohort, "_" , test, "_", output_type[i],"_", subfix,".rds")
    } else {
      file_name = paste0("output/finalditch/cohort/", cohort, "_" , test, "_", output_type[i], ".rds")
    }
    print(file_name)
    saveRDS(export, file_name)
  }
}

wav_to_matrix(cohort2_train,output_type=c("01false"), augment="true", subfix="aug_set1")

# cohort2_train %>%
#   group_by(species_id) %>%
#   mutate(n_true = length(which(cate == "true"))) %>%
#   group_by(species_id, cate) %>%
#   sample_n(., n_true)

wav_to_matrix(cohort2_train[1:10,],output_type=c("01false"),use_augment_only=TRUE, augment="both", subfix="aug_setboth")


wav_to_matrix(cohort2_train,output_type=c("01false"),use_augment_only=TRUE,augment="both",subfix="aug_setboth")

#############################################################
#### Species specific training and testing
#############################################################

wav_to_matrix_species <- function(df, augment = FALSE, add_negs = 0, output_type = "01true", subfix=NULL){
  print("starting species_specific data")
  species_list <- unique(df$species_id)
  cohort = unique(df$cohort)
  test = ifelse(unique(df$test) == "yes", "test", "train")
  
  for(k in 1:length(species_list)){
    print(paste0("starting ____ s", species_list[k]))
    tmp <- df %>%
      filter(species_id == species_list[k])
    
    if(augment == TRUE){
      
      if(add_negs > 0){
        tmp <- df %>%
          filter(species_id != species_list[k]) %>%
          filter(cate == "true") %>%
          sample_n(., add_negs) %>%
          mutate(cate = "false") %>%
          rbind(tmp, .)
        }

      multiplier = floor(table(tmp$cate)[[1]]/table(tmp$cate)[[2]])
      print(paste0("augmenting additional data ___ multiplier : ", multiplier))
      
    } else{
      multiplier = 1
    }
    # use_cores <- ifelse(table(tmp$cate)[[1]]*2 <= 5000, TRUE, FALSE)
    res <- specmaker_v4species(tmp, augment = augment, multi=multiplier, use_multicore = TRUE)
    
    if(length(which(is.na(res$value)))>0){stop("there are NAs in data!")}
    
    for(i in 1:length(output_type)){
      export <- keras_unmasked_v2(res, test_type = output_type[i])
      if(!is.null(subfix)){
        file_name = paste0("output/finalditch/species/", cohort, "_" , test, "_s", species_list[k] ,"_", output_type[i],"_", subfix,".rds")
      } else {
        file_name = paste0("output/finalditch/species/", cohort, "_" , test, "_s", species_list[k] ,"_", output_type[i], ".rds")
      }
      print(file_name)
      saveRDS(export, file_name)
    }
    }
}
  
 
  args = commandArgs(trailingOnly=TRUE)
  if(args[[1]] == 1){
    wav_to_matrix(cohort1_test,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 2){
    wav_to_matrix(cohort2_test,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 3){
    wav_to_matrix(cohort3_test,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 4){
    wav_to_matrix(cohort1_train,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 5){
    wav_to_matrix(cohort2_train,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 6){
    wav_to_matrix(cohort3_train,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 7){
    wav_to_matrix(cohort1_train,output_type=c("01true","01false"),augment=TRUE,subfix="aug")
  } else if(args[[1]] == 8){
    wav_to_matrix(cohort2_train,output_type=c("01true","01false"),augment=TRUE,subfix="aug")
  } else if(args[[1]] == 9){
    wav_to_matrix(cohort3_train,output_type=c("01true","01false"),augment=TRUE,subfix="aug")
  } else if(args[[1]] == 10){
    wav_to_matrix_species(cohort1_test,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 11){
    wav_to_matrix_species(cohort2_test,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 12){
    wav_to_matrix_species(cohort3_test,output_type=c("01true","01false"),subfix="noaug")
  } else if(args[[1]] == 13){
    wav_to_matrix_species(cohort1_train,output_type=c("01true","01false"),add_negs=25,augment=TRUE,subfix="augnegs")
  } else if(args[[1]] == 14){
    wav_to_matrix_species(cohort2_train,output_type=c("01true","01false"),add_negs=25,augment=TRUE,subfix="augnegs")
  } else if(args[[1]] == 15){
    wav_to_matrix_species(cohort3_train,output_type=c("01true","01false"),add_negs=25,augment=TRUE,subfix="augnegs")
  } else {
    print("no more")
  }


# print(args[[1]])
# args[[1]]