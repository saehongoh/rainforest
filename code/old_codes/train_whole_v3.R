
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

train_reader <- function(file, mask=FALSE){
  tmp <- readRDS(file) %>%
    group_by(unique_id, zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    group_by(unique_id) %>%
    mutate(value = value/max(value)) %>%
    ungroup() 
  
  if(mask == TRUE){
    tmp %>%
      select(unique_id, species_id, tscore, FreqHz, t_min, t_max, f_min, f_max, time, value, zts) %>%
      group_by(unique_id) %>%
      mutate(value = ifelse(FreqHz < f_min | FreqHz > f_max, 0, value)) %>%
      mutate(value = ifelse(time < t_min | time > t_max, 0, value)) %>%
      ungroup() %>%
      mutate(unique_id = paste0(unique_id, "_m")) %>%
      select(-f_min, -f_max)

      
  } else {
    tmp %>%
      select(unique_id, species_id, tscore, FreqHz, t_min, t_max, time, value, zts)
  }
  }


keras_whole <- function(data){
  freq_bin <- data.frame(FreqHz = unique(data$FreqHz), 
                         FreqBin1 = cut(unique(data$FreqHz), 4, labels = c(1,2,3,4)))
  
  tmp <- data  %>%
    left_join(., freq_bin, by="FreqHz") %>%
    group_by(unique_id, species_id, FreqHz) %>%
    mutate(zts = 1:n()) %>%
    ungroup()

  f1 <- tmp %>%
    dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    dplyr::filter(FreqBin1 == 2) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    dplyr::filter(FreqBin1 == 3) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f4 <- tmp %>%
    dplyr::filter(FreqBin1 == 4) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix(),
         f2 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix(),
         f3 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix(),
         f4 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  # train <- parallel::mclapplylapply(1:length(times), function(x) lister(x))
  
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  
  output_x_lab <- f1 %>% 
    dplyr::filter(zts == 1) %>% 
    select(unique_id, species_id, tscore) %>%
    mutate(tscore = as.numeric(as.character(tscore))) %>%
    mutate(species_id = paste0("s", species_id)) %>%
    dcast(unique_id ~ species_id, value.var = "tscore") %>%
    select(-unique_id) %>%
    mutate_all(., function(x) ifelse(!is.na(x), 1, x)) %>%
    mutate_all(., function(x) ifelse(is.na(x), 0, x)) %>%
    as.matrix()
  return(list(output_x, output_x_lab))
}


files <- list.files("output/processed_data_whole2", pattern="data", full.names = TRUE)
files <- files[!grepl("dataX|dataY",files)]
keys <- list.files("output/processed_data_whole2", pattern="key", full.names = TRUE)
key <- do.call(rbind, lapply(1:length(keys), function(x) readRDS(keys[x])))

for(i in 1:length(files)){
  print(files[i])
  data <- rbind(train_reader(files[i], mask=FALSE),
                train_reader(files[i], mask=TRUE))

  key <- readRDS(keys[i])

  data <- data %>%
    # mutate(time = as.numeric(as.character(time))) %>%
    mutate(unique_id2 = case_when(time >= 0 & time < 20 ~ paste0(unique_id, "_f1"),
                                  time >= 20 & time < 40 ~ paste0(unique_id, "_f2"),
                                  time >= 40 & time < 60 ~ paste0(unique_id, "_f3"))) %>%
    mutate(tscore = ifelse(time > t_min & time < t_max, tscore, 0)) %>%
    group_by(unique_id2) %>%
    mutate(maxScore = max(tscore) >0) %>%
    filter(maxScore == TRUE) %>%
    select(-t_min, -t_max, -time, -maxScore)

  train_x <- sample(key$unique_id, nrow(key)*.90)
  train_y <- key[!(key$unique_id %in% train_x),]$unique_id

  data_x <- data[data$unique_id %in% c(train_x, paste0(train_x, "_m")),]
  data_y <- data[data$unique_id %in% c(train_y, paste0(train_y, "_m")),]

  data_x <- data_x %>%
    mutate(unique_id = unique_id2)

  data_y <- data_y %>%
    mutate(unique_id = unique_id2)

  data_x <- data_x[,1:ncol(data_x)-1]
  data_y <- data_y[,1:ncol(data_y)-1]

  saveRDS(data_x, paste0(files[i], "_dataX.RDS"))
  saveRDS(data_y, paste0(files[i], "_dataY.RDS"))
  }

##########################################################################
rm(list = setdiff(ls(), lsf.str()))
##########################################################################

# x_files <- list.files("output/processed_data_whole2", pattern="dataX", full.names = TRUE)
# y_files <- list.files("output/processed_data_whole2", pattern="dataY", full.names = TRUE)
# 
# read_rds <- function(files, x){
#   print(x)
#   readRDS(files[x])
# }
# 
# ########
# 
# print("loading data")
# 
# y_list <- lapply(1:length(y_files), function(x) read_rds(y_files, x))
# data_y <- data.table::rbindlist(y_list)
# 
# print("preparing for keras")
# 
# tmp <- keras_whole(data_y)
# train_y <- tmp[[1]]
# train_y_lab <- tmp[[2]]
# 
# print("saving backup files")
# 
# saveRDS(train_y, "output/multispecies/whole_train_y_masked.RDS")
# saveRDS(train_y_lab, "output/multispecies/whole_train_y_lab_masked.RDS")
# 
# #######
# 
# print("loading data")
# 
# data_x <- lapply(1:length(x_files), function(x) read_rds(x_files, x))
# 
# data_x <- data.table::rbindlist(data_x)
# 
# print("preparing for keras")
# 
# tmp <- keras_whole(data_x)
# train_x <- tmp[[1]]
# train_x_lab <- tmp[[2]]
# 
# print("saving backup files")
# 
# saveRDS(train_x, "output/multispecies/whole_train_x_masked.RDS")
# saveRDS(train_x_lab, "output/multispecies/whole_train_x_lab_masked.RDS")

# 
# ##########################################################################
# rm(list = setdiff(ls(), lsf.str()))
# ##########################################################################

# print("##########################################################################")
# print("############################### firing up ################################")
# print("##########################################################################")
# 
# train_x <- readRDS("output/multispecies/whole_train_x.RDS")
# train_x_lab <- readRDS("output/multispecies/whole_train_x_lab.RDS")
# train_y <- readRDS("output/multispecies/whole_train_y.RDS")
# train_y_lab <- readRDS("output/multispecies/whole_train_y_lab.RDS")
# 
# max_iter = 7
# 
# do.call(rbind, strsplit(list.files("output_trained_whole_set1", pattern="orig"), "_"))
# 
# # #### Model iterations
# iter_list <- vector('list', max_iter)
# j=1
# 
# repeat {
#   print(paste(j," ____ out ____ of ____ ", max_iter))
#   
#   model <- keras_model_sequential()
# 
#   model %>%
#     layer_conv_2d(input_shape = c(dim(train_x)[-1]),
#                   filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     # layer_conv_2d(filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_max_pooling_2d(pool_size = c(2,2) ) %>%   #--------Max Pooling
#     layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
#     layer_dropout(rate = 0.60) %>%
#     layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_dropout(rate = 0.50) %>%
#     layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_dropout(rate = 0.50) %>%
#     layer_activation(activation = 'relu') %>%
#     layer_flatten() %>%
#     # layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
#     layer_dense(units = dim(train_x_lab)[2], activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
#     # layer_dense(units = 2, activation = 'sigmoid') %>%
#     compile(
#       loss = 'categorical_crossentropy',
#       metrics = c("accuracy"),
#       optimizer = optimizer_adam(lr=1e-4)
#     )
# 
#   model %>%  fit(train_x, train_x_lab, batch_size = 32, epochs = 100, shuffle = TRUE,
#                  validation_split=0.20,
#                  verbose=1,
#                  callbacks=list(callback_early_stopping(patience=3, verbose=1)))
# 
#   eval_x <- model %>% evaluate(train_x, train_x_lab)
#   eval_y <- model %>% evaluate(train_y, train_y_lab)
# 
#   file_name = paste0("output/trained_whole_set1/orig_iter_", j, ".h5")
# 
#   model %>% save_model_hdf5(file_name)
# 
#   export <- data.frame(iter = j, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
#              test_loss = eval_y[[1]], test_accuracy = eval_y[[2]],
#              results_name = file_name)
#   
#   saveRDS(export, paste0("output/trained_whole_set1/orig_iter_", j, ".RDS"))
# 
#   if(j==max_iter){break}
#   j = j + 1
# }
# 
# rm(list = setdiff(ls(), lsf.str()))
# 
# print("##########################################################################")
# print("############################### firing up ################################")
# print("##########################################################################")
# 
# train_x <- readRDS("output/whole_train_x4.RDS")
# train_x_lab <- readRDS("output/whole_train_x_lab4.RDS")
# train_y <- readRDS("output/whole_train_y4.RDS")
# train_y_lab <- readRDS("output/whole_train_y_lab4.RDS")
# 

max_iter = 7

# #### Model iterations
iter_list <- vector('list', max_iter)
j=1

repeat {
  print(paste(j," ____ out ____ of ____ ", max_iter))
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                  filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    # layer_conv_2d(filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2) ) %>%   #--------Max Pooling
    layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dropout(rate = 0.60) %>%
    layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dropout(rate = 0.50) %>%
    layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dropout(rate = 0.50) %>%
    layer_activation(activation = 'relu') %>%
    layer_flatten() %>%
    # layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dense(units = dim(train_x_lab)[2], activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    # layer_dense(units = 2, activation = 'sigmoid') %>%
    compile(
      loss = 'categorical_crossentropy',
      metrics = c("accuracy"),
      optimizer = optimizer_adam(lr=1e-4)
    )
  
  model %>%  fit(train_x, train_x_lab, batch_size = 32, epochs = 100, shuffle = TRUE,
                 validation_split=0.20,
                 verbose=1,
                 callbacks=list(callback_early_stopping(patience=3, verbose=1)))
  
  eval_x <- model %>% evaluate(train_x, train_x_lab)
  eval_y <- model %>% evaluate(train_y, train_y_lab)
  
  file_name = paste0("output/trained_whole_set1/mod_iter_", j, ".h5")
  
  model %>% save_model_hdf5(file_name)
  
  export <- data.frame(iter = j, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
                       test_loss = eval_y[[1]], test_accuracy = eval_y[[2]],
                       results_name = file_name)
  
  saveRDS(export, paste0("output/trained_whole_set1/mod_iter_", j, ".RDS"))
  
  if(j==max_iter){break}
  j = j + 1
}
