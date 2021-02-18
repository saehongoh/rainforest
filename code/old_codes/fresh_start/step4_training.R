
# library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
# library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)
# require(ggplot2)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

train_prepper <- function(input){
  train <- input[[1]]
  train_lab <- input[[2]]
  
  template <- c("unique_id", paste0("s", seq(0,23,1)))
  train_lab <- data.frame(train_lab)[,c(match(template, colnames(train_lab)))]
  
  names <- colnames(train_lab)
  
  train_lab <- train_lab %>%
    select(-unique_id) %>% 
    as.data.frame() %>%
    mutate_all(., function(x) ifelse(x == 0, 1, 0)) %>%
    as.matrix() %>%
    to_categorical()
  
  tmp <- train_lab[,,1]
  colnames(tmp) <- names[-1]
  
  list(train, tmp)
}

#######################
print("reading key")
#######################

#### Unmasked files
unmasked_files <- list.files("output/unmasked", pattern="posonly", full.names = TRUE)
unmasked_files <- data.frame(do.call(rbind, strsplit(unmasked_files,"_|[.]"))[,2:4], file_id = unmasked_files)
colnames(unmasked_files)[1:3] <- c('cat','batch','aug')

test_unmasked <- unmasked_files[unmasked_files$aug == "org",]
test_unmasked <- lapply(1:nrow(test_unmasked), function(x) train_prepper(readRDS(test_unmasked$file_id[x])))

train_unmasked <- unmasked_files[unmasked_files$aug != "org",] %>%
  arrange(aug) %>%
  group_by(aug) %>%
  group_split(.)

train_unmasked <- train_unmasked[1:3]
#### masked species-specific files
species_X <- list.files("output/ind_keras", pattern="dataX", full.names = TRUE)
species_X <- data.frame(do.call(rbind, strsplit(species_X,"_|/|[.]"))[,4:6], file_id = species_X)
colnames(species_X)[1:3] <- c('species_id','data_type','batch')

species_X <- species_X %>%
  group_by(batch) %>%
  group_split(.)
  
species_Y <- list.files("output/ind_keras", pattern="dataY", full.names = TRUE)
species_Y <- data.frame(do.call(rbind, strsplit(species_Y,"_|/|[.]"))[,4:6], file_id = species_Y)
colnames(species_Y)[1:3] <- c('species_id','data_type','batch')
# test_unmasked <- lapply(1:nrow(test_unmasked), function(x) train_prepper(readRDS(test_unmasked$file_id[x])))

#### Real data for evaluation
real_data <- train_prepper(readRDS("output/ind_training_files/unmasked_set3.RDS"))

#######################
print("preparing sequential model")
#######################

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(real_data[[1]])[-1]),
                filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_flatten() %>%
  layer_dense(units = 24, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.60) %>%
  layer_dense(units = 24, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_dense(units = 24, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(real_data[[2]])[2], activation = 'softmax',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-3)
  )


#### Train species specific masked data

for(j in 1:length(species_X)){
  
  ### Masked species training
  train_key <- species_X[[j]]
  test_key <- species_Y
  
  for(i in 1:nrow(train_key)){
      
      print(paste("working on ___ ", train_key$file_id[i]))
      train_data <- train_prepper(readRDS(train_key$file_id[i]))
      # print(paste("any NAs? ___ ", length(which(is.na(train_data[[1]])))))
      model %>% fit(train_data[[1]], train_data[[2]], batch_size = 10, epochs = 100, shuffle = TRUE, 
                    validation_split=0.15,
                    verbose=1,
                    callbacks=list(callback_early_stopping(patience=1, verbose=1)))
  
      test_data <- train_prepper(readRDS(test_key$file_id[i]))
      
      print("evaluating on test data")
      model %>% evaluate(test_data[[1]], test_data[[2]])
      
      print("evaluating on real data")
      model %>% evaluate(real_data[[1]], real_data[[2]])
      
      # model %>% predict_proba(train_data[[1]]) %>% round(., digits=3)
  
      file_name = paste0("output/unmasked_model/species_masked_backup.h5")
      model %>% save_model_hdf5(file_name)
      
  }
  
  #### unmasked
  tmp_key <- train_unmasked[[j]]
  
  for(i in 1:nrow(tmp_key)){
    
    print(paste("working on ___ ", tmp_key$file_id[i]))
    train_data <- train_prepper(readRDS(tmp_key$file_id[i]))
    
    # patience_es = ifelse(j >= 5, 5, j)
    
    model %>% fit(train_data[[1]], train_data[[2]], batch_size = 10, epochs = 50, shuffle = TRUE, 
                  validation_split=0.15,
                  verbose=1,
                  callbacks=list(callback_early_stopping(patience=1, verbose=1)))
    
    print("evaluating on test set")
    model %>% evaluate(test_unmasked[[i]][[1]], test_unmasked[[i]][[2]])
    
    print("evaluating on real data")
    model %>% evaluate(real_data[[1]], real_data[[2]])
    # model %>% predict_proba(train_data[[1]]) %>% round(., digits=3)
    
  }
  print("saving batch model")
  file_name = paste0("output/unmasked_model/posonly_aug",j,".h5")
  model %>% save_model_hdf5(file_name)
}


#### Train unmasked
# print("starting training on unmasked data")
# j=1
# for(j in 1:length(train_unmasked)){
#   tmp_key <- train_unmasked[[j]]
#   
#   for(i in 1:nrow(tmp_key)){
#     
#     print(paste("working on ___ ", tmp_key$file_id[i]))
#     train_data <- train_prepper(readRDS(tmp_key$file_id[i]))
#     
#     # patience_es = ifelse(j >= 5, 5, j)
#     
#     model %>% fit(train_data[[1]], train_data[[2]], batch_size = 10, epochs = 50, shuffle = TRUE, 
#                   validation_split=0.15,
#                   verbose=1,
#                   callbacks=list(callback_early_stopping(patience=1, verbose=1)))
#     
#     print("evaluating on test set")
#     model %>% evaluate(test_data[[i]][[1]], test_data[[i]][[2]])
#     
#     print("evaluating on real data")
#     model %>% evaluate(real_data[[1]], real_data[[2]])
#     # model %>% predict_proba(train_data[[1]]) %>% round(., digits=3)
#     
#   }
#   print("saving batch model")
#   file_name = paste0("output/unmasked_model/posonly_aug",j,".h5")
#   model %>% save_model_hdf5(file_name)
# }
# 
# print("saving final model")
# file_name = paste0("output/unmasked_model/posonly_all.h5")
# model %>% save_model_hdf5(file_name)
# model %>% evaluate(real_data[[1]], real_data[[2]])

