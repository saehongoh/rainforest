
# library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
# library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)
# require(ggplot2)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

train_prepper_species <- function(input, species_id){
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
  
  tmp <- tmp[,colnames(tmp) == species_id] %>% to_categorical()
  colnames(tmp) <- c('FALSE','TRUE')
  list(train, tmp)
}


#######################
print("reading key")
#######################

#### species files
# species_files <- list.files("output/resized", pattern="_s", full.names = TRUE)
# species_files <- data.frame(do.call(rbind, strsplit(species_files,"_|[.]"))[,2:3], file_id = species_files)
# colnames(species_files)[1:2] <- c('species','aug')
# 
# species_key <- species_files %>%
#   arrange(aug) %>%
#   group_by(aug) %>%
#   group_split(.)

#### positives files
pos_files <- list.files("output/resized", pattern="posonly", full.names = TRUE)
pos_files <- data.frame(do.call(rbind, strsplit(pos_files,"/|_|[.]"))[,5], file_id = pos_files)
colnames(pos_files)[1] <- c('aug')
# pos_files <- pos_files[c(1,3:6),]

#### positives/neg files
# posneg_files <- list.files("output/resized", pattern="posneg", full.names = TRUE)
# posneg_files <- data.frame(do.call(rbind, strsplit(posneg_files,"/|_|[.]"))[,5:6], file_id = posneg_files)
# colnames(posneg_files)[1:2] <- c('batch','aug')
# tmp_posneg <- posneg_files %>% filter(aug == "aug1")

#### species files

species_files <- list.files("output/resized/species", pattern="train", full.names = TRUE)
species_files <- data.frame(do.call(rbind, strsplit(species_files,"/|_|[.]"))[,5:6], file_id = species_files)
colnames(species_files)[1:2] <- c('species_id','aug')

# test_dataY <- train_prepper(readRDS("output/resized/test_aug1.RDS"))

#######################
print("preparing sequential model")
#######################
tmp_frame <- train_prepper_species(readRDS("output/resized/test_ori.RDS"), "s0")

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(tmp_frame[[1]])[-1]),
                filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_conv_1d(input_shape = c(dim(test_data1[[1]])[-1]),
  #               filters=32, kernel_size = c(10), activation = 'relu')
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  # layer_flatten() %>%
  # layer_dense(input_shape = c(dim(test_data1[[1]])[-1]),
  #             units = 48, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.60) %>%
  layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(tmp_frame[[2]])[2], activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=5e-4)
  )

##########
#### Setup
##########

species_list <- unique(species_files$species_id)
# train_files <- species_files %>% filter(aug == "aug")
# test_files <- species_files %>% filter(aug == "ori")

for(i in 1:length(species_list)){
  print(paste("working on ___ ", species_list[i]))
  tmp_key <- species_files %>% filter(species_id == species_list[i])
  test_data <- train_prepper_species(readRDS("output/resized/test_ori.RDS"), unique(tmp_key$species_id))
  
  # model1 <- load_model_hdf5("output/resized_species/model_s1_postSpecies.h5")
  # model2 <- load_model_hdf5("output/resized_species/model_s1_postTraining.h5")
  # test_data[[2]][,2]
  # model1 %>% predict_classes(test_data[[1]])
  # model1 %>% predict_proba(test_data[[1]]) %>% round(., digits=2) %>% .[,2]
  # model2 %>% predict_classes(test_data[[1]])
  # model2 %>% predict_proba(test_data[[1]]) %>% round(., digits=2) %>% .[,2]
  # data.frame(act= test_data[[2]][,2],
  #            postS = model1 %>% predict_proba(test_data[[1]]) %>% round(., digits=2) %>% .[,2],
  #            postT = model2 %>% predict_proba(test_data[[1]]) %>% round(., digits=2) %>% .[,2])
  
  r=1
  repeat{
    for(j in 1:nrow(tmp_key)){
      
      patience_es = ifelse(r == 1, 1, 0)
      
      train_data <- train_prepper_species(readRDS(tmp_key$file_id[j]), unique(tmp_key$species_id))

      model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/12), epochs = 50, shuffle = TRUE, 
                    validation_split=0.15,
                    verbose=1,
                    callbacks=list(callback_early_stopping(patience=patience_es, verbose=1)))
      
      print("evalulating on testing data")
      model %>% evaluate(test_data[[1]], test_data[[2]])
      # model %>% predict_classes(test_data[[1]])
      # model %>% predict_proba(test_data[[1]])
    }
    if(r==2){break}
    r=r+1
    
  }
  
  eval <- model %>% evaluate(test_data[[1]], test_data[[2]])
  print(paste("saving ongoing model for : ", unique(tmp_key$species_id), " : post-species accuracy : ", round(eval[[2]],digits=3) ))
  file_name = paste0("output/resized_species/model_", unique(tmp_key$species_id), "_postSpecies.h5")
  model %>% save_model_hdf5(file_name)

  # Additional training on pos augmented data 
  for(j in 1:nrow(pos_files)){
    train_data <- train_prepper_species(readRDS(pos_files$file_id[j]), unique(tmp_key$species_id))
    
    model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/12), epochs = 50, shuffle = TRUE, 
                  validation_split=0.15,
                  verbose=1,
                  callbacks=list(callback_early_stopping(patience=0, verbose=1)))
    
    print("evalulating on testing data")
    model %>% evaluate(test_data[[1]], test_data[[2]])
  }
  
  eval <- model %>% evaluate(test_data[[1]], test_data[[2]])
  print(paste("saving ongoing model for : ", unique(tmp_key$species_id), " : final accuracy : ", round(eval[[2]],digits=3)))
  
  file_name = paste0("output/resized_species/model_", unique(tmp_key$species_id), "_postTraining.h5")
  model %>% save_model_hdf5(file_name)
}
