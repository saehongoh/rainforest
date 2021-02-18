
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

#### species files
species_files <- list.files("output/resized", pattern="_s", full.names = TRUE)
species_files <- data.frame(do.call(rbind, strsplit(species_files,"_|[.]"))[,2:3], file_id = species_files)
colnames(species_files)[1:2] <- c('species','aug')

species_key <- species_files %>%
  arrange(aug) %>%
  group_by(aug) %>%
  group_split(.)

#### positives files
pos_files <- list.files("output/resized", pattern="posonly", full.names = TRUE)
pos_files <- data.frame(do.call(rbind, strsplit(pos_files,"/|_|[.]"))[,5], file_id = pos_files)
colnames(pos_files)[1] <- c('aug')

#### Test data for evaluation
test_data1 <- train_prepper(readRDS("output/resized/test_ori.RDS"))
# test_data2 <- train_prepper(readRDS("output/resized/test_aug1.RDS"))

#######################
print("preparing sequential model")
#######################

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(test_data1[[1]])[-1]),
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
  layer_dropout(rate = 0.70) %>%
  layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(test_data1[[2]])[2], activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-4)
  )

##############################################
##### Train positive data Set1
#############################################
tmp_files <- pos_files[1:3,]
for(i in 1:nrow(tmp_files)){
  print(paste("working on ___ ", tmp_files$file_id[i]))
  train_data <- train_prepper(readRDS(tmp_files$file_id[i]))

  model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/12), epochs = 50, shuffle = TRUE, 
                validation_split=0.15,
                verbose=1,
                callbacks=list(callback_early_stopping(patience=2, verbose=1)))
  
  print("evalulating on trained data")
  model %>% evaluate(train_data[[1]], train_data[[2]])
  
  print("evalulating on testing data")
  model %>% evaluate(test_data1[[1]], test_data1[[2]])
  # prediction_accuracy(test_data1)

  print("saving ongoing model")
  file_name = paste0("output/resized_model/positives_tmp_",i,".h5")
  model %>% save_model_hdf5(file_name)
}

print("saving final model")
file_name = paste0("output/resized_model/positives_post.h5")
model %>% save_model_hdf5(file_name)

##############################################
##### Train species_specific data with false positives Set1
#############################################

aug_data <- species_key[[1]]
aug_data <- aug_data[sample(1:nrow(aug_data)),]
# ori_data <- species_key[[2]]

# lapply(1:nrow(ori_data), function(x) dim(train_prepper(readRDS(ori_data$file_id[x]))[[1]]))

for(i in 1:nrow(aug_data)){
    # model <- load_model_hdf5("output/resized_model/species_tmp1.h5")

    print(paste("working on ___ ", aug_data$file_id[i]))
    train_data <- train_prepper(readRDS(aug_data$file_id[i]))
    # ori_data <- train_prepper(readRDS(ori_data$file_id[i]))
    
    tmp <- model %>% predict_proba(test_data1[[1]]) %>% round(., digits=3) %>% .[,i]
    before <- round(length(which(ifelse(tmp > mean(tmp), 1, 0) == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits=4)
    
    model %>% fit(train_data[[1]], train_data[[2]], batch_size = nrow(train_data[[2]])/10, epochs = 50, shuffle = TRUE, 
                  validation_split=0.15,
                  verbose=1,
                  callbacks=list(callback_early_stopping(patience=1, verbose=1)))
    
    print("Set1: evalulating on testing data")
    model %>% evaluate(test_data1[[1]], test_data1[[2]])
    # prediction_accuracy(test_data1)
    
    base0 <- round(length(which(rep(0, length(train_data[[2]][,i])) == train_data[[2]][,i]))/length(train_data[[2]][,i]), digits = 4)
    base1 <- round(length(which(rep(1, length(train_data[[2]][,i])) == train_data[[2]][,i]))/length(train_data[[2]][,i]), digits = 4)
    tmp <- model %>% predict_proba(train_data[[1]]) %>% round(., digits=3) %>% .[,i]
    act <- round(length(which(ifelse(tmp > mean(tmp), 1, 0) == train_data[[2]][,i]))/length(train_data[[2]][,i]), digits=4)
    print(paste("Set1: For trained data:", colnames(train_data[[2]])[i], " : all 0 : ", base0 , " : all 1 : ", base1, " : act : ", act))
    
    # base0 <- round(length(which(rep(0, length(ori_data[[2]][,i])) == ori_data[[2]][,i]))/length(ori_data[[2]][,i]), digits = 4)
    # base1 <- round(length(which(rep(1, length(ori_data[[2]][,i])) == ori_data[[2]][,i]))/length(ori_data[[2]][,i]), digits = 4)
    # act <- round(length(which(model %>% predict_proba(ori_data[[1]]) %>% round(., digits=3) %>% .[,i] == ori_data[[2]][,i]))/length(ori_data[[2]][,i]), digits=4)
    # print(paste("For original data:", colnames(ori_data[[2]])[i], " : all 0 : ", base0 , " : all 1 : ", base1, " : test 1 : ", act))
    
    base0 <- round(length(which(rep(0, length(test_data1[[2]][,i])) == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits = 4)
    base1 <- round(length(which(rep(1, length(test_data1[[2]][,i])) == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits = 4)
    tmp <- model %>% predict_proba(test_data1[[1]]) %>% round(., digits=3) %>% .[,i]
    after <- round(length(which(ifelse(tmp > mean(tmp), 1, 0) == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits=4)
    print(paste("For testing data:", colnames(test_data1[[2]])[i], " : all 0 : ", base0 , " : all 1 : ", base1, " : before : ", before, " : after : ", after))
    
    print("saving ongoing model")
    file_name = paste0("output/resized_model/species_tmp_",aug_data$species[i],".h5")
    model %>% save_model_hdf5(file_name)
    
  }

print("saving final model")
file_name = paste0("output/unmasked_model/species_post.h5")
model %>% save_model_hdf5(file_name)


# ##############################################
# ##### Train positive data Set2
# #############################################
# tmp_files <- pos_files[4:6,]
# 
# for(i in 1:nrow(tmp_files)){
#   print(paste("Set2: working on ___ ", tmp_files$file_id[i]))
#   train_data <- train_prepper(readRDS(tmp_files$file_id[i]))
#   
#   model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/12), epochs = 50, shuffle = TRUE, 
#                 validation_split=0.15,
#                 verbose=1,
#                 callbacks=list(callback_early_stopping(patience=2, verbose=1)))
#   
#   print("Set2: evalulating on trained data")
#   model %>% evaluate(train_data[[1]], train_data[[2]])
#   
#   print("Set2: evalulating on testing data")
#   model %>% evaluate(test_data1[[1]], test_data1[[2]])
#   # prediction_accuracy(test_data1)
#   
#   print("Set2: saving ongoing model")
#   file_name = paste0("output/resized_model/positives_tmpSet2_",i,".h5")
#   model %>% save_model_hdf5(file_name)
# }
# 
# print("Set2: saving final model")
# file_name = paste0("output/resized_model/positives_postSet2.h5")
# model %>% save_model_hdf5(file_name)
# 
# 
# ##############################################
# ##### Train species_specific data with false positives Set2
# #############################################
# 
# aug_data <- species_key[[2]]
# # ori_data <- species_key[[2]]
# 
# # lapply(1:nrow(ori_data), function(x) dim(train_prepper(readRDS(ori_data$file_id[x]))[[1]]))
# 
# for(i in 1:nrow(aug_data)){
#   
#   #### unmasked
#   print(paste("Set2: working on ___ ", aug_data$file_id[i]))
#   train_data <- train_prepper(readRDS(aug_data$file_id[i]))
#   # ori_data <- train_prepper(readRDS(ori_data$file_id[i]))
#   
#   # patience_es = ifelse(j >= 5, 5, j)
#   before <- round(length(which(model %>% predict_proba(test_data1[[1]]) %>% round(., digits=3) %>% .[,i] == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits=4)
#   
#   model %>% fit(train_data[[1]], train_data[[2]], batch_size = nrow(train_data[[2]])/10, epochs = 50, shuffle = TRUE, 
#                 validation_split=0.15,
#                 verbose=1,
#                 callbacks=list(callback_early_stopping(patience=1, verbose=1)))
#   
#   print("Set2: evalulating on testing data")
#   model %>% evaluate(test_data1[[1]], test_data1[[2]])
#   # prediction_accuracy(test_data1)
#   
#   base0 <- round(length(which(rep(0, length(train_data[[2]][,i])) == train_data[[2]][,i]))/length(train_data[[2]][,i]), digits = 4)
#   base1 <- round(length(which(rep(1, length(train_data[[2]][,i])) == train_data[[2]][,i]))/length(train_data[[2]][,i]), digits = 4)
#   act <- round(length(which(model %>% predict_proba(train_data[[1]]) %>% round(., digits=3) %>% .[,i] == train_data[[2]][,i]))/length(train_data[[2]][,i]), digits=4)
#   print(paste("Set2: For trained data:", colnames(train_data[[2]])[i], " : all 0 : ", base0 , " : all 1 : ", base1, " : test 1 : ", act))
#   
#   # base0 <- round(length(which(rep(0, length(ori_data[[2]][,i])) == ori_data[[2]][,i]))/length(ori_data[[2]][,i]), digits = 4)
#   # base1 <- round(length(which(rep(1, length(ori_data[[2]][,i])) == ori_data[[2]][,i]))/length(ori_data[[2]][,i]), digits = 4)
#   # act <- round(length(which(model %>% predict_proba(ori_data[[1]]) %>% round(., digits=3) %>% .[,i] == ori_data[[2]][,i]))/length(ori_data[[2]][,i]), digits=4)
#   # print(paste("For original data:", colnames(ori_data[[2]])[i], " : all 0 : ", base0 , " : all 1 : ", base1, " : test 1 : ", act))
#   
#   base0 <- round(length(which(rep(0, length(test_data1[[2]][,i])) == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits = 4)
#   base1 <- round(length(which(rep(1, length(test_data1[[2]][,i])) == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits = 4)
#   after <- round(length(which(model %>% predict_proba(test_data1[[1]]) %>% round(., digits=3) %>% .[,i] == test_data1[[2]][,i]))/length(test_data1[[2]][,i]), digits=4)
#   print(paste("For testing data:", colnames(test_data1[[2]])[i], " : all 0 : ", base0 , " : all 1 : ", base1, " : before : ", before, " : after : ", after))
#   
#   print("Set2: saving ongoing model")
#   file_name = paste0("output/resized_model/species_tmpSet2_",i,".h5")
#   model %>% save_model_hdf5(file_name)
#   
# }
# 
# print("Set2: saving final model")
# file_name = paste0("output/unmasked_model/species_postSet2.h5")
# model %>% save_model_hdf5(file_name)

