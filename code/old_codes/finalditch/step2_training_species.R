
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
  tmp <- train_lab[,grepl(paste0(species_id,"_"), colnames(train_lab))] %>% as.matrix()
  # colnames(tmp) <- c('FALSE','TRUE')
  list(train, tmp)
}

train_prepper <- function(input){
  train <- input[[1]]
  train_lab <- input[[2]]
  rownames(train_lab) <- NULL
  train_lab <- train_lab[,2:ncol(train_lab)] %>% as.matrix()
  list(train, train_lab)
}

#######################
print("reading key")
#######################

#### test files with one species
test_files <- list.files("output/lastditch/species", pattern="test", full.names = TRUE)
test_files <- data.frame(do.call(rbind, strsplit(test_files,"/|_|[.]RDS"))[,4:7], file_id = test_files)
colnames(test_files)[1:4] <- c('cohort','test','species','type')
test_files$activation <- ifelse(grepl("true|false", test_files$type), "softmax", "sigmoid")

#### test files with other species
test_files2 <- list.files("output/lastditch/", pattern="test", full.names = TRUE)
test_files2 <- data.frame(do.call(rbind, strsplit(test_files2,"/|_|[.]RDS"))[,4:6], file_id = test_files2)
colnames(test_files2)[1:3] <- c('cohort','test','type')
test_files2$activation <- ifelse(grepl("true|false", test_files2$type), "softmax", "sigmoid")

#### train files
train_files <- list.files("output/lastditch/species", pattern="train", full.names = TRUE)
train_files <- data.frame(do.call(rbind, strsplit(train_files,"/|_|[.]RDS"))[,4:7], file_id = train_files)
colnames(train_files)[1:4] <- c('cohort','test','species','type')
# train_files <- train_files %>% filter(n== "n20")
train_files$activation <- ifelse(grepl("true|false", train_files$type), "softmax", "sigmoid")

i=2
train_files[i,]
test_data <- train_prepper_species(readRDS(test_files$file_id[i]), paste0("s", test_files$species[i]))
test_data2 <- train_prepper_species(readRDS(test_files2$file_id[i]), paste0("s", test_files$species[i]))
train_data <- train_prepper_species(readRDS(train_files$file_id[i]), paste0("s", train_files$species[i]))
dim(test_data[[1]])
dim(test_data2[[1]])
dim(train_data[[1]])
# test_dataY <- train_prepper(readRDS("output/resized/test_aug1.RDS"))
head(train_data[[2]])

#######################
print("preparing sequential model")
#######################

##########
#### Setup
##########

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(test_data)[-1]),
                filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
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
  layer_dense(units = dim(test_data[[2]])[2], activation = test_files$activation[i],  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-4)
  )

model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=2, verbose=1)))

print("evalulating on testing data")
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
test_data[[2]]
model %>% predict_classes(test_data[[1]])
test_data[[2]][,2]

print("evalulating on testing data with other species")
eval2 <- model %>% evaluate(test_data2[[1]], test_data2[[2]])
model %>% predict_proba(test_data2[[1]]) %>% round(., digits = 3)
test_data2[[2]]
which(test_data2[[2]][,2] == 1) %in% which(model %>% predict_classes(test_data2[[1]]) == 1)

#############
#### Training
#############

species_list <- unique(species_files$species_id)

for(i in 1:length(species_list)){
  print(paste("working on ___ ", species_list[i]))
  
  test_data1 <- train_prepper_species(readRDS(test_noaug$file_id[i]), species_list[i])
  test_data2 <- train_prepper_species(readRDS("output/january/test_withothers_all.RDS"), species_list[i])
  train_data <- train_prepper_species(readRDS(species_files$file_id[i]), species_list[i])
  
  model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/20), epochs = 50, shuffle = TRUE, 
                validation_split=0.15,
                verbose=1,
                callbacks=list(callback_early_stopping(patience=3, verbose=1)))
  
  print("evalulating on trained data")
  model %>% evaluate(train_data[[1]], train_data[[2]])
  
  print("evalulating on testing data")
  eval1 <- model %>% evaluate(test_data1[[1]], test_data1[[2]])
  model %>% predict_proba(test_data1[[1]]) %>% round(., digits =3)
  test_data1[[2]]
  model %>% predict_classes(test_data1[[1]])
  test_data1[[2]][,2]
  
  print("evalulating on testing data")
  eval1 <- model %>% evaluate(test_data2[[1]], test_data2[[2]])
  cbind(model %>% predict_proba(test_data2[[1]]) %>% round(., digits =3), test_data2[[2]],
        model %>% predict_classes(test_data2[[1]]))
  # model %>% predict_classes(test_data2[[1]])
  # test_data2[[2]][,2]
  # 
  # print("evalulating with others")
  # eval2 <- model %>% evaluate(test_data2[[1]], test_data2[[2]])
  
  print(paste("saving ongoing model for : ",  species_list[i]))
  print(paste("raw test acc : ", round(eval1[[2]], digits=3), "with others acc : ", round(eval2[[2]], digits=3)))
  file_name = paste0("output/january_model/model_", species_list[i], ".h5")
  model %>% save_model_hdf5(file_name)
  
}

