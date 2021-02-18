
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

#### test files
test_noaug <- list.files("output/january", pattern="NoAug", full.names = TRUE)
test_noaug <- data.frame(do.call(rbind, strsplit(test_noaug,"/|_|[.]RDS"))[,4:5], file_id = test_noaug)
colnames(test_noaug)[1:2] <- c('name','species_id')

# test_withothers <- list.files("output/january", pattern="_withother", full.names = TRUE)
# test_withothers <- data.frame(do.call(rbind, strsplit(test_withothers,"/|_|[.]RDS"))[,4:5], file_id = test_withothers)
# colnames(test_withothers)[1:2] <- c('name','species_id')

species_files <- list.files("output/january", pattern="train", full.names = TRUE)
species_files <- data.frame(do.call(rbind, strsplit(species_files,"/|_|[.]RDS"))[,4], file_id = species_files)
colnames(species_files)[1] <- c('species_id')

# test_dataY <- train_prepper(readRDS("output/resized/test_aug1.RDS"))

#######################
print("preparing sequential model")
#######################

##########
#### Setup
##########
tmp_frame <- train_prepper_species(readRDS(test_noaug$file_id[1]), test_noaug$species_id[1])

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(tmp_frame[[1]])[-1]),
                filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.15) %>%
  layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.15) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  # layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  # layer_dropout(rate = 0.60) %>%
  # layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_dropout(rate = 0.50) %>%
  layer_dense(units = dim(tmp_frame[[2]])[2], activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(tmp_frame[[2]])[2], activation = 'softmax',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'binary_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-3)
  )

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

