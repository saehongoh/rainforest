
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

# #### test files with one species
# test_species <- list.files("output/finalditch/species", pattern="test", full.names = TRUE)
# test_species <- data.frame(do.call(rbind, strsplit(test_species,"/|_|[.]rds"))[,4:8], file_id = test_species)
# colnames(test_species)[1:5] <- c('cohort','test','species','type','aug')
# test_species$activation <- ifelse(grepl("true|false", test_species$type), "softmax", "sigmoid")

# #### test files with other species by cohort
# test_cohort <- list.files("output/finalditch/cohort_t2", pattern="test", full.names = TRUE)
# test_cohort <- data.frame(do.call(rbind, strsplit(test_cohort,"/|_|[.]rds"))[,5:9], file_id = test_cohort)
# colnames(test_cohort)[1:5] <- c('cohort','test','type','aug',"set1")
# test_cohort$activation <- ifelse(grepl("true|false", test_cohort$type), "softmax", "sigmoid")

# #### train files with one species
# train_species <- list.files("output/finalditch/species", pattern="train", full.names = TRUE)
# train_species <- data.frame(do.call(rbind, strsplit(train_species,"/|_|[.]rds"))[,4:8], file_id = train_species)
# colnames(train_species)[1:5] <- c('cohort','test','species','type','aug')
# train_species$activation <- ifelse(grepl("true|false", train_species$type), "softmax", "sigmoid")

#### test files with other species by cohort
train_cohort <- list.files("output/finalditch/cohort_t2", pattern="train", full.names = TRUE)
train_cohort <- data.frame(do.call(rbind, strsplit(train_cohort,"/|_|[.]rds"))[,5:9], file_id = train_cohort)
colnames(train_cohort)[1:5] <- c('cohort','test','type','aug','set')
train_cohort$activation <- ifelse(grepl("true|false", train_cohort$type), "softmax", "sigmoid")

#######################
print("cohort-wise training")
#######################
train_tmp <- train_cohort %>% filter(aug == "aug")

i=1
train_tmp[i,]
# test_cohort[i,]
train_data <- train_prepper(readRDS(train_tmp$file_id[i]))
test_data <- train_prepper(readRDS("output/finalditch/cohort_t2/cohort2_test_01false_noaug_set1.rds"))
dim(train_data[[1]])
dim(test_data[[1]])
head(train_data[[2]])
head(test_data[[2]])

#######################
print("preparing sequential model")
#######################

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(test_data)[-1]),
                filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_batch_normalization() %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_dense(units = dim(test_data[[2]])[2], activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.70) %>%
  layer_dense(units = dim(test_data[[2]])[2], activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_dense(units = dim(test_data[[2]])[2], activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(test_data[[2]])[2], activation = train_tmp$activation[i],  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-3)
  )

#######################
print("training set 1: run 1")
#######################
i=1
train_data <- train_prepper(readRDS(train_tmp$file_id[i]))
model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=2, verbose=1)))

file_name = paste0("output/finalditch/models_t2/cohort2_set",i,"_run1.h5")
model %>% save_model_hdf5(file_name)

print("evalulating on testing data")
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]

# preds <- ifelse(preds == 0, NA, preds)
cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = 0.95)
preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
colnames(preds2) <- colnames(preds)

prediction_counter <- function(x){
  total_actual = length(which(act[,x] == 1))
  total_predicted = length(which(preds2[,x] == 1))
  true_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == TRUE))
  false_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == FALSE))
  data.frame(species_id = colnames(act)[x], total_actual, total_predicted, true_positive = true_positive, false_positive)
}
# noaug <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
#######################
print("training set 1: run 2")
#######################

# model <- load_model_hdf5("output/finalditch/models/cohort2_post_set1_run1.h5")
model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=1, verbose=1)))

file_name = paste0("output/finalditch/models/cohort2_post_set1_run2.h5")
model %>% save_model_hdf5(file_name)

print("evalulating on testing data")
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]

# preds <- ifelse(preds == 0, NA, preds)
cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = 0.95)
preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
colnames(preds2) <- colnames(preds)

prediction_counter <- function(x){
  total_actual = length(which(act[,x] == 1))
  total_predicted = length(which(preds2[,x] == 1))
  true_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == TRUE))
  false_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == FALSE))
  data.frame(species_id = colnames(act)[x], total_actual, total_predicted, true_positive = true_positive, false_positive)
}
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training set 2: run 1")
#######################

train_data2 <- train_prepper(readRDS("output/finalditch/cohort/cohort2_train_01false_aug_set2.rds"))
dim(train_data2[[1]])
names <- colnames(train_data[[2]])[!(colnames(train_data[[2]]) %in% colnames(train_data2[[2]]))]
template <- matrix(0, nrow = nrow(train_data2[[2]]), ncol = length(names))
colnames(template) <- names
template <- cbind(train_data2[[2]], template)
template <- template[,match(colnames(train_data[[2]]), colnames(template2))]
colnames(template) == colnames(train_data[[2]])
train_data2[[2]] <- template

model %>% fit(train_data2[[1]], train_data2[[2]], batch_size = floor(nrow(train_data2[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=2, verbose=1)))

file_name = paste0("output/finalditch/models/cohort2_post_set2_run1.h5")
model %>% save_model_hdf5(file_name)
model <- load_model_hdf5("output/finalditch/models/cohort2_post_set2_run1.h5")
print("evalulating on testing data")
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]

# preds <- ifelse(preds == 0, NA, preds)
cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = 0.99999)
preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
colnames(preds2) <- colnames(preds)
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training set 2: run 2")
#######################

model %>% fit(train_data2[[1]], train_data2[[2]], batch_size = floor(nrow(train_data2[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=1, verbose=1)))

file_name = paste0("output/finalditch/models/cohort2_post_set2_run2.h5")
model %>% save_model_hdf5(file_name)

print("evalulating on testing data")
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]

# preds <- ifelse(preds == 0, NA, preds)
cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = 0.99)
preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
colnames(preds2) <- colnames(preds)
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("species-specific training")
#######################

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

