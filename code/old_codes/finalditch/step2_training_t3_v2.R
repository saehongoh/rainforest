
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

train_prepper_collapser <- function(input, species_id){
  train <- input[[1]]
  train_lab <- input[[2]]
  tmp <- train_lab[,grepl(paste0(species_id,"_"), colnames(train_lab))]
  negs <- train_lab[,!(grepl(paste0(species_id,"_"), colnames(train_lab)))]
  negs <- negs[,2:ncol(negs)]
  train_lab <- data.frame(tmp , others=matrixStats::rowMaxs(as.matrix(negs))) %>% as.matrix()
  # colnames(train_lab)[1:2] <- c('true')
  list(train, train_lab)
}

#######################
print("reading key")
#######################
#######################
print("cohort-wise training")
#######################
# train_tmp <- train_cohort %>% filter(cohort == "cohort2")
# test_tmp <- test_cohort %>% filter(cohort == "cohort2")

# test_data <- train_prepper(readRDS(test_tmp$file_id[1]))
test_data <- train_prepper_collapser(readRDS("output/finalditch/cohort_t3/cohort2_test_01false_noaug.rds"), "s12")
test_data_pos <- train_prepper_collapser(readRDS("output/finalditch/cohort_t3/cohort2_test_01false_noaug_trueonly.rds"), "s12")
# dim(train_data[[1]])
dim(test_data[[1]])
dim(test_data_pos[[1]])

#######################
print("preparing sequential model")
#######################

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(test_data[[1]])[-1]),
                filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_conv_2d(filters = 32, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_dropout(rate = 0.25) %>%
  # layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_dense(units = dim(test_data[[2]])[2]*4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = dim(test_data[[2]])[2]*2, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = dim(test_data[[2]])[2]*2, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(test_data[[2]])[2], activation = "softmax",  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-1)
  )

#######################
print("training cohort 2: original data")
#######################
train_data <- train_prepper_collapser(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_noaug.rds"), "s12")
model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1, 
              callbacks=list(callback_early_stopping(patience=5, monitor = "val_loss", verbose=1)))

# file_name = paste0("output/finalditch/models_t3/cohort2_set",i,"_run1.h5")
# model %>% save_model_hdf5(file_name)

print("evalulating on testing data")
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]

# preds <- ifelse(preds == 0, NA, preds)
cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = 0.94)
preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
colnames(preds2) <- colnames(preds)
# pred_classes <- colnames(test_data[[2]])[model %>% predict_classes(test_data[[1]])]
# act_classes <- sapply(1:nrow(test_data[[2]]), function(x) names(which.max(test_data[[2]][x,])))

prediction_counter <- function(x){
  total_actual = length(which(act[,x] == 1))
  total_predicted = length(which(preds2[,x] == 1))
  true_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == TRUE))
  false_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == FALSE))
  data.frame(species_id = colnames(act)[x], total_actual, total_predicted, true_positive = true_positive, false_positive)
}
# noaug <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))


preds <- model %>% predict_proba(test_data_pos[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
act <- test_data_pos[[2]][,grepl("true", colnames(test_data_pos[[2]]))]
# preds <- ifelse(preds == 0, NA, preds)
cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = 0.80)
preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
colnames(preds2) <- colnames(preds)
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training cohort 2: augmented data set1")
#######################
train_data_extra <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_subaug_set1.rds"))
# model <- load_model_hdf5("output/finalditch/models/cohort2_post_set1_run1.h5")
model %>% fit(train_data_extra[[1]], train_data_extra[[2]], batch_size = floor(nrow(train_data_extra[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=2, verbose=1)))

# file_name = paste0("output/finalditch/models/cohort2_post_set1_run2.h5")
# model %>% save_model_hdf5(file_name)

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
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training cohort 2: back to original data 1")
#######################
# train_data <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_noaug_set2.rds"))
model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1, 
              callbacks=list(callback_early_stopping(patience=2, monitor = "val_loss", verbose=1)))

# file_name = paste0("output/finalditch/models_t3/cohort2_set",i,"_run1.h5")
# model %>% save_model_hdf5(file_name)

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
# pred_classes <- colnames(test_data[[2]])[model %>% predict_classes(test_data[[1]])]
# act_classes <- sapply(1:nrow(test_data[[2]]), function(x) names(which.max(test_data[[2]][x,])))
# noaug <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training cohort 2: augmented data set 2")
#######################
train_data_extra <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_subaug_set4.rds"))
# model <- load_model_hdf5("output/finalditch/models/cohort2_post_set1_run1.h5")
model %>% fit(train_data_extra[[1]], train_data_extra[[2]], batch_size = floor(nrow(train_data_extra[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=2, verbose=1)))

# file_name = paste0("output/finalditch/models/cohort2_post_set1_run2.h5")
# model %>% save_model_hdf5(file_name)

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
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training cohort 2: back to original data 2")
#######################
# train_data <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_noaug_set2.rds"))
model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1, 
              callbacks=list(callback_early_stopping(patience=2, monitor = "val_loss", verbose=1)))

# file_name = paste0("output/finalditch/models_t3/cohort2_set",i,"_run1.h5")
# model %>% save_model_hdf5(file_name)

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
# pred_classes <- colnames(test_data[[2]])[model %>% predict_classes(test_data[[1]])]
# act_classes <- sapply(1:nrow(test_data[[2]]), function(x) names(which.max(test_data[[2]][x,])))
# noaug <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training cohort 2: augmented data set 3")
#######################
train_data_extra <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_subaug_set5.rds"))
# model <- load_model_hdf5("output/finalditch/models/cohort2_post_set1_run1.h5")
model %>% fit(train_data_extra[[1]], train_data_extra[[2]], batch_size = floor(nrow(train_data_extra[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1,
              callbacks=list(callback_early_stopping(patience=2, verbose=1)))

# file_name = paste0("output/finalditch/models/cohort2_post_set1_run2.h5")
# model %>% save_model_hdf5(file_name)

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
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))

#######################
print("training cohort 2: back to original data 3")
#######################
# train_data <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_noaug_set2.rds"))
model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1, 
              callbacks=list(callback_early_stopping(patience=2, monitor = "val_loss", verbose=1)))

# file_name = paste0("output/finalditch/models_t3/cohort2_set",i,"_run1.h5")
# model %>% save_model_hdf5(file_name)

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
# pred_classes <- colnames(test_data[[2]])[model %>% predict_classes(test_data[[1]])]
# act_classes <- sapply(1:nrow(test_data[[2]]), function(x) names(which.max(test_data[[2]][x,])))
# noaug <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x)))
