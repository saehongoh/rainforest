
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


prediction_counter <- function(x, cut_off){
  act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]
  cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = cut_off)
  preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
  colnames(preds2) <- colnames(preds)
  total_actual = length(which(act[,x] == 1))
  total_predicted = length(which(preds2[,x] == 1))
  true_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == TRUE))
  false_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == FALSE))
  data.frame(species_id = colnames(act)[x], total_actual, total_predicted, true_positive = true_positive, false_positive)
}

#######################
print("cohort-wise training")
#######################

# test_data <- train_prepper(readRDS(test_tmp$file_id[1]))
test_data <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_test_01false_noaug.rds"))
# dim(train_data[[1]])
dim(test_data[[1]])

#######################
print("preparing sequential model")
#######################

model <- keras_model_sequential()

model %>% 
  layer_conv_2d(input_shape = c(dim(test_data[[1]])[-1]),
                filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_conv_2d(filters = 64, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_dense(units = dim(test_data[[2]])[2]*4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.6) %>%
  layer_dense(units = dim(test_data[[2]])[2]*2, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_dense(units = dim(test_data[[2]])[2]*2, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(test_data[[2]])[2], activation = "softmax",  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"), 
    optimizer = optimizer_adam(lr=1e-3)
  )

#######################
print("training cohort 2: original data")
#######################
train_data <- train_prepper(readRDS("output/finalditch/cohort_t4/cohort2_train_01false_subaug_set1.rds"))
# dim(train_data[[1]])

model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
              validation_split=0.15,
              verbose=1, 
              callbacks=list(callback_early_stopping(patience=3, monitor = "val_loss", verbose=1)))

print("evalulating on testing data")

eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
res_frame <- data.frame(set = 0, test_accuracy= eval1[[2]], true_positive_rate = mean(res$true_positive/res$total_actual),
                        false_positive_rate =  mean(res$false_positive/res$total_actual))
print("current progress")
print(res_frame)
#######################
print("Additional training")
#######################
train_cohort <- list.files("output/finalditch/cohort_t3_noguas", pattern="cohort2", full.names = TRUE)
train_cohort <- train_cohort[grepl("set", train_cohort)]
train_cohort <- data.frame(do.call(rbind, strsplit(train_cohort,"/|_|[.]rds"))[,5:9], file_id = train_cohort)
colnames(train_cohort)[1:5] <- c('cohort','test','type','aug','set')

for(i in 1:nrow(train_cohort)){
  train_data_extra <- train_prepper(readRDS(train_cohort$file_id[i]))
  # train_data_extra <- train_prepper(readRDS(train_cohort))
  
  model %>% fit(train_data_extra[[1]], train_data_extra[[2]], batch_size = floor(nrow(train_data_extra[[2]])/10), epochs = 50, shuffle = TRUE, 
                validation_split=0.15,
                verbose=1,
                callbacks=list(callback_early_stopping(patience=2, verbose=1)))
  
  print(paste0(i,"post additiona set: evalulating on testing data"))
  eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
  
  train_data <- train_prepper(readRDS("output/finalditch/cohort_t3/cohort2_train_01false_noaug.rds"))
  model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
                validation_split=0.15,
                verbose=1, 
                callbacks=list(callback_early_stopping(patience=3, monitor = "val_loss", verbose=1)))
  
  print(paste0(i,"post back to original"))
  file_name = paste0("output/finalditch/models_t3/cohort2_set",i,".h5")
  model %>% save_model_hdf5(file_name)
  
  print(paste0(i,"post back to original: evalulating on testing data"))
  eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
  preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
  colnames(preds) <- colnames(test_data[[2]])
  preds <- preds[,grepl("true", colnames(preds))]
  res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
  res_frame2 <- data.frame(set = i, test_accuracy= eval1[[2]], true_positive_rate = mean(res$true_positive/res$total_actual),
                           false_positive_rate =  mean(res$false_positive/res$total_actual))
  res_frame <- rbind(res_frame, res_frame2)
  print(paste0(i,"current progress"))
  print(res_frame)
}
