
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
print("preparing for cohort-wise training")
#######################
args = commandArgs(trailingOnly=TRUE)
# args[[1]] = c("cohort1")

files <- list.files("output/finalized/keras_frames", pattern=args[[1]], full.names = TRUE)
test_file <- files[grepl("test", files)]
test_data <- train_prepper(readRDS(test_file))

train_files <- files[!(grepl("test", files))]

#######################
print("creating sequential model")
#######################

model <- keras_model_sequential()

model %>%
  layer_conv_2d(input_shape = c(dim(test_data[[1]])[-1]),
                filters = 16, kernel_size = c(4,4), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 16, kernel_size = c(4,2), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_dropout(rate = 0.1) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(4,2), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_dropout(rate = 0.1) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_conv_2d(filters = 64, kernel_size = c(4,4), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(4,2), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # layer_dropout(rate = 0.1) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
  layer_dense(units = 240, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dropout(rate = 0.6) %>%
  layer_dense(units = 120, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_dense(units = 120, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.50) %>%
  layer_activation(activation = 'relu') %>%
  layer_flatten() %>%
  layer_dense(units = dim(test_data[[2]])[2], activation = "softmax",  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
  compile(
    loss = 'categorical_crossentropy',
    metrics = c("accuracy"),
    optimizer = optimizer_adam(lr=1e-3)
  )

# model <- keras::load_model_hdf5("output/finalditch/model_t5/cohort2_set10.h5")

#######################
print(paste0("training  " , args[[1]]))
#######################
tmp_frame <- data.frame(cohort=args[[1]], round= 0, set = 0, test_accuracy= 0, true_positive_rate =0, false_positive_rate =  0)
data.table::fwrite(tmp_frame, paste0("output/finalized/",args[[1]], "_tmpres.txt"), sep="\t", row.names = FALSE, quote = FALSE)

r=1
repeat{
  for(i in 1:length(train_files)){
    print(paste0(args[[1]], " : round : ", r, " : set : ", i," : training"))
    
    train_data <- train_prepper(readRDS(train_files[i]))
    # train_data <- train_prepper(readRDS(train_cohort[2]))
    dim(train_data[[1]])
    
    model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/10), epochs = 50, shuffle = TRUE, 
                  validation_split=0.15,
                  verbose=1,
                  callbacks=list(callback_early_stopping(patience=4, verbose=1)))
    
    print(paste0(i," evaluating on test set"))
    
    eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
    preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
    colnames(preds) <- colnames(test_data[[2]])
    preds <- preds[,grepl("true", colnames(preds))]
    res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
    res_frame <- data.frame(cohort = args[[1]], round = r, set = i, 
                            test_accuracy= eval1[[2]], 
                            true_positive_rate = mean(res$true_positive/res$total_actual),
                            false_positive_rate =  mean(res$false_positive/res$total_actual))

    print(paste0("round : ", r, " : set : ", i," : current progress"))
    print(res_frame)
    file_name = paste0("output/finalized/trained_models/",args[[1]], "_r",r,"_set",i,".h5")
    print(file_name)
    model %>% save_model_hdf5(file_name)
    
    tmp <- data.table::fread(paste0("output/finalized/",args[[1]],"_tmpres.txt"))
    tmp <- rbind(tmp, res_frame)
    data.table::fwrite(tmp, paste0("output/finalized/",args[[1]],"_tmpres.txt"), sep="\t", row.names = FALSE, quote = FALSE)
    }
  if(r==2){break}
  r=r+1
}

# ifelse(preds[,2] > quantile(preds[,2], 0.95), 1, 0)
# ifelse(preds[,2] > preds[,1], 0.95), 1, 0)

# ###### Check
# trained_model <- keras::load_model_hdf5("output/finalditch/model_t6/cohort2_r2_set10.h5")
# eval1 <- trained_model %>% evaluate(test_data[[1]], test_data[[2]])
# preds <- trained_model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
# 
# # preds
# # act <- test_data[[2]]
# # colnames()
# colnames(preds) <- colnames(test_data[[2]])
# 
# preds <- preds[,grepl("true", colnames(preds))]
# act<- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]
# 
# data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.92)))
# res_frame <- data.frame(set = i, test_accuracy= eval1[[2]], true_positive_rate = mean(res$true_positive/res$total_actual),
#                         false_positive_rate =  mean(res$false_positive/res$total_actual))
