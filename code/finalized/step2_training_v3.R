
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

# test <- load_model_hdf5("output/finalized/trained_models2/cohort1_r1_set10.h5")
# summary(test)

#######################
print("preparing for cohort-wise training")
#######################
args = commandArgs(trailingOnly=TRUE)
# args[[1]] = c("cohort1")

# files <- list.files("output/finalized/keras_frames", pattern=args[[1]], full.names = TRUE)
# test_file <- files[grepl("test", files)]
# test_data <- train_prepper(readRDS(test_file))
# train_files <- files[!(grepl("test", files))]

#######################
print(paste0("training  " , args[[1]]))
#######################

training_log = paste0("output/finalized/full_run/",args[[1]], "_traininglog.txt")
if(!file.exists(training_log)){
  tmp_frame <- data.frame(timestamp = format(Sys.time(),format='%H:%M'), 
                          cohort=args[[1]], round= 0, set = 0, test_loss=0, test_acc= 0, true_pos_rate =0, false_pos_rate =  0)
  data.table::fwrite(tmp_frame, training_log, sep="\t", row.names = FALSE, quote = FALSE)
}

r=as.numeric(as.character(args[[2]]))

repeat{
  
  if(r <= 3){
    print("using training set")
    files <- list.files("output/finalized/keras_frames", pattern=args[[1]], full.names = TRUE)
    dropout1 = 0.60
    dropout2 = 0.50
    # batch_div = 10
  } else if(r > 3 & r <=6){
    print("using all")
    files <- list.files("output/finalized/keras_frames_all", pattern=args[[1]], full.names = TRUE)
    dropout1 = 0.65
    dropout2 = 0.55
    # batch_div = 8
  } else {
    print("using raw")
    files <- list.files("output/finalized/keras_raw", pattern=args[[1]], full.names = TRUE)
    dropout1 = 0.80
    dropout2 = 0.70
    # batch_div = 8
  }
  
  test_file <- files[grepl("test", files)]
  test_data <- train_prepper(readRDS(test_file))
  train_files <- files[!(grepl("test", files))]
  
  model <- keras_model_sequential()
  
  output_neurons <- dim(test_data[[2]])[2]
  
  neurons1 <- dplyr::case_when(args[[1]] == "cohort1" ~ output_neurons*16, 
                               args[[1]] == "cohort2" ~ output_neurons*8, 
                               args[[1]] == "cohort3" ~ output_neurons*14)
  neurons2 <- dplyr::case_when(args[[1]] == "cohort1" ~ output_neurons*6, 
                               args[[1]] == "cohort2" ~ output_neurons*2, 
                               args[[1]] == "cohort3" ~ output_neurons*6)
  # neurons1 <- output_neurons*8
  # neurons2 <- output_neurons*2
  # neurons3 <- output_neurons*2
  
  model %>%
    layer_conv_2d(input_shape = c(dim(test_data[[1]])[-1]),
                  filters = 16, kernel_size = c(4,4), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(4,2), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 16, kernel_size = c(2,2), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%  
    layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 32, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 32, kernel_size = c(4,2), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%   
    layer_conv_2d(filters = 64, kernel_size = c(4,4), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(2,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(4,2), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(2,2), activation = 'tanh', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%  
    layer_dense(units = neurons1, activation = "relu", 
                bias_regularizer = regularizer_l2(l=0.01)) %>%
    layer_dropout(rate = dropout1) %>%
    layer_dense(units = neurons2, activation = "relu", 
                bias_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dropout(rate = dropout2) %>%
    layer_dense(units = neurons2, activation = "relu", 
                bias_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dropout(rate = dropout2) %>%
    layer_activation(activation = 'relu') %>%
    layer_flatten() %>%
    layer_dense(units = output_neurons, 
                activity_regularizer = regularizer_l2(l = 0.01), 
                activation = "softmax")
  
  if(!is.null(args[[3]])){
    start_i = 1
  } else {
    start_i = as.numeric(as.character(args[[3]]))
  }
  
  for(i in start_i:length(train_files)){
    print(paste0(args[[1]], " : round : ", r, " : set : ", i," : training"))
    
    if(r == 1 & i == 1){
      model %>% compile(loss = 'categorical_crossentropy', metrics = c("accuracy"), optimizer = optimizer_adam(lr=1e-3))
      summary(model)
    } else {
      
      # tmp <- data.table::fread(training_log)
      # tmp <- tmp %>% top_n(., test_acc, n=6) %>%
      #   top_n(., -test_loss, n=5)
      # print(tmp)
      # 
      # top <- tmp %>% top_n(., test_acc, n=1) %>%
      #   top_n(., -test_loss, n=1)
      # print(top)
      # 
      # tmp <- paste0("output/finalized/full_run/", tmp$cohort, "_r", tmp$round, "_set", tmp$set, ".h5")
      # top <- paste0("output/finalized/full_run/", top$cohort, "_r", top$round, "_set", top$set, ".h5")
      
      model_list <- list.files("output/finalized/full_run", pattern=args[[1]], full.names = TRUE)
      model_list <- model_list[!grepl(".txt", model_list)]
      # model_list <- model_list[(model_list %in% tmp)]
      # model_list <- model_list[!(model_list %in% top)]
      
      # model = load_model_hdf5(filepath = top)
      # model %>% evaluate(test_data[[1]], test_data[[2]])
      
      print(paste0("using pretrained weights : loading ", length(model_list), " weights"))
      x=1
      repeat{
        model %>% load_model_weights_hdf5(filepath = model_list[x])
        if(x == length(model_list)){break}
        x=x+1
      }
      
      learning_rate <- dplyr::case_when(r <= 2 ~ 1e-3, r >= 3 ~ 1e-4)
      print(learning_rate)
      model %>% compile(loss = 'categorical_crossentropy', metrics = c("accuracy"), optimizer = optimizer_adam(lr=learning_rate))
      
    }
    
    # patience_m = ifelse(r == 2, 6, 4)
    patience_m <- dplyr::case_when(r <= 2 ~ 6, r == 3 ~ 6, r == 3 ~ 4, r == 4 ~ 6, r == 5 ~ 6, r >= 6 ~ 4)
    print(patience_m)
    batch_div  <- dplyr::case_when(r == 1 ~ 10, r == 2 ~ 9, r == 3 ~ 8, r == 4 ~ 10, r == 5 ~ 9, r >= 6 ~ 8)
    print(batch_div)
    
    train_data <- train_prepper(readRDS(train_files[i]))
    print(dim(train_data[[1]]))
    
    model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/batch_div), epochs = 100, shuffle = TRUE, 
                  validation_split=0.15,
                  verbose=1,
                  callbacks=list(callback_early_stopping(patience=patience_m, verbose=1)))
    
    print(paste0(i," evaluating on test set"))
    
    eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
    preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
    colnames(preds) <- colnames(test_data[[2]])
    preds <- preds[,grepl("true", colnames(preds))]
    res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
    res_frame <- data.frame(timestamp = format(Sys.time(),format='%H:%M'), 
                            cohort = args[[1]], round = r, set = i, 
                            test_loss = round(eval1[[1]], digits=4),
                            test_acc= round(eval1[[2]], digits=4), 
                            true_pos_rate = round(mean(res$true_positive/res$total_actual), digits=4),
                            false_pos_rate =  round(mean(res$false_positive/res$total_actual), digits=4))

    print(paste0("round : ", r, " : set : ", i," : current progress"))
    print(res_frame)
    
    file_name = paste0("output/finalized/full_run/",args[[1]], "_r",r,"_set",i,".h5")
    print(file_name)
    model %>% save_model_hdf5(file_name)
    
    tmp <- data.table::fread(training_log)
    tmp <- rbind(tmp, res_frame)
    data.table::fwrite(tmp, training_log, sep="\t", row.names = FALSE, quote = FALSE)
    # if(i==1){break}
    }
  if(r>=4){break}
  r=r+1
}

# # ifelse(preds[,2] > quantile(preds[,2], 0.95), 1, 0)
# # ifelse(preds[,2] > preds[,1], 0.95), 1, 0)
# 
# # ###### Check
# trained_model <- keras::load_model_hdf5("output/finalized/full_run/cohort2_r5_set10.h5")
# eval1 <- trained_model %>% evaluate(train_data[[1]], train_data[[2]])
# preds <- trained_model %>% predict_proba(train_data[[1]]) %>% round(., digits = 3)
# colnames(preds) <- colnames(train_data[[2]])
# preds <- preds[,grepl("true", colnames(preds))]
# res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 1)))
# res_frame <- data.frame(timestamp = format(Sys.time(),format='%H:%M'), 
#                         cohort = args[[1]], round = r, set = i, 
#                         test_loss = round(eval1[[1]], digits=4),
#                         test_acc= round(eval1[[2]], digits=4), 
#                         true_pos_rate = round(mean(res$true_positive/res$total_actual), digits=4),
#                         false_pos_rate =  round(mean(res$false_positive/res$total_actual), digits=4))
# 
# print(paste0("round : ", r, " : set : ", i," : current progress"))
# print(res_frame)
# 
# # preds
# # act <- test_data[[2]]
# # colnames()
# colnames(preds) <- colnames(test_data[[2]])
# 
# preds <- preds[,grepl("true", colnames(preds))]
# act<- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]
# 
# data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.88)))
# res_frame <- data.frame(set = i, test_accuracy= eval1[[2]], true_positive_rate = mean(res$true_positive/res$total_actual),
#                         false_positive_rate =  mean(res$false_positive/res$total_actual))
