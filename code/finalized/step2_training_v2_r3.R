
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
args[[1]] = c("cohort1")
# 
files <- list.files("output/finalized/keras_frames_test", pattern=args[[1]], full.names = TRUE)
test_file <- files[grepl("test", files)]
test_data <- train_prepper(readRDS(test_file))

# train_files <- files[!(grepl("test", files))]
train_files <- files

#######################
print("creating sequential model")
#######################

pre_trained <- dplyr::case_when(args[[1]] == "cohort1" ~ "output/finalized/trained_models/cohort1_r2_set20.h5",
                                args[[1]] == "cohort2" ~ "output/finalized/trained_models/cohort2_r2_set10.h5",
                                args[[1]] == "cohort3" ~ "output/finalized/trained_models/cohort3_r2_set20.h5")
print(pre_trained)
# model_ori <- keras::load_model_hdf5(pre_trained)

model <- keras::load_model_hdf5(pre_trained)

# model$output %>% 
#     layer_dropout(rate = 0.9)  %>%
#     layer_dense(units = dim(test_data[[2]])[2], activation = "softmax",  kernel_regularizer = regularizer_l2(l = 0.01)) 

#######################
print(paste0("training  " , args[[1]]))
#######################
eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
tmp_frame <- data.frame(cohort=args[[1]], round= 0, set = 0, test_accuracy= eval1[[2]], 
                        true_positive_rate = mean(res$true_positive/res$total_actual), 
                        false_positive_rate =  mean(res$false_positive/res$total_actual))
data.table::fwrite(tmp_frame, paste0("output/finalized/",args[[1]], "_tmpres_r3.txt"), sep="\t", row.names = FALSE, quote = FALSE)

r=3
repeat{
  for(i in 1:length(train_files)){
    print(paste0(args[[1]], " : round : ", r, " : set : ", i," : training"))
    
    train_data <- train_prepper(readRDS(train_files[i]))
    # train_data <- train_prepper(readRDS(train_cohort[2]))
    dim(train_data[[1]])
    
    model %>% fit(train_data[[1]], train_data[[2]], batch_size = floor(nrow(train_data[[2]])/4), epochs = 50, shuffle = TRUE, 
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
    
    tmp <- data.table::fread(paste0("output/finalized/",args[[1]],"_tmpres_r3.txt"))
    tmp <- rbind(tmp, res_frame)
    data.table::fwrite(tmp, paste0("output/finalized/",args[[1]],"_tmpres_r3.txt"), sep="\t", row.names = FALSE, quote = FALSE)
    }
  if(r==4){break}
  r=r+1
}


