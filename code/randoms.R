
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

#######################
print(paste0("training  " , args[[1]]))
#######################

training_log = paste0("output/finalized/full_run/",args[[1]], "_traininglog.txt")

print("using training set")
files <- list.files("output/finalized/keras_frames", pattern=args[[1]], full.names = TRUE)

test_file <- files[grepl("test", files)]
test_data <- train_prepper(readRDS(test_file))
train_files <- files[!(grepl("test", files))]


tmp <- data.table::fread(training_log)
# tmp <- tmp %>% top_n(., test_acc, n=6) %>%
#   top_n(., -test_loss, n=5)
# print(tmp)
# 
# top <- tmp %>% top_n(., test_acc, n=1) %>%
#   top_n(., -test_loss, n=1)
# print(top)

# tmp <- paste0("output/finalized/full_run/", tmp$cohort, "_r", tmp$round, "_set", tmp$set, ".h5")
# top <- paste0("output/finalized/full_run/", top$cohort, "_r", top$round, "_set", top$set, ".h5")
# 
model_list <- list.files("output/finalized/full_run", pattern=args[[1]], full.names = TRUE)
model_list <- model_list[!grepl(".txt", model_list)]
# model_list <- model_list[(model_list %in% tmp)]
# model_list <- model_list[!(model_list %in% top)]
tmp2 <- tmp %>%
  filter(round <=3) %>%
  top_n(., test_acc, n=1)

tmp3 <- tmp %>%
  filter(round > 3) %>%
  top_n(., test_acc, n=1)

models <- rbind(tmp2, tmp3) %>%
  arrange(-test_acc, test_loss)

models$file <- paste0("output/finalized/full_run/", models$cohort, "_r", models$round, "_set", models$set, ".h5")

model = load_model_hdf5(filepath = models$file[1])
model %>% evaluate(test_data[[1]], test_data[[2]])

x=2
repeat{
  print(models$file[x])
  model %>% load_model_weights_hdf5(filepath = models$file[x])
  if(x == nrow(models)){break}
  x=x+1
}

eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
preds <- low %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
colnames(preds) <- colnames(test_data[[2]])
preds <- preds[,grepl("true", colnames(preds))]
res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
res_frame1 <- data.frame(timestamp = format(Sys.time(),format='%H:%M'), 
                        cohort = args[[1]], 
                        test_loss = round(eval1[[1]], digits=4),
                        test_acc= round(eval1[[2]], digits=4), 
                        true_pos_rate = round(mean(res$true_positive/res$total_actual), digits=4),
                        false_pos_rate =  round(mean(res$false_positive/res$total_actual), digits=4))

res
res_frame1
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
