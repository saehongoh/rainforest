
# library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
# library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
# require(fields, warn.conflicts = F, quietly = T)
# library(keras, warn.conflicts = F, quietly = T)
# require(ggplot2)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")
args = commandArgs(trailingOnly=TRUE)
# args[[1]] = 1
# test_files <- list.files("data/test_wav/", pattern=".wav")
# which(grepl("f1ce", test_files))
# length(test_files) - which(grepl("f1ce", test_files))

test_rds <- list.files("output/finalized/test_rds_2f", pattern = ".rds", full.names=TRUE)
test_rds <- data.frame(recording_id = do.call(rbind, strsplit(test_rds, "/|[.]"))[,4],
                       file_id = test_rds)

traininglogs <- list.files("output/finalized/full_run", pattern = ".txt", full.names=TRUE)
df1 <- data.table::rbindlist(lapply(1:length(traininglogs), function(x) data.table::fread(traininglogs[x])))
# df1 <- data.table::fread("output/finalized/model_evaluations.txt")
df1 <- df1 %>%
  group_by(cohort) %>%
  top_n(., test_acc, n=1) %>%
  top_n(., true_pos_rate, n=1) %>%
  slice_min(., test_loss)
  # slice_max(., true_pos_rate)
df1$file_id <- paste0("output/finalized/full_run/",df1$cohort,"_r",df1$round,"_set",df1$set,".h5")

cut <- nrow(test_rds)/6
if(args[[1]] == 1){
  test_rds <- test_rds[1:cut,]
  print("doing first batch")
} else if(args[[1]] == 2){
  test_rds <- test_rds[(cut+1):(cut*2),]
  print("doing second batch")
} else if(args[[1]] == 3){
  test_rds <- test_rds[((cut*2)+1):(cut*3),]
  print("doing third batch")
} else if(args[[1]] == 4){
  test_rds <- test_rds[((cut*3)+1):(cut*4),]
  print("doing fourth batch")
} else if(args[[1]] == 5){
  test_rds <- test_rds[((cut*4)+1):(cut*5),]
  print("doing fifth batch")
} else if(args[[1]] == 6){
  test_rds <- test_rds[((cut*5)+1):(nrow(test_rds)),]
  print("doing sixth batch")
}

#####################
make_preds_wrapper <- function(tmp){
  
  make_preds <- function(tmp, x){
    fetch_names <- list.files("output/finalized/keras_frames", pattern="test", full.names = TRUE)
    df_names <- lapply(1:length(fetch_names), function(x) colnames(readRDS(fetch_names[x])[[2]]))
    
    trained_models <- dplyr::case_when(x == 1 ~ df1[df1$cohort == "cohort1",]$file_id,
                                       x == 2 ~ df1[df1$cohort == "cohort2",]$file_id,
                                       x == 3 ~ df1[df1$cohort == "cohort3",]$file_id)
    # print(trained_models)
    c1_preds <- keras::load_model_hdf5(trained_models) %>% 
      # keras::predict_proba(tmp[[x]][[1]]) %>% round(., digits = 3)
      keras::predict_proba(tmp[[x]][[1]])
    
    c1_preds <- cbind(tmp[[x]][[2]], c1_preds)
    colnames(c1_preds) <- df_names[[x]]
    
    c1_preds %>%
      reshape2::melt(., id.var="unique_id") %>%
      tidyr::separate(unique_id, sep="_", c('recording_id','train_species_id','time_bin', "t_min", "t_max")) %>%
      tidyr::separate(variable, c('test_species_id','cate')) %>%
      filter(train_species_id == test_species_id) %>% 
      dcast(recording_id + train_species_id + time_bin + t_min + t_max + test_species_id ~ cate, value.var = "value") %>%
      mutate(t_min = as.numeric(as.character(t_min)),
             t_max = as.numeric(as.character(t_max))) %>%
      arrange(train_species_id, t_min)
  }
  
  # res1 <- data.table::rbindlist(lapply(1:3, function(x) make_preds(tmp, x))) %>%
  #   group_by(recording_id, test_species_id) %>%
  #   summarise(pred = max(true), .groups = 'drop') %>%
  #   dcast(recording_id ~test_species_id, value.var = "pred")
  
  res <- data.table::rbindlist(lapply(1:3, function(x) make_preds(tmp, x))) %>%
    group_by(recording_id, test_species_id)
  
  return(res)
}

export <- data.table::rbindlist(pbmcapply::pbmclapply(1:2, 
                                                      function(x) make_preds_wrapper(readRDS(test_rds$file_id[x])), 
                                                      mc.cores = parallel::detectCores() - 2,
                                                      ignore.interactive = getOption("ignore.interactive", T)))

export <- data.table::rbindlist(pbmcapply::pbmclapply(1:nrow(test_rds), 
                                                      function(x) make_preds_wrapper(readRDS(test_rds$file_id[x])), 
                                                      mc.cores = parallel::detectCores() - 2,
                                                      ignore.interactive = getOption("ignore.interactive", T)))


write.csv(export, paste0("output/finalized/submissions/fullrun2_preds_b",args[[1]], ".csv"), row.names = F, quote = F)
write.csv(df1,paste0("output/finalized/submissions/fullrun2_models_b",args[[1]], ".csv"), row.names = F, quote = F)

# # 
# files <- list.files("output/finalized/submissions", pattern="fullrun_preds", full.names = TRUE)
# export <- data.table::rbindlist(lapply(1:length(files), function(x) data.table::fread(files[x])))
# 
# template <- read.csv("data/sample_submission.csv")
# # # if(nrow(template) != nrow(export)){
# # #   print("adding missing files")
# # #   to_add <- template[!(template$recording_id %in% export$recording_id),]
# # #   export <- rbind(to_add, export) %>%
# # #     ungroup()
# # #   }
# # #
# export <- export[match(template$recording_id, export$recording_id),]
# 
# # write.csv(export, paste0("output/finalized/submissions/2f_full.csv"), row.names = F, quote = F)
# 
# # 
files <- list.files("output/finalized/submissions", pattern="fullrun2_preds", full.names = TRUE)
export <- data.table::rbindlist(lapply(1:length(files), function(x) data.table::fread(files[x])))
template <- read.csv("data/sample_submission.csv")

export <- export %>%
  group_by(recording_id, test_species_id) %>%
  # filter(false > 0.45 & true > 0.1)
  # mutate(true_adj = ifelse(false > 0.25, 0, true))  %>%
  # mutate(true_adj = ifelse(true_adj < 0.5, 0, true_adj))  %>%
  summarise(pred = max(true), .groups = 'drop') %>%
  dcast(recording_id ~test_species_id, value.var = "pred") %>%
  as.data.frame()
export <- export[,match(colnames(template), colnames(export))]
export <- export[match(template$recording_id, export$recording_id),]
write.csv(export, paste0("output/finalized/submissions/fullrun_unrounded.csv"), row.names = F, quote = F)

# export %>%
#   filter(recording_id == "fd93fb244") %>%
#   filter(test_species_id == "s19")


############# EXTRA tests
# 
# require(keras)
# models <- list.files("output/finalized/trained_models2", pattern = "h5", full.names = TRUE)
# models <- data.frame(do.call(rbind, strsplit(models, "_|/|[.]|set"))[,c(5,6,8)], files = models)
# colnames(models)[1:3] <- c('cohort','round','set')
# models <- models %>%
#   mutate(set = as.numeric(as.character(set))) %>%
#   arrange(cohort, round, set)
# 
# table_maker <- function(models, i){
#   tmp <- models[i,]
#   test_file <- dplyr::case_when(tmp$cohort == "cohort1" ~ "output/finalized/keras_frames_all/cohort1_test_01false_noaug.rds",
#                                 tmp$cohort == "cohort2" ~ "output/finalized/keras_frames_all/cohort2_test_01false_noaug.rds",
#                                 tmp$cohort == "cohort3" ~ "output/finalized/keras_frames_all/cohort3_test_01false_noaug.rds")
#   
#   train_prepper <- function(input){
#     train <- input[[1]]
#     train_lab <- input[[2]]
#     rownames(train_lab) <- NULL
#     train_lab <- train_lab[,2:ncol(train_lab)] %>% as.matrix()
#     list(train, train_lab)
#   }
#   prediction_counter <- function(x, cut_off){
#     act <- test_data[[2]][,grepl("true", colnames(test_data[[2]]))]
#     cutoff <- matrixStats::colQuantiles(preds, na.rm=TRUE, probs = cut_off)
#     preds2 <- do.call(cbind, lapply(1:ncol(preds), function(x) ifelse(preds[,x] >= cutoff[x], 1, 0)))
#     colnames(preds2) <- colnames(preds)
#     total_actual = length(which(act[,x] == 1))
#     total_predicted = length(which(preds2[,x] == 1))
#     true_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == TRUE))
#     false_positive = length(which((which(preds2[,x] == 1) %in% which(act[,x] == 1)) == FALSE))
#     data.frame(species_id = colnames(act)[x], total_actual, total_predicted, true_positive = true_positive, false_positive)
#   }
#   test_data <- train_prepper(readRDS(test_file))
#   model <-  keras::load_model_hdf5(tmp$files) 
#   eval1 <- model %>% evaluate(test_data[[1]], test_data[[2]])
#   preds <- model %>% predict_proba(test_data[[1]]) %>% round(., digits = 3)
#   colnames(preds) <- colnames(test_data[[2]])
#   preds <- preds[,grepl("true", colnames(preds))]
#   res <- data.table::rbindlist(lapply(1:ncol(preds), function(x) prediction_counter(x, 0.95)))
#   
#   tmp$test_loss= round(eval1[[1]], digits=4)
#   tmp$test_acc= round(eval1[[2]], digits=4)
#   tmp$true_pos_rate = round(mean(res$true_positive/res$total_actual), digits=4)
#   tmp$false_pos_rate =  round(mean(res$false_positive/res$total_actual), digits=4)
#   tmp[,c(1,2,3,5,6,7,8,4)]
#   # return(tmp)
# }
# 
# results <- data.table::rbindlist(lapply(1:nrow(models), function(i) table_maker(models, i)))
# write.table(results %>% select(-files), "output/finalized/model_evaluations.txt",sep ="\t", row.names = F, quote = F)
# results %>%
#   group_by(cohort) %>%
#   slice_max(., true_pos_rate) %>%
#   slice_max(., test_acc)
# #####################
# 
# 
# c1_preds <- keras::load_model_hdf5(tmp$files) %>% 
#   keras::predict_proba(test_data[[1]]) %>% round(., digits = 3)
# 
# tester <- function(tmp, x){
#   fetch_names <- list.files("output/finalized/keras_frames2", pattern="test", full.names = TRUE)
#   df_names <- lapply(1:length(fetch_names), function(x) colnames(readRDS(fetch_names[x])[[2]]))
#   
#   trained_models <- dplyr::case_when(x == 1 ~ "output/finalized/trained_models2/cohort2_r5_set8.h5",
#                                      x == 2 ~ "output/finalized/trained_models2/cohort2_r5_set9.h5",
#                                      x == 3 ~ "output/finalized/trained_models2/cohort2_r5_set10.h5")
#   
#   # trained_models <- paste0("output/finalized/trained_models2/cohort1_r5_set",x,".h5")
#   
#   c1_preds <- keras::load_model_hdf5(trained_models) %>% 
#     keras::predict_proba(tmp[[1]]) %>% round(., digits = 3)
#   
#   c1_preds <- cbind(tmp[[2]], c1_preds)
#   colnames(c1_preds) <- df_names[[2]]
#   
#   c1_preds %>%
#     reshape2::melt(., id.var="unique_id") %>%
#     tidyr::separate(unique_id, sep="_", c('recording_id','train_species_id','time_bin', "t_min", "t_max")) %>%
#     tidyr::separate(variable, c('test_species_id','cate')) %>%
#     filter(train_species_id == test_species_id) %>% 
#     reshape2::dcast(recording_id + train_species_id + time_bin + t_min + t_max + test_species_id ~ cate, value.var = "value") %>%
#     mutate(t_min = as.numeric(as.character(t_min)),
#            t_max = as.numeric(as.character(t_max))) %>%
#     arrange(train_species_id, t_min)
# }
# 
# tmp <- data.table::rbindlist(lapply(1:3, function(x) tester(readRDS(test_rds$file_id[3])[[2]], x) %>% mutate(test = x)))
# 
# tmp %>%
#   filter(train_species_id == "s9") %>%
#   dcast(recording_id + train_species_id + time_bin + t_min +t_max ~test, value.var="true") %>%
#   arrange(t_min)
# 
# tmp %>%
#   filter(train_species_id == "s9") %>%
#   dcast(recording_id + train_species_id + time_bin + t_min +t_max ~test, value.var="false") %>%
#   arrange(t_min)
# 
# tmp %>%
#   group_by(recording_id, train_species_id, time_bin, t_min, t_max, test_species_id) %>%
#   summarise(false = sd(false), true = sd(true)) %>%
#   ggplot(aes(x=train_species_id, y=true)) +
#   geom_boxplot()
# 
# tmp %>%
#   filter(train_species_id == "s9") %>%
#   ggplot(aes(x=as.factor(test), y=true)) +
#   geom_boxplot() 
