library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(matrixStats, warn.conflicts = F, quietly = T)
# library(data.table, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")


data_key <- list.files("output/processed_data_set4/", pattern="key", full.names = TRUE)
data_key <- do.call(rbind, lapply(1:length(data_key), function(x) readRDS(data_key[x])))

durations <- data_key %>%
  mutate(duration = round(duration, digits=3)) %>%
  group_by(species_id, multiplier) %>%
  summarise(duration = max(duration)) %>%
  mutate(species_id = as.character(species_id))

train_key <- list.files("output/trained_models_set5/", pattern="res", full.names=TRUE)
train_key <- do.call(rbind, lapply(1:length(train_key), function(x) readRDS(train_key[x])))

train_key$ver <- do.call(rbind, strsplit(train_key$results_name,"_"))[,4]

top_res <- train_key %>%
  filter(ver != "V1") %>%
  group_by(species_id) %>%
  # select(-dim_x, -dim_y) %>%
  arrange(-test_accuracy) %>%
  slice_max(test_accuracy, n=1) %>%
  slice_min(test_loss, n=1) %>%
  left_join(., durations, by = "species_id") %>%
  mutate(multiplier = ifelse(species_id == 13.4, 2.5, multiplier))  %>%
  mutate(multiplier = ifelse(species_id == 5.4, 3, multiplier))

top_res <- top_res %>%
  filter(!(species_id %in% c("13.4", "5.4", "20.4")))



keras_predictions <- function(files){
  
  test_files <- files
  
  for(i in 1:nrow(test_files)){
    print(paste0("working on __ ", i, " __ out of __ ",nrow(test_files), " : ",test_files[i,]$recording_id))
    
    test_f25 <- test_reader(test_file_wav =  test_files[i,], window_multiplier = 2.5)
    test_f3 <- test_reader(test_file_wav =  test_files[i,], window_multiplier = 3)
    test_f4 <- test_reader(test_file_wav =  test_files[i,], window_multiplier = 4)
    
    ############
    status_x <- rep(x = NA, times = nrow(top_res))
    pb <- txtProgressBar(0, length(status_x), style = 3)
    ############
    
    make_preds <- function(j){
      #########################
      setTxtProgressBar(pb, j)
      status_x[j]
      #########################
      
      # print(top_res$species_id[j])
      if(top_res$multiplier[j] == 4){
        test_x <- test_keras_prepper_v2(tmp =  test_f4, train_dim =  top_res$dim_x[j])
      } else if(top_res$multiplier[j] == 3){
        test_x <- test_keras_prepper_v2(tmp =  test_f3, train_dim =  top_res$dim_x[j])
      } else if(top_res$multiplier[j] == 2.5){
        test_x <- test_keras_prepper_v2(tmp =  test_f25, train_dim =  top_res$dim_x[j])
      }
      
      class <- keras::load_model_hdf5(top_res$results_name[j]) %>% keras::predict_classes(test_x[[1]])
      prob <- keras::load_model_hdf5(top_res$results_name[j]) %>% keras::predict_proba(test_x[[1]]) %>%
        round(., digits = 3)
      colnames(prob) = c('prob0', 'prob1')
      
      cbind(test_files[i,], species_id = top_res$species_id[j], class, prob,
                             row.names = NULL)
    }
    
    res_list <- lapply(1:nrow(top_res), function(x) make_preds(x))

    saveRDS(data.table::rbindlist(res_list), paste0("results_proba/", test_files[i,]$recording_id,"_proba.RDS"))
    
    ##########################################################################
    rm(test_f25)
    rm(test_f3)
    rm(test_f4)
    gc(reset = TRUE)
    ##########################################################################
    
  }
  
}


test_files_full <- list.files("data/test_wav", pattern="wav", full.names = TRUE)
test_files_full <- data.frame(recording_id = do.call(rbind, strsplit(test_files_full, "/|.wav"))[,4], file_id = test_files_full)

batch_files <- test_files_full[1:498,]
batch_files <- batch_files[26:nrow(batch_files),]
batch_files <- batch_files[20:nrow(batch_files),]
batch_files <- batch_files[35:nrow(batch_files),]
batch_files <- batch_files[18:nrow(batch_files),]
batch_files <- batch_files[62:nrow(batch_files),]
batch_files <- batch_files[45:nrow(batch_files),]
batch_files <- batch_files[27:nrow(batch_files),]

keras_predictions(batch_files)



# test <- readRDS("results_proba/000316da7_proba.RDS")
# test2 <- test %>% 
#   filter(species_id == 21) %>%
#   group_by(recording_id, file_id, species_id) %>%
#   mutate(max_row = n(), row_seq = 1:n())  %>%
#   mutate(frame = c(rep("f1",unique(max_row)/3), rep("f2",unique(max_row)/3), rep("f3", unique(max_row)/3))) %>%
#   # mutate(frame = ifelse(row_seq <= max_row/, "f1", "f2")) %>%
#   ungroup() %>%
#   group_by(recording_id, file_id, species_id, frame) %>%
#   mutate(frame_seq = 1:n())
# 
# test2 %>%
#   # filter(species_id == 7) %>%
#   ggplot(aes(x=frame_seq, group=frame_seq, y=prob1)) +
#   ylim(0,1) +
#   geom_boxplot()
# 
# test2 %>%
#   select(prob1, frame_seq, frame) %>%
#   dcast(frame_seq  ~ frame, value.var = "prob1") %>%
#   mutate(average_conf = (f1 + f2 + f3)/3) 
