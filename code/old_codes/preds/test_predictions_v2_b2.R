require(dplyr)
require(reshape2)
require(matrixStats)

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


test_reader <- function(test_file_wav, window_multiplier=2){
  
  tmp <- specmaker(test_file_wav, low_f = 50, high_f = 8500, window_multiplier = window_multiplier)  %>%
    group_by(FreqHz) %>%
    mutate(value = value - mean(value)) %>%
    ungroup() %>%
    mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
    mutate(zts = seqt - min(seqt) + 1) %>%
    ungroup() %>%
    group_by(zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value)) 
  
  return(tmp)}


keras_predictions <- function(files, x){
  
  test_files <- files[x,]
  
  for(i in 1:nrow(test_files)){
    print(paste0("working on __ ", i, " __ out of __ ",nrow(test_files), " : ",test_files[i,]$recording_id))
    
    test_f25 <- test_reader(test_file_wav =  test_files[i,], window_multiplier = 2.5)
    test_f3 <- test_reader(test_file_wav =  test_files[i,], window_multiplier = 3)
    test_f4 <- test_reader(test_file_wav =  test_files[i,], window_multiplier = 4)
    
    res_list <- vector('list', nrow(top_res))
    
    ############
    status_x <- rep(x = NA, times = nrow(top_res))
    pb <- txtProgressBar(0, length(status_x), style = 3)
    ############
    
    for(j in 1:nrow(top_res)){
      
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
      
      res_list[[j]] <- cbind(test_files[i,], species_id = top_res$species_id[j], class, prob,
                             row.names = NULL)
      rm(test_x)
      
    }
    
    saveRDS(do.call(rbind, res_list), paste0("results/", test_files[i,]$recording_id,"_proba.RDS"))
    
    ##########################################################################
    rm(list = setdiff(ls(), lsf.str()))
    ##########################################################################
    
  }
  
}


test_files_full <- list.files("data/test_wav", pattern="wav", full.names = TRUE)
test_files_full <- data.frame(recording_id = do.call(rbind, strsplit(test_files_full, "/|.wav"))[,4], file_id = test_files_full)

test_files <- test_files_full[499:996,]

batches <- split(1:nrow(test_files), ceiling(1:nrow(test_files)/20))

for(i in 1:length(batches)){
  predictions(test_files, batches[[i]])
}

