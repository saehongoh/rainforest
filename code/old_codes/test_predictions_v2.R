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

test_keras_prepper_v2 <- function(tmp, train_dim){
  
  train_dim <- unlist(train_dim)
  y_dim <- train_dim[3]
  zts <- as.numeric(as.character(unique(tmp$zts)))
  max_zt <- max(unique(tmp$zts))
  
  frames <-  data.frame(zts = zts, 
                        breaks1 = cut(zts, include.lowest=TRUE, breaks=seq(0, max_zt, y_dim-1)), 
                        breaks2 = cut(zts, include.lowest=TRUE, breaks=seq(floor(y_dim*0.5), floor(y_dim*0.5) + max_zt, y_dim-1))) %>%
    mutate_all(., function(x) ifelse(is.na(x), 0, as.character(x))) %>%
    melt(., id.vars=c("zts")) %>%
    filter(value != 0) %>%
    mutate(frame_id = paste0(variable, "_", value)) %>%
    select(-variable, -value) %>%
    mutate(zts = as.numeric(as.character(zts)))
  
  tmp <- tmp %>%
    left_join(frames, ., by ="zts") %>%
    group_by(frame_id, FreqHz) %>%
    mutate(zts2 = 1:n()) %>%
    ungroup() %>%
    mutate(zts = as.numeric(as.character(zts2)))
  
  freq_bin <- data.frame(FreqHz = unique(tmp$FreqHz), 
                         FreqBin1 = cut(unique(tmp$FreqHz), 4, labels = c(1,1,2,2)),
                         FreqBin2 = cut(unique(tmp$FreqHz), 4, labels = c(0,3,3,0)))
  tmp <- tmp %>%
    select(frame_id, zts, value, FreqHz) %>%
    left_join(., freq_bin, by="FreqHz") 
  
  f1 <- tmp %>%
    filter(FreqBin1 == 1) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    filter(FreqBin1 == 2) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    filter(FreqBin2 == 3) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% filter(zts == times[x])%>%select(-frame_id, -zts) %>% as.matrix(),
         f2 %>% filter(zts == times[x])%>%select(-frame_id, -zts) %>% as.matrix(),
         f3 %>% filter(zts == times[x])%>%select(-frame_id, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp_z$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  if(all(dim(output_x)[-1] != unlist(train_dim)[-1])){print("dimensions don't match")}
  return(list(output_x))
}


test_files_full <- list.files("data/test_wav", pattern="wav", full.names = TRUE)
test_files_full <- data.frame(recording_id = do.call(rbind, strsplit(test_files_full, "/|.wav"))[,4], file_id = test_files_full)

# test_files <- test_files_full[test_files_full$recording_id %in% sample(test_files_full$recording_id, 10),]

test_files <- test_files_full[1:498,],
# test_files <- test_files_full[499:996,]
# test_files <- test_files_full[997:1494,]
# test_files <- test_files_full[1495:1992,]

export_list <- vector('list', nrow(test_files))

for(i in 1:nrow(test_files)){
  print(paste0("working on ___ ", i, " ___ out of ___ ",nrow(test_files), " : ",test_files[i,]$recording_id))

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
    
    system.time(
    res_list[[j]] <- cbind(test_files[i,], species_id = top_res$species_id[j], 
                           value = keras::load_model_hdf5(top_res$results_name[j]) %>% keras::predict_classes(test_x[[1]]), 
                           row.names = NULL)
    )
  }
  
  export_list[[i]]  <- do.call(rbind, res_list)
  
}

saveRDS(do.call(rbind, export_list), "predictions.RDS")