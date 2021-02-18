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


test_keras_prepper <- function(test_file_wav, train_dim, window_multiplier=2){
  
  train_dim <- unlist(train_dim)
  y_dim <- train_dim[3]
  x_dim <- train_dim[2]
  
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
  
  frames <-  data.frame(zts = unique(tmp$zts), 
                        breaks1 = cut(unique(tmp$zts), include.lowest=TRUE, breaks=seq(0, max(unique(tmp$zts)), y_dim-1)), 
                        breaks2 = cut(unique(tmp$zts), include.lowest=TRUE, breaks=seq(floor(y_dim*0.2), floor(y_dim*0.2)+ max(unique(tmp$zts)), y_dim-1)), 
                        breaks3 = cut(unique(tmp$zts), include.lowest=TRUE, breaks=seq(floor(y_dim*0.4), floor(y_dim*0.4)+ max(unique(tmp$zts)), y_dim-1)), 
                        breaks4 = cut(unique(tmp$zts), include.lowest=TRUE, breaks=seq(floor(y_dim*0.6), floor(y_dim*0.6)+ max(unique(tmp$zts)), y_dim-1)), 
                        breaks5 = cut(unique(tmp$zts), include.lowest=TRUE, breaks=seq(floor(y_dim*0.8), floor(y_dim*0.8)+ max(unique(tmp$zts)), y_dim-1))) %>%
    mutate_all(., function(x) ifelse(is.na(x), 0, as.character(x))) %>%
    melt(., id.vars=c("zts")) %>%
    filter(value != 0) %>%
    mutate(frame_id = paste0(variable, "_", value)) %>%
    select(-variable, -value) %>%
    mutate(zts = as.numeric(as.character(zts)))
  
  tmp_z <- tmp %>%
    left_join(frames, ., by ="zts") %>%
    group_by(frame_id, FreqHz) %>%
    mutate(zts2 = 1:n()) %>%
    ungroup() %>%
    mutate(frame_id = paste0(zts, "_", frame_id)) %>%
    mutate(zts = as.numeric(as.character(zts2)))
  
  freq_bin <- data.frame(FreqHz = unique(tmp$FreqHz), 
                         FreqBin1 = cut(unique(tmp$FreqHz), 4, labels = c(1,1,2,2)),
                         FreqBin2 = cut(unique(tmp$FreqHz), 4, labels = c(0,3,3,0)))
  
  f1 <- tmp_z %>%
    dplyr::select(frame_id, zts, value, FreqHz) %>%
    left_join(., freq_bin, by="FreqHz") %>%
    filter(FreqBin1 == 1) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  f2 <- tmp_z %>%
    dplyr::select(frame_id, zts, value, FreqHz) %>%
    left_join(., freq_bin, by="FreqHz") %>%
    filter(FreqBin1 == 2) %>%
    dcast(frame_id + zts ~ FreqHz) 
  
  f3 <- tmp_z %>%
    dplyr::select(frame_id, zts, value, FreqHz) %>%
    left_join(., freq_bin, by="FreqHz") %>%
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
  output_x_lab <- f1 %>% filter(zts == 1) %>% select(frame_id)
  
  if(all(dim(output_x)[-1] == unlist(train_dim)[-1])){print("all good")} else {print("dimensions don't match")}
  return(list(output_x, output_x_lab))
}




test_files_full <- list.files("data/test_wav", pattern="wav", full.names = TRUE)
test_files_full <- data.frame(recording_id = do.call(rbind, strsplit(test_files_full, "/|.wav"))[,4], file_id = test_files_full)

test_files <- test_files_full[test_files_full$recording_id %in% sample(test_files_full$recording_id, 10),]

export_list <- vector('list', nrow(top_res))
for(j in 1:nrow(top_res)){
  model1 <- keras::load_model_hdf5(top_res$results_name[j])
  res_list <- vector('list', length(test_files))
  
  for(i in 1:10){
    print(paste0("looking for species ___ ",top_res$species_id[j] , " ___ in ____ ", test_files[i,]$recording_id))
    test_x <- test_keras_prepper(test_file_wav =  test_files[i,], train_dim =  top_res$dim_x[j], window_multiplier = top_res$multiplier[j])
    res_list[[i]] <- cbind(test_files[i,], species_id = top_res$species_id[j], value = model1 %>% keras::predict_classes(test_x[[1]]), row.names = NULL)
  }
  
  export_list[[j]] <- do.call(rbind, res_list)
}

saveRDS(do.call(rbind, export_list), "predictions.RDS")