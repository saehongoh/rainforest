require(dplyr)
require(reshape2)
require(matrixStats)
require(data.table)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

key <- list.files("output/processed_data_set4/", pattern="key", full.names = TRUE)
key <- do.call(rbind, lapply(1:length(key), function(x) readRDS(key[x])))

duration <- key %>%
  mutate(duration = round(duration, digits=3)) %>%
  select(species_id, multiplier, duration) %>%
  distinct(.) %>%
  mutate(species_id = (as.character(species_id)))

multi_list <- unique(duration$multiplier)

test_files <- list.files("data/test_wav", pattern="wav", full.names = TRUE)
test_files <- data.frame(recording_id = do.call(rbind, strsplit(test_files, "/|.wav"))[,4], file_id = test_files)


############
status_x <- rep(x = NA, times = nrow(test_files))
pb <- txtProgressBar(0, length(status_x), style = 3)
############
  
for(i in 1:nrow(test_files)){
  #########################
  setTxtProgressBar(pb, i)
  status_x[i] 
  #########################
  
  for(j in 1:length(multi_list)){
    tmp <- specmaker(test_files[i,], low_f = 50, high_f = 8500, window_multiplier = multi_list[j])  %>%
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
    
    fwrite(tmp, paste0("test_files/",test_files$recording_id[i],"_", multi_list[j], ".txt"), row.names = F, col.names = T, quote = F, sep = '\t')
  }
}
