library(tuneR, warn.conflicts = F, quietly = T) # nice functions for reading and manipulating .wav files
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

### Import data keys
## False positives
train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  # mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  # mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  # mutate(species_id = as.numeric(as.character(species_id))) %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup()

## True positives
train_tp <- read.csv("data/train_tp.csv") %>% mutate(cate = "true") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  # mutate(songtype_id = ifelse(species_id == 16, 1, songtype_id)) %>%
  # mutate(species_id = ifelse(songtype_id == 4, paste0(species_id,".",songtype_id), species_id)) %>%
  # mutate(species_id = as.numeric(as.character(species_id)))  %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup()

### Filter out some lone positive samples
# train_tp <- train_tp %>%
#   filter(!(species_id == 5 & duration > 3)) %>%
#   filter(!(species_id == 13 & duration > 3)) %>%
#   mutate(species_id = ifelse(species_id == 20 & duration > 3.5, species_id + 0.4, species_id))

# train_fp <- train_fp %>%
#   # filter(!(species_id == 5 & duration > 3)) %>%
#   # filter(!(species_id == 13 & duration > 3)) %>%
#   mutate(species_id = ifelse(species_id == 20 & duration > 3.5, species_id + 0.4, species_id))

# train_fp <- train_fp %>%
#   filter(species_id == 20) %>%
#   mutate(species_id = 20 + 0.4) %>%
#   rbind(., train_fp)

train_tp <- train_tp %>%
  mutate(score1 = duration/60) %>%
  mutate(score2 = (f_max-f_min)/max(train_tp$f_max)) %>%
  mutate(score3 = score1*score2) %>%
  mutate(tscore = score3/max(score3))

train_tp <- train_tp %>%
  mutate(t_min = ifelse(duration < 1.5,t_min - duration/2, t_min)) %>%
  mutate(t_max = ifelse(duration < 1.5, t_max + duration/2, t_max)) %>%
  mutate(duration = t_max - t_min)

# train_tp %>%
#  select(species_id, f_min, f_max) %>%
#  distinct(.) %>%
#  melt(., id.var="species_id") %>%
#  ggplot(aes(x=as.factor(species_id), y=value)) +
#  geom_line()

train_fp <- train_fp %>%
  mutate(score1 = duration/60) %>%
  mutate(score2 = (f_max-f_min)/max(train_tp$f_max)) %>%
  mutate(score3 = score1*score2)  %>%
  mutate(tscore = -score3/max(score3))

train_fp <- train_fp %>%
  mutate(t_min = ifelse(duration < 1.5,t_min - duration/2, t_min)) %>%
  mutate(t_max = ifelse(duration < 1.5, t_max + duration/2, t_max)) %>%
  mutate(duration = t_max - t_min)

file_counts <- merge(data.frame(table(train_tp$species_id)), data.frame(table(train_fp$species_id)), by="Var1")
colnames(file_counts) <- c('species_id', 'n_true', 'n_false')

org_tp <- train_tp %>%
  mutate(unique_id = paste0(unique_id, "_org"))

org_fp <- train_fp %>%
  mutate(unique_id = paste0(unique_id, "_org"))

full_files <- rbind(org_fp)
full_files <- full_files[sample(nrow(full_files)),]

############
status_x <- rep(x = NA, times = nrow(full_files))
pb <- txtProgressBar(0, length(status_x), style = 3)
############
  
for(i in 1:nrow(full_files)){
  
  #########################
  setTxtProgressBar(pb, i)
  status_x[i] 
  #########################
  
  tmp_key <- full_files[i,]
  # tmp_key <- full_files[full_files$recording_id == "81b493ca3", ]
  res <- specmaker(tmp_key, low_f = 50, high_f = 14000, window_multiplier = 4)  %>%
    group_by(FreqHz) %>%
    mutate(value = value - mean(value)) %>%
    ungroup() %>%
    # mutate(value = (value - min(value)) / (max(value) - min(value)))  %>%
    mutate(zts = seqt - min(seqt)) %>%
    group_by(zts) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()  %>%
    mutate(value = scale(value, center=TRUE)) %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    mutate(value = value/max(value)) 
  
  # res %>%
  #   ggplot(aes(x = time, y = FreqHz, col = value)) +
  #   geom_tile() +
  #   geom_vline(xintercept=tmp_key$t_min, col="black", alpha=1, lty=3) +
  #   geom_vline(xintercept=tmp_key$t_max, col="black", alpha=1, lty=3) +
  #   geom_hline(yintercept=tmp_key$f_min, col="black", alpha=1, lty=3) +
  #   geom_hline(yintercept=tmp_key$f_max, col="black", alpha=1, lty=3) +
  #   scale_color_gradient2(low = ("white"), mid = "white", high = ("red")) +
  #   facet_wrap(cate~unique_id, nrow=6) + theme_bw()
  
  res <- res %>%
    select(unique_id, species_id, cate, tscore, FreqHz, t_min, t_max, f_min, f_max, time, value, zts) %>%
    mutate(value = ifelse(FreqHz < f_min | FreqHz > f_max, 0, value)) %>%
    mutate(value = ifelse(time < t_min | time > t_max, 0, value)) %>%
    ungroup() %>%
    # mutate(unique_id = paste0(unique_id, "_m")) %>%
    select(-f_min, -f_max)

  # res2 %>%
  #   ggplot(aes(x = time, y = FreqHz, col = value)) +
  #   geom_tile() +
  #   geom_vline(xintercept=tmp_key$t_min, col="black", alpha=1, lty=3) +
  #   geom_vline(xintercept=tmp_key$t_max, col="black", alpha=1, lty=3) +
  #   geom_hline(yintercept=tmp_key$f_min, col="black", alpha=1, lty=3) +
  #   geom_hline(yintercept=tmp_key$f_max, col="black", alpha=1, lty=3) +
  #   scale_color_gradient2(low = ("white"), mid = "white", high = ("red")) +
  #   scale_x_continuous(breaks=seq(0, 60, 10)) +
  #   facet_wrap(cate~unique_id, nrow=6) + theme_bw()
  
  saveRDS(res, paste0("output/ind_processed/ind_",unique(res$unique_id), "_", unique(res$species_id),"_", unique(res$cate),".rds"))
  saveRDS(tmp_key, paste0("output/ind_processed/key_",unique(res$unique_id), "_", unique(res$species_id),"_", unique(res$cate),".rds"))
  
  if(unique(res$cate) == "true"){
    
    j=1
    repeat{
      
      if(unique(res$t_min) > 30){
        shift_time = -sample(seq(10,25,5),1)
        add_time = 60
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time < 0, time + add_time, time))
        
      } else if(unique(res$t_min) < 30){
        shift_time = sample(seq(10,25,5),1)
        add_time = -60
        res <- res %>%
          mutate(time = time + shift_time) %>%
          mutate(time = ifelse(time > 60, time + add_time, time))
      }
      
      res <- res %>%
        mutate(unique_id = gsub("_org","_aug", unique_id))
      
      tmp_key <- tmp_key %>%
        mutate(unique_id = gsub("_org","_aug", unique_id))
      
      saveRDS(res, paste0("output/ind_processed/ind_",unique(res$unique_id), j, "_", unique(res$species_id),"_", unique(res$cate),".rds"))
      saveRDS(tmp_key, paste0("output/ind_processed/key_",unique(res$unique_id),j, "_", unique(res$species_id),"_", unique(res$cate),".rds"))
    if(j==5){break}
    j=j+1
    }
    
    
  }
  
  # test %>%
  #   ggplot(aes(x = time, y = FreqHz, col = value)) +
  #   geom_tile() +
  #   geom_vline(xintercept=tmp_key$t_min, col="black", alpha=1, lty=3) +
  #   geom_vline(xintercept=tmp_key$t_max, col="black", alpha=1, lty=3) +
  #   geom_hline(yintercept=tmp_key$f_min, col="black", alpha=1, lty=3) +
  #   geom_hline(yintercept=tmp_key$f_max, col="black", alpha=1, lty=3) +
  #   scale_color_gradient2(low = ("white"), mid = "white", high = ("red")) +
  #   scale_x_continuous(breaks=seq(0, 60, 10)) +
  #   facet_wrap(cate~unique_id, nrow=6) + theme_bw()
  
  }
