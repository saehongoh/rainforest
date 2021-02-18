require(dplyr)
## True positives
train_tp <- read.csv("data/train_tp.csv") %>% mutate(cate = "true") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3))

train_fp <- read.csv("data/train_fp.csv") %>% mutate(cate = "false") %>%
  mutate(file_id = paste0("data/train_wav/",recording_id, ".wav")) %>%
  mutate(t_min = round(t_min, digits=4)) %>%
  mutate(t_max = round(t_max, digits=4)) %>%
  mutate(duration = round(t_max - t_min, digits=3))

train_key <- rbind(train_tp, train_fp)  %>%
  group_by(species_id)  %>%
  mutate(t_max = ifelse(species_id == 13 & duration != min(duration), t_max - (duration - min(duration)), t_max)) %>%
  mutate(t_max = ifelse(species_id == 9 & duration != max(duration), t_max + (max(duration) - duration), t_max)) %>%
  mutate(duration = round(t_max - t_min, digits=3)) %>%
  ungroup()

### Normalize by species
train_key <- train_key  %>%
  group_by(species_id) %>%
  mutate(f_min = min(f_min), f_max = max(f_max)) %>%
  mutate(duration_diff = max(duration) - duration) %>%
  mutate(lower_bound =  (t_min-duration_diff) < 0) %>%
  mutate(upper_bound =  (t_max-duration_diff) > 60) %>%
  mutate(t_min = ifelse(duration <= max(duration) & !lower_bound, t_min - (duration_diff), t_min)) %>%
  mutate(t_max = ifelse(duration <= max(duration) & lower_bound, t_max + (duration_diff), t_max)) %>%
  mutate(duration = round(t_max - t_min, digits=1))  %>%
  ungroup() %>%
  mutate(cohort = case_when(f_max > 5900 ~ "cohort3", 
                            f_max <= 5900 & duration <= 3 ~ "cohort1",
                            f_max <= 5900 & duration >= 3 ~ "cohort2"))

### Normalize by cohort
train_key <- train_key %>%
  group_by(recording_id) %>%
  mutate(unique_id = paste(recording_id, 1:n(), sep="_")) %>%
  ungroup() %>%
  group_by(cohort) %>%
  mutate(duration_diff = max(duration) - duration) %>%
  mutate(lower_bound =  (t_min-duration_diff) < 0) %>%
  mutate(upper_bound =  (t_max-duration_diff) > 60) %>%
  mutate(t_min = ifelse(duration <= max(duration) & !lower_bound, t_min - (duration_diff), t_min)) %>%
  mutate(t_max = ifelse(duration <= max(duration) & lower_bound, t_max + (duration_diff), t_max)) %>%
  mutate(duration = round(t_max - t_min, digits=1))  %>%
  select(-duration_diff, -lower_bound, -upper_bound) %>%
  mutate(t_max = ifelse(t_max > 60, 60, t_max))

# table(train_key$species_id, train_key$duration, train_key$cohort)
# table(train_key$species_id, train_key$cohort)
# table(train_key$species_id, train_key$duration)
# table(train_key$duration, train_key$cohort)
# 
# train_tp %>%
#   select(species_id, duration) %>%
#   distinct(.) %>%
#   arrange(species_id)

# train_key %>%
#   filter(cohort == "cohort2")

# windows <- data.frame(t_min=seq(0,50, 10), t_max=seq(10,60,10), window=paste0(rep("window", 6), seq(1,6,1)))
# require(IRanges)
# rangesA <- IRanges::IRanges(start = windows$t_min, end = windows$t_max, names= windows$window)
# rangesB <- IRanges::IRanges(start = train_key$t_min, end= train_key$t_max, names= train_key$unique_id)
# ov <- IRanges::findOverlaps(rangesB, rangesA, type="within")
# 
# tmp <- data.frame(unique_id = train_key$unique_id[queryHits(ov)],
#                   window = windows$window[subjectHits(ov)],
#                   w_start = windows$t_min[subjectHits(ov)],
#                   w_end = windows$t_max[subjectHits(ov)])  %>%
#   group_by(unique_id) %>%
#   slice_max(., w_start)

# train_key %>%
#   select(species_id, f_min, f_max) %>%
#   distinct(.) %>%
#   melt(., id.var = "species_id") %>%
#   ggplot(aes(x=as.factor(species_id), y=value)) +
#   geom_hline(yintercept=5700) +
#   geom_hline(yintercept=4800, col="red") +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ value < 5900, scale="free_x")

# train_key %>%
#   select(species_id, f_min, f_max) %>%
#   distinct(.) %>%
#   melt(., id.var = "species_id") %>%
#   ggplot(aes(x=as.factor(species_id), y=value)) +
#   geom_hline(yintercept=5900) +
#   geom_hline(yintercept=4600, col="red") +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ value < 4600, scale="free_x")

# 
# train_key %>%
#   filter(cohort == "cohort3") %>%
#   select(species_id, duration) %>%
#   distinct(.) %>%
#   arrange(duration)
########
### Note to self: put species 10 into both cohorts. Currently in cohort 2
########

# res <- specmaker(train_key[5,], low_f = 50, high_f = 14000, window_multiplier = 1)  %>%
#   filter(time > w_start & time < w_end) %>%
#   # filter(FreqHz > f_min & FreqHz < f_max) %>%
#   group_by(FreqHz) %>%
#   mutate(value = value - mean(value)) %>%
#   ungroup() %>%
#   mutate(value = round(value, digits=3))
#
# res %>%
#   ggplot(aes(x=time, y=FreqHz, fill=value)) +
#   geom_tile() +
#   geom_hline(yintercept=5700) +
#   geom_hline(yintercept=4600, col="red") +
#   scale_fill_gradient2(low = ("white"), mid = "white", high = ("red"))

cohort1 <- train_key %>%
  filter(cohort == "cohort1") 

cohort2 <- train_key %>%
  filter(cohort == "cohort2")

cohort3 <- train_key %>%
  filter(cohort == "cohort3")

#### PARSE TESTING SET

cohort1 <- cohort1[sample(1:nrow(cohort1)),]
cohort2 <- cohort2[sample(1:nrow(cohort2)),]
cohort3 <- cohort3[sample(1:nrow(cohort3)),]

cohort1_test <- cohort1 %>%
  group_by(species_id, cate) %>%
  sample_n(., 5) %>%
  mutate(test = "yes") 

cohort1_train <- cohort1 %>%
  filter(!(unique_id %in% cohort1_test$unique_id)) %>%
  mutate(test = "no")

cohort2_test <- cohort2 %>%
  group_by(species_id, cate) %>%
  sample_n(., 5) %>%
  mutate(test = "yes")

cohort2_train <- cohort2 %>%
  filter(!(unique_id %in% cohort2_test$unique_id)) %>%
  mutate(test = "no")

cohort3_test <- cohort3 %>%
  group_by(species_id, cate) %>%
  sample_n(., 5) %>%
  mutate(test = "yes")

cohort3_train <- cohort3 %>%
  filter(!(unique_id %in% cohort3_test$unique_id)) %>%
  mutate(test = "no")

saveRDS(rbind(cohort1_test, cohort1_train), "output/finalditch/cohort1_key.rds")
saveRDS(rbind(cohort2_test, cohort2_train), "output/finalditch/cohort2_key.rds")
saveRDS(rbind(cohort3_test, cohort3_train), "output/finalditch/cohort3_key.rds")
