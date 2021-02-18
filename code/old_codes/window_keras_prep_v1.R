
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
# library(keras, warn.conflicts = F, quietly = T)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")

keras_whole <- function(data){
  freq_bin <- data.frame(FreqHz = unique(data$FreqHz), 
                         FreqBin1 = cut(unique(data$FreqHz), 4, labels = c(1,2,3,4)))
  
  tmp <- data  %>%
    left_join(., freq_bin, by="FreqHz") %>%
    group_by(unique_id, species_id, FreqHz) %>%
    mutate(zts = 1:n()) %>%
    ungroup()
  
  f1 <- tmp %>%
    dplyr::filter(FreqBin1 == 1) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f2 <- tmp %>%
    dplyr::filter(FreqBin1 == 2) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f3 <- tmp %>%
    dplyr::filter(FreqBin1 == 3) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  f4 <- tmp %>%
    dplyr::filter(FreqBin1 == 4) %>%
    dcast(unique_id + species_id + tscore + zts ~ FreqHz) 
  
  lister <- function(x){
    list(f1 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix(),
         f2 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix(),
         f3 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix(),
         f4 %>% dplyr::filter(zts ==times[x])%>%select(-unique_id, -species_id, -tscore, -zts) %>% as.matrix())
  }
  
  times <- unique(tmp$zts)
  train <- lapply(1:length(times), function(x) lister(x))
  # train <- parallel::mclapplylapply(1:length(times), function(x) lister(x))
  
  output_x <- array(c(as.numeric(unlist(train))), dim=c(dim(train[[1]][[1]]), length(train), length(train[[1]])))
  
  output_x_lab <- f1 %>% 
    dplyr::filter(zts == 1) %>% 
    select(unique_id, species_id, tscore) %>%
    mutate(tscore = as.numeric(as.character(tscore))) %>%
    mutate(species_id = paste0("s", species_id)) %>%
    dcast(unique_id ~ species_id, value.var = "tscore") %>%
    select(-unique_id) %>%
    mutate_all(., function(x) ifelse(!is.na(x), 1, x)) %>%
    mutate_all(., function(x) ifelse(is.na(x), 0, x)) %>%
    as.matrix()
  return(list(output_x, output_x_lab))
}


x_files <- list.files("output/processed_data_whole2", pattern="dataX", full.names = TRUE)
y_files <- list.files("output/processed_data_whole2", pattern="dataY", full.names = TRUE)

#######
# randomly split into three setA, B, C
files <- gsub("_dataX.RDS","", x_files)
A <- sample(files, length(files)/3)
B <- sample(files[!(files %in% A)], length(files)/3)
C <- files[!(files %in% A | files %in% B)]

# Set1: A + B
x_files_set1 <- x_files[x_files %in% paste0(c(A, B), "_dataX.RDS")]
y_files_set1 <- y_files[y_files %in% paste0(c(A, B), "_dataY.RDS")]

# Set2: A + C
x_files_set2 <- x_files[x_files %in% paste0(c(A, C), "_dataX.RDS")]
y_files_set2 <- y_files[y_files %in% paste0(c(A, C), "_dataY.RDS")]

# Set3: B + C
x_files_set3 <- x_files[x_files %in% paste0(c(B, C), "_dataX.RDS")]
y_files_set3 <- y_files[y_files %in% paste0(c(B, C), "_dataY.RDS")]


read_rds <- function(files, x){
  print(x)
  readRDS(files[x])
}

########

print("loading data")

keras_lister <- function(files){
  files <- lapply(1:length(files), function(x) read_rds(y_files, x))
  data_y <- data.table::rbindlist(files)

  print("preparing for keras")

  tmp <- keras_whole(data_y)
  return(tmp)
}

## Y Set1
tmp <- keras_lister(y_files_set1)
train <- tmp[[1]]
train_lab <- tmp[[2]]

saveRDS(train, "output/multispecies/masked_train_y_set1.RDS")
saveRDS(train_lab, "output/multispecies/masked_train_y_lab_set1.RDS")

## Y Set2
tmp <- keras_lister(y_files_set2)
train <- tmp[[1]]
train_lab <- tmp[[2]]

saveRDS(train, "output/multispecies/masked_train_y_set2.RDS")
saveRDS(train_lab, "output/multispecies/masked_train_y_lab_set2.RDS")

## Y Set3
tmp <- keras_lister(y_files_set3)
train <- tmp[[1]]
train_lab <- tmp[[2]]

saveRDS(train, "output/multispecies/masked_train_y_set3.RDS")
saveRDS(train_lab, "output/multispecies/masked_train_y_lab_set3.RDS")


## x Set1
tmp <- keras_lister(x_files_set1)
train <- tmp[[1]]
train_lab <- tmp[[2]]

saveRDS(train, "output/multispecies/masked_train_x_set1.RDS")
saveRDS(train_lab, "output/multispecies/masked_train_x_lab_set1.RDS")

## x Set2
tmp <- keras_lister(x_files_set2)
train <- tmp[[1]]
train_lab <- tmp[[2]]

saveRDS(train, "output/multispecies/masked_train_x_set2.RDS")
saveRDS(train_lab, "output/multispecies/masked_train_x_lab_set2.RDS")

## x Set3
tmp <- keras_lister(x_files_set3)
train <- tmp[[1]]
train_lab <- tmp[[2]]

saveRDS(train, "output/multispecies/masked_train_x_set3.RDS")
saveRDS(train_lab, "output/multispecies/masked_train_x_lab_set3.RDS")


# 
# 
# print("loading data")
# 
# data_x <- lapply(1:length(x_files), function(x) read_rds(x_files, x))
# 
# data_x <- data.table::rbindlist(data_x)
# 
# print("preparing for keras")
# 
# tmp <- keras_whole(data_x)
# train_x <- tmp[[1]]
# train_x_lab <- tmp[[2]]
# 
# print("saving backup files")
# 
# saveRDS(train_x, "output/multispecies/whole_train_x_masked.RDS")
# saveRDS(train_x_lab, "output/multispecies/whole_train_x_lab_masked.RDS")

