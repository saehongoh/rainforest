

args = commandArgs(trailingOnly=TRUE)

# Rscript --vanilla sillyScript.R iris.txt out.txt

## Setup
if(length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} 

# print(args[[1]])

require(dplyr, warn.conflicts = F, quietly = T)
require(reshape2, warn.conflicts = F, quietly = T)
require(matrixStats, warn.conflicts = F, quietly = T)
require(keras)
# library(data.table, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns_bash.R")
top_res <- readRDS("output/top_res.RDS")

# args <- c(list("data/wavs/batch1/000316da7.wav"), list("data/test_wav/41402125e.wav"))
test_files <- unlist(args[[1]])
file_name <- unlist(strsplit(test_files,"/|.wav"))[4]

existing <- do.call(rbind, strsplit(list.files("results_proba/"),"_"))[,1]
if(file_name %in% existing){
  
  stop("predictions already exist")
  
} else{
  

print(paste0("working on __ ", file_name))

test_f25 <- test_reader(test_file_wav =  test_files, window_multiplier = 2.5)
test_f3 <- test_reader(test_file_wav =  test_files, window_multiplier = 3)
test_f4 <- test_reader(test_file_wav =  test_files, window_multiplier = 4)

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

    class <- load_model_hdf5(top_res$results_name[j]) %>% predict_classes(test_x[[1]])
    prob <- load_model_hdf5(top_res$results_name[j]) %>% predict_proba(test_x[[1]]) %>%
      round(., digits = 3)
    colnames(prob) = c('prob0', 'prob1')

    cbind(file_name, test_files, species_id = top_res$species_id[j], class, prob,
          row.names = NULL)
}

res_list <- lapply(1:nrow(top_res), function(x) make_preds(x))

saveRDS(do.call(rbind, res_list), paste0("bash/results_proba/", file_name,"_proba.RDS"))
}

# data.table::rbindlist(res_list)
# 
# test <- list.files("bash/wavs/", pattern=".wav", recursive = TRUE)
# length(unique(do.call(rbind, (strsplit(test, "/")))[,2]))
