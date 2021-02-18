
# library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)

setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")
source("code/fns.R")

####

write_summary <- function(){
keys <- list.files("output/trained_models_set5/", pattern="res", full.names=TRUE)
res_tmp <- do.call(rbind, lapply(1:length(keys), function(x) readRDS(keys[x])))

res_tmp$ver <- do.call(rbind, strsplit(res_tmp$results_name,"_"))[,4]

test <- res_tmp %>%
  filter(ver == "V2") %>%
  # filter(species_id == 10)
  mutate(total_test = test_positive + test_negatve) %>%
  mutate(wrong1 = (total_test-1)/total_test) %>%
  mutate(wrong2 = (total_test-2)/total_test) %>%
  mutate(wrong3 = (total_test-3)/total_test) %>%
  mutate(wrong4 = (total_test-4)/total_test) %>%
  # mutate(over98 = test_accuracy >= 0.979,
  #        over96 = test_accuracy < 0.979 & test_accuracy >= 0.959) %>%
  group_by(file_name, wrong1, wrong2, wrong3, wrong4, species_id) %>%
  summarise(max_acc = round(max(test_accuracy), digits=2),
            mean_acc = round(mean(test_accuracy), digits=2),
            n_at_wrong1 = length(which(test_accuracy >= wrong1 - 0.001)),
            n_at_wrong2 = length(which(test_accuracy >= wrong2 - 0.001 & test_accuracy < wrong1 - 0.001)),
            n_at_wrong3 = length(which(test_accuracy >= wrong3 - 0.001 & test_accuracy < wrong2 - 0.001)),
            # n_at_wrong3 = length(which(test_accuracy >= wrong4 - 0.001 & test_accuracy < wrong3 - 0.001)),
            max_iter = max(iter), .groups = 'drop')

write.table(test, "output/trained_models_set5/summary.txt", row.names = F, col.names = T, quote = F, sep = '\t')
return(test)
}


repeat_trainer <- function(iterations, randomize = FALSE){
  
  print("##########################################################################")
  print("############################### firing up ################################")
  print("##########################################################################")
  
  files <- list.files("output/processed_data_set4", pattern="data", full.names = TRUE)
  
  test <- write_summary()
  
  passed_species_to_omit1 <- test[test$n_at_wrong1 >= 1,]$file_name
  passed_species_to_omit2 <- test[test$n_at_wrong2 >= 2, ]$file_name
  # passed_species_to_omit2 <- test[test$n_at_wrong2 >= 3,]$file_name
  passed_species_to_omit3 <- test[test$n_at_wrong3 >= 4,]$file_name
  passed_species_to_omit4 <- test[test$max_acc >= test$wrong4 & test$max_iter >= 30,]$file_name
  
  print(paste("omitting species ____ ", sort(unique(c(passed_species_to_omit1, 
                                                 passed_species_to_omit2,
                                                 passed_species_to_omit3,
                                                 passed_species_to_omit4)))))
  
  files <- files[!(files %in% unique(c(passed_species_to_omit1, 
                                       passed_species_to_omit2, passed_species_to_omit3, 
                                       passed_species_to_omit4)))]
  
  if(randomize == TRUE){
    files <- sample(files)
  }

  max_iter = iterations
  
  ############
  status_x <- rep(x = NA, times = length(files))
  pb <- txtProgressBar(0, length(status_x), style = 3)
  ############
  
  for(i in 1:length(files)){
    
    #########################
    setTxtProgressBar(pb, i)
    status_x[i] 
    #########################
    
    data <- readRDS(files[i])
    
    data <- data %>%
      group_by(unique_id, zts) %>%
      mutate(value = value - mean(value)) %>%
      ungroup()  %>%
      mutate(value = scale(value, center=TRUE)) %>%
      mutate(value = ifelse(value < 0, 0, value))  %>%
      group_by(unique_id) %>%
      mutate(value = value/max(value)) %>%
      ungroup()
    
    species <- unlist(strsplit(files[i],"_"))[[5]]
    
    #### Model iterations V2
    iter_list <- vector('list', max_iter)
    j=1
    
    repeat {
      
      if(length(test[test$species_id == species,]$max_iter) > 0){
        iter_k = j + test[test$species_id == species,]$max_iter
      } else {
        iter_k = j
      }

      print(paste(species," ____ V2 ____ intra ____ ", j, " inter ____ ", iter_k))
      
      pos_ids <- (unique(data[data$cate == "true_positive",]$unique_id))
      negs_to_sample <- length(which(grepl("neu",pos_ids)))*5
      neg_ids <- sample(unique(data[data$cate == "false_positive",]$unique_id), negs_to_sample)
      
      if(length(pos_ids) > length(neg_ids)){
        toMatch <- paste(c("_neu","_p05","_m05"), collapse = "|")
        pos_ids <- c(pos_ids[grepl(toMatch,pos_ids)],
                     sample(pos_ids[!grepl(toMatch,pos_ids)], length(neg_ids) - length(pos_ids[grepl(toMatch,pos_ids)])))
      }
      
      data_tmp <- data %>%
        filter(unique_id %in% pos_ids | unique_id %in% neg_ids) 
      
      data_x <- data_tmp %>%
        filter(unique_id %in% sample(pos_ids, length(pos_ids)*0.9) | unique_id %in% sample(neg_ids, length(neg_ids)*0.9))
      
      data_y <- data_tmp %>%
        filter(!(unique_id %in% data_x$unique_id))
      
      train_sum <- data_x %>%
        select(unique_id, cate) %>%
        distinct() %>%
        group_by(cate) %>%
        summarise(n=n(), .groups = 'drop')
      
      test_sum <- data_y %>%
        select(unique_id, cate) %>%
        distinct() %>%
        group_by(cate) %>%
        summarise(n=n(), .groups = 'drop')
      
      tmp <- keras_prepper_v2(data_x)
      train_x <- tmp[[1]]
      train_x_lab <- tmp[[2]]
      
      tmp <- keras_prepper_v2(data_y)
      train_y <- tmp[[1]]
      train_y_lab <- tmp[[2]]
      
      res_table <- data.frame(file_name = files[i],species_id = species)
      res_table$dim_x <- list(dim(train_x))
      res_table$dim_y <- list(dim(train_y))
      res_table$train_positive <- train_sum$n[2]
      res_table$train_negatve <- train_sum$n[1]
      res_table$test_positive <- test_sum$n[2]
      res_table$test_negatve <- test_sum$n[1]
      
      rm(tmp)
      rm(data_y)
      rm(data_x)
      #########
      
      model <- keras_model_sequential()
      
      model %>% 
        layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                      filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
        layer_conv_2d(filters = 16, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
        layer_conv_2d(filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
        layer_max_pooling_2d(pool_size = c(2,2) ) %>%   #--------Max Pooling
        layer_dense(units = 8, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
        layer_dropout(rate = 0.60) %>%
        layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
        layer_dropout(rate = 0.50) %>%
        layer_dense(units = 4, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
        layer_dropout(rate = 0.50) %>%
        layer_activation(activation = 'relu') %>%
        layer_flatten() %>%
        layer_dense(units = 2, activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
        compile(
          loss = 'categorical_crossentropy',
          metrics = c("accuracy"), 
          optimizer = optimizer_adam(lr=1e-4)
        )
      
      model %>%  fit(train_x, train_x_lab, batch_size = 10, epochs = 100, shuffle = TRUE, 
                     validation_split=0.20,
                     verbose=0,
                     callbacks=list(callback_early_stopping(patience=5, verbose=1)))
      
      eval_x <- model %>% evaluate(train_x, train_x_lab)
      eval_y <- model %>% evaluate(train_y, train_y_lab)
      
      file_name = paste0("output/trained_models_set5/species_V2_", res_table$species_id, "_iter_", iter_k, ".h5")
      
      iter_list[[j]] <- cbind(res_table, data.frame(iter = iter_k, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
                                                    test_loss = eval_y[[1]], test_accuracy = eval_y[[2]], results_name = file_name))
      
      model %>% save_model_hdf5(file_name)
      rm(model)
      
      tmp <- data.frame(do.call(rbind, iter_list))
      
      
      good_early_stop1 = length(which(tmp$test_accuracy >= test[test$species_id == species,]$wrong1))
      good_early_stop2 = length(which(tmp$test_accuracy >= test[test$species_id == species,]$wrong2))
      good_early_stop3 = length(which(tmp$test_accuracy >= test[test$species_id == species,]$wrong3))
      # bad_early_stop = length(which(tmp$test_accuracy <= 0.80))
      print(paste0("average accuracy thus far _____ ", round(mean(tmp$test_accuracy), digits=3), " _____ ",
                   "max accuracy thus far _____ ", round(max(tmp$test_accuracy), digits=3)))
      
      if(good_early_stop1 >= 2){break}
      if(good_early_stop2 >= 3){break}
      if(good_early_stop2 >= 4){break}
      if(j == max_iter){break}
      
      j = j + 1
      
    }
    export <- do.call(rbind, iter_list)
    saveRDS(export, paste0("output/trained_models_set5/species_V2_", res_table$species_id, "_",paste0(min(export$iter), "-", max(export$iter)), "_res.RDS"))
    print("##########################################################################")
    print("############################## finishing up ##############################")
    print("##########################################################################")
    
    test <- write_summary()
    
  }
  
}

repeat_trainer(5)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)
repeat_trainer(5, randomize = TRUE)


