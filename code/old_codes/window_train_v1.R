library(dplyr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T)
setwd("/Volumes/R2D2/Users/eoh/Documents/R_projects/rainforest/")

source("code/fns.R")


print("##########################################################################")
print("############################### firing up ################################")
print("##########################################################################")

train_x <- readRDS("output/multispecies/whole_train_x.RDS")
train_x_lab <- readRDS("output/multispecies/whole_train_x_lab.RDS")
train_y <- readRDS("output/multispecies/whole_train_y.RDS")
train_y_lab <- readRDS("output/multispecies/whole_train_y_lab.RDS")

max_iter = 7

# do.call(rbind, strsplit(list.files("output_trained_whole_set1", pattern="orig"), "_"))

# #### Model iterations
iter_list <- vector('list', max_iter)
j=1

repeat {
  print(paste(j," ____ out ____ of ____ ", max_iter))

  model <- keras_model_sequential()

  model %>%
    layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                  filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    # layer_conv_2d(filters = 8, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
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
    # layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dense(units = dim(train_x_lab)[2], activation = 'sigmoid',  kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    # layer_dense(units = 2, activation = 'sigmoid') %>%
    compile(
      loss = 'categorical_crossentropy',
      metrics = c("accuracy"),
      optimizer = optimizer_adam(lr=1e-4)
    )

  model %>%  fit(train_x, train_x_lab, batch_size = 32, epochs = 100, shuffle = TRUE,
                 validation_split=0.20,
                 verbose=1,
                 callbacks=list(callback_early_stopping(patience=3, verbose=1)))

  eval_x <- model %>% evaluate(train_x, train_x_lab)
  eval_y <- model %>% evaluate(train_y, train_y_lab)

  file_name = paste0("output/trained_whole_set1/orig_iter_", j, ".h5")

  model %>% save_model_hdf5(file_name)

  export <- data.frame(iter = j, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
             test_loss = eval_y[[1]], test_accuracy = eval_y[[2]],
             results_name = file_name)

  saveRDS(export, paste0("output/trained_whole_set1/orig_iter_", j, ".RDS"))

  if(j==max_iter){break}
  j = j + 1
}


##########################################################################
rm(list = setdiff(ls(), lsf.str()))
##########################################################################


print("##########################################################################")
print("############################### firing up ################################")
print("##########################################################################")

train_x <- readRDS("output/multispecies/whole_train_x4.RDS")
train_x_lab <- readRDS("output/multispecies/whole_train_x_lab4.RDS")
train_y <- readRDS("output/multispecies/whole_train_y4.RDS")
train_y_lab <- readRDS("output/multispecies/whole_train_y_lab4.RDS")


max_iter = 7

# #### Model iterations
iter_list <- vector('list', max_iter)
j=1

repeat {
  print(paste(j," ____ out ____ of ____ ", max_iter))

  model <- keras_model_sequential()

  model %>%
    layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                  filters = 32, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
    layer_conv_2d(filters = 64, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
    layer_conv_2d(filters = 128, kernel_size = c(4,4), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
    layer_dense(units = dim(train_x_lab)[2]*10, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dropout(rate = 0.20) %>%
    # layer_dense(units = dim(train_x_lab)[2], activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    # layer_dropout(rate = 0.20) %>%
    # layer_dense(units = dim(train_x_lab)[2], activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    # layer_dropout(rate = 0.20) %>%
    layer_activation(activation = 'relu') %>%
    layer_flatten() %>%
    # layer_dense(units = 16, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dense(units = dim(train_x_lab)[2], activation = 'softmax', kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    # layer_dense(units = 2, activation = 'sigmoid') %>%
    compile(
      loss = 'categorical_crossentropy',
      metrics = c("accuracy"),
      optimizer = optimizer_adam(lr=1e-3)
    )
  
  model %>%
    layer_conv_2d(input_shape = c(dim(train_x)[-1]),
                  filters = 32, kernel_size = c(3,3), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%   #--------Max Pooling
    layer_dropout(rate = 0.25) %>%
    layer_flatten() %>%
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = dim(train_x_lab)[2], activation = 'softmax', kernel_regularizer = regularizer_l2(l = 0.01)) %>%   
    compile(
      loss = 'categorical_crossentropy',
      metrics = c("accuracy"),
      optimizer = optimizer_adam(lr=1e-3)
    )

  model %>%  fit(train_x, train_x_lab, batch_size = 32, epochs = 100, shuffle = TRUE,
                 validation_split=0.20,
                 verbose=1,
                 callbacks=list(callback_early_stopping(patience=3, verbose=1)))

  eval_x <- model %>% evaluate(train_x, train_x_lab)
  eval_y <- model %>% evaluate(train_y, train_y_lab)

  file_name = paste0("output/trained_whole_set1/mod_iter_", j, ".h5")

  model %>% save_model_hdf5(file_name)

  export <- data.frame(iter = j, train_loss = eval_x[[1]], train_accuracy = eval_x[[2]],
                       test_loss = eval_y[[1]], test_accuracy = eval_y[[2]],
                       results_name = file_name)

  saveRDS(export, paste0("output/trained_whole_set1/mod_iter_", j, ".RDS"))

  if(j==max_iter){break}
  j = j + 1
}