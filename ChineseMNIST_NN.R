#Packages
library(keras)
library(tidyverse)
library(magick)

################################################################
#Original transformation from images to matrix
mnist_test <- NULL
for(i in list.files(path = "U:\\Downloads\\archive\\data\\test", full.names = TRUE)) {
  input <- image_read(i)
  input_data <- as.numeric(input[[1]][1, , ])
  mnist_test <- rbind(mnist_test, input_data)
}
rownames(mnist_test) <- list.files(path = "U:\\Downloads\\archive\\data\\test")

mnist_train <- NULL
for(i in list.files(path = "U:\\Downloads\\archive\\data\\train", full.names = TRUE)) {
  input <- image_read(i)
  input_data <- as.numeric(input[[1]][1, , ])
  mnist_train <- rbind(mnist_train, input_data)
}
rownames(mnist_train) <- list.files(path = "U:\\Downloads\\archive\\data\\train")

################################################################
#Load previously transformed and saved data
load("chinese_mnistv2.RData")

#create y (labels) 
y_test_c <- rep(c(1,10,2:9), 100)
y_test_c <- to_categorical(y_test_c, 11)
y_test_c <- y_test_c[,c(-1)]

y_train_c <- rep(c(1,10,2:9), 900)
y_train_c <- to_categorical(y_train_c, 11)
y_train_c <- y_train_c[,c(-1)]

#rescale to unit intervals (from gray scale values)
mnist_test <- mnist_test / 255
mnist_train <- mnist_train / 255

################################################################

#Trying out different optimization algorithms

#SGD
#training
modelnn <- keras_model_sequential()
modelnn %>%
  layer_dense(units = 512, activation = "relu",
              input_shape = c(4096)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")


modelnn %>%
  compile(loss = "categorical_crossentropy",
          optimizer = optimizer_sgd(),
          metrics = c("accuracy"))


system.time(
  history <- modelnn %>%
    fit(mnist_train , y_train_c, 
        epochs = 20, batch_size = 1,
        validation_split = 0.2))
plot(history , smooth = FALSE)

#test
modelnn %>% evaluate(mnist_test, y_test_c,verbose = 0)


##RMSProp
#training
modelnn <- keras_model_sequential()
modelnn %>%
  layer_dense(units = 128, activation = "relu",
              input_shape = c(4096)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = "softmax")


modelnn %>%
  compile(loss = "categorical_crossentropy",
          optimizer = optimizer_rmsprop(learning_rate = 0.001),
          metrics = c("accuracy"))

system.time(
  history <- modelnn %>%
    fit(mnist_train , y_train_c, 
        epochs = 20, batch_size = 128,
        validation_split = 0.2)
)
plot(history , smooth = FALSE)

#test
modelnn %>% evaluate(mnist_test, y_test_c,verbose = 0)


##Adam
#training
modelnn <- keras_model_sequential()
modelnn %>%
  layer_dense(units = 512, activation = "relu",
              input_shape = c(4096)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = "softmax")


modelnn %>%
  compile(loss = "categorical_crossentropy",
          optimizer = optimizer_adam(learning_rate = 0.001),
          metrics = c("accuracy"))

system.time(
  history <- modelnn %>%
    fit(mnist_train , y_train_c, 
        epochs = 20, batch_size = 128,
        validation_split = 0.2)
)
plot(history , smooth = FALSE)

#test
modelnn %>% evaluate(mnist_test, y_test_c,verbose = 0)


##Comparison of training set

xx <- rep(c("Loss", "Val_loss", "Accuracy","Val_accuracy"), each = 3)
x <- factor(xx, level = c("Loss", "Val_loss", "Accuracy","Val_accuracy"))
yy <- rep(c('SGD','RMSProp','Adam'),times = 4)
y <- factor(yy, level = c('SGD','RMSProp','Adam'))
z <- c(0.02806 , 0.1207 ,0.06575 , 0.8975 , 0.743 , 0.8022 ,
       0.9914, 0.9619 ,0.9796 ,  0.8239, 0.8006 , 0.8317)
comp <- data.frame(x = x, y = y, z = z)
ggplot(data = comp, mapping = aes(x = x, y = z, fill = y)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = "measure", y ="value")
