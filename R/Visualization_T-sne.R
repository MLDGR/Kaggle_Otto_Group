library(ggplot2)
library(readr)
library(Rtsne)
library(plot3D)
library(rgl)
set.seed(1)
num_rows_sample <- 61878

train <- read_csv("train.csv")
train_sample <- train[sample(1:nrow(train), size = num_rows_sample),]
features <- train_sample[,c(-1, -95)]

tsne <- Rtsne(as.matrix(features), check_duplicates = FALSE, pca = TRUE,
              perplexity=30, theta=0.5, dims=3)
tsne$Y
embedding <- as.data.frame(tsne$Y)
embedding$Class <- as.numeric(sub("Class_", "", train_sample[,95]))
plot<-plot3d(x=embedding$V1, y=embedding$V2 ,z=embedding$V3,col=embedding$Class, pch = ".")
