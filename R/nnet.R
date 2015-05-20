# set seed
set.seed(1337)

source("Load-Transform-Data.R")

# fit and predict
fit<-nnet(target ~ ., train, size = 9, rang = 0.1, decay = 5e-4, maxit = 500)
predicted<-as.data.frame(predict(fit,test,type="raw"))

id<-1:dim(test)[1]
output<-cbind(id,predicted)
write.csv(output,file="neural_network_1.csv",row.names=FALSE)
