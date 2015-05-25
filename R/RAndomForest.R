source("Load-Transform-Data.R")
depth<-26
mtries<-14
ntree<-2300
impotance<-T
balance<-T
vars<-(ncol(train)-1)

model <- h2o.randomForest(x = 1:(ncol(train)-1),depth =10 ,type = "BigData",
                          y = ncol(train),
                          data = dat_h2o,
                          classification = TRUE,
                          ntree = 1800,
                          importance = TRUE,
                          balance.classes = T)



## Using the DNN model for predictions
h2o_yhat_test <- h2o.predict(model, testh2o)

## Converting H2O format into data frame
df_yhat_test <- as.data.frame(h2o_yhat_test)

(df_yhat_test)[1,-1]


pred<-(df_yhat_test)[,-1]


pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))


write.csv(pred,file=paste('subRF_',depth,mtries,ntree,impotance,balance,vars,".csv"), quote=FALSE,row.names=FALSE)

