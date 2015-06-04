library(h2o)
localH2O = h2o.init(ip = "localhost",nthreads = 6, port = 54321, startH2O = TRUE, 
                    Xmx = '6g')

train <- read.csv("train.csv/train.csv")

for(i in 2:94){
  train[,i] <- as.numeric(train[,i])
  train[,i] <- sqrt(train[,i]+(3/8))
}


test <- read.csv("test.csv/test.csv")

for(i in 2:94){
  test[,i] <- as.numeric(test[,i])
  test[,i] <- sqrt(test[,i]+(3/8))
}
#source("Load-Transform-Data.R")

#load("trainSMOTEscalelog.RData")
train.hex <- as.h2o(localH2O,train)
test.hex <- as.h2o(localH2O,test[,2:94])
#test.hex <- as.h2o(localH2O,test)

predictors <- 2:(ncol(train.hex)-1)
response <- ncol(train.hex)

submission <- read.csv("subXgBoost 0.01 14 1 1.4 0 4 0.9 0.6 3 5000 20150320 .csv")
submission[,2:10] <- 0
submissionMG <- read.csv("subXgBoost 0.01 14 1 1.4 0 4 0.9 0.6 3 5000 20150320 .csv")
submissionMG[,2:10] <- 0
submissionMGS<-submissionMG

listaPAramteros<-list(c(1024,512,256),c(2048,1024,512),c(1600,750,320))
listaPAramteros2<-list(c(0.5,0.5,0.5),c(0.5,0.5,0.5),c(0.5,0.5,0.5))


for(i in 1:40){
  print(i)
  model <- h2o.deeplearning(x=predictors,
                            balance_classes = T,variable_importances = T,
                            y=response,
                            data=train.hex,
                            classification=T,
                            activation="RectifierWithDropout",
                            hidden=c(1024,512,256),
                            hidden_dropout_ratio=c(0.5,0.5,0.5),#c(0.55,0.55,0.55),
                            input_dropout_ratio=0.05,#0.02,
                            epochs=120,#50,
                            l1=1e-5,
                            l2=1e-5,
                            rho=0.99,
                            epsilon=1e-8,
                            train_samples_per_iteration=2400,#2000,
                            max_w2=10,
                            seed=1)
  prediccion<-as.data.frame(h2o.predict(model,test.hex))[,2:10]
  submission[,2:10] <- submission[,2:10] + prediccion
  submissionMG[,2:10] <- submissionMG[,2:10] * prediccion
  submissionMGS[,2:10] <-sqrt( submissionMGS[,2:10] * prediccion)
  print(i)
  write.csv(submission,file="submissionBalanceoTImportanceT.csv",row.names=FALSE)
  write.csv(submission,file="submissionMGBalanceoTImportanceT.csv",row.names=FALSE)

}
print("Escribiendo salida")
submission[,2:10] <- submission[,2:10] /40
write.csv(submissionMGS,file="submissionPromedioMGBalanceoTImportanceT.csv",row.names=FALSE)
write.csv(submission,file="submissionPromedioBalanceoTImportanceT.csv",row.names=FALSE)

h2o.shutdown(localH2O)
Y

