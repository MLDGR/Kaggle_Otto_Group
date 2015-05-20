require(RSNNS)

train = read.csv('balanceados.csv',header=TRUE,stringsAsFactors = F)
train = read.csv('train.csv/train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('test.csv/test.csv',header=TRUE,stringsAsFactors = F)
names(train)<-c(names(test),"target")
train$target<-as.factor(train$target)
train = train[,-1]
test = test[,-1]
train<-train[,c(names,"target")]
test = test[,names]
train[,-dim(train)[2]] <- log(train[,-dim(train)[2]]+1)
test<- log(test+1)
train[,-dim(train)[2]] <- scale(train[,-dim(train)[2]])
test<- scale(test)

iningAndTestSet(train)

#shuffle the vector
train <- train[sample(1:nrow(train),length(1:nrow(train))),1:ncol(train)]

trainValues <- train[,1:(dim(train)[2]-1)]
trainTargets <- decodeClassLabels(train[,dim(train)[2]])
#irisTargets <- decodeClassLabels(iris[,5], valTrue=0.9, valFalse=0.1)

train <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.2)
train <- normTrainingAndTestSet(train)

model <- mlp(train[,-dim(train)[2]], train[,dim(train)[2]], size=26, learnFuncParams=c(0.1),n.ensemble=15,pruneFunc=T,
             maxit=250,learnFunc = "Rprop", shufflePatterns = TRUE)


model <- mlp(train$inputsTrain, train$targetsTrain,, size=5, learnFuncParams=c(0.1),#,n.ensemble=15,pruneFunc=T,
             maxit=26,learnFunc = "Rprop"), shufflePatterns = TRUE)

model <- mlp(train$inputsTrain, train$targetsTrain, size=28, learnFuncParams=c(0.1),
             maxit=250, inputsTest=train$inputsTest, targetsTest=train$targetsTest,
             learnFunc = "Rprop", shufflePatterns = TRUE)


predictions <- predict(model,train$inputsTest)
confusionMatrix(train$targetsTest,predictions)
table<-confusionMatrix(train$targetsTrain,fitted.values(model))
p1<-table[1,1]/sum(table[1,])
p2<-table[2,2]/sum(table[2,])
p3<-table[3,3]/sum(table[3,])
p4<-table[4,4]/sum(table[4,])
p5<-table[5,5]/sum(table[5,])
p6<-table[6,6]/sum(table[6,])
p7<-table[7,7]/sum(table[7,])
p8<-table[8,8]/sum(table[8,])
p9<-table[9,9]/sum(table[9,])
listError<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9)
print("Train error")
mean(listError)
table<-confusionMatrix(train$targetsTest,predictions)
p1<-table[1,1]/sum(table[1,])
p2<-table[2,2]/sum(table[2,])
p3<-table[3,3]/sum(table[3,])
p4<-table[4,4]/sum(table[4,])
p5<-table[5,5]/sum(table[5,])
p6<-table[6,6]/sum(table[6,])
p7<-table[7,7]/sum(table[7,])
p8<-table[8,8]/sum(table[8,])
p9<-table[9,9]/sum(table[9,])
listError<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9)
print("Test error")
mean(listError)


summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,train$inputsTest)
pred <- predict(model,test)

plotRegressionError(predictions[,2], train$targetsTest[,2])

confusionMatrix(train$targetsTrain,fitted.values(model))
confusionMatrix(train$targetsTest,predictions)

plotROC(fitted.values(model)[,2], train$targetsTrain[,2])
plotROC(predictions[,2], train$targetsTest[,2])

confusionMatrix(iris$targetsTrain, encodeClassLabels(fitted.values(model),
                                                     method="402040", l=0.4, h=0.6))



pred = format(pred, digits=6,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))


write.csv(pred,file=paste('RSNNS_MLP',".csv"), quote=FALSE,row.names=FALSE)



mlp.default <- function(x, y, size=c(5), maxit=100,  
                        initFunc="Randomize_Weights", initFuncParams=c(-0.3, 0.3), 
                        learnFunc="Std_Backpropagation", learnFuncParams=c(0.2, 0.0), 
                        updateFunc="Topological_Order", updateFuncParams=c(0.0),
                        hiddenActFunc="Act_Logistic",
                        shufflePatterns=TRUE, linOut=FALSE, inputsTest=NULL, targetsTest=NULL, pruneFunc=NULL, pruneFuncParams=NULL, ...) 