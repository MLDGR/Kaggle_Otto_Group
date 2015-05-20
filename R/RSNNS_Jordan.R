source("Load_Data.R")

modelJordan <- jordan(as.matrix(train), train[,-dim(train)[2]],shufflePatterns = T,
                      size=c(18), learnFuncParams=c(0.09), maxit=16, linOut=T)

modelJordan <- jordan(train$inputsTrain, train$targetsTrain,shufflePatterns = T, linOut=F,
                      size=c(46), learnFuncParams=c(0.01), maxit=500)
modelJordan <- jordan(trainValues, trainTargets,shufflePatterns = T, linOut=F,
                      size=c(46), learnFuncParams=c(0.01), maxit=500)
table<-confusionMatrix(train$targetsTrain,fitted.values(modelJordan))
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
print("JORDAN Train error")
mean(listError)

predictions <- predict(modelJordan,train$inputsTest)
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
print("JORDAN Test error")
mean(listError)


names(modelJordan)
par(mar=c(1,1,1,1))
par(mfrow=c(3,3))
plotIterativeError(modelJordan)
plotRegressionError(train$targetsTrain, modelJordan$fitted.values)
plotRegressionError(train$targetsTest, modelJordan$fittedTestValues)
hist(modelJordan$fitted.values - train$targetsTrain, col="lightblue")

plot(inputs, type="l")
plot(inputs[1:100], type="l")
lines(outputs[1:100], col="red")
lines(modelJordan$fitted.values[1:100], col="green")

pred <- predict(modelJordan,test)
pred = format(pred, digits=6,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))

x<-analyzeClassification(y = pred,method = "WTA")

x<-apply(pred, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

write.csv(pred,file=paste('RSNNS_MLP',".csv"), quote=FALSE,row.names=FALSE)

