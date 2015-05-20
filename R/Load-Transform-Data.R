source("librerias.R")
library(Hmisc)

train = read.csv('../Data/train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('../Data/test.csv',header=TRUE,stringsAsFactors = F)
train$target<-as.factor(train$target)
train = train[,-1]
test = test[,-1]
##############################################################################################
##                               Cargar datos balanceados                                   ##
##############################################################################################

# train = read.csv('balanceados.csv',header=TRUE,stringsAsFactors = F)
# test = read.csv('test.csv/test.csv',header=TRUE,stringsAsFactors = F)
# 
# load("train.SMOTE.scale.RData")
# #test=load("tests.SMOTE.scale.RData")
# names(train)<-c(names(test),"target")
# train$target<-as.factor(train$target)
# train = train[,-1]
# test = test[,-1]


Log=T
Logit=F
Scal=T
TDF_Idf=F
##############################################################################################
##                                Transformacion Logaritmica                                ##
##############################################################################################
if(Log==T){
  train[,-dim(train)[2]] <- log(train[,-dim(train)[2]]+1)
  test<- as.data.frame(log(test+1))
}
##############################################################################################
##                                Transformacion Logit                                ##
##############################################################################################
if(Logit==T){
  dat2 <-as.data.frame( sapply(train, function(x) if(!is.factor(x)) {
    plogis(x)
  } else {
    x
  }))
  train[,-dim(train)[2]]<-dat2[,-dim(dat2)[2]]
  test <- as.data.frame(sapply(test, function(x) if(!is.factor(x)) {
    plogis(x)
  }))
}
##############################################################################################
##                                Transformacion Scale                                    ##
##############################################################################################
if(Scal==T){
  train[,-dim(train)[2]] <- scale(1+train[,-dim(train)[2]])
  test<- as.data.frame(scale(1+test))
}
##############################################################################################
##                                Transformacion Tdf-idf                                   ##
##############################################################################################

if(TDF_Idf==T){
  d<-train[,-94]
  tf <- d
  idf <- log(nrow(d)/colSums(d))
  tfidf <- d
  
  for(word in names(idf)){
    tfidf[,word] <- tf[,word] * idf[word]
  }
  
  train[,-dim(train)[2]]<-tfidf
  
  d<-test
  tf <- d
  idf <- log(nrow(d)/colSums(d))
  test <- d
  
  for(word in names(idf)){
    test[,word] <- tf[,word] * idf[word]
  }
}




##############################################################################################
##                                Division por clases                                       ##
##############################################################################################

class<-as.factor(unique(train$target))
Data_clases<-lapply(1:length(class),function(x){
  train[train$target==class[x],]
})
describe(train$target)
#          Class_1 Class_2 Class_3 Class_4 Class_5 Class_6 Class_7 Class_8 Class_9
#Frequency    1929   16122    8004    2691    2739   14135    2839    8464    4955
#%               3      26      13       4       4      23       5      14       8

#posiciones<-sample(dim(vec1)[1],diff,replace = T)
#elementos<-vec1[posiciones,]


trainBalanceado<-rbind(Data_clases[[2]],Data_clases[[1]],Data_clases[[1]],Data_clases[[1]],
                       Data_clases[[1]], Data_clases[[1]], Data_clases[[1]],
                       Data_clases[[3]],Data_clases[[3]],Data_clases[[4]],Data_clases[[4]],
                       Data_clases[[4]],Data_clases[[4]],Data_clases[[4]], Data_clases[[5]],
                       Data_clases[[5]], Data_clases[[5]],Data_clases[[5]],
                       Data_clases[[5]],Data_clases[[6]],Data_clases[[7]],Data_clases[[7]],
                       Data_clases[[7]],Data_clases[[7]],Data_clases[[7]],
                       Data_clases[[8]],Data_clases[[8]],Data_clases[[9]],Data_clases[[9]],
                       Data_clases[[9]])



