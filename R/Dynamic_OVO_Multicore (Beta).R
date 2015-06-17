#(Beta) Multicore version One vs One algorithm with dynamic reduction on competent classes with Random Forest

OVO.DYNAMIC<-function(train,test){
  library(randomForest)
  library(kknn)
  atributos<-colnames(train[,-ncol(train)])
  clases<-sort(unique(train[,ncol(train)]))
  
  OVO<-function(train,test,clases){#Generate m*(m-1)/2 binary models
    cat("####Generando",length(clases)*(length(clases)-1)/2,"#####\n")
    pred<-data.frame(X1.1=matrix(ncol = 1, nrow = nrow(test)))
    for (i in clases){
      pred<-cbind(pred,as.data.frame(mclapply (1:length(clases),function(j){ 
        pos1=match(i,clases)
        pos2=match(j,clases)
        if(i==j){#Avoid generate model with two equal class
          if(i==1){
            return(pred)
          }else if(i!=1){
            temp<-pred[1] 
            colnames(temp)=paste(pos1,pos2,sep=".") 
          }
        }else if(i<j){
          cat("OVO Random Forest",i,"frente a",j,"\n")
          #Binarize{
          aux0<-train[which(train[,dim(train)[2]]==i),]
          aux1<-train[which(train[,dim(train)[2]]==j),]
          aux0[,ncol(aux0)]<-0
          aux1[,ncol(aux1)]<-1
          aux<-rbind(aux0,aux1)
          aux[,ncol(aux)]<-as.factor(aux[,ncol(aux)])
          aux<-aux[sample(nrow(aux)),]
          #}
          model<-randomForest(as.formula(paste(names(train)[ncol(train)],"~",paste(atributos,collapse="+"))),data=aux,replace=TRUE,ntree=1000,mtry=2) 
          temp<-predict(model,newdata=test,type="prob")
          temp<-as.data.frame(temp)
          colnames(temp)[1]<-paste("X",paste(pos1,pos2,sep="."),sep="")
        }else if(i>j){  ##Avoid repeat models
          nombre1=paste("X",paste(pos2,pos1,sep="."),sep="")
          nombre2=paste("X",paste(pos1,pos2,sep="."),sep="")
          temp<-as.data.frame(1-pred[,which(nombre1==colnames(pred))]) #Probability is the reciprocal of first model 
          colnames(temp)[ncol(temp)]<-nombre2
        }
        return(temp[1])
      },         mc.preschedule = TRUE, mc.set.seed = TRUE,
      mc.silent = FALSE, mc.cores = getOption("mc.cores", 4L),
      mc.cleanup = TRUE, mc.allow.recursive = TRUE))) #mclapplys's end
      
    }
    return(pred[,-1])
  }
  
  transforma.dinamica<-function(i,pred.matrix,model.knn){
    votacion<-t(matrix(pred.matrix[i,],length(clases),length(pred.matrix)/length(clases))) ##Transform row of 82 columns into a 9x9 matrix
    votacion<-apply(votacion,1:2,as.numeric)
    numeros.clases=match(names(table(model.knn$CL[i,])),clases) #Take neighboring classes that model.knn has generated to delete non-neighbor class rows 
    if(length(numeros.clases)==1){
      return(numeros.clases)
    }else{
      votacion<-votacion[numeros.clases,numeros.clases]
      rownames(votacion)=numeros.clases
      colnames(votacion)=numeros.clases
      votacion<-cbind(votacion,apply(votacion,1,sum,na.rm=T))
      return(rownames(votacion)[which.max(votacion[,ncol(votacion)])]) #Return winner class
    }
  }
  
  pred.matrix<-OVO(train,test,clases) #Generate votation "hypermatrix" 
  cat("Generando knn vecinos\n")
  model.knn<-kknn(as.formula(paste(colnames(train)[ncol(train)],"~.")),train=train,test=test,k=3*length(clases)) #Genera la lista de vecinos
  pred.test<-t(lapply(1:nrow(test),transforma.dinamica,pred.matrix,model.knn))
  pred.test<-data.frame(matrix(unlist(pred.test), nrow=length(pred.test), byrow=T))
  colnames(pred.test)=colnames(train)[ncol(train)]
  levels(pred.test[,ncol(pred.test)])=levels(test[,ncol(test)])
  return(pred.test)
}