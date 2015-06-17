###### Random oversampling #####

library(unbalanced)
library(caret)

roversampling<-function(data){
  clases<-sort(unique(as.character(data[,dim(data)[2]])))
  frecuencias<-as.data.frame(table(data[,dim(data)[2]]))
  mayoritaria<-as.character(frecuencias[which(frecuencias[,2]==max(frecuencias[,2])),1])
  minoritarias<-setdiff(clases,mayoritaria)
  c.sinteticos<-NULL
  for (minoritaria in minoritarias){
    cat("ROver class",minoritaria,"vs. class",mayoritaria,"\n")
    aux0<-data[which(data[,dim(data)[2]]==mayoritaria),] #binarize
    aux1<-data[which(data[,dim(data)[2]]==minoritaria),]
    aux1[,dim(aux1)[2]]<-1
    aux0[,dim(aux0)[2]]<-0
    aux<-rbind(aux0,aux1)
    aux<-aux[order(as.integer(rownames(aux))),]
    balanceado<-ubOver(aux[,-ncol(aux)],aux[,ncol(aux)],k=0) #r. oversampling with unbalanced library
    balanceado=cbind(balanceado$X,balanceado$Y)
    colnames(balanceado)[ncol(balanceado)]<-"class"
    balanceado<-balanceado[balanceado[,ncol(balanceado)]==1,]
    balanceado$target<-minoritaria
    c.sinteticos<-rbind(c.sinteticos,balanceado)
  }
  c.sinteticos<-rbind(c.sinteticos,data[which(data[,dim(data)[2]]==mayoritaria),])
  rownames(c.sinteticos)<-c(1:dim(c.sinteticos)[1])
  return(c.sinteticos)
}

#### Random undersampling

rundersampling<-function(data){
  c.sinteticos<-NULL
  clases<-sort(unique(as.character(data[,dim(data)[2]])))
  frecuencias<-as.data.frame(table(data[,dim(data)[2]]))
  minoritaria<-as.character(frecuencias[which(frecuencias[,2]==min(frecuencias[,2])),1])
  mayoritarias<-setdiff(clases,minoritaria)
  for(mayoritaria in mayoritarias){
    pclass=frecuencias[which(frecuencias[1]==minoritaria),2]
    nclass=frecuencias[which(frecuencias[1]==mayoritaria),2]
    aux=data[data[,dim(data)[2]]==mayoritaria,]
    reduce=nclass-(nclass-pclass)/(nclass+pclass)*nclass
    c.sinteticos<-rbind(c.sinteticos,aux[sample(nrow(reduce),replace=F)[1:round(reduce)],])
  }
  return(rbind(c.sinteticos,data[data[,dim(data)[2]]==minoritaria,]))
}

ovoTomek<-function(data){
  clases<-sort(unique(as.character(data[,dim(data)[2]])))
  clasesaux<-clases
  for (i in clases){
    for (j in (setdiff(clasesaux,i))){
      cat("Tomek link clase",i,"frente a clase",j,"\n")
      aux0<-data[which(data[,dim(data)[2]]==i),] #Binarice
      aux1<-data[which(data[,dim(data)[2]]==j),]
      if(dim(aux0)[1]>dim(aux1)[1]){
        aux1[,dim(aux1)[2]]<-1
        aux0[,dim(aux0)[2]]<-0
      }else{
        aux1[,dim(aux1)[2]]<-0
        aux0[,dim(aux0)[2]]<-1
      }
      
      aux<-rbind(aux0,aux1)
      aux<-aux[order(as.integer(rownames(aux))),]
      balanceado<-ubTomek(aux[,-dim(aux)[2]],aux[,dim(aux)[2]])
      if(length(balanceado$id.rm)!=0){
        data<-data[-balanceado$id.rm,]
      }
    }
    clasesaux<-setdiff(clasesaux,i)
  }
  return(data)
}


library(parallel)

######SMOTE#####
getNeighbors<-function(i,distancia){
  order(distancia[,i])[2:6]
}

synthetic.instance <- function(x, neighbors, data){
  
  # random neighbor
  n <- sample(1:5,1)
  neighbor <- data[neighbors[n],]
  
  # construct the synthetic instance
  pos <- runif(1)
  new.instance <- NULL
  nAtts <- dim(data)[2]
  for (f in 1:nAtts){
    if(is.factor(data[,f])){ # nominal feature
      random <- runif(1)
      if(random < 0.5){
        value <- x[f]
      } else {
        value <- neighbor[f]
      }
      new.instance <- c(new.instance, value)
    } else { # numeric feature
      value <- round((1 - pos) * x[f] + pos * neighbor[f])
      new.instance <- c(new.instance, value)
    }
  }
  
  return(new.instance)
}


#Generate synthetic instances

f.sinteticos<-function(i,data,distancia,minoritaria,nclass,pclass){  
  cat(i,"    ",minoritaria,"    ",nclass-pclass,"    \n")
  x <- sample(1:dim(data)[1], 1)
  
  nn <- getNeighbors(x,distancia)
  synthetic.instance(data[x,], nn,data)
}

#####GLOBALCS#######
GLOBALCS<-function(df){
  
  c.sinteticos<-NULL
  classes <-unique( as.character(df[,dim(df)[2]]))
  frecuencias<-as.data.frame(table(df[,dim(df)[2]]))
  mayoritaria<-as.character(frecuencias[which(frecuencias[,2]==max(frecuencias[,2])),1])
  minoritarias<-setdiff(classes,mayoritaria)
  for(minoritaria in minoritarias){
    cat("Balanceando la clase ",minoritaria,"\n")
    nclass<-sum(df[,dim(df)[2]]==mayoritaria)
    pclass<-sum(df[,dim(df)[2]]==minoritaria)
    indices.minoritarios<-which(df[,dim(df)[2]]==minoritaria)
    data<-df[indices.minoritarios,-dim(df)[2]]
    distancia<-as.matrix(dist(data, diag=T, upper=T))
    sinteticos<-(mclapply(1:(nclass-pclass),f.sinteticos,data,distancia,minoritaria,nclass,pclass,
                          mc.preschedule = TRUE, mc.set.seed = TRUE,
                          mc.silent = FALSE, mc.cores = getOption("mc.cores", 4L),
                          mc.cleanup = TRUE, mc.allow.recursive = FALSE))
    sinteticos<-data.frame(matrix(unlist(sinteticos), nrow=length(sinteticos), byrow=T))
    aux<-cbind(sinteticos,minoritaria)
    names(aux)<-names(df)
    c.sinteticos<-rbind(c.sinteticos,aux)
    
  }
  ds<-rbind(df,c.sinteticos)
  rownames(ds)<-c(1:dim(ds)[1])
  return(ds)
}
