
Umbrales <- function(pred,umbral){
  SalidaProb<-apply(pred,1,function(pred){
    pred=pred[-1]
    orden<-order(pred,decreasing=T)
    if(pred[orden[1]]>=umbral){
      T
    } else{
      F
    }
  })
   SalidaProb
}
UmbralesInf <- function(pred,umbral){
  SalidaProb<-apply(pred,1,function(pred){
    pred=pred[-1]
    orden<-order(pred,decreasing=T)
    if(pred[orden[1]]<=umbral){
      T
    } else{
      F
    }
  })
  SalidaProb
}

Mejores <- function(pred1,pred2){
  id=pred1[,1]
  pred1=pred1[,-1]
  pred2=pred2[,-1]
  SalidaProb<-sapply(1:dim(pred1)[1],function(x){
    Aux_pred1=pred1[x,]
    Aux_pred2=pred2[x,]
    orden1<-order(Aux_pred1,decreasing=T)
    orden2<-order(Aux_pred2,decreasing=T)
    if(Aux_pred1[orden1[1]]>=Aux_pred2[orden2[1]]){
      salida<-as.numeric(Aux_pred1)
    } else{
      salida<-as.numeric(Aux_pred2)
    }
    #print(class(salida))
    (salida)
  })
  SalidaProb<-as.data.frame(t(SalidaProb))
  SalidaProb<-as.data.frame(cbind(id,SalidaProb))
}

##############################################################################

predXG<-read.csv("Best/subXgBoost 0.008 15 1 0.8 0 4 0.9 0.6 3 3200 20150320 .csv")
predPcc<-read.csv("../../..//2-Kaggle//Otto-python//benchmarkPCC.csv")
predNN<-read.csv("../../..//2-Kaggle//Otto-python//NN_2hidden-0.001.csv")

pred<-predNN
hist(pred[,2:5])
hist(pred[,5:9])

umbral=0.999
umbral2=0.85
sum(pred[,2:10]>umbral)
sum(predXG[,2:10]>umbral)

sum((predXG[,2:10]>umbral)&(pred[,2:10]>umbral))
(113116-96345)+(123183-96345)

Todos_0.99<-(predXG[,2:10]>umbral)|(predNN[,2:10]>umbral)
Buenos_NN<- sum((!IgualesXG_NN))


MejoresXG<-Umbrales(pred = predXG,umbral)
MejoresNN<-Umbrales(pred = predNN,umbral)
MejoresPcc<-Umbrales(pred = predPcc,umbral)

IgualesXG_NN<-MejoresNN & MejoresXG
TodosXG_NN<-MejoresNN | MejoresXG
MejoresNNSinXG<-MejoresNN & (!MejoresXG) & TodosXG_NN

predXG[MejoresNNSinXG,]<-predNN[MejoresNNSinXG,]



sum((predXG[,2:10]>umbral)&(pred[,2:10]<umbral2))

lista<-as.data.frame(t(c(sum(pred[,2]>umbral),sum(pred[,3]>umbral), sum(pred[,4]>umbral),sum(pred[,5]>umbral),
    sum(pred[,6]>umbral), sum(pred[,7]>umbral),sum(pred[,8]>umbral), sum(pred[,9]>umbral),
    sum(pred[,10]>umbral))))
names(lista)<-(c(paste("Class_",1:9)))

plot(x=as.factor(names(lista)),y=lista[1,])
plot(lista[1,])


predBestSin6<-read.csv("Best/UnaClasesubXgBoost 0.01 14 1 0.9 0 4 0.9 0.6 3 2200 20150320 .csv")
predBest<-read.csv("Best/subXgBoost 0.008 15 1 0.8 0 4 0.9 0.6 3 3200 20150320 .csv")

pred<-predBestSin6
pred[ValoresTest6,]<-predBest[ValoresTest6,]
pred<-predBest
ValoresTest6<-pred[,7]>0.9

write.csv(ValoresTest6,file="Best/TestClase6.csv",
          quote=FALSE,row.names=FALSE)




pred<-predBest
pred<-apply(pred,1,function(pred){
  pred=pred[-1]
  orden<-order(pred,decreasing=T)
  if(pred[orden[1]]>0.9){
    pred[orden[1]]=0.999
    pred[orden[2]]=0.0002
    pred[orden[3]]=0.0002
    pred[orden[c(-1,-2,-3)]]=0.0001
  }else if(pred[orden[1]]>0.8){
    pred[orden[1]]=0.9
    pred[orden[2]]=0.09
    pred[orden[3]]=0.004
    pred[orden[c(-1,-2,-3)]]=0.001 
  } else if(pred[orden[1]]>0.7){
    pred[orden[1]]=0.75
    pred[orden[2]]=0.12
    pred[orden[3]]=0.024
    pred[orden[c(-1,-2,-3)]]=0.001 
  } else{
    pred
  }
  pred
})
pred=t(pred)

pred<-data.frame(1:nrow(pred),pred)
names(predSalida) = c('id', paste0('Class_',1:9))




predSalida<-Mejores(predXG,predNN)
names(predSalida) = c('id', paste0('Class_',1:9))
write.csv(predSalida,file="SubmisionXGB_NN.csv",
          quote=FALSE,row.names=FALSE)
##############################################################################

load("Best/sumasvotacion.RData")
load("Best/pesos.RData")
load("Best/orden.RData")

predBest1<-read.csv("Best/submissionPromedio.csv")
#predBest2<-read.csv("Best/submissionPromedioMGBalanceoImportance.csv")
predBest3<-(read.csv("Best/submissionBalanceoImportance.csv"))
predBest3[,2:10]<-predBest3[,2:10]/20


predBestXG1<-read.csv("Best/0.43225/subXgBoost 0.01 14 1 0.8 0 3 0.9 0.6 3 2200 20150320 .csv")
predBestXG2<-read.csv("Best/0.43332/subXgBoost 0.012 15 1 0.8 0 3 0.9 0.6 3 2500 20150320 .csv")
predBestXG3<-read.csv("Best/0.44304/subXgBoost 0.17 10 1 0.8 0 3 0.9 0.6 3 220 20150320 .csv")
predBestXG4<-read.csv("Best/0.4455/subXgBoost 10 1 0.8 0 3 0.9 0.6 3 180 20150320 .csv")
predBestXG5<-read.csv("Best/0.4455BalanceoCopyPast/subXgBoost 0.19 10 1 0.8 0 3 0.9 0.6 3 220 20150320 .csv")
predBestXG6<-read.csv("Best/0.4459/subXgBoost 0.2 10 1 0.8 0 3 0.9 0.6 3 165 20150320 .csv")
predBestXG7<-read.csv("Best/subXgBoost 0.008 15 1 0.8 0 4 0.9 0.6 3 3200 20150320 .csv")
predBestXG8<-read.csv("Best/subXgBoost 0.005 14 1 0.9 0 4 0.9 0.6 3 5700 20150320 .csv")


#x<-sqrt(((sumasvotacion[1,])/sum(sumasvotacion[1,],na.rm = T))*predBestXG8[1,-1])
pred<-predBestXG1
pred[,2:10]<-(((predBestXG1[,2:10]+predBestXG2[,2:10]+predBestXG3[,2:10]+predBestXG4[,2:10]+predBestXG5[,2:10]+
         predBestXG6[,2:10]+predBestXG7[,2:10]+predBestXG8[,2:10])/8)+((predBest1[,2:10]+predBest3[,2:10])/2))/2

pred[,2:10]<-(predBest3[,2:10]+predBestXG2[,2:10])/2

 for(i in 1:dim(pred)[1]){
   print(i)
   #print(sum(is.na(sumasvotacion[i,])))
   if(sum(!is.na(sumasvotacion[i,])==9)){
     epsion<-0
   }else{
     epsion<-sum(pred[i,c(F,!is.na(sumasvotacion[i,]))])/sum(9-sum(is.na(sumasvotacion[i,])))
   }
   
   #print(paste("Epsilon: ",epsion))
   pred[i,c(F,!is.na(sumasvotacion[i,]))]<-pred[i,c(F,!is.na(sumasvotacion[i,]))]+epsion
 
   x<-(((sumasvotacion[i,])/sum(sumasvotacion[i,],na.rm = T))+pred[i,-1])/2
   #print(paste("Pesos*Sol: ",x))
   pred[i,2:10]<-x
 }

pred[is.na(pred)] <- 0
nn.pred<-pred
cambia2<-function(i){
  indice<-which.max(nn.pred[i,])
    nn.pred[i,indice]=1
    nn.pred[i,-indice]=0
  return(nn.pred[i,])
}
cambiado2<-sapply(1:nrow(nn.pred),cambia2)

write.csv(pred,file="BalanceoImportanceDeep_predBestXG2.csv",
          quote=FALSE,row.names=FALSE)


a<-apply(pred,1,function(pred){
  i<-pred[1]
  print(i)
  pred=pred[-1]
  epsion<-sum(pred[c(F,is.na(sumasvotacion[i,]))])/ sum(9-sum(is.na(sumasvotacion[i,])))
  pred[c(F,!is.na(sumasvotacion[i,]))]<-pred[c(F,!is.na(sumasvotacion[i,]))]+epsion
  x<-(((sumasvotacion[i,])/sum(sumasvotacion[i,],na.rm = T))+pred)/2
  
  pred<-x
  pred})

a2=t(a)
a2=as.data.frame(cbind(id=as.integer(pred[,1]),a2))
a2[1,]
pred[is.na(pred)] <- 0


epsion<-sum(predBestXG8[1,c(F,is.na(sumasvotacion[1,]))])/sum(9-sum(is.na(sumasvotacion[1,])))
predBestXG8[1,c(F,!is.na(sumasvotacion[1,]))]<-predBestXG8[1,c(F,!is.na(sumasvotacion[1,]))]+epsion

x<-(((sumasvotacion[1,])/sum(sumasvotacion[1,],na.rm = T))+predBestXG8[1,-1])/2



predBest1<-read.csv("Best/submissionPromedio.csv")
predBest2<-read.csv("Best/submissionPromedioBalanceoLog.csv")

predBest3<-read.csv("Best/NN_2hidden-0.001.csv")
#predBest3[,2:10]<-predBest3[,2:10]/20
salidaMA<-predBest3

salida[,2:10] <- (predBest3[,2:10]+predBest2 [,2:10])/2

salidaMG<-predBest3

salida[,2:10] <- sqrt(predBest3[,2:10]*predBest2 [,2:10])





salida[,2:10] <- (15*(predBest1[,2:10]) +(30*predBest2[,2:10]) +predBest3 [,2:10])/65
salida[,2:10] <- (predBest2[,2:10] + ((predBest3 [,2:10])*2)+ predBest4 [,2:10]+ 
  predBest5 [,2:10]+ predBest1 [,2:10])/6
salida[,2:10] <- sqrt(predBest3[,2:10]*predBest2 [,2:10])
salida[,2:10] <- (predBest3[,2:10]+predBest2 [,2:10])/2


write.csv(salida,file="NN_DeepLearning_cambiosMedAtr.csv",
          quote=FALSE,row.names=FALSE)



pred<-predBest1
pred<-predBest2
pred<-predBest3

a<-apply(pred,1,function(pred){
  pred=pred[-1]
  orden<-order(pred,decreasing=T)
    pred[orden[1]]=1
    pred[orden[2]]=2
    pred[orden[3]]=3
    pred[orden[4]]=4
    pred[orden[5]]=5
    pred[orden[6]]=6
    pred[orden[7]]=7
    pred[orden[8]]=8
    pred[orden[9]]=9
  pred})
a=t(a)
a=as.data.frame(cbind(id=as.integer(pred[,1]),a))
a[1,]

predBest1<-a
predBest2<-a
predBest3<-a



a=t(a)
a=as.data.frame(cbind(id=as.integer(pred[,1]),a))

z<-apply(x,2,function(col,df2)
{
  apply(df2,2,function(col2,col1)
  {
    col2+col1
  },col)
},y)

pred1<-predBest2
pred2<-predBest3
m<-apply(pred1,1,function(pred){
  pred1=pred1[-1]
  orden1<-order(pred1,decreasing=F)
  apply(pred2,1,function(pred){
    pred2=pred2[-1]
    orden2<-order(pred2,decreasing=F)
    if(all(orden1[1:3] %in% orden2[1:3])){
      1
    }else{
      0
    }}
  )
})




pred1<-randomForest
pred2<-predBest2
Orden1<-apply(pred1,1,function(pred1){
  pred1=pred1[-1]
  pred1<-order(pred1,decreasing=F)
  pred1
})
Orden1=t(Orden1)
Orden1=as.data.frame(cbind(Orden1))
Orden1[1,]

Orden2<-apply(pred2,1,function(pred2){
  pred2=pred2[-1]
  pred2<-order(pred2,decreasing=F)
  pred2
})
Orden2=t(Orden2)
Orden2=as.data.frame(cbind(Orden2))
Orden2[1,]



P  <-sapply(1:dim(Orden2)[1],function(x){
    if(all(Orden1[x,1:4] %in% Orden2[x,1:4])&clase3[x]){
      1
    }else{
      0
    }
})
mean(P)
sum(P)/sum(clase3)

if(all(Orden1[1:3] %in% Orden2[1:3])){
  1
}else{
  0
}







loglike.logit = function(data, par){
  logistic = 1/(1+exp(-par[1]-par[2]*data$x))
  with(data,-log(prod(logistic^y*(1-logistic)^(1-y))))
}
resultado = optim(par=c(0,0), fn=, data=predBest2)




















