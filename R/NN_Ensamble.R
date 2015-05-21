NN1<-read.csv("NN/NN_2hidden-0.001prueba1.csv")
NN2<-read.csv("NN/NN_2hidden-0.001prueba2.csv")
NN3<-read.csv("NN/NN_2hidden-0.001prueba3.csv")
NN4<-read.csv("NN/NN_2hidden-0.001prueba4.csv")
NN5<-read.csv("NN/NN_2hidden-0.001prueba5.csv")
NN6<-read.csv("NN/NN_2hidden-0.001prueba6.csv")
NN7<-read.csv("NN/NN_2hidden-0.001prueba7.csv")
NN8<-read.csv("NN/NN_2hidden-0.001prueba8.csv")
NN9<-read.csv("NN/NN_2hidden-0.001prueba9.csv")
NN10<-read.csv("NN/NN_2hidden-0.001prueba10.csv")

NN_ADA1<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba1.csv")
NN_ADA2<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba2.csv")
NN_ADA3<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba3.csv")
NN_ADA4<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba4.csv")
NN_ADA5<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba5.csv")
NN_ADA6<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba6.csv")
NN_ADA7<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba7.csv")
NN_ADA8<-read.csv("NN/Sub_4H120-100-100-150-100-adagrad-0.005-50prueba8.csv")


Ensamble_NN<-(NN1+NN2+NN3+NN4+NN5+NN6+NN7+NN8)/8
Ensamble_NNADA<-(NN_ADA1+NN_ADA2+NN_ADA3+NN_ADA4)/4#+NN_ADA5+NN_ADA6+NN_ADA7+NN_ADA8+NN_ADA9+NN_ADA10)



