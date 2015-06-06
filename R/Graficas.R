##############################
#Pima
##############################
Test <- c(0.52,0.44,0.45,0.48,0.60)

Train <- c(0.47,0.42,0.44,0.46,0.56)


# graficamos
coor=c("Scale","Log(scale(x+1))","Anscombe","Logit","TF-IDF")#as.integer(c(1:4))
y1=Train
y2=Test
scal=c(1,2)
datos<-data.frame(coor,y1,y2)



ggplot(datos, aes(x=coor))   
#geom_line(aes(y=y1, colour = "Cv test"), size = 0.85) +
  geom_point(aes(y=y1, colour = "Cv test"), size=3.3)+
  #geom_line(aes(y=y2, colour = "Kaggle test"), size = 0.85) +
  geom_point(aes(y=y2, colour = "Kaggle test"),size=3.3)+
  labs(x = "Trasformacion", y = "Error",title="Trasformaciones probadas NN", colour = "") +
  scale_colour_manual(values = c("blue","green"))+
  scale_shape_manual(values = c(1,2))





##############################
Test <- c(0.50,0.70,0.69,0.50,0.70,0.69)

Train <- c(0.70,0.85,1.2,0.97,0.88,1.93)


# graficamos
coor=c("RectifierWithDropout", "Tahn", "Tahn with dropout", 
       "Rectifier", "Rectifier with dropout", "Maxout")
#as.integer(c(1:4))
y1=Train
y2=Test
scal=c(1,2)
datos<-data.frame(coor,y1,y2)

Test <- c(1,1,1)
Train <- c(0.461,0.445,0.433)
coor=c("NN version 1", "NN version 2", "Ensamble")
#as.integer(c(1:4))
y1=Train
y2=Test
scal=c(1,2)
datos<-data.frame(coor,y1,y2)


p <- ggplot(datos,aes(x =coor , y=y1,fill=y1))+geom_bar(colour = "black",stat="identity")
p +coord_cartesian(ylim = c(0.42, 0.48)) +
  labs(x = "Metodo", y = "Error",title="NN python vs Ensamble")+
  scale_fill_gradient2(limits=c(0.42, 0.48),midpoint =0.45 ,
                       low="green",mid="yellow" , high="red")


##############################
Train <- c(0.434,0.4336,0.433,0.4323)
Test <- c(0.4337,0.432,0.4302,0.4293)

# graficamos
coor=c(10,20,30,40)
y1=Train
y2=Test
scal=c(1,2)
datos<-data.frame(coor,y1,y2)


ggplot(datos, aes(x=coor))   +
geom_line(aes(y=y1, colour = "balanceo+Importance"), size = 0.85) +
geom_point(aes(y=y1, colour = "balanceo+Importance"), size=3.3)+
  geom_line(aes(y=y2, colour = "Balanceo+Importance/2"), size = 0.85) +
  geom_point(aes(y=y2, colour = "Balanceo+Importance/2"),size=3.3)+
  labs(x = "Numero de ejeciciones", y = "Error",title="Ensamble H2O", colour = "") +
  scale_colour_manual(values = c("blue","green"))
  






p <- ggplot(datos,aes(x =coor , y=y1,fill=y1))+geom_bar(colour = "black",stat="identity")
p +coord_cartesian(ylim = c(0.6, 2)) +
  labs(x = "Funcion", y = "Error",title="H2O funciones")+
  scale_fill_gradient2(limits=c(0.6, 2),midpoint =0.8 ,
                       low="green" ,mid="yellow", high="red")

















ggplot(datos, aes(coor, y1, fill = Lugar)) + geom_bar(position = "dodge")+
   coord_flip() + scale_y_continuous(formatter = "percent") +
   scale_fill_manual(values = c("cornflowerblue", "darkslateblue")) +
   labs(x = "Participaci´on sector cultural en el PIB (2007)",y = "")