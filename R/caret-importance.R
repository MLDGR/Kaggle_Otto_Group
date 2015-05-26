
library(mlbench)
library(caret)

set.seed(7)

# carga el conjunto de datos
source("Load-Transform-Data.R")
attach(train)

# prepara el esquema de entrenamiento
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# aprende el modelo
model <- train(target~., data=train, method="knn", 
               preProcess="scale", trControl=control)

# estima la importancia de las variables
importance <- varImp(model, scale=FALSE)

# muestra los datos del analisis
print(importance)

# representa graficamente los resultados
plot(importance,lw=3)