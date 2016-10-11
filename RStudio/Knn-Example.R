#Código para classificação e regressão.
source("knn.R")
treinamento<-iris[-150,]
teste<-iris[150,]
teste[,5]<-NA
knn.class.simple(treinamento, teste, k=3)

#[1] virginica virginica virginica
knn.class(treinamento, teste, k=3)
#$class
#[1] virginica
#Levels: setosa versicolor virginica

#$probabilidades

#setosa versicolor  virginica 
#0          0          1 
knn.reg(treinamento[,-5], teste[,-5], k=3)
#[1] 1.933333
