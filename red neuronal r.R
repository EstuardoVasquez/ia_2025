# se requieren algunos paquetes
#Cargar paquetes
library(tidyverse)
library(caret)
library(neuralnet)
library(dplyr)
# CARGA DE DATOS
datos = iris
#Separacion en grupo de entrenamiento y pruebas
muestra = caret::createDataPartition(datos$Species, p=0.8, list = F)
train = datos[muestra,]
test = datos[-muestra,] # todos los que no sean de muestra
# Analisis exploratorio
head(train, 5)
tail(train, 4)
train[6: 10,]
sepal_length = train$Sepal.Length
hist(sepal_length)
#Entrenamiento de la red neuronal
red.neuronal = neuralnet::neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                                    data = train, 
                                    hidden = c(2,3))
red.neuronal$act.fct()
#Visualizacion de la red neuronal
plot(red.neuronal)
#Aplicar la red al conjunto de pruebas para predecir la especie 
prediccion  = predict(red.neuronal, test, type='class')
#Decodificar la columna que contiene el maximo y por ende la especie de la que se trata
decodificarCol = apply(prediccion,1, which.max)
#Crear nueva columna con los valores decodificados
codificado = data.frame(decodificarCol)
codificado = codificado %>%
  mutate(especie = recode(decodificarCol, "1" = "Setosa", "2" = "Versicolor", "3" = "Virginica"))
test$Especie.Pred = codificado$especie
