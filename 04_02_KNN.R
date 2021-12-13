# Cargamos distintas librerias que seran necessarias.

library(ISLR)
library(vcd)
library(class)
library(VIM)
library(caret)

# Cargamos la base de datos.
load(paste0(pathData,"dataDEFINITIVA.RData"))
set.seed(007)

test <- sample(1:nrow(dd), size = nrow(dd)/3)
dataTrain <- dd[-test,]
dataTest <- dd[test,]

aux <- dd
aux[test, 18] <- NA

# Aplicamos el knn para diferentes valores de k (1:20)

dftot <- c()

for (i in 1:20){
  set.seed(007)
  cat("#############################################")
  cat("\n")
  cat(paste("Matriz de confusión para la k =",i))
  cat("\n")
  cat("#############################################")
  cat("\n")
  cat("\n")
  result <- kNN(aux, metric = "gower", variable = "y", k = i)
  ti <- table(truth = dd$y[test],pred =result$y[test])
  ti_cm <- confusionMatrix(ti, positive = "yes")
  print(ti_cm)
  Accur <- ti_cm$overall[1]
  dftot <- c(dftot, Accur)
  cat("\n")
}

which.max(dftot)

plot(dftot, main ="Accuracy para k", xlab = "k", ylab = "Accuracy", 
     col = ifelse(dftot == max(dftot), "red", "black"),
     pch = ifelse(dftot == max(dftot), 19, 1))
lines(dftot)

# Observamos la mayor accuracy en k = 12
# Procedemos a hacer la k-fold cv fijando k en 12

# Haremos el k-fold a partir de la función createFolds del paquete caret.
# Y también la función confusionMatrix

fold <- createFolds(dd$y, k=10)
acu_cv <- c()

for (i in 1:10){
  set.seed(007)
  test <- fold[[i]]
  aux <- dd
  aux[test, 18] <- NA
  result <- kNN(aux, metric = "gower", variable = "y", k = 12)
  cat("#############################################")
  cat("\n")
  cat(paste("Matriz de confusión para la iteración",i))
  cat("\n")
  cat("#############################################")
  cat("\n")
  cat("\n")
  ti <- table(truth = dd$y[test],pred =result$y[test])
  ti_cm <- confusionMatrix(ti, positive = "yes")
  print(ti_cm)
  Accur <- ti_cm$overall[1]
  acu_cv <- c(acu_cv, Accur)
  cat("\n")
}

acu_cv
mean(acu_cv)

# Obtenemos una accuracy de 0.7798
