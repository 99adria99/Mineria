# Cargamos la base de datos.
load(paste0(pathData,"dataDEFINITIVA.RData"))

# Utilizamos el metodo k-fold cross validation.

# Haremos el k-fold a partir de la función createFolds del paquete caret.
# Y también la función confusionMatrix

library(caret)
library(naivebayes)

set.seed(007)
fold <- createFolds(dd$y, k=10)
acu_cv <- c()

for (i in 1:10){
  set.seed(007)
  test <- dd[fold[[i]],]
  train <- dd[-fold[[i]],]
  nb <- naive_bayes(y ~ ., data = train)
  pred <- predict(nb, test)
  cat("#############################################")
  cat("\n")
  cat(paste("Matriz de confusión para la iteración",i))
  cat("\n")
  cat("#############################################")
  cat("\n")
  cat("\n")
  ti <- table(pred, test[,18])
  ti_cm <- confusionMatrix(ti, positive = "yes")
  print(ti_cm)
  Accur <- ti_cm$overall[1]
  acu_cv <- c(acu_cv, Accur)
  cat("\n")
  cat("\n")
}


acu_cv
mean(acu_cv)

#Probabilidades para cada variable
tables(nb)

# Obtenemos una accuracy media de 0.7861

# Realizamos ahora otra vez lo mismo pero usando kernel.

set.seed(007)
fold <- createFolds(dd$y, k=10)
acu_cv_k <- c()

for (i in 1:10){
  set.seed(007)
  test <- dd[fold[[i]],]
  train <- dd[-fold[[i]],]
  nb_kernel <- naive_bayes(x = train[,-18], y = train[ ,18], usekernel = TRUE)
  pred <- predict(nb_kernel, test)
  cat("#############################################")
  cat("\n")
  cat(paste("Matriz de confusión para la iteración",i))
  cat("\n")
  cat("#############################################")
  cat("\n")
  cat("\n")
  ti <- table(pred, test[,18])
  ti_cm <- confusionMatrix(ti, positive = "yes")
  print(ti_cm)
  Accur <- ti_cm$overall[1]
  acu_cv_k <- c(acu_cv_k, Accur)
  cat("\n")
  cat("\n")
}

acu_cv_k
mean(acu_cv_k)

#Probabilidades para cada variable
tables(nb_kernel)

# Obtenemos una accuracy media de 0.7710

# Observamos que tenemos una mejor accuracy sin usar el kernel.
