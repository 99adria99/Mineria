# Cargamos todos los paquetes necessarios.

list.of.packages = c("rpart", "randomForest", "rpart.plot") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# Cargamos la base de datos.
load(paste0(pathData,"dataDEFINITIVA.RData"))

set.seed(007)
training <- sample(1:nrow(dd), round(2*nrow(dd)/3))
p1 <- rpart(y ~ ., data = dd[training,], method = "class")
attributes(p1)

summary(p1)

plot(p1)
text(p1, use.n = T, cex = 0.7)

p1l <- predict(p1, data = dd[training, ])

p1lp <- p1l[, 2]
p1lp[p1l[, 2] < 0.5] <- 0
p1lp[p1l[, 2] >= 0.5] <- 1

indicadores <- as.numeric(names(p1lp))
t1 <- table(dd$y[indicadores], p1lp)
t1


# Utilizamos el metodo k-fold cross validation.

# Haremos el k-fold a partir de la función createFolds del paquete caret.
# Y también la función confusionMatrix

library(caret)

set.seed(007)
fold <- createFolds(dd$y, k=10)
dftot <- c()

for (i in 1:10){
  test <- dd[fold[[i]],]
  train <- dd[-fold[[i]],]
  set.seed(007)
  p1 <- rpart(y ~ ., data = train, method = "class")
  p1l <- predict(p1, data = test)
  pred <- p1l[, 2]
  pred[p1l[, 2] < 0.5] <- "no"
  pred[p1l[, 2] >= 0.5] <- "yes"
  indicadores <- as.numeric(names(pred))
  cat("#############################################")
  cat("\n")
  cat(paste("Matriz de confusión para la iteración",i))
  cat("\n")
  cat("#############################################")
  cat("\n")
  cat("\n")
  ti <- table(truth = dd$y[indicadores], pred)
  ti_cm <- confusionMatrix(ti, positive = "yes")
  print(ti_cm)
  Accur <- ti_cm$overall[1]
  dftot <- c(dftot, Accur)
  cat("\n")
  cat("\n")
}

dftot
mean(dftot)

# Obtenemos una accuracy de 0.7107979

# Realizamos un arbol con todas las observaciones

set.seed(007)
p1 <- rpart(y ~ ., data = dd, method = "class")

plot(p1)
text(p1, use.n = T, cex = 0.7)

# Para la variable month: bdegh = aug, jul, jun, may, nov.
# Para la variable job: beh = blue-collar, management, services.
