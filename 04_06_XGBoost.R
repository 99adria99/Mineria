# Cargamos todos los paquetes necesarios
list.of.packages <-c("ggplot2","caret","recipes","xgboost","dplyr","Matrix","plyr","knitr","tidyr","reshape2","MLmetrics") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# Cargamos la base de datos.
load(paste0(pathData,"dataDEFINITIVA.RData"))

# Partici�n de los datos
# Xgboost requiere matrices num�ricas, para funcionar correctamente debemos convertir nuestra columnas 
# de datos de tipo car�cter a tipo num�rico. Le restamos una unidad para obtener valores 0 y 1 en las variables 
# binarias, especialmente en la variable respuesta ya que el algoritmo espera esos valores.

#Convertimos las variables categ�ricas(que estan definidas como factores) a numericas
VarCat<-colnames(Filter(is.factor,dd))
dd[,VarCat]<-lapply(dd[,VarCat],function(x) as.numeric(x)-1)

#Ejemplo Variable Education
library(kableExtra)
gg<-dd$education
kable(data.frame(Education_Categorica=levels(gg),Education_Numerica=as.numeric(levels(as.factor(dd$education))))) %>% kable_styling()

# Como es el caso para todos los algoritmos de predicci�n supervisados, necesitamos dividir nuestros 
# datos en un conjunto de entrenamiento, que aprender� las caracter�sticas de los datos y generar� un modelo 
# de predicci�n; y un conjunto de prueba, con el que validamos el modelo generado.

# Se crean los �ndices de las observaciones de entrenamiento
set.seed(1234)
train <- createDataPartition(y = dd$y, p = 0.8, list = FALSE, times = 1)
datos_train <- dd[train, ]
datos_test  <- dd[-train, ]

# Es importante verificar que la distribuci�n de la variable respuesta es similar en el conjunto de 
# entrenamiento y en el de test. Por defecto, la funci�n utilizada garantiza una distribuci�n aproximada.

prop.table(table(datos_train$y))
prop.table(table(datos_test$y))

# XGBoost

# Como ya lo mencionamos, la implementaci�n XGBoost de R requiere que los datos que usemos sean matrices, 
# espec�ficamente de tipo DMatrix, as� que necesitamos convertir nuestros sets de entrenamiento y prueba a 
# este tipo de estructura.

# Usaremos la funci�n xgb.DMatrix() de xgboost para la conversi�n.

# Esta funci�n espera una matriz num�rica como primer argumento y tambi�n se pueden especificar algunos 
# atributos adicionales al objeto que devolver�. Nosotros definiremos el atributo label para identificar la 
# variable objetivo en nuestros datos.

# Al usar esta funci�n es muy importante que tu data no incluya la columna con la variable objetivo, de lo 
# contrario, obtendr�s una precisi�n perfecta en tus predicciones, que ser� in�til con datos nuevos. 
# Para ello implementamos la funci�n sparse.model.matrix del paquete "", creamos una matriz en la que 
# separamos la variable respuesta y las dem�s.

#Construcci�n Matriz Training

##Construcci�n de la matriz de datos: Sparse (*), a ser procesada por el algorimo,(No incluye la variable respuesta "y")
trainm <- sparse.model.matrix(y ~.-1, data = datos_train)
colnames(trainm)

#Variable objetivo o respuesta
train<-datos_train %>% select(-y)
train_label <- datos_train[,"y"]

#Se crea la matriz de la data de entrenamiento con la libreria: xgboost, donde se introduce la matrix sparse anterior (*).
train_matrix <- xgb.DMatrix(data =as.matrix(trainm), label= train_label)
train_matrix

######################################################
# Entrenamiento Modelo    ############################
######################################################

# Aplicamos el algoritmo directamente con nuestro datos de entrenamiento. Hacemos 300 iteraciones para 
# obtener el mayor accuracy.

#Algoritmo
xgb <- xgb.train(data = train_matrix, objective = "binary:logistic",nrounds = 300,eta =  0.3, max_depth = 6, gamma = 0, subsample = 0.5,
                 min_child_weight =1, colsample_bytree=1, watchlist= list(train=train_matrix))

head(xgb$evaluation_log)

data.frame <- xgb$evaluation_log
ggplot(data.frame,aes(x=iter, y=train_logloss)) + geom_line(size=1) + theme_classic() + labs(x="Iteraciones", y = "Error") + theme(legend.title=element_blank())

# Generaci�n de Predicciones
pred_train <- predict(xgb, newdata = train_matrix)
prediction_train<-as.numeric(pred_train > 0.5)

#Evaluaci�n del modelo

# Matriz de Confusi�n
Matriz_Confusion_Train<-ConfusionMatrix(prediction_train,train_label)
Matriz_Confusion_Train
round(Matriz_Confusion_Train*100/nrow(datos_train),2)

# Accuracy
Accuracy_Train <- Accuracy(prediction_train,train_label)
paste0(Accuracy_Train*100,"%")
#paste0(round((sum(diag(Matriz_Confusion_Train))/length(pred_train))*100,2),"%")


######################################################
# Crossvalidation      ###############################
######################################################


#xgb_params <- list(objective = "binary:logistic", eval_metric = "auc",eta =  0.3, max_depth = 6, gamma = 0, subsample = 0.5,
#                       min_child_weight =1, colsample_bytree=1)

#xgb_cv <- xgb.cv(params = xgb_params, data = train_matrix, nrounds = 300, nfold = 10,
#                  prediction = TRUE, showsd = TRUE, stratified = TRUE, verbose = TRUE,
#                  print.every.n = 1,early_stopping_rounds = 10)

#Eval<- data.frame(paste0(round(mean(xgb_cv$evaluation_log$train_auc_mean)*100,2)," %"), paste0(round(mean(xgb_cv$evaluation_log$test_auc_mean)*100,2)," %"))
#colnames(Eval) <- c("train_auc_mean","test_auc_mean")
#Eval    #kable(Eval) %>% kableExtra::kable_styling()

# Representaci�n Cv
#data.frame <- xgb_cv$evaluation_log %>% select(contains("mean"))
#data.frame <- melt(data.frame)
#data.frame$it <- rep(1:nrow(xgb_cv$evaluation_log),2)
#ggplot(data.frame,aes(x=it, y=value , by = variable, col=variable)) + geom_line() + ylim(0.85,1)

set.seed(1234)

xgb_grid <- expand.grid(nrounds = 300, eta =  0.3, max_depth = 6, gamma = 0, subsample = 0.5, min_child_weight =1, colsample_bytree=1)


xgb_trcontrol <- trainControl(method = "cv", number = 10, verboseIter = TRUE, returnData = FALSE, returnResamp = "all", 
                              classProbs =  FALSE, allowParallel = TRUE)

xgb_train <- train(x = as.matrix(train), y = as.factor(train_label), trControl = xgb_trcontrol, tuneGrid = xgb_grid, method = "xgbTree")

paste0(round(xgb_train$results$Accuracy*100,2)," %")

#kable(xgb_train$results) %>% kable_styling()   library(kableExtra)

# El accuracy promedio estimado mediante validaci�n cruzada repetida es de 0.854, el modelo predice 
# correctamente la supervivencia de los pasajeros un 85% de las veces. Este valor ser� contrastado m�s adelante 
# cuando se calcule el accuracy del modelo con par�metros optimos aplicado al conjunto de test.


######################################################
# Selecci�n de Hiperpar�metros       #################
######################################################

set.seed(1234)
# Seleccionamos los hiperpar�metros a optimizar
xgb_grid <- expand.grid(
  nrounds = 50,
  eta = c(0.01, 0.05, 0.1, 0.3),
  max_depth = c(3, 6),
  gamma = c(0,1), 
  subsample = c(0.5, 0.75, 1),
  min_child_weight = c(1,3),
  colsample_bytree=1
)


# Definimos tambi�n los par�metros de control
xgb_trcontrol <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all", 
  classProbs =  FALSE,
  allowParallel = TRUE
)

#Entrenamos el modelo para cada combinaci�n de hiperpar�metros definidos
xgb_train <- train(
  x = as.matrix(train),
  y = as.factor(train_label),
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree"
)

#Comprobamos Hiperpar�metros
Params<-xgb_train$bestTune
Params

xgb_train$metric

ggplot(xgb_train$results,aes(x=factor(eta),y=Accuracy,fill=factor(eta))) + theme_bw() + geom_boxplot()
ggplot(xgb_train$results,aes(x=factor(subsample),y=Accuracy,fill=factor(subsample))) + theme_bw() + geom_boxplot()
ggplot(xgb_train$results,aes(x=factor(gamma),y=Accuracy,fill=factor(gamma))) + theme_bw() + geom_boxplot()
ggplot(xgb_train$results,aes(x=factor(max_depth),y=Accuracy,fill=factor(max_depth))) + theme_bw() + geom_boxplot()
ggplot(xgb_train$results,aes(x=factor(min_child_weight),y=Accuracy,fill=factor(min_child_weight))) + theme_bw() + geom_boxplot()

#max_depth = 6, eta = 0.05, gamma = 1, colsample_bytree = 1, min_child_weight = 1, subsample = 0.75
xgb_train$results[xgb_train$results$Accuracy==max(xgb_train$results$Accuracy),]

######################################################
# MODELO FINAL / Aplicamos Datos test   ##############
######################################################

# Comprobamos el modelo para los datos de test
  
#Construcci�n Matriz Test

#Se repite el proceso anterior, pero esta vez con el set de pruebas.
testm <- sparse.model.matrix(y ~.-1, data = datos_test)
test_label <- datos_test[,"y"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
```

#APLICAMOS XGBOOST
set.seed(1234)
xgb <- xgb.train(data = train_matrix, objective = "binary:logistic",
                 nrounds = 50, max.depth = 6, eta = 0.05, gamma=1, 
                 min_child_weight = 1, subsample = 0.75, colsample_bytree = 1,
                 watchlist= list(train=train_matrix,test=test_matrix),prediction=T)

head(xgb$evaluation_log)

# Curva de logloss 
data.frame <- xgb$evaluation_log[,c(2,3)]
data.frame <- melt(data.frame)
data.frame$it <- rep(1:nrow(xgb$evaluation_log),2)
ggplot(data.frame,aes(x=it, y=value , by = variable, col=variable)) + geom_line(size=1) + theme_classic() + labs(x="Iteraciones", y = "Error") + theme(legend.title=element_blank())


# Es fundamental hacer notar como desciende el error: mlogloss, durante la fase de entrenamiento: curva roja, 
# a medida que se desarrollan las 300 iteraciones, as� como, tambi�n sucede los mismo, en la fase de prueba: curva azul.

#Generaci�n de Predicci�n
pred_test <- predict(xgb, newdata = test_matrix)
prediction_test<-as.numeric(pred_test > 0.5)
pred_train <- predict(xgb, newdata = train_matrix)
prediction_train<-as.numeric(pred_train > 0.5)

# Evaluaci�n de Modelo

#Matrix de Confusi�n
Matriz_Confusion_Test<-ConfusionMatrix(prediction_test,test_label)
Matriz_Confusion_Test
Matriz_Confusion_Test*100/nrow(datos_test)

confusion_matrix <- as.data.frame(table(prediction_test,test_label))
ggplot(data = confusion_matrix,
       mapping = aes(x = prediction_test,
                     y = test_label)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightgray" ,
                      high = "darkgray",
                      breaks=c(55,600),
                      labels=c("Low","High")) + 
  theme_classic(base_size = 14) + labs(x="Predicci�n",y="Valores observados") + ggtitle("Datos de Test")

#Otras medidas
pred_test <- predict(xgb, newdata = test_matrix, type="raw")
prediction_test<-as.numeric(pred_test > 0.5)
prediction_test_pr<-factor(prediction_test,labels = c("no","yes"))
test_label_pr<-factor(test_label,labels = c("no","yes"))

metrics<-confusionMatrix(data = prediction_test_pr, reference = test_label_pr,positive = "yes")
metrics<-metrics$byClass
metrics <- data.frame(as.list(metrics)) %>% mutate_all(round, 4) %>% select( Specificity, Recall, Sensitivity, F1)
metrics
#kable(metrics) %>% kable_styling()

# low = "#132B43",
# high = "#56B1F7",
# 
# low = "#56B1F7",
# high = "#132B43",
# trans = "log",
# 
# low = "#D95F02" ,
# high = "#7570B3"


#Accuracy
Accuracy_Test <- Accuracy(prediction_test,test_label)
Accuracy_Train <- Accuracy(prediction_train,train_label)

data.frame(Accuracy_Train,Accuracy_Test)
#kable(data.frame(Accuracy_Train=paste0(round(Accuracy_Train*100,2)," %"), Accuracy_Test=paste0(round(Accuracy_Test*100,2)," %"))) %>% kable_styling()


#Error

Error_Train<-1-Accuracy_Train
Error_Test<-1-Accuracy_Test
data.frame(Error_Train,Error_Test)
#kable(data.frame(Error_Train=paste0(round(Error_Train,3)*100," %"), Error_Test=paste0(round(Error_Test,3)*100," %"))) %>% kable_styling()


######################################################
# Importancia de las variables en el modelo   ########
######################################################

#Importancia
xgb.imp<-xgb.importance(colnames(train_matrix), model = xgb)
head(xgb.imp)

xgb.plot.importance(xgb.imp)
xgb.ggplot.importance(xgb.imp ,show.legend=F) + scale_fill_discrete(name = "Importancia") + ggtitle("Importancia de las Variables") + theme_classic()

# Se puede observar que las variables que m�s aportan informaci�n en la clasificaci�n son las siguientes 
# por orden de importancia: 1. duration, 2. euribor3m, por lo tanto, podemos observar las variables en las que 
# tenemos que enfocarnos para hallar nuestras soluciones.
