# EL preprocessing es la parte mas importante y en nuestro trabajo lo haremos siguendo los siguientes puntos:
# -Tratamiento de los datos no balanceados y posterior balanceo
# -Importamos como NA's los "Unknown"
# -Transformación de p-days
# -Eliminación de variables (defoult y n_employees)
# -Tranformación de algunas variables mal etiquetadas
# -Tratamiento de NA's a traves de MICE
# -Estadarización de las variables numéricas
# -Tratamiento de outliers
# -Tratamiento de possible correlación (posterior eliminación de emp.var.rate)
# -Guardamos la base de datos en un r.data que se llama "dataDEF.RData"


# IMPORTACIÓN DE LOS DATOS
# Importamos de nuevo las bases de datos, esta vez importando también la full.
dd1 <- read.csv2(paste0(pathData,"bank-additional.csv"))
(a<-table(dd1$y))
c(a[1]*100/(a[1]+a[2]),a[2]*100/(a[1]+a[2]))
#Vemos que la variable y esta muy desbalanceada porque contiene muchos mas "no" que "si"
#Si no solucionamos este problema la realización de los diferentes modelos seria erronea.


# BALANCEAMINETO DE LOS DATOS
# A través de añadir más observaciones con variable respuesta positiva podremos balancear los datos y por ende realizar mejores estimaciones en los métodos
dd2 <- read.csv2(paste0(pathData,"bank-additional-full.csv")) #es la base original de internet
# Cojemos solo las observaciones necessarias y donde y = "yes"
dd2_pos <- dd2[which(dd2$y=="yes"),]

#La idea es añadir las obs suficientes para que la  variable "y" 
#esté representada por 40% "si" y "60" no
set.seed(007)
sel <- sample(1:nrow(dd2_pos), 2200)#2200 obs seran necesarias para cumplir el objetivo
dd_additional <- dd2_pos[sel,]
#juntamos la base llena de "si" con nuestra base de datos original
dd <- rbind(dd1,dd_additional)
#Eliminamos las duplicaciones que se hayan podido crear
dd <- dd[-which(duplicated(dd)),]
#Comprobamos que efectivamente hemos balanceado la base
(a<-table(dd$y))
c(a[1]*100/(a[1]+a[2]),a[2]*100/(a[1]+a[2]))

# IMPORTACIÓN COMO NA'S LOS "UNKNOWN"
sum(dd$pdays==999)
dd[dd=="unknown"] <- NA
dd$pdays[dd$pdays==999] <- NA

# TRATAMIENTO DE LA VARIABLE P.DAYS
#En las variables categóricas no hemos realizado ningun cambio relevante porque no hay ninguna que tenga una cantidad de niveles importante (en cuyo caso tendriamos que juntarlos para una mejor interpretación)
#Para tractar la variable "pdays" (numero de dias que han pasado desde que se contactó con el cliente)
#crearemos una nueva variable que contenga dos niveles: Sí (en el caso que se haya contactado) y No (en caso contrario)

dd$contacted<- "no" #creamos toda la variable con caracter "no"
dd$contacted[dd$pdays>=0]<- "yes" #cambiamos aquellos nos por yes si la observacion pdays no es un  NA

#por último cambiamos los NA de pdays por 0.
dd$pdays[is.na(dd$pdays)]<- 0



# ELIMINACIÓN DE LAS VARIABLES "DEFAULT" Y "NR.EMPLOYEES"
# Quitamos la variable default ya que praticamente solo contiene "no".
# Quitamos también la variable nr.employees ya que no comprendemos su contenido.
dd$default <- NULL
dd$nr.employed <- NULL


#TRANFORMACIÓN DE ALGUNAS VARIABLES MAL ETIQUETADAS
#A veces hay errores de etiquetación y lo tenemos que tener presente
dd[,15:18] <- lapply(dd[,15:18], as.character)
dd[,15:18] <- lapply(dd[,15:18], as.numeric)

#TRATAMIENTO DE NA'S A TRAVÉS DEL MÉTODO MICE
ListaVar <- sapply(dd,class)
ContieneNA <- colSums(is.na(dd))>0

VarInt <- which(ListaVar == "integer")
VarCat <- which(ListaVar == "character")
VarNum <- which(ListaVar == "numeric")

# Transformamos las variables "character" a "factor"
dd[,VarCat] <- lapply(dd[,VarCat], as.factor)
View(data.frame(ListaVar,ContieneNA))
# Observamos que solo tenemos missing en variables categoricas.

# Aplicamos el MICE para la imputación de missings.
library(Rcpp)
library(mice)

tempData <- mice(dd, m=1, maxit = 50, meth = "polyreg", seed = 007)
dd <- complete(tempData)
colSums(is.na(dd[,VarNum])) #vemos que ya no hay NA's en las numéricas


# ESTANDARIZACIÓN DE LAS VARIABLES NUMÉRICAS CONTINUAS
#La estandarización es necesaria para tener buenas propiedades cuando apliquemos los diferentes métodos de classificación vistos en clase
dd[,15:18] <- lapply(dd[,15:18], scale)
dd[,15:18] <- lapply(dd[,15:18], as.numeric)


# DETECCIÓN DE OUTLIERS
# Existen valores "extraños" que se deben eliminar porque suponen un problema al realizar cálculos de cualquier tipo
# Hacemos un data frame que contenga las variables numericas.
ddnum <- dd[,c(VarInt,VarNum)]

# Creamos una nueva variable en el df, que contenga la distancia de mahalanobis.
ddnum$mahal <- mahalanobis(ddnum, colMeans(ddnum), cov(ddnum))

# Calculamos los p-value de mahalanobis
ddnum$p <- pchisq(ddnum$mahal, df=8, lower.tail=FALSE)

# Consideramos como outlier los p-values menores a 0.001
outlier <- which(ddnum$p < 0.001)

dd <- dd[-outlier,]
summary(dd)
# Observamos cambios despues de quitar 245 observaciones.


# ESTUDIO DE LA CORRELACIÓN
#el último paso del preprocesamiento es estudiar la correlación entre variables explicativas
#si encontramos que dos o más variables estan correlacionadas entre si una debe ser eliminada para solucionar errores de confusión 
# y mejorar asi propiedades de los modelos y métodos que realizaremos
cor(dd[,c(VarInt, VarNum)])
#La variable emp.var.rate esta muy correlacionada con euribor3m y bastante con cons.price.idx
#observamos las correlaciones otra vez si eliminamos dicha variable
dd$emp.var.rate<-NULL
ListaVar <- sapply(dd,class)
VarNum <- which(ListaVar == "numeric")
VarInt <- which(ListaVar == "integer")
cor(dd[,c(VarInt, VarNum)])

# Guardamos la base de datos como dataDEFINITIVA.
save(dd, file = paste0(pathData,"dataDEFINITIVA.RData"))

