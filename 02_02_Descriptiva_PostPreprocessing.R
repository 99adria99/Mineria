# Cargamos todos los paquetes necesarios
list.of.packages <- c("ggplot2","RColorBrewer","dplyr","tidyr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}

lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

#Cargamos los datos de estudio
load(paste0(pathData,"dataDEFINITIVA.RData"))
#Pequeña introducción a los datos
summary(dd)
str(dd)

#Listado de Variables según su clase
ListaVar <- sapply(dd,class)
ListaVar

# Creamos vectores que contengan los indices del tipo de variable.
VarCat <- which(ListaVar == "factor")
VarNum <- which(ListaVar == "numeric")
VarInt <- which(ListaVar == "integer")

#Resumen
sapply(dd[,VarCat], table)
sapply(dd[,VarNum], summary)

#Análisis Descriptivo

# Variables categóricas (https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot)

names<-names(VarCat)

loopCat <- function(names) {
  n.use<-sym(names)
  nb.cols<-length(levels(dd[[n.use]]))
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  plotbar <- ggplot(dd) + geom_bar(aes(x=!! n.use, fill=!! n.use),show.legend=FALSE) +
    scale_fill_manual(values =mycolors) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(plotbar)
}

loopCat1 <- function(names) {
  #Parametros pre gráfica
  n.use<-sym(names)
  nb.cols<-length(levels(dd[[n.use]]))
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  per <- dd %>% count(!! n.use) %>% mutate(pct = n / sum(n))
  #Gráfica
  pieplot<-ggplot(per, aes(x = "", y = pct, fill = !! n.use )) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(x = 1.6, label = scales::percent(pct, accuracy = .1)), position = position_stack(vjust = .5)) + scale_fill_manual(values =mycolors) + coord_polar(theta="y") + theme_void()
  
  return(pieplot)
}

lapply(names,loopCat)
lapply(names,loopCat1)

#Alternativa Rápida

for (i in VarCat){
  barplot(table(dd[,i], useNA = "ifany"), main = paste("Barplot de", names(dd)[i]), col = "steelblue",las=if(i==2 || i==4){2},cex.names = if(i==2 || i==4){0.5})
  pie(table(dd[,i], useNA = "ifany"),labels = paste0(round(table(dd[,i])/nrow(dd)*100,2) ,"%"), main = paste("Pieplot de", names(dd)[i]), col = colors()[(1:length(table(dd[,i])))*8])
  legend("bottomright",legend = names(table(dd[,i])), fill = colors()[(1:length(table(dd[,i])))*8],cex=if(length(table(dd[,i]))>5){0.8}else{1.2})
}

# Variables Numéricas

# Hacemos histogramas y boxplots para las variables numericas.

# Histogramas
loopNum <- function(names) {
  n.use<-sym(names)
  plothist <- ggplot(dd,aes(x=!! n.use)) + geom_histogram(col='black', fill='steelblue',bins = 10)+theme_classic()
  return(plothist)
}

# Boxplots
loopNum1 <- function(names) {
  n.use<-sym(names)
  plotbox <- ggplot(dd,aes(x=!! n.use)) + geom_boxplot(col='black', fill='steelblue') + theme_classic()
  return(plotbox)
}

names<-names(VarNum)
lapply(names,loopNum)

names<-c(names(VarNum),names(VarInt))
lapply(names,loopNum1)


# Variables Discretas


# Barplots
loopInt <- function(names) {
  n.use<-sym(names)
  per <- dd %>% count(!! n.use) %>% mutate(pct = n / sum(n))
  plotbar <-  ggplot(per, aes(x = !! n.use , y = pct )) +
    geom_bar(stat = "identity", fill="steelblue")+ theme_classic()
  return(plotbar)
}

names<-names(VarInt)
lapply(names,loopInt)


#Alternativa Rápida

for (i in VarNum){
  hist(dd[,i], main = paste("Histograma de", names(dd)[i]), col = "steelblue", xlab = NULL)
  boxplot(dd[,i], main = paste("Boxplot de", names(dd)[i]), col = "steelblue", horizontal = TRUE)
}

# Histograma de pdays de los clientes que han contactado con el banco.(pdays)

contacted <- subset(dd, dd$contacted== "yes")
hist(contacted$pdays, main = ("Histograma de pdays"), col = "steelblue", xlab = NULL)


# Análisis Bivariante


# Re-factorizar la variable "month"
dd$month.new <- factor(x = dd$month,
                       levels = c("jan","feb","mar","apr","may","jun",
                                  "jul","aug","sep","oct","nov","dec"),
                       labels = c("enero","febrero","marzo",
                                  "abril","mayo","junio",
                                  "julio","agosto","septiembre",
                                  "octubre","noviembre","diciembre"))

#Creamos los bucles

#Categóricas
loopbivarCat <- function(names) {
  n.use<-sym(names)
  plotbar<-ggplot(dd, aes(x =!! n.use, fill = y)) + geom_bar(position = "fill") + ggtitle(paste0("Contratación del producto por:",names)) + theme_classic() + xlab(names) + ylab("Densidad") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(plotbar)
}

#Numéricas
loopbivarNum <- function(names) {
  n.use<-sym(names)
  boxplot<-ggplot (dd, aes(x = y, y = !! n.use, fill = y)) + ggtitle(paste0("Contratación del producto por:",names)) +  geom_boxplot() + theme_classic()
  return(boxplot)
}

#Representación de gráficas
names<-names(VarCat)
lapply(names,loopbivarCat)
names<-c(names(VarNum),names(VarInt))
lapply(names,loopbivarNum)

#Clientes que han contactado con el banco (pdays)
ggplot(contacted, aes(x = y, y = pdays, fill = y)) + 
  ggtitle("Contratación del producto por:pdays") +  geom_boxplot() + theme_classic()

#Otros Gráficos

ggplot(dd,aes(x=age,fill = marital)) + geom_bar() +  ggtitle ("Edad vs situación sentimental") + xlab("Edad") + ylab("Estado") + labs(fill="Estado")  + theme_classic()



#A mano

# plot1 <- ggplot(dd, aes(x=month.new, fill = y)) + geom_bar(position = "fill") + ggtitle("Contratación del producto por mes") +
#   theme_classic() + xlab("Mes") + ylab("Densidad") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot2<-ggplot(dd, aes(x=job, fill = y)) + geom_bar(position = "fill") +  ggtitle("Contratación del producto según el tipo de trabajo del cliente") + xlab("Tipos de Trabajo") + ylab("Densidad") + labs(fill= "y") + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot3<-ggplot(dd, aes(x=marital, fill = y)) + geom_bar(position = "fill") +  ggtitle("Contratación del producto según el tipo de trabajo del cliente") + xlab("Tipos de Trabajo") + ylab("Densidad") + labs(fill= "y") + theme_classic()
# 
# plot4<-ggplot(dd, aes(x=poutcome, fill = y)) + geom_bar(position = "fill") +  ggtitle("Contratación del producto según el tipo de trabajo del cliente") + xlab("Tipos de Trabajo") + ylab("Densidad") + labs(fill= "y") + theme_classic()
# 
# plot5<-ggplot(dd, aes(x=contacted, fill = y)) + geom_bar(position = "fill") +  ggtitle("Contratación del producto según el contacto ") + xlab("Tipos de Trabajo") + ylab("Densidad") + labs(fill= "y") + theme_classic()
# 
# plot6<-ggplot (dd, aes(x=y, y=age, fill=y))+   geom_boxplot() + theme_classic()
# 
# plot7<-ggplot (dd, aes(x=y, y=emp.var.rate, fill=y))+   geom_boxplot() + theme_classic()
# 
# plot8<-ggplot(dd,aes(x=age,fill = marital)) + geom_bar() +  ggtitle ("Edad vs situación sentimental") + xlab("Edad") + ylab("Estado") + labs(fill="y")  + theme_classic()