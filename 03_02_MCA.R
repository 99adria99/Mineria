# Cargamos todos los paquetes necesarios
list.of.packages <- c("FactoMineR","Matrix","factoextra","ggplot2","flexdashboard","ggthemes","DT","corrplot") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}

lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# Cargamos Datos y seleccionamos las variables categóricas
load(paste0(pathData,"dataDEFINITIVA.RData"))

colnames(dd)[which(colnames(dd)=="y")]<-"y.respuesta"

index_suplementaries_categ<-c(4, 9) 
index_suplementaries_numer<-c(1, 10, 11, 12,13 ,15 ,16, 17)

# Usamos todas las variables númericas como suplementarias y las variables "education", "contact", 
# "day_of_week" y "contacted" como suplementarias categóricas.


# Ejecutamos el Análisis de Correspondencia Múltiple mediante dos métodos. 
# El primero de ellos es que calcula por defecto llamado "Indicator" y el segundo es el método de Burt. 
# Antes de proceder al análisis de los resultados, comprobamos cuál de los dos métodos es más optimo en 
# nuestro problema. Si observamos el porcentaje de variabilidad explicada, podemos ver como el método de Burt 
# tiene valores más altos.

# Realizamos el ACM y comprobamos la variabilidad explicada de cada método.

# Método por defecto (Indicator)
ACM.Ind <- MCA(dd,quanti.sup =index_suplementaries_numer, quali.sup=index_suplementaries_categ )

fviz_screeplot(ACM.Ind, addlabels = TRUE , method="Indicator", ncp = 20, main="Variabilidad explicada por cada dimensión", xlab="Dimensiones", ylab="Porcetaje de Variabilidad explicada")

fviz_eig(ACM.Ind, addlabels = TRUE, ylim = c(0, 10),barcolor="steelblue", barfill="steelblue", linecolor="black")+  theme_fivethirtyeight(base_size =14, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Varianza Explicada")+
  ylab('% Variación Explicada') +
  xlab("Dimensiones")+
  geom_hline(yintercept = (100/nrow(ACM.Ind$eig)), size=1)

# Método Burt (Método Alternativo)

ACM.Burt <- MCA(dd  ,quanti.sup =index_suplementaries_numer,  quali.sup=index_suplementaries_categ, method="Burt",graph = FALSE)

fviz_screeplot(ACM.Burt, addlabels = TRUE, ncp = 17, title="Variabilidad Explicada Método de Burt",xlab="Dimensiones",ylab="Porcentage de variabilidad explicada")

fviz_eig(ACM.Burt, addlabels = TRUE, ylim = c(0, 15),barcolor="steelblue", barfill="steelblue", linecolor="black")+  theme_fivethirtyeight(base_size =14, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Varianza Explicada")+
  ylab('% Variación Explicada') +
  xlab("Dimensiones")+
  geom_hline(yintercept = (100/nrow(ACM.Burt$eig)), size=1)

# Escogemos el método de Burt, ya que tiene mayor variabilidad explicada en las primeras dimensiones. 
# Si nos fijamos en las 3 primeras, que son las que se pueden analizar gráficamente, con el método de Burt 
# podemos explicar hasta un 23.3% de la variabilidad, mientras que solo un 16.6% con el Indicator. 
# Aún teniendo un valor mayor en el método de Burt, los resultados que obtengamos no van a ser muy precisos 
# ya que la variabilidad explicada es baja, lo cuál es normal teniendo en cuenta el número de observaciones que 
# tenemos y la suma total de niveles de cada variable.

# Procedemos con el algoritmo a raiz de la matriz de Burt
########################################################################

### EigenValores y Varianza

ACM<-ACM.Burt
eigenValores <- get_eigenvalue(ACM)

datatable(data =round(eigenValores[,1:2],4), extensions = 'Scroller',
          options = list(dom='tp',  class = 'stripe compact hover cell-border'))

### Interpretación
# - En la grafica podemos ver la varianza explicada de cada una de las dimensiones que más aportan.
# - En la linea horizontal de color negro se muestra el valor de la varianza explicada que tomaria cada dimension si las 25 aportaran lo mismo.
# - Esto criterios nos dice que con 5 u 6 dimensiones es suficiente para el analisis de los datos.

### Varianza  Acumulada Explicada
########################################################################

#Calculamos Varianza acumulada
Acumulado <- ACM$eig[,3][1:25]
Dimension <- c(1:25)
datos <- data.frame(cbind(Dimension, Acumulado))
datos


ggplot(data = datos, aes(x=Dimension, y=Acumulado))+
  geom_bar(stat = "identity", fill="steelblue", color="steelblue") +
  geom_line(color="black")+
  geom_point(color="black") +
  geom_hline(yintercept = 80, size=1) +
  geom_text(label= paste(round(Acumulado,1),"%"), color="black", size=2, vjust = 1.4) +
  theme_fivethirtyeight(base_size =14, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Varianza Acumulada Explicada")+
  ylab('% Variación Explicada') +
  xlab("Dimensiones") +
  scale_x_continuous(breaks = c(1:25)) 


### Variables
########################################################################

#Contribuciones a cada eje y test-values significativos
dim<-dimdesc(ACM)

dim[[1]]$quali
dim[[2]]$quali

fviz_mca_var(ACM, choice = "var", repel = TRUE, col.var = "red4")+
  theme_fivethirtyeight(base_size =14, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Variables ACM")+
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

### Aportación de variables

ACM$var$eta2
corrplot(ACM$var$eta2, is.corr=FALSE,  method=c("color"),addCoef.col = "black")

### Interpretación

# - En la grafica de arriba podemos observar que tanto pesa cada una de las 7 variables en las primeras 5 dimensiones.
# - La variable  month es el que mas pesa en la dimensiones 3, 4 y 5.
# - Las variables housing y loan son las que pesan 0 en la dimensiones 1,2 y 3.
# - La variable job  es la que mas aporta a la dimesion 2.
# - El grafico de la izquierda mapea las coordenadas de los pesos de cada variable respecto a la dimension 1 y 2


# Categorias Variables
########################################################################
  
### Dimensión 1

#CONTRIBUCIÓN DE LAS CATEGORIAS A LA DIMENSIÓN 1
fviz_contrib(ACM, choice = "var", axes = 1, fill = "steelblue", color = "steelblue") +
  theme_fivethirtyeight(base_size =10, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle=40, hjust=1),
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Contribución de Categorias", subtitle = "A la Dimensión 1")+
  ylab('% Contribución') +
  xlab("Categorias")

#CONTRIBUCIÓN DE LAS CATEGORIAS A LA DIMENSIÓN 2
fviz_contrib(ACM, choice = "var", axes = 2, fill = "steelblue", color = "steelblue") +
  theme_fivethirtyeight(base_size =10, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle=40, hjust=1),
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Contribución de Categorias", subtitle = "A la Dimensión 2")+
  ylab('% Contribución') +
  xlab("Categorias") 

#CONTRIBUCIÓN DE LAS CATEGORIAS A LA DIMENSIÓN 3
fviz_contrib(ACM, choice = "var", axes = 3, fill = "steelblue", color = "steelblue") +
  theme_fivethirtyeight(base_size =10, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle=40, hjust=1),
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Contribución de Categorias", subtitle = "A la Dimensión 3")+
  ylab('% Contribución') +
  xlab("Categorias") 

### CONTRIBUCIÓN DE LAS CATEGORIAS A LA DIMENSIÓN 1 y 2


fviz_contrib(ACM, choice = "var", axes = (1:2), fill = "steelblue", color = "steelblue") +
  theme_fivethirtyeight(base_size =10, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle=40, hjust=1),
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Contribución de Categorias", subtitle = "A la Dimensión 1 y 2")+
  ylab('% Contribución') +
  xlab("Categorias")  

### CONTRIBUCIÓN DE LAS CATEGORIAS A LA DIMENSIÓN 1 , 2 y 3
fviz_contrib(ACM, choice = "var", axes = (1:3), fill = "steelblue", color = "steelblue") +
  theme_fivethirtyeight(base_size =10, base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle=40, hjust=1),
        plot.background = element_rect(fill = 'white', colour = 'white'), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.title.y  = element_text(), 
        axis.title.x = element_text()) +
  labs(title = "Contribución de Categorias", subtitle = "A la Dimensión 1, 2 y 3")+
  ylab('% Contribución') +
  xlab("Categorias") 

# Gráficos

# Representamos los datos en las 2 primeras dimensiones
plot(ACM,invisible=c("ind","quali.sup") ,cex=0.5)
plot(ACM,invisible=c("ind", cex=0.3))

#Cos2 per a les dimensiones 1 y 2
fviz_mca_var(ACM, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal(),xlim = c(-1,1.5))

# Estos gráficos nos muestran la relación entre niveles de las variables. Por ejemplo, viendo la 
# gráfica podríamos deducir que succes y contacted_yes estan muy relacionados.

#Gráfico de todas las variables
plotellipses(ACM,keepvar=c("quali"), cex=0.8)

fviz_ellipses(ACM, 1:11, pointsize = 0.5, geom = "point", addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none', axis.title.y  = element_text(), axis.title.x = element_text()) +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

#Gráficos individuales para todas las variables
#Creamos bucle
Elipses<-function(names){
  elipse<- fviz_ellipses(ACM, names , pointsize = 0.5, geom = "point", addEllipses = TRUE, repel=TRUE) +
    theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
    theme(axis.title.y  = element_text(), axis.title.x = element_text()) +
    ylab('Dimension 2') +
    xlab("Dimension 1") +
    labs(title =paste0("ACM - ",names) , subtitle = "Por Factores")
  return(elipse)
}
#Representamos gráficas
names<-names(dd)
lapply(names,Elipses)

# Una vez tenemos una visión global de las variables

#Variables comparadas con y

#JOB
fviz_ellipses(ACM, c("y.respuesta", "job"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "job"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")


#MARITAL
fviz_ellipses(ACM, c("y.respuesta", "marital"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "marital"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

#POUTCOME
fviz_ellipses(ACM, c("y.respuesta", "poutcome"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "poutcome"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")



#MONTH
fviz_ellipses(ACM, c("y.respuesta", "month"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "month"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")


#HOUSING
fviz_ellipses(ACM, c("y.respuesta", "housing"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "housing"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

#CONTACTED
fviz_ellipses(ACM, c("y.respuesta", "contacted"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "contacted"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")
#LOAN
fviz_ellipses(ACM, c("y.respuesta", "loan"), geom = "point" , pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_light( base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta", "loan"), geom = "point", pointsize = 0.5, addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  theme(legend.position = 'none') +
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

# En este apartado se compraran tres gráficas para ver relaciones. Habría que buscar las variables más 
# importantes o con mayor relación.

#Comparación entre 3 o más variables

#windows()

fviz_ellipses(ACM, c("y.respuesta","contacted", "marital"), pointsize = 0.5, geom = "point", addEllipses = TRUE, repel=TRUE) +
  theme_light(base_family = "serif", base_size = 13)+
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta","contacted", "marital"), pointsize = 0.5, geom = "point", addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

#windows()

fviz_ellipses(ACM, c("y.respuesta","contacted", "poutcome"), pointsize = 0.5, geom = "point", addEllipses = TRUE, repel=TRUE) +
  theme_light(base_family = "serif", base_size = 13)+
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")

fviz_ellipses(ACM, c("y.respuesta","contacted", "poutcome"), pointsize = 0.5, geom = "point", addEllipses = TRUE, repel=TRUE) +
  theme_solarized_2(light = FALSE, base_family = "serif", base_size = 13)+
  ylab('Dimension 2') +
  xlab("Dimension 1") +
  labs(title = "Biplot - ACM", subtitle = "Por Factores")