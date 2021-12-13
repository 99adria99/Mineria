# PREREQUISITS: 
# Les variables  qualitatives han pasat a ser factors i no hi ha NA's (la base ha estat preprocesada)

#IMPORTACIÓ BASE DE DADES PROCESADA
load(paste0(pathData,"dataDEFINITIVA.RData"))
objects(dd)

# VISUALITZACIÓ DE LA DATA
# PRINCIPAL COMPONENT ANALYSIS(PCA) AMB VARIABLE RESP "Y" 
# nomes agafant les numeriques per fer el pca trobarem les components principals
attach(dd)
names(dd)

numeriques<-which(sapply(dd,is.numeric))
dcon<-dd[,numeriques]

#CREACIÓ DE LES COMPONENTS PRINCIPALS PCA
#amb la funció prcomp podem creac les components i procedir a reduir les dimensions
pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)
print(pc1)

#QUIN PERCENTATGE DE INERCIA ES REPRESENTADA EN EL SUBESPAIS?
#Volem saber quantes dimensions son necesaries per representar fins el 80% de la inercia (variabilitat)
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)  

#el primer component suposa un 33% de inercia, veurem com necesitem fins a 5 components principals per arribar fins el 80%
#inercia acomulada en el subespai del 1r al 8è
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum 
#Amb les 5 primeres components, conservam el 82,57% de la variància total.
#La resta de la informació és "soroll", motiu pel qual seleccionam com a 5 les dimensions signifcatives. 

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 5

print(pc1)
attributes(pc1)
pc1$rotation #components principals

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE 5 DIMENSIONS
#el comando pc1$x mostra les variables numériques transformades a través del pca
dim(pc1$x)
dim(dcon)

Psi = pc1$x[,1:nd]
dim(Psi)
#Acurtem la base en les 5 components principals significatives

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) #Vector que servira pels gráfics



#GRAFICS DELS INDIVIDUS
#la idea ara es graficar les components per extreure conclusions
#Per començar grafiquem la primera i segona component
eje1<-1
eje2<-2
#podem triar en cada eix quina dimensió volem de les 5 que comformen el 80%

plot(Psi[,eje1],Psi[,eje2],ylab = "Comp 2",xlab="Comp 1",main="Proyección de observaciones entre PC1 y PC2")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
#aquest grafic mostra la representacio de les components 1 i 2


#També podem fer gráfics de 3 dimensions tot i que tenen un cost més alt
library(rgl)
plot3d(Psi[,1],Psi[,2],Psi[,3],ylab = "Componente 2",xlab = "Componente 1",zlab = "Componente 3")  #GARFIC INTERACTIU!!! so cool


#PROJECCIÓ DE VARIABLES NUMÉRIQUES  
Phi = cor(dcon,Psi)  #ojo correlacions altes
View(Phi)

#select your axis

X<-Phi[,eje1]
Y<-Phi[,eje2]

#zooms
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-1,1),ylab = "Comp 2",xlab="Comp 1",main="Proyección de variables entre PC1 y PC2")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=1.1)



#PROJECCIÓ DE LA VARIABLE RESPOSTA

varcat=dd[,18] #ens demana la variable resposta (y)
plot(Psi[,1],Psi[,2],col=varcat,ylab = "Comp 2",xlab="Comp 1",main="Proyección de la variable respuesta entre PC1 y PC2")
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)


#PROJECCIÓ DE TOTES LES VARIABLES
#anomenem vector de categoriques
ListaVar <- sapply(dd,class)
Varfact <- which(ListaVar == "factor")
dcat<- Varfact

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1), ylim=c(-2,2),ylab = "Comp 2",xlab="Comp 1",main="Proyección de todas las variables entre PC1 y PC2")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#add projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="black", cex=0.8)
colors<-rainbow(length(dcat))
#add centroids
c<-1

for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
  text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.8)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat],pch=1,col=colors, cex=0.7)

