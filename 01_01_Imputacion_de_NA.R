# Importamos los datos
dd <- read.csv2(paste0(pathData,"bank-additional.csv"))


# Codificamos como NA tanto los "unknown" como los 999 en el caso de pdays
colSums(dd=="unknown")
sum(dd$pdays==999)
dd[dd=="unknown"] <- NA
dd$pdays[dd$pdays==999] <- NA
(NAnum <- colSums(is.na(dd)))
sum(is.na(dd))
sum(is.na(dd))/(4119*21)*100

barplot(NAnum[NAnum!=0], main = "Numero de NA por variable", ylab = "Frecuencia", xlab = "Variables", col = "light blue")

NAnum[NAnum!=0]
NAnum[NAnum!=0]/dim(dd)[1]*100

# Guardamos los nuevos datos como data
save(dd, file = paste0(pathData,"data.RData"))
     