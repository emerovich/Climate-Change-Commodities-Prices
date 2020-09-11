#install.packages("missMDA")
#install.packages("factoextra")

library(tidyverse)
library(reshape2)
library(lubridate)
library(missMDA)
library(factoextra)
estados <- c("IA","IL","IN","KS","MI","MN","MO","ND","NE","OH","SD","WI")
matriz_frecuencia <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/matriz_frecuencia_tmax_95.csv")
ghcn_daily_weather_stations <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather/GHCN daily weatherstations.csv") #este dataset contiene informacion respecto a latitud y longitud y nombre de estado en que esta cada weather station

#for(i in estados){
#  select(matriz_frecuencia, as.vector(select(filter(ghcn_daily_weather_stations, ST==i), ID)))
#}


#select(matriz_frecuencia, colnames(matriz_frecuencia)[colnames(matriz_frecuencia) %in% as.vector(select(filter(ghcn_daily_weather_stations, ST=="IL"), ID)))
#as.vector(select(filter(ghcn_daily_weather_stations, ST==i), ID))

#colnames(matriz_frecuencia) %in%  as.vector(select(filter(ghcn_daily_weather_stations, ST=="MN"),ID))

matriz_melted <- melt(matriz_frecuencia, id.vars=c("anios","meses","dias"))
matriz_melted <- left_join(matriz_melted, select(ghcn_daily_weather_stations, ID, ST), by=c("variable"="ID"))
matriz_melted$Fecha <- ymd(paste0(matriz_melted$anios, "-",matriz_melted$meses, "-",matriz_melted$dias))
matriz_melted <- filter(matriz_melted, lubridate::month(Fecha)==04 |lubridate::month(Fecha)==05 |lubridate::month(Fecha)==06 |lubridate::month(Fecha)==07 |lubridate::month(Fecha)==08 |lubridate::month(Fecha)==09 |lubridate::month(Fecha)==10 |lubridate::month(Fecha)==11)
matriz_melted <- select(matriz_melted, Fecha, ST, variable, value)

matriz_IA <- dcast(select(filter(matriz_melted, ST=="IA"), Fecha, variable, value), Fecha~variable)
matriz_IL <- dcast(select(filter(matriz_melted, ST=="IL"), Fecha, variable, value), Fecha~variable)
matriz_IN <- dcast(select(filter(matriz_melted, ST=="IN"), Fecha, variable, value), Fecha~variable)
matriz_KS <- dcast(select(filter(matriz_melted, ST=="KS"), Fecha, variable, value), Fecha~variable)
matriz_MI <- dcast(select(filter(matriz_melted, ST=="MI"), Fecha, variable, value), Fecha~variable)
matriz_MN <- dcast(select(filter(matriz_melted, ST=="MN"), Fecha, variable, value), Fecha~variable)
matriz_MO <- dcast(select(filter(matriz_melted, ST=="MO"), Fecha, variable, value), Fecha~variable)
matriz_ND <- dcast(select(filter(matriz_melted, ST=="ND"), Fecha, variable, value), Fecha~variable)
matriz_NE <- dcast(select(filter(matriz_melted, ST=="NE"), Fecha, variable, value), Fecha~variable)
matriz_OH <- dcast(select(filter(matriz_melted, ST=="OH"), Fecha, variable, value), Fecha~variable)
matriz_SD <- dcast(select(filter(matriz_melted, ST=="SD"), Fecha, variable, value), Fecha~variable)
matriz_WI <- dcast(select(filter(matriz_melted, ST=="WI"), Fecha, variable, value), Fecha~variable)

PCA_IA <- prcomp(na.omit(matriz_IA[,2:ncol(matriz_IA)]), scale. = TRUE)
PCA_IL <- prcomp(na.omit(matriz_IL[,2:ncol(matriz_IL)]), scale. = TRUE)
PCA_IN <- prcomp(na.omit(matriz_IN[,2:ncol(matriz_IN)]), scale. = TRUE)
PCA_KS <- prcomp(na.omit(matriz_KS[,2:ncol(matriz_KS)]), scale. = TRUE)
PCA_MI <- prcomp(na.omit(matriz_MI[,2:ncol(matriz_MI)]), scale. = TRUE)
PCA_MN <- prcomp(na.omit(matriz_MN[,2:ncol(matriz_MN)]), scale. = TRUE)
PCA_MO <- prcomp(na.omit(matriz_MO[,2:ncol(matriz_MO)]), scale. = TRUE)
PCA_ND <- prcomp(na.omit(matriz_ND[,2:ncol(matriz_ND)]), scale. = TRUE)
PCA_NE <- prcomp(na.omit(matriz_NE[,2:ncol(matriz_NE)]), scale. = TRUE)
PCA_OH <- prcomp(na.omit(matriz_OH[,2:ncol(matriz_OH)]), scale. = TRUE)
PCA_SD <- prcomp(na.omit(matriz_SD[,2:ncol(matriz_SD)]), scale. = TRUE)
PCA_WI <- prcomp(na.omit(matriz_WI[,2:ncol(matriz_WI)]), scale. = TRUE)


peso_primer_component <- as.data.frame(rbind(c("IA",PCA_IA$sdev[1]^2/sum(PCA_IA$sdev^2)),
                                             c("IL",PCA_IL$sdev[1]^2/sum(PCA_IL$sdev^2)),
                                             c("IN",PCA_IN$sdev[1]^2/sum(PCA_IN$sdev^2)),
                                             c("KS",PCA_KS$sdev[1]^2/sum(PCA_KS$sdev^2)),
                                             c("MI",PCA_MI$sdev[1]^2/sum(PCA_MI$sdev^2)),
                                             c("MN",PCA_MN$sdev[1]^2/sum(PCA_MN$sdev^2)),
                                             c("MO",PCA_MO$sdev[1]^2/sum(PCA_MO$sdev^2)),
                                             c("ND",PCA_ND$sdev[1]^2/sum(PCA_ND$sdev^2)),
                                             c("NE",PCA_NE$sdev[1]^2/sum(PCA_NE$sdev^2)),
                                             c("OH",PCA_OH$sdev[1]^2/sum(PCA_OH$sdev^2)),
                                             c("SD",PCA_SD$sdev[1]^2/sum(PCA_SD$sdev^2)),
                                             c("WI",PCA_WI$sdev[1]^2/sum(PCA_WI$sdev^2)))
)

write_csv(peso_primer_component, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/peso_primer_componente_PCA.csv")

#prcomp(matriz_IL[,2:ncol(matriz_IL)],scale=FALSE)

producto_interno_IA <- as.matrix(matriz_IA[,2:ncol(matriz_IA)]) * PCA_IA$rotation[,1]
producto_interno_IL <- as.matrix(matriz_IL[,2:ncol(matriz_IL)]) * PCA_IL$rotation[,1]
producto_interno_IN <- as.matrix(matriz_IN[,2:ncol(matriz_IN)]) * PCA_IN$rotation[,1]
producto_interno_KS <- as.matrix(matriz_KS[,2:ncol(matriz_KS)]) * PCA_KS$rotation[,1]
producto_interno_MI <- as.matrix(matriz_MI[,2:ncol(matriz_MI)]) * PCA_MI$rotation[,1]
producto_interno_MN <- as.matrix(matriz_MN[,2:ncol(matriz_MN)]) * PCA_MN$rotation[,1]
producto_interno_MO <- as.matrix(matriz_MO[,2:ncol(matriz_MO)]) * PCA_MO$rotation[,1]
producto_interno_ND <- as.matrix(matriz_ND[,2:ncol(matriz_ND)]) * PCA_ND$rotation[,1]
producto_interno_NE <- as.matrix(matriz_NE[,2:ncol(matriz_NE)]) * PCA_NE$rotation[,1]
producto_interno_OH <- as.matrix(matriz_OH[,2:ncol(matriz_OH)]) * PCA_OH$rotation[,1]
producto_interno_SD <- as.matrix(matriz_SD[,2:ncol(matriz_SD)]) * PCA_SD$rotation[,1]
producto_interno_WI <- as.matrix(matriz_WI[,2:ncol(matriz_WI)]) * PCA_WI$rotation[,1]


matriz_IA$ponderado_IA <- apply(producto_interno_IA, 1, sum, na.rm=TRUE)
matriz_IL$ponderado_IL <- apply(producto_interno_IL, 1, sum, na.rm=TRUE)
matriz_IN$ponderado_IN <- apply(producto_interno_IN, 1, sum, na.rm=TRUE)
matriz_KS$ponderado_KS <- apply(producto_interno_KS, 1, sum, na.rm=TRUE)
matriz_MI$ponderado_MI <- apply(producto_interno_MI, 1, sum, na.rm=TRUE)
matriz_MN$ponderado_MN <- apply(producto_interno_MN, 1, sum, na.rm=TRUE)
matriz_MO$ponderado_MO <- apply(producto_interno_MO, 1, sum, na.rm=TRUE)
matriz_ND$ponderado_ND <- apply(producto_interno_ND, 1, sum, na.rm=TRUE)
matriz_NE$ponderado_NE <- apply(producto_interno_NE, 1, sum, na.rm=TRUE)
matriz_OH$ponderado_OH <- apply(producto_interno_OH, 1, sum, na.rm=TRUE)
matriz_SD$ponderado_SD <- apply(producto_interno_SD, 1, sum, na.rm=TRUE)
matriz_WI$ponderado_WI <- apply(producto_interno_WI, 1, sum, na.rm=TRUE)


indice_pca <- select(matriz_IA, Fecha)
indice_pca <- left_join(indice_pca, select(matriz_IA, Fecha, ponderado_IA), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_IL, Fecha, ponderado_IL), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_IN, Fecha, ponderado_IN), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_KS, Fecha, ponderado_KS), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_MI, Fecha, ponderado_MI), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_MN, Fecha, ponderado_MN), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_MO, Fecha, ponderado_MO), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_ND, Fecha, ponderado_ND), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_NE, Fecha, ponderado_NE), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_OH, Fecha, ponderado_OH), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_SD, Fecha, ponderado_SD), by=c("Fecha"="Fecha"))
indice_pca <- left_join(indice_pca, select(matriz_WI, Fecha, ponderado_WI), by=c("Fecha"="Fecha"))

indice_pca$indice <- apply(indice_pca[,2:ncol(indice_pca)], 1, mean, na.rm=TRUE)

write_csv(select(indice_pca, Fecha, indice), "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/indice_pca.csv")