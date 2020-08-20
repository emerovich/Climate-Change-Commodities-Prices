library(tidyverse)
library(reshape2)
library(lubridate)
library(Rfast)
library(ggplot2)
library(plotly)


#C:\Users\Horacio Merovich\Dropbox\Paper Climate Change & Commodity Price Dynamics\Datos\Outputs

setwd("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs")
matriz_frecuencia_prcn <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/matriz_frecuencia_precipitaciones.csv")
matriz_frecuencia_tmax <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/matriz_frecuencia_tmax.csv")
ghcn_daily_weather_stations <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather/GHCN daily weatherstations.csv") #este dataset contiene informacion respecto a latitud y longitud y nombre de estado en que esta cada weather station

matriz_frecuencia_prcn$fecha <- ymd(paste0(matriz_frecuencia_prcn$anios,"-",matriz_frecuencia_prcn$meses,"-",matriz_frecuencia_prcn$dias))
matriz_frecuencia_tmax$fecha <- ymd(paste0(matriz_frecuencia_tmax$anios,"-",matriz_frecuencia_tmax$meses,"-",matriz_frecuencia_tmax$dias))

matriz_frecuencia_prcn <- matriz_frecuencia_prcn[,4:ncol(matriz_frecuencia_prcn)] 
matriz_frecuencia_tmax <- matriz_frecuencia_tmax[,4:ncol(matriz_frecuencia_tmax)] 

matriz_frecuencia_prcn <-  matriz_frecuencia_prcn[,c(ncol(matriz_frecuencia_prcn),1:(ncol(matriz_frecuencia_prcn)-1))]
matriz_frecuencia_tmax <-  matriz_frecuencia_tmax[,c(ncol(matriz_frecuencia_tmax),1:(ncol(matriz_frecuencia_tmax)-1))]

estados <- c("IA","IL","IN","KS","MI","MN","MO","ND","NE","OH","SD","WI")

ratio_promedio_prcp <- numeric(length(estados))
ratio_promedio_tmax <- numeric(length(estados))
ratio_promedio_tmin <- numeric(length(estados))
ratios_historicos_por_estado <- as.data.frame(cbind(estados, ratio_promedio_prcp, ratio_promedio_tmax,
                                                    ratio_promedio_tmin))


for(i in 1:length(estados)){
  test_prcp <- select(matriz_frecuencia_prcn, fecha, colnames(matriz_frecuencia_prcn)
                      [colnames(matriz_frecuencia_prcn) %in% filter(ghcn_daily_weather_stations, ST==as.character(ratios_historicos_por_estado[i,1]))$ID])
  test_prcp$max <- apply(test_prcp[,2:ncol(test_prcp)], 1, max, na.rm=TRUE)
  test_prcp$second_max <- apply(test_prcp[,2:(ncol(test_prcp)-1)], 1, Rfast::nth, 2, descending=TRUE, na.rm=TRUE)
  ratios_historicos_por_estado[i,2] <- sum(test_prcp$max-test_prcp$second_max)/nrow(test_prcp)
  
  test_tmax <- select(matriz_frecuencia_tmax, fecha, colnames(matriz_frecuencia_tmax)
                      [colnames(matriz_frecuencia_tmax) %in% filter(ghcn_daily_weather_stations, ST==as.character(ratios_historicos_por_estado[i,1]))$ID])
  test_tmax$max <- apply(test_tmax[,2:ncol(test_tmax)], 1, max, na.rm=TRUE)
  test_tmax$second_max <- apply(test_tmax[,2:(ncol(test_tmax)-1)], 1, Rfast::nth, 2, descending=TRUE, na.rm=TRUE)
  ratios_historicos_por_estado[i,3] <- sum(test_tmax$max-test_tmax$second_max)/nrow(test_tmax)
  
  test_tmax$min <- apply(test_tmax[,2:(ncol(test_tmax)-2)], 1, min, na.rm=TRUE)
  test_tmax$second_min <- apply(test_tmax[,2:(ncol(test_tmax)-3)], 1, Rfast::nth, 2, descending=FALSE, na.rm=TRUE)
  ratios_historicos_por_estado[i,4] <- sum(abs(test_tmax$min-test_tmax$second_min))/nrow(test_tmax)
}

write_csv(ratios_historicos_por_estado, "C:/Users/ezequ/Documents/ratios_historicos_por_estado.csv")


prcp_database <- read_csv("C:/Users/ezequ/Documents/prcp_database.csv")
tmax_top_database <- read_csv("C:/Users/ezequ/Documents/tmax_top_database.csv")
tmax_bottom_database <- read_csv("C:/Users/ezequ/Documents/tmax_bottom_database.csv")
ratios_historicos_por_estado <- read_csv("C:/Users/ezequ/Documents/ratios_historicos_por_estado.csv")

prcp_database <- left_join(prcp_database, select(ratios_historicos_por_estado,estados,ratio_promedio_prcp),by=c("estado"="estados"))
tmax_top_database <- left_join(tmax_top_database, select(ratios_historicos_por_estado,estados,ratio_promedio_tmax),by=c("estado"="estados"))
tmax_bottom_database <- left_join(tmax_bottom_database, select(ratios_historicos_por_estado,estados,ratio_promedio_tmin),by=c("estado"="estados"))

prcp_database$ratio <- (prcp_database$primer_valor-prcp_database$segundo_valor)/prcp_database$ratio_promedio_prcp
tmax_top_database$ratio <- (abs(tmax_top_database$primer_valor-tmax_top_database$segundo_valor))/tmax_top_database$ratio_promedio_tmax
tmax_bottom_database$ratio <- (abs(tmax_bottom_database$primer_valor-tmax_bottom_database$segundo_valor))/tmax_bottom_database$ratio_promedio_tmin


prcp_database <- select(prcp_database, fecha, estado, estacion, NAME, primer_valor, segundo_valor, promedio, ratio_promedio_prcp, ratio)
tmax_top_database <- select(tmax_top_database, fecha, estado, estacion, NAME, primer_valor, segundo_valor, promedio, ratio_promedio_tmax, ratio)
tmax_bottom_database <- select(tmax_bottom_database, fecha, estado, estacion, NAME, primer_valor, segundo_valor, promedio, ratio_promedio_tmin, ratio)

prcp_database <- arrange(prcp_database, desc(primer_valor))
tmax_top_database <- arrange(tmax_top_database, desc(primer_valor))
tmax_bottom_database <- arrange(tmax_bottom_database, primer_valor)

colnames(prcp_database) <- c("Fecha", "Estado", "ID", "Nombre", "Max PRCP", "Second Max PRCP","Promedio", "Diferencia Promedio", "Ratio")
colnames(tmax_top_database) <- c("Fecha", "Estado", "ID", "Nombre", "Max TMAX", "Second Max TMAX","Promedio", "Diferencia Promedio", "Ratio")
colnames(tmax_bottom_database) <- c("Fecha", "Estado", "ID", "Nombre", "Bottom TMAX", "Second Bottom TMAX","Promedio", "Diferencia Promedio", "Ratio")

write_csv(prcp_database[1:1000,], "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/PRCP_Top_1000.csv")
write_csv(tmax_top_database[1:219144,], "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/TMAX_Top_219144.csv")
write_csv(tmax_bottom_database[1:219144,], "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/TMAX_Bottom_219144.csv")

boxplot_prcp <-  ggplot(prcp_database, mapping = aes(month(Fecha), Ratio)) + geom_boxplot() +
  coord_flip() +
  ggtitle(paste0("Boxplot del ratio de diferencias entre primer y segunda  precipitación por Estado")) +
  xlab("Mes") +
  ylab("Ratios")
  
ggplotly(boxplot_prcp)



boxplot_tmax_top <-  ggplot(tmax_top_database, mapping = aes(as.factor(month(Fecha)), Ratio)) + geom_boxplot() +
  coord_flip() +
  ggtitle(paste0("Boxplot del ratio de diferencias entre primer y segunda tmax por Mes")) +
  xlab("Mes") +
  ylab("Ratios")

ggplotly(boxplot_tmax_top)

boxplot_tmax_bottom <-  ggplot(tmax_bottom_database, mapping = aes(as.factor(month(Fecha)), Ratio)) + geom_boxplot() +
  coord_flip() +
  ggtitle(paste0("Boxplot del ratio de diferencias entre primera y segunda tmax mas baja por Mes")) +
  xlab("Mes") +
  ylab("Ratios")

ggplotly(boxplot_tmax_bottom)


arrange(tmax_top_database, desc(Ratio))[1:10,]

tmax_top_database[221413,]
