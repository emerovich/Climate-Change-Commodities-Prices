#install.packages("Rfast")

library(tidyverse)
library(reshape2)
library(lubridate)
library(Rfast)

#C:\Users\Horacio Merovich\Dropbox\Paper Climate Change & Commodity Price Dynamics\Datos\Outputs

setwd("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs")
matriz_frecuencia_prcn <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/matriz_frecuencia_precipitaciones.csv")
matriz_frecuencia_tmax <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/matriz_frecuencia_tmax.csv")
#promedios_diarios_estado_prcn <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/precipitaciones_promedio_diarias_por_estado_boxplot.csv")
#promedios_diarios_estado_tmax <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/tmax_promedio_diaria_por_estado_boxplot.csv")
ghcn_daily_weather_stations <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather/GHCN daily weatherstations.csv") #este dataset contiene informacion respecto a latitud y longitud y nombre de estado en que esta cada weather station

#colnames(promedios_diarios_estado_prcn) <- c("estado","promedio","fecha")
#colnames(promedios_diarios_estado_tmax) <- c("estado","promedio","fecha")

#promedios_diarios_estado_prcn$fecha <- mdy(promedios_diarios_estado_prcn$fecha)
#promedios_diarios_estado_tmax$fecha <- ymd(promedios_diarios_estado_tmax$fecha)

matriz_frecuencia_prcn$fecha <- ymd(paste0(matriz_frecuencia_prcn$anios,"-",matriz_frecuencia_prcn$meses,"-",matriz_frecuencia_prcn$dias))
matriz_frecuencia_tmax$fecha <- ymd(paste0(matriz_frecuencia_tmax$anios,"-",matriz_frecuencia_tmax$meses,"-",matriz_frecuencia_tmax$dias))

#matriz_frecuencia_prcn <- matriz_frecuencia_prcn[,4:ncol(matriz_frecuencia_prcn)] %>% melt(id.vars=c("fecha"))
#matriz_frecuencia_tmax <- matriz_frecuencia_tmax[,4:ncol(matriz_frecuencia_tmax)] %>% melt(id.vars=c("fecha"))

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

estado <- character(nrow(matriz_frecuencia_prcn)*12)
estacion <- character(nrow(matriz_frecuencia_prcn)*12)
primer_valor <- numeric(nrow(matriz_frecuencia_prcn)*12)
segundo_valor <- numeric(nrow(matriz_frecuencia_prcn)*12)
promedio <- numeric(nrow(matriz_frecuencia_prcn)*12)
fecha <- as.Date(rep(matriz_frecuencia_prcn$fecha,12))
prcp_database <- as.data.frame(cbind(as.Date(fecha),estado,estacion,primer_valor,segundo_valor,promedio))
tmax_top_database <- as.data.frame(cbind(as.Date(fecha),estado,estacion,primer_valor,segundo_valor, promedio))
tmax_bottom_database <- as.data.frame(cbind(as.Date(fecha),estado,estacion,primer_valor,segundo_valor,promedio))

#i <- 3
#prueba <- test_tmax[8161,]
#j <- 8161
#test_tmax <- select(matriz_frecuencia_tmax, fecha, colnames(matriz_frecuencia_tmax)
#                    [colnames(matriz_frecuencia_tmax) %in% filter(ghcn_daily_weather_stations, ST==as.character(ratios_historicos_por_estado[i,1]))$ID])
#tmax_top_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),6] <- mean(as.numeric(test_tmax[j,c(-1,-(Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1,index.return = TRUE, descending = TRUE)+1))]), na.rm = TRUE)

#prueba2 <- test_tmax[j,c(-1,-(Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1,index.return = TRUE, descending = TRUE)+1))]

for(i in 1:nrow(ratios_historicos_por_estado)){
  test_prcp <- select(matriz_frecuencia_prcn, fecha, colnames(matriz_frecuencia_prcn)
[colnames(matriz_frecuencia_prcn) %in% filter(ghcn_daily_weather_stations, ST==as.character(ratios_historicos_por_estado[i,1]))$ID])
  suma_diferencia <- 0
  for(j in 1:nrow(test_prcp)){
    suma_diferencia <- suma_diferencia+ Rfast::nth(as.numeric(test_prcp[j,2:ncol(test_prcp)]), 1, descending = TRUE, na.rm = TRUE) - Rfast::nth(as.numeric(test_prcp[j,2:ncol(test_prcp)]), 2, descending = TRUE, na.rm = TRUE)
    prcp_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),3] <- colnames(test_prcp[,2:ncol(test_prcp)])[Rfast::nth(as.numeric(test_prcp[j,2:ncol(test_prcp)]), 1,index.return = TRUE, descending = TRUE)] 
    prcp_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),2] <- as.character(ratios_historicos_por_estado[i,1])
    prcp_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),4] <- Rfast::nth(as.numeric(test_prcp[j,2:ncol(test_prcp)]), 1, descending = TRUE, na.rm = TRUE)
    prcp_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),5] <- Rfast::nth(as.numeric(test_prcp[j,2:ncol(test_prcp)]), 2, descending = TRUE, na.rm = TRUE)
    prcp_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),6] <- mean(as.numeric(test_prcp[j,c(-1,-(Rfast::nth(as.numeric(test_prcp[j,2:ncol(test_prcp)]), 1,index.return = TRUE, descending = TRUE)+1))]), na.rm = TRUE)

    #if(is.na(suma_diferencia)){
    #  print(j)
    #}
  }
  ratios_historicos_por_estado[i,2] <- suma_diferencia/nrow(test_prcp)
  test_tmax <- select(matriz_frecuencia_tmax, fecha, colnames(matriz_frecuencia_tmax)
                      [colnames(matriz_frecuencia_tmax) %in% filter(ghcn_daily_weather_stations, ST==as.character(ratios_historicos_por_estado[i,1]))$ID])
  suma_diferencia_tmax_top <- 0
  suma_diferencia_tmax_bottom <- 0
  for(j in 1:nrow(test_tmax)){
    suma_diferencia_tmax_top <- suma_diferencia_tmax_top+ abs(Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1, descending = TRUE, na.rm = TRUE) - Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 2, descending = TRUE, na.rm = TRUE))
    #if(is.na(suma_diferencia)){
    #  print(j)
    #}
    suma_diferencia_tmax_bottom <- suma_diferencia_tmax_bottom+ abs(Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1, descending = FALSE, na.rm = TRUE) - Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 2, descending = FALSE, na.rm = TRUE))
    tmax_top_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),3] <- colnames(test_tmax[,2:ncol(test_tmax)])[Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1,index.return = TRUE, descending = TRUE)]
    tmax_top_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),2] <- as.character(ratios_historicos_por_estado[i,1])
    tmax_top_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),4] <- Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1, descending = TRUE, na.rm = TRUE)
    tmax_top_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),5] <- Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 2, descending = TRUE, na.rm = TRUE)
    tmax_top_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),6] <- mean(as.numeric(test_tmax[j,c(-1,-(Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1,index.return = TRUE, descending = TRUE)+1))]), na.rm = TRUE)
    
    tmax_bottom_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),3] <- colnames(test_tmax[,2:ncol(test_tmax)])[Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1,index.return = TRUE, descending = FALSE)]
    tmax_bottom_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),2] <- as.character(ratios_historicos_por_estado[i,1])
    tmax_bottom_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),4] <- Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1, descending = FALSE, na.rm = TRUE)
    tmax_bottom_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),5] <- Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 2, descending = FALSE, na.rm = TRUE)
    tmax_bottom_database[(j+nrow(matriz_frecuencia_prcn)*(i-1)),6] <- mean(as.numeric(test_tmax[j,c(-1,-(Rfast::nth(as.numeric(test_tmax[j,2:ncol(test_tmax)]), 1,index.return = TRUE, descending = FALSE)+1))]), na.rm = TRUE)
  }
  ratios_historicos_por_estado[i,3] <- suma_diferencia_tmax_top/nrow(test_tmax)
  ratios_historicos_por_estado[i,4] <- suma_diferencia_tmax_bottom/nrow(test_tmax)
  print(i)
}

rm(test_tmax)
rm(test_prcp)
rm(ratio_promedio_prcp)
rm(ratio_promedio_tmax)
rm(ratio_promedio_tmin)


prcp_database$fecha <-  as.Date(rep(matriz_frecuencia_prcn$fecha,12))
tmax_top_database$fecha <- as.Date(rep(matriz_frecuencia_prcn$fecha,12))
tmax_bottom_database$fecha <- as.Date(rep(matriz_frecuencia_prcn$fecha,12))

rm(matriz_frecuencia_tmax)
rm(matriz_frecuencia_prcn)

prcp_database <- left_join(prcp_database, select(ghcn_daily_weather_stations, ID, ST, NAME), by=c("estacion"="ID"))
tmax_top_database <- left_join(tmax_top_database, select(ghcn_daily_weather_stations, ID, ST, NAME), by=c("estacion"="ID"))
tmax_bottom_database <- left_join(tmax_bottom_database, select(ghcn_daily_weather_stations, ID, ST, NAME), by=c("estacion"="ID"))

rm(ghcn_daily_weather_stations)

prcp_database <- left_join(prcp_database, ratios_historicos_por_estado, by=c("estacion"="ID"))
tmax_top_database <- left_join(tmax_top_database, ratios_historicos_por_estado, by=c("estacion"="ID"))
tmax_bottom_database <- left_join(tmax_bottom_database, ratios_historicos_por_estado, by=c("estacion"="ID"))

rm(ratios_historicos_por_estado)

write_csv(prcp_database, "C:/Users/ezequ/Documents/prcp_database.csv")
write_csv(tmax_top_database, "C:/Users/ezequ/Documents/tmax_top_database.csv")
write_csv(tmax_bottom_database, "C:/Users/ezequ/Documents/tmax_bottom_database.csv")



#Lo siguiente quedó de una verison vieja. NO CORRER


matriz_frecuencia_prcn <- matriz_frecuencia_prcn %>% melt(id.vars="fecha")
matriz_frecuencia_tmax <- matriz_frecuencia_tmax %>% melt(id.vars="fecha")


top50_prcn <- matriz_frecuencia_prcn %>% arrange(desc(value))
top50_tmax <- matriz_frecuencia_tmax %>% arrange(desc(value))
bottom50_tmax <- matriz_frecuencia_tmax %>% arrange(value)

top50_prcn <- top50_prcn[1:1000,] %>% left_join(ghcn_daily_weather_stations[,c(2,6,7)], by=c("variable"="ID"))
top50_tmax <- top50_tmax[1:1000,] %>% left_join(ghcn_daily_weather_stations[,c(2,6,7)], by=c("variable"="ID"))
bottom50_tmax <- bottom50_tmax[1:1000,] %>% left_join(ghcn_daily_weather_stations[,c(2,6,7)], by=c("variable"="ID"))


top50_prcn <- left_join(top50_prcn, promedios_diarios_estado_prcn, by=c("fecha"="fecha","ST"="estado"))
top50_tmax <- left_join(top50_tmax, promedios_diarios_estado_tmax, by=c("fecha"="fecha","ST"="estado"))
bottom50_tmax <- left_join(bottom50_tmax, promedios_diarios_estado_tmax, by=c("fecha"="fecha","ST"="estado"))


top50_prcn <- top50_prcn[,c(4,5,2,1,3,6)]
top50_tmax <- top50_tmax[,c(4,5,2,1,3,6)]
bottom50_tmax <- bottom50_tmax[,c(4,5,2,1,3,6)]

colnames(top50_prcn)[c(3,5)] <- c("ID","prcn")
colnames(top50_tmax)[c(3,5)] <- c("ID","tmax")
colnames(bottom50_tmax)[c(3,5)] <- c("ID","tmax")

write_csv(top50_prcn, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/top50_prcn.csv")
write_csv(top50_tmax, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/top50_tmax.csv")
write_csv(bottom50_tmax, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/bottom50_tmax.csv")
