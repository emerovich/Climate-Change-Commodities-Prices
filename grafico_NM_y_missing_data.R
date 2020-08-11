#install.packages("plotly")
library(tidyverse)
library(plotly)
library(lubridate)

setwd("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather")

ghcn_daily_weather_stations <- read_csv("GHCN daily weatherstations.csv") #este dataset contiene informacion respecto a latitud y longitud y nombre de estado en que esta cada weather station
setwd("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos")
observaciones_por_weather_station <- read_csv("observaciones_por_weather_station.csv")

initial_year <- 1970
final_year <- 2019
cutoff <- 0.91
year_range <- length(initial_year:final_year)


observaciones_por_weather_station_filtered <- filter(observaciones_por_weather_station, nro_observaciones >= year_range*12*cutoff)


observaciones_por_weather_station_filtered <- left_join(observaciones_por_weather_station_filtered, ghcn_daily_weather_stations, by=c("ws_code"="ID"))

fechas <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/precipitaciones_por_estacion.csv")


#genero bases por estado

stations_IA <- observaciones_por_weather_station_filtered %>% filter(ST=="IA" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_IL <- observaciones_por_weather_station_filtered %>% filter(ST=="IL"& ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_IN <- observaciones_por_weather_station_filtered %>% filter(ST=="IN" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_KS <- observaciones_por_weather_station_filtered %>% filter(ST=="KS" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_MI <- observaciones_por_weather_station_filtered %>% filter(ST=="MI" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_MN <- observaciones_por_weather_station_filtered %>% filter(ST=="MN" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_MO <- observaciones_por_weather_station_filtered %>% filter(ST=="MO" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_ND <- observaciones_por_weather_station_filtered %>% filter(ST=="ND" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_NE <- observaciones_por_weather_station_filtered %>% filter(ST=="NE" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_OH <- observaciones_por_weather_station_filtered %>% filter(ST=="OH" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_SD <- observaciones_por_weather_station_filtered %>% filter(ST=="SD" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)
stations_WI <- observaciones_por_weather_station_filtered %>% filter(ST=="WI" & ws_code %in% colnames(fechas)) %>% select(ws_code, ST)

precipitaciones_IA <- fechas %>% select(anios, meses, dias,stations_IA$ws_code) 
precipitaciones_IL <- fechas %>% select(anios, meses, dias,stations_IL$ws_code)
precipitaciones_IN <- fechas %>% select(anios, meses, dias,stations_IN$ws_code)
precipitaciones_KS <- fechas %>% select(anios, meses, dias,stations_KS$ws_code)
precipitaciones_MI <- fechas %>% select(anios, meses, dias,stations_MI$ws_code)
precipitaciones_MN <- fechas %>% select(anios, meses, dias,stations_MN$ws_code)
precipitaciones_MO <- fechas %>% select(anios, meses, dias,stations_MO$ws_code)
precipitaciones_ND <- fechas %>% select(anios, meses, dias,stations_ND$ws_code)
precipitaciones_NE <- fechas %>% select(anios, meses, dias,stations_NE$ws_code)
precipitaciones_OH <- fechas %>% select(anios, meses, dias,stations_OH$ws_code)
precipitaciones_SD <- fechas %>% select(anios, meses, dias,stations_SD$ws_code)
precipitaciones_WI <- fechas %>% select(anios, meses, dias,stations_WI$ws_code)

#Agrego una columna que representa el promedio de lluvias de cada dia en las estaciones de un mismo estado
precipitaciones_IA$promedio <- rowMeans(precipitaciones_IA[startsWith(names(precipitaciones_IA),"US")], na.rm = TRUE)
precipitaciones_IL$promedio <- rowMeans(precipitaciones_IL[startsWith(names(precipitaciones_IL),"US")], na.rm = TRUE)
precipitaciones_IN$promedio <- rowMeans(precipitaciones_IN[startsWith(names(precipitaciones_IN),"US")], na.rm = TRUE)
precipitaciones_KS$promedio <- rowMeans(precipitaciones_KS[startsWith(names(precipitaciones_KS),"US")], na.rm = TRUE)
precipitaciones_MI$promedio <- rowMeans(precipitaciones_MI[startsWith(names(precipitaciones_MI),"US")], na.rm = TRUE)
precipitaciones_MN$promedio <- rowMeans(precipitaciones_MN[startsWith(names(precipitaciones_MN),"US")], na.rm = TRUE)
precipitaciones_MO$promedio <- rowMeans(precipitaciones_MO[startsWith(names(precipitaciones_MO),"US")], na.rm = TRUE)
precipitaciones_ND$promedio <- rowMeans(precipitaciones_ND[startsWith(names(precipitaciones_ND),"US")], na.rm = TRUE)
precipitaciones_NE$promedio <- rowMeans(precipitaciones_NE[startsWith(names(precipitaciones_NE),"US")], na.rm = TRUE)
precipitaciones_OH$promedio <- rowMeans(precipitaciones_OH[startsWith(names(precipitaciones_OH),"US")], na.rm = TRUE)
precipitaciones_SD$promedio <- rowMeans(precipitaciones_SD[startsWith(names(precipitaciones_SD),"US")], na.rm = TRUE)
precipitaciones_WI$promedio <- rowMeans(precipitaciones_WI[startsWith(names(precipitaciones_WI),"US")], na.rm = TRUE)


precipitaciones_por_estado <- as.data.frame(cbind(fechas$anios, fechas$meses, fechas$dias, precipitaciones_IA$promedio,
                                                  precipitaciones_IL$promedio, precipitaciones_IN$promedio, 
                                                  precipitaciones_KS$promedio, precipitaciones_MI$promedio,
                                                  precipitaciones_MN$promedio, precipitaciones_MO$promedio,
                                                  precipitaciones_ND$promedio, precipitaciones_NE$promedio,
                                                  precipitaciones_OH$promedio, precipitaciones_SD$promedio,
                                                  precipitaciones_WI$promedio))

colnames(precipitaciones_por_estado) <- c("anios", "meses", "dias", "IA", "IL", "IN", "KS", 
                                          "MI", "MN", "MO", "ND", "NE", "OH", "SD", "WI")

precipitaciones_por_estado$promedio <- rowMeans(precipitaciones_por_estado[c(4:15)], na.rm = TRUE)/10

precipitaciones_por_estado <- filter(precipitaciones_por_estado, meses %in% 5:10)

#esta es una manera poco eficiente de calcular la precipitacion, tendencia y diferencia acumulada, pero como el dataset es relativamente chico no es tan grave en termino de tiempo

precipitaciones_por_estado$precipitacion_acumulada_verano <- 0
precipitaciones_por_estado$trend <- 0
precipitaciones_por_estado$trend_acumulada <- 0

for(i in seq_along(1:nrow(precipitaciones_por_estado))){
  if(precipitaciones_por_estado[i,2] %in% 6:8){
    precipitaciones_por_estado[i,17] <- precipitaciones_por_estado[i-1,17]+precipitaciones_por_estado[i,16]
    precipitaciones_por_estado[i,18] <- precipitaciones_por_estado[i-1,18]+ 3.4
    precipitaciones_por_estado[i,19] <- precipitaciones_por_estado[i,17]-precipitaciones_por_estado[i,18]
  }
}

#Armo el grafico

precipitaciones_por_estado$fecha <- ymd(paste0(precipitaciones_por_estado$anios,"-",precipitaciones_por_estado$meses,"-",precipitaciones_por_estado$dias))

fig <- plot_ly(precipitaciones_por_estado, x = ~fecha, y = ~precipitacion_acumulada_verano, name = 'Precipitacion acumulada durante el verano', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~trend_acumulada, name = 'Diferencia entre Precipitacion y Tendencia', mode = 'lines')
fig %>% layout(title = "Precipitaciones Acumuladas en los meses de verano 1970-2019",
               xaxis = list(title = "Fecha"),
               yaxis = list (title = "Precipitaciones(mm)"))

precipitaciones_por_estado <- precipitaciones_por_estado[,4:ncol(precipitaciones_por_estado)]
precipitaciones_por_estado <- precipitaciones_por_estado[,c(17,1:16)]

write_csv(precipitaciones_por_estado, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/precipitacion_acumulada_verano_1970_2019.csv")


#ANALISIS DE MISSING DATA

na_IA <- as.data.frame(sapply(precipitaciones_IA, function(x) sum(is.na(x))))
na_IL <- as.data.frame(sapply(precipitaciones_IL, function(x) sum(is.na(x))))
na_IN <- as.data.frame(sapply(precipitaciones_IN, function(x) sum(is.na(x))))
na_KS <- as.data.frame(sapply(precipitaciones_KS, function(x) sum(is.na(x))))
na_MI <- as.data.frame(sapply(precipitaciones_MI, function(x) sum(is.na(x))))
na_MN <- as.data.frame(sapply(precipitaciones_MN, function(x) sum(is.na(x))))
na_MO <- as.data.frame(sapply(precipitaciones_MO, function(x) sum(is.na(x))))
na_ND <- as.data.frame(sapply(precipitaciones_ND, function(x) sum(is.na(x))))
na_NE <- as.data.frame(sapply(precipitaciones_NE, function(x) sum(is.na(x))))
na_OH <- as.data.frame(sapply(precipitaciones_OH, function(x) sum(is.na(x))))
na_SD <- as.data.frame(sapply(precipitaciones_SD, function(x) sum(is.na(x))))
na_WI <- as.data.frame(sapply(precipitaciones_WI, function(x) sum(is.na(x))))

setwd("C:/Users/ezequ/Documents")
write.csv(na_IA, "C:/Users/ezequ/Documents/na_IA.csv")
write.csv(na_IL, "C:/Users/ezequ/Documents/na_IL.csv")
write.csv(na_IN, "C:/Users/ezequ/Documents/na_IN.csv")
write.csv(na_KS, "C:/Users/ezequ/Documents/na_KS.csv")
write.csv(na_MI, "C:/Users/ezequ/Documents/na_MI.csv")
write.csv(na_MN, "C:/Users/ezequ/Documents/na_MN.csv")
write.csv(na_MO, "C:/Users/ezequ/Documents/na_MO.csv")
write.csv(na_ND, "C:/Users/ezequ/Documents/na_ND.csv")
write.csv(na_NE, "C:/Users/ezequ/Documents/na_NE.csv")
write.csv(na_OH, "C:/Users/ezequ/Documents/na_OH.csv")
write.csv(na_SD, "C:/Users/ezequ/Documents/na_SD.csv")
write.csv(na_WI, "C:/Users/ezequ/Documents/na_WI.csv")
