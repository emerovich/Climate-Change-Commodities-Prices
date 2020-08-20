library(tidyverse)
library(reshape2)
library(lubridate)
library(plotly)
setwd("C:/Users/ezequ/Documents/Climate Change & Commodities Prices/Datos/Outputs")
matriz_frecuencia_prcn <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/Outputs/matriz_frecuencia_precipitaciones.csv")
ghcn_daily_weather_stations <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather/GHCN daily weatherstations.csv") #este dataset contiene informacion respecto a latitud y longitud y nombre de estado en que esta cada weather station


for(i in 4:ncol(matriz_frecuencia_prcn)){
  matriz_frecuencia_prcn[,i] <- (matriz_frecuencia_prcn[,i]/10)*0.0393701
}

ghcn_daily_weather_stations <- filter(ghcn_daily_weather_stations, ST=="MN")

matriz_frecuencia_prcn <- select(matriz_frecuencia_prcn, anios, meses, dias, colnames(matriz_frecuencia_prcn)[colnames(matriz_frecuencia_prcn) %in% ghcn_daily_weather_stations$ID])


matriz_frecuencia_prcn <-  matriz_frecuencia_prcn[,c(1,4:ncol(matriz_frecuencia_prcn))]


matriz_frecuencia_prcn_melted <- melt(matriz_frecuencia_prcn, id.vars = c("anios"))

matriz_frecuencia_prcn_melted <- filter(matriz_frecuencia_prcn_melted, value>=2)

matriz_frecuencia_prcn_count <- matriz_frecuencia_prcn_melted %>% 
  group_by(anios) %>% 
  count()

chart1 <- ggplot(matriz_frecuencia_prcn_count, mapping = aes(anios, n)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Numero de observaciones por año que registran por lo menos 2 pulgadas de lluvia en un dia")+
  xlab("Fecha")+
  ylab("Numero de registros")

ggplotly(chart1)

matriz_frecuencia_prcn_count <- matriz_frecuencia_prcn_melted %>% 
  group_by(anios) %>% 
  count()

matriz_frecuencia_prcn_max <- matriz_frecuencia_prcn_melted %>% 
  group_by(anios) %>% 
  summarise(Value = max(value))


chart2 <- ggplot(matriz_frecuencia_prcn_max, mapping = aes(anios, Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Maxima precipitacion registrada cada año")+
  xlab("Fecha")+
  ylab("Precipitaciones (pulgadas)")

ggplotly(chart2)