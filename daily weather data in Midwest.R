#Datos climáticos de NOAA (GHCN)
#The Global Historical Climatology Network (GHCN)
#GHCN-Daily contains records from over 100,000 stations in 180 countries and territories. 
#NCEI provides numerous daily variables, including maximum and minimum temperature, 
#total daily precipitation, snowfall, and snow depth; however, about one half of 
#the stations report precipitation only. 

#PRCP = Precipitation (tenths of mm)
#SNOW = Snowfall (mm)
#SNWD = Snow depth (mm)
#TMAX = Maximum temperature (tenths of degrees C)
#TMIN = Minimum temperature (tenths of degrees C)

#ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily
# "ghcnd-all.tar.gz" zipped file que contiene las mediciones diarias, un archivo .dly por cada estación.

rm(list = ls()) 
setwd("C:/Users/Magdalena Cornejo/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather")

#install.packages("rnoaa")
#install.packages("dplyr")
library(rnoaa)
library(dplyr)

#Preparando la información:
noaa_dir <- "C:/Users/Magdalena Cornejo/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather/ghcnd_hcn/"
noaaout <- "C:/Users/Magdalena Cornejo/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather/"

stnscsv <- paste0(noaaout,"stations.csv")
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )
stns <- read.fortran(paste0(noaa_dir,"ghcnd-stations.txt"),
                     typedcols, 
                     comment.char="")
hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")
names(stns) <- hdrs
write.csv(stns,stnscsv) #son 115082 estaciones meteorológicas
head(stns)

inventorycsv <- paste0(noaaout,"inventory.csv") #inventario de las estaciones. Reporta LAT/LON y año de inicio y fin del monitoreo
invcols <- c( "A11", "X1", "F8", "X1", "F9", "X1","A4",
              "X1","I4", "X1", "I4" )
inv <- read.fortran(paste0(noaa_dir,"ghcnd-inventory.txt"),
                    invcols,
                    comment.char="")
invhdrs <- c("ID", "LAT", "LON", "ELEM" , "FIRST", "LAST")
names(inv) <- invhdrs
write.csv(inv,inventorycsv)
head(inv)

#################Estados del Midwest#################
#IA                        IOWA
#IL                    ILLINOIS
#IN                     INDIANA
#KS                      KANSAS
#MI                    MICHIGAN
#MN                   MINNESOTA
#MO                    MISSOURI
#ND                NORTH DAKOTA
#NE                    NEBRASKA
#OH                        OHIO
#SD                SOUTH DAKOTA
#WI                   WISCONSIN

#Voy filtrando lo que voy a querer buscar en los datos:
data1 <- inv[grep("US+",inv$ID),] #me quedo solo con datos de US (prefijo "US")
data2 <- data1[data1$ELEM == "PRCP",] #me quedo solo con estaciones que monitorean precipitaciones
data2 <- data2[data2$FIRST <= 1970,] #me quedo con datos desde 1970
data2 <- data2[data2$LAST >= 2019,] #me quedo con datos hasta 2019

us_stns <- stns[grep("US+",stns$ID),]
unique(us_stns$ST)

midwest_stns <-  filter(us_stns, us_stns$ST=="IA" | us_stns$ST=="IL" | us_stns$ST=="IN" | us_stns$ST=="KS" | us_stns$ST=="MI" | us_stns$ST=="MN" | us_stns$ST=="MO" | us_stns$ST=="ND" | us_stns$ST=="NE" | us_stns$ST=="OH" | us_stns$ST=="SD" | us_stns$ST=="WI")
data3 <- merge(midwest_stns, data2)
length(data3$ID) #son 2565 estaciones meteorológicas iniciales


#Ahora leo la información para esas estaciones a partir de los archivos dly:
numFiles <- length(data3$ID)
dirname <- paste0(noaa_dir,"ghcnd_all/")
for (i in 1:numFiles) {
  infile <- paste0(dirname, data3$ID[i], ".dly")
  outfile <- paste0(noaaout, data3$ID[i], ".csv")
  cols <- c( "A11", "I4", "I2", "A4",
             rep( c( "I5", "A1", "A1", "A1"), 31) )
  df <- read.fortran(infile, cols, na.strings="-9999") # -9999 indica missing data
  tmp <- c("Val","xxM","xxQ","xxS") 
  vhdrs <- paste(   rep(tmp,31),   rep(1:31,each=4), sep="")
  hdrs <- c("ID", "year", "month", "element", vhdrs)
  names(df) <- hdrs
  df <- df[df$year >= 1970 & df$year <= 2019 ,]
  df <- df[df$element == "PRCP",]
  df_out <- dplyr::select(df, -matches("xx*")) 
  write.csv(df_out, outfile)
}


#EZEQUIEL:
#trabajar directamente con los archivos .csv que están en la carpeta "Datos/GHCN daily weather"
#a cada archivo/estacion asociarle los datos de LAT/LON/ST/NAME que están en "GHCN daily weatherstations"
#acomodar los datos para que sean series temporales ya que cada columna de "VALUE" es el valor diario de PRCP de 1 a 31
#filtrar y analizar cuántas estaciones meteorológicas logran cubrir al menos el 90% del período
#realizar gráfico de lluvia acumulada y comparar con el archivo "Rain data NM"

#luego, replicar todo para TMAX (temperatura máxima) --> Esto solo una vez de que estemos convencidos de todo el procedimiento para PRCP

#install.packages("reshape2")
#install.packages("maps")
library(tidyverse)
library(reshape2)
library(maps)
library(lubridate)

setwd("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather")

initial_year <- 1970
final_year <- 2019
cutoff <- 0.95

year_range <- final_year-initial_year

lista_archivos <- as.list(list.files(path="C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/GHCN daily weather", pattern="*.csv", full.names=FALSE, recursive=FALSE))


lista_archivos <- lista_archivos[lista_archivos != "GHCN daily weatherinventory.csv"] #Borro de la lista estos dos archivos que estan en el directorio pero no son datos de las weatherstations
lista_archivos <- lista_archivos[lista_archivos != "GHCN daily weatherstations.csv"]

#Itero una vez por todos los archivos y registro cuantas observaciones tiene cada uno
#En caso que el loop tarde mucho leer el csv en la linea 141
observaciones_por_weather_station <- data.frame(ws_code = character(length(lista_archivos)),
                                                nro_observaciones = integer(length(lista_archivos)))
for (i in 1:length(lista_archivos)){
  datos_estacion_meteorologica_iteracion <- read_csv(as.character(lista_archivos[i]))
  observaciones_por_weather_station[i,1] <- datos_estacion_meteorologica_iteracion[1,1]
  observaciones_por_weather_station[i,2] <- nrow(datos_estacion_meteorologica_iteracion)
  print(i)
}

#write_csv(observaciones_por_weather_station, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/observaciones_por_weather_station.csv")
#observaciones_por_weather_station <- as.data.frame(read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/observaciones_por_weather_station.csv"))

#Me fijo cuantas estaciones me quedan para distintos posibles valores de cutoff

posibles_cutoffs <- seq(0.75, 0.95, 0.01)

weather_station_por_cutoff <- data.frame(cutoff = integer(length(posibles_cutoffs)),
                                                nro_weather_stations = length(posibles_cutoffs))

for(j in 1:length(posibles_cutoffs)){
  contador <- 0
  for (i in 1:length(lista_archivos)){
    if(observaciones_por_weather_station[i,2] > year_range*12*posibles_cutoffs[j]){
      contador <- contador + 1
    }
  }
  weather_station_por_cutoff[j,1] <- posibles_cutoffs[j]
  weather_station_por_cutoff[j,2] <- contador
}

setwd("C:/Users/ezeq/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos")
write_csv(weather_station_por_cutoff, "weather_station_por_cutoff_1970_2019.csv")

#Leo los csv de las weather station de interes para armar la base de datos de precipitaciones


#filtro observaciones por weather station para tener los candidatos que pueden llegar a tener un cierto porcentaje de observaciones en el periodo especificado para acotar el espacio de busqueda de csv que tengo que levantar
observaciones_por_weather_station_filtered <- filter(observaciones_por_weather_station, nro_observaciones >= year_range*12*cutoff)

anios <- as.numeric(sort(rep(seq(1970,2019,1),12*31)))
meses <- c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31),rep(6,31),rep(7,31),rep(8,31),
  rep(9,31),rep(10,31),rep(11,31),rep(12,31))
dias <- rep(seq(1,31,1),12*length(seq(1970,2019,1)))
fechas <- as.data.frame(cbind(anios,meses,as.character(dias)))
fechas$anios <- as.double(fechas$anios)
fechas$meses <- as.double(fechas$meses)
colnames(fechas) <- c("anios", "meses", "dias")



for(i in 1:nrow(observaciones_por_weather_station_filtered)){
  weather_station_data <- read_csv(paste0(as.character(observaciones_por_weather_station_filtered[i,1]),".csv"))
  weather_station_data <- melt(weather_station_data, id.vars = c("ID", "year", "month", "element")) #esto lo convierte en formato serie de tiempo
  weather_station_data <- arrange(weather_station_data, year, month)
  weather_station_data$variable <- str_sub(weather_station_data$variable, 4, -1)
  colnames(weather_station_data)[6] <- weather_station_data[1,1]
  fechas <- left_join(fechas, weather_station_data[,c(2,3,5,6)], by= c("anios"="year", "meses"="month", "dias"="variable"))
}

#Borro los dias 31 para los meses que no tienen dia 31
fechas <- fechas %>% filter(!(meses==4 & dias=="31"))
fechas <- fechas %>% filter(!(meses==6 & dias=="31"))
fechas <- fechas %>% filter(!(meses==9 & dias=="31"))
fechas <- fechas %>% filter(!(meses==11 & dias=="31"))
fechas <- fechas %>% filter(!(meses==2 & dias=="30"))
fechas <- fechas %>% filter(!(meses==2 & dias=="31"))


#ahora quiero borrar el mes 2 y dia 29 de los anios no bisiestos
for(i in 1970:2019){
  if(leap_year(i)==FALSE){
    fechas <- fechas %>% filter(!(anios == i & meses==2 & dias=="29"))
  }
}

write_csv(fechas, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/precipitaciones_por_estacion.csv")

ghcn_daily_weather_stations <- read_csv("GHCN daily weatherstations.csv") #este dataset contiene informacion respecto a latitud y longitud y nombre de estado en que esta cada weather station

observaciones_por_weather_station_filtered <- left_join(observaciones_por_weather_station_filtered, ghcn_daily_weather_stations, by=c("ws_code"="ID"))
estaciones_por_estado <- observaciones_por_weather_station_filtered %>% group_by(ST) %>% tally()
write_csv(estaciones_por_estado, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/estaciones_por_estado.csv")

#genero bases por estado

stations_IA <- observaciones_por_weather_station_filtered %>% filter(ST=="IA") %>% select(ws_code, ST)
stations_IL <- observaciones_por_weather_station_filtered %>% filter(ST=="IL") %>% select(ws_code, ST)
stations_IN <- observaciones_por_weather_station_filtered %>% filter(ST=="IN") %>% select(ws_code, ST)
stations_KS <- observaciones_por_weather_station_filtered %>% filter(ST=="KS") %>% select(ws_code, ST)
stations_MI <- observaciones_por_weather_station_filtered %>% filter(ST=="MI") %>% select(ws_code, ST)
stations_MN <- observaciones_por_weather_station_filtered %>% filter(ST=="MN") %>% select(ws_code, ST)
stations_MO <- observaciones_por_weather_station_filtered %>% filter(ST=="MO") %>% select(ws_code, ST)
stations_ND <- observaciones_por_weather_station_filtered %>% filter(ST=="ND") %>% select(ws_code, ST)
stations_NE <- observaciones_por_weather_station_filtered %>% filter(ST=="NE") %>% select(ws_code, ST)
stations_OH <- observaciones_por_weather_station_filtered %>% filter(ST=="OH") %>% select(ws_code, ST)
stations_SD <- observaciones_por_weather_station_filtered %>% filter(ST=="SD") %>% select(ws_code, ST)
stations_WI <- observaciones_por_weather_station_filtered %>% filter(ST=="WI") %>% select(ws_code, ST)

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


#CArgo datos de produccion de maiz en EEUU por estado en 2018
#https://worldpopulationreview.com/state-rankings/corn-production-by-state

us_corn_2018 <- read_csv("C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/us_corn_production_2018.csv")
us_corn_2018 <- us_corn_2018[1:12,1:2]
us_corn_2018$share <- us_corn_2018[,2]/sum(us_corn_2018[,2])
us_corn_2018 <- us_corn_2018[c(1,2,5,7,12,4,10,11,3,8,6,9),] #ordeno para que esten en el mismo orden

precipitaciones_por_estado$ponderado <- as.numeric(nrow(precipitaciones_por_estado))

#esto es un manera fea pero rapida de armar el ponderado pero funciona. DEspues fijarme si lo puedo hacer en menos lineas

for(i in 1:nrow(precipitaciones_por_estado)){
  precipitaciones_por_estado[i,ncol(precipitaciones_por_estado)] <- sum(as.numeric(precipitaciones_por_estado[i,4:(ncol(precipitaciones_por_estado)-1)])*us_corn_2018[,3])
}

write_csv(precipitaciones_por_estado, "C:/Users/ezequ/Dropbox/Paper Climate Change & Commodity Price Dynamics/Datos/precipitaciones_por_estado.csv")

#Empiezo a mirar la dispersion geografica

states_map <- map_data("state")

weather_station_map <- left_join(observaciones_por_weather_station[observaciones_por_weather_station$nro_observaciones > year_range*12*cutoff,], ghcn_daily_weather_stations, by=c("ws_code" ="ID"))
weather_station_map$ST_name <- tolower(state.name[match(weather_station_map$ST, state.abb)])

weather_station_map <- weather_station_map %>% group_by(ST_name) %>% tally()

weather_station_map <- left_join(states_map, weather_station_map, by = c("region" = "ST_name"))

ggplot(weather_station_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = n), color = "white")+
  scale_fill_viridis_c(option = "C")
