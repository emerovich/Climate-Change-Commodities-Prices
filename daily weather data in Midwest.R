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
data2 <- data2[data2$FIRST >= 1970,] #me quedo con datos desde 1970
data2 <- data2[data2$LAST == 2019,] #me quedo con datos hasta 2019

us_stns <- stns[grep("US+",stns$ID),]
unique(us_stns$ST)

midwest_stns <-  filter(us_stns, us_stns$ST=="IA" | us_stns$ST=="IL" | us_stns$ST=="IN" | us_stns$ST=="KS" | us_stns$ST=="MI" | us_stns$ST=="MN" | us_stns$ST=="MO" | us_stns$ST=="ND" | us_stns$ST=="NE" | us_stns$ST=="OH" | us_stns$ST=="SD" | us_stns$ST=="WI")
data3 <- merge(midwest_stns, data2, all.y=TRUE)
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


