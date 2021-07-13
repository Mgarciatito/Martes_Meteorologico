#Autor: Miguel Angel Garcia Tito
#Contacto : garciatitomiguel@gmail.com

rm(list = ls())

dir <- 'D:/Martes_Meteoro/'

setwd(dir)

source('5_Scripts/Functions.R')

if (!require('pacman')) install.packages('pacman',
                                         dependencies = T)

# Procesando datos puntuales ----------------------------------------------

data <- "1_Insumos/long_lat_2.csv"

nc <- "2_Grillados/Prec.nc"

start <- '1981-01-01'

end <- '2016-12-01'

ouput <- '3_Salida/data_pp_2016.csv'

Datos.mensuales(data, nc, start , end)


# Procesando datos areales ------------------------------------------------

pacman::p_load(tidyverse, raster)
 
shp <- '1_Insumos/Chancay/Cuenca_chancay_huaral.shp'

nc <- "2_Grillados/Pp_2021_Pisco.nc"

start <- '1981-01-01'

end <- '2021-03-01'

ouput <- '3_Salida/data_areal_2016.csv'

Datos.areal(nc = nc, shp = shp, start = start, end = end )
