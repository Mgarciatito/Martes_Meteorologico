###########################################################################
########################Martes de Meteorosssssssssssssssssss###############
###########################################################################
#Autor: Miguel Angel Garcia Tito
#Contacto : garciatitomiguel@gmail.com

#Primero que se les viene a la mente cuando piensan en R?
#Seguro algo aburrido, programacion, o cosas como que si 
#no corre que pasara?, bueno para eso este pequeño taller.
#Si no logras correr alguna linea consulta sin miedo.
#Que la fuerza y bugs nos acompañen!!!!!!!!!!!!!!!!!!

# Extraccion de datos PISCO -----------------------------------------------

rm(list = ls())

if (!require('pacman')) install.packages('pacman',
                   dependencies = T)

pacman::p_load(raster, ncdf4)

dir <- 'D:/Martes_Meteoro/'

setwd(dir)

getwd()


# Extrayendo datos PISCO V2.1 ---------------------------------------------

data <- '1_Insumos/long_lat_2.csv'

nc <- '2_Grillados/Evapo.nc'

ouput <- '3_Salida/data_etp.csv'

start <- '1981-01-01'

end <- '2016-12-01'

Datos.mensuales <- function(data, nc){
  
  require(pacman)
  
  pacman::p_load(xts, zoo, lattice)
  
  long_lat <- read.csv(data, header = T, sep = ',')
  
  grid <- raster::brick(nc)
  
  sp::coordinates(long_lat) <- ~XX+YY
  
  raster::projection(long_lat) <- raster::projection(grid)
  
  points_long_lat<- raster::extract(grid[[1]], long_lat, 
                                    cellnumbers = T)[,1]
  
  data_long_lat<- t(grid[points_long_lat])
  
  colnames(data_long_lat) <- as.character(long_lat$NN)
  
  Fecha <- seq(as.Date(start), as.Date(end),
               by='month')
  
  data <- data.frame(Fecha, data_long_lat)
  
  write.csv(data, file = ouput, row.names = F)
  
  idx <- as.Date(data[,1])
  
  data.matrix <- data[,-1]
  
  data.xts <- xts(data.matrix, order.by = idx )
  
  data.zoo <- as.zoo(data.xts)
  
  xyplot(data.xts,xlab = "Fecha")
  
}

Datos.mensuales(data,nc)


# EXTRAYENDO DATOS ACTUALES -----------------------------------------------

nc <- '2_Grillados/Pp_2021_Pisco.nc'

ouput <- '3_Salida/data_pp_2021.csv'

start <- '1981-01-01'

end <- '2021-03-01'

Datos.mensuales(data,nc)


# Calculando la Evapotranspiracion ----------------------------------------

tmax <- read.csv("3_Salida/data_tmax.csv", header = T)[-1]

tmin <- read.csv("3_Salida/data_tmin.csv", header = T)[-1]

etp_pisco <- read.csv('3_Salida/data_etp.csv', header = T)

Fecha <- seq(as.Date("1981-01-01"), as.Date('2016-12-01'),
             by='month')

tmedia <- (tmax+tmin)/2

# write.csv(data.frame(Fecha,tmedia),
#       'Salida/tmedia.csv', row.names=F)

pacman::p_load(SPEI)

long_lat <- read.csv('1_Insumos/long_lat_2.csv', header = T, sep = ',',)

thor <- data.frame(Fecha, thornthwaite(tmedia$ACOBAMBA,
                                       long_lat$YY[2]))

write.csv(thor, '3_Salida/thornthwaite.csv', row.names = F)

har <- data.frame(Fecha, hargreaves(Tmin = tmin$ACOBAMBA,
                  Tmax = tmax$ACOBAMBA,
                  lat = long_lat$YY[2]))

write.csv(har, '3_Salida/hargreaves', row.names = F)

par(mfrow=c(3,1))

plot(thor, col = 'red', ylab = 'Evapotranspiración (mm)', 
     main = 'Thornthwaite', type = 'l')

plot(har, col = 'red', ylab = 'Evapotranspiración (mm)',
     main = 'Hargreaves', type = 'l')

plot(etp_pisco$ACOBAMBA, col = 'red', ylab = 'Evapotranspiración (mm)',
     main = 'Oudin', type = 'l')


# Podemos hacerlo con GGPLOT2? --------------------------------------------

#Pues obvio xD

library(reshape2)

data <- cbind(etp_pisco$ACOBAMBA, thor$PET_tho, har$ET0_har)

colnames(data) <- c('Oudin', 'Thornthwaite', 'Hargreaves')

data2 <- data.frame(Fecha, data)

data <- melt(data2 , id.vars = 'Fecha') 

colnames(data)

ggplot(data = data, aes(x = Fecha, y = value, col = variable))+
  geom_line()+ theme_bw()+
  ggtitle('Comparación de Evapotranspiración (mm)') + 
  labs(subtitle = 'Introducción a la Hidrometeorología', 
       caption = 'Autor : Miguel Garcia', x = 'Fecha',
       y = 'Evapotranspiración (mm)')


# Extraccion de PISCO AREAL -----------------------------------------------

library(tidyverse)

grid <- raster::brick('2_Grillados/Prec.nc')

shp <- shapefile('1_Insumos/Cuencas_hidrograficas/Cuencas_hidrograficas.shp')

#plot(shp, main = 'Cuencas', col = 'blue', axes = T)

shp <- shp[shp$NOMBRE %in% c('Cuenca Santa'), ] 

plot(shp, main = 'Cuenca Santa', col = 'blue', axes = T)

proj4string(grid) <- proj4string(shp) 

pp.areal <- raster::extract(grid, shp, fun=mean)

data.frame(Fecha,t(pp.areal)) %>% 
  plot(main = 'Serie Santa', type = 'l', 
       ylab = 'Precipitación', col = 'blue')

final <- data.frame(Fecha,t(pp.areal)) 

colnames(final) <- c('Fecha', 'Pp')

write.csv(final, 
          '3_Salida/pp_media_areal.csv', row.names=F)


# Haciendo algunos graficos  xD -----------------------------------------------


# Creando series de tiempo ------------------------------------------------


p1 <- ggplot(final, aes(x=Fecha, y = Pp))+
  geom_line(color = 'blue')+
  geom_point(size = 1, colour = 'blue')+
  ggtitle('Cuenca Santa')+
  theme_bw()+
  geom_smooth(color= 'black', span = 0.4, size = 1)+
  scale_x_date(date_breaks = "3 year",
               date_labels = "%Y",expand = c(0,0))+ 
  ylab('Precipitación (mm)')


# Creando Boxplots --------------------------------------------------------

data  <- final %>% mutate(mes=format(Fecha,'%b'))

p <- ggplot(data,aes(mes,Pp)) + geom_boxplot() +
  xlab('Precipitacion') +  ylab('Meses')

p <- ggplot(data,aes(reorder(mes,Fecha),Pp, fill = mes)) + 
  geom_boxplot(outlier.colour = 'red', outlier.size = 3, outlier.shape = 8)+
  xlab('Meses') + ylab('Precipitación')+
  theme_bw()+
  ggtitle('Cuenca Santa Areal')

data$mes <- factor(data$mes, 
                   levels = c('Ene.','Feb.', 'Mar.',
                              'Abr.', 'May.', 'Jun.',
                              'Jul.', 'Ago.', 'Set.',
                              'Oct.', 'Nov.', 'Dic.'))

p2 <- ggplot(data,aes(reorder(mes,Fecha),Pp, fill = mes)) + 
  geom_boxplot(outlier.colour = 'red', outlier.size = 3, outlier.shape = 8)+
  xlab('Meses') + ylab('Precipitación')+
  theme_bw()+
  ggtitle('Cuenca Santa Areal')

# Creando Histogramas -----------------------------------------------------

p3 <- ggplot(data,aes(Pp)) + 
  geom_histogram(col='blue',fill = 'blue',binwidth = 5) + 
  theme_bw()+
  ylab('Meses')


p4 <- ggplot(data,aes(Pp,fill=mes)) + geom_density(alpha=0.25 ,bw=15)+
  theme_bw()


# Creando Scatterplots ----------------------------------------------------

pp <- read.csv('3_Salida/data_pp.csv', header = T) %>% data.frame()

colnames(pp)

str(pp)

data  <- pp %>% mutate(mes=format(as.Date(Fecha),'%b'))

data$mes <- factor(data$mes, 
                   levels = c('Ene.','Feb.', 'Mar.',
                              'Abr.', 'May.', 'Jun.',
                              'Jul.', 'Ago.', 'Set.',
                              'Oct.', 'Nov.', 'Dic.'))

colnames(pp)

ggplot(pp, aes(CARAMPOMA,PUCRO)) +geom_point(col='blue') +
  theme_bw()

p5 <- ggplot(data, aes(CARAMPOMA,PUCRO)) +geom_point(aes(col=mes)) +
  theme_bw()


# Y si lo combinamos? -----------------------------------------------------

library(ggpubr)

ggarrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)

# Espero te haya corrido todo, sino pues consulta sin miedo al exito!!!
