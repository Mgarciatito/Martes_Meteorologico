#Autor: Miguel Angel Garcia Tito
#Contacto : garciatitomiguel@gmail.com

Datos.mensuales <- function(data, nc, start, end){
  
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


Datos.areal <- function(nc, shp, start, end){
  
  require(pacman)
  
  pacman::p_load(xts, zoo, lattice, ggplot2, tidyverse,
                 raster)
  
  grid <- raster::brick(nc)
  
  shp <- shapefile(shp)
  
  proj4string(grid) <- proj4string(shp)
  
  areal <- raster::extract(grid, shp, fun=mean)
  
  Fecha <- seq(as.Date(start), as.Date(end),
               by='month')
  
  final <- data.frame(Fecha,t(areal))
  
  colnames(final) <- c('Fecha', 'Pp')
  
  write.csv(data, file = ouput, row.names = F)
  
  print(ggplot(final, aes(x=Fecha, y = Pp))+
          geom_line(color = 'blue')+
          geom_point(size = 1, colour = 'blue')+
          ggtitle('Precipitacion Areal')+
          theme_bw()+
          geom_smooth(color= 'black', span = 0.4, size = 1)+
          scale_x_date(date_breaks = "3 year",
                       date_labels = "%Y",expand = c(0,0))+ 
          ylab('Precipitación (mm)'))
}



Completacion <- function(csv){
  
  pacman::p_load(cutoffR, tidyverse)
  
  data <- read.csv(csv, header = T, sep = ',')
  
  matrix <- xts(data[,-1], order.by= as.Date(data[,1], 
                                             sep = '/',
                                             format = '%d/%m/%Y'))

  obs <- data.frame(matrix,date=index(matrix),check.names = FALSE)
  
  comm <- cutoff(obs, method = c("correlation"), corr = "spearman",
               cutoff = 0.7)
  
  df <- data.frame(data[,1], comm) %>% as_tibble()
  
  names(df)[names(df) == "data...1."] <- "Fecha"
  
  write.csv(df, file = output, row.names = F, quote = F)
  
  xyplot(as.zoo(comm), xlab = 'Años', main = 'Completados')

}
