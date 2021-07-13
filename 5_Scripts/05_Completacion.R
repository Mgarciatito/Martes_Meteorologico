### Completacion de datos mensuales faltantes
### Metodo CUTOFF (Feng et al, 2014): Imputacion optimizada de una correlacion cruzada
### http://dx.doi.org/10.1016/j.jhydrol.2014.11.012

#Autor: Miguel Angel Garcia Tito
#Contacto : garciatitomiguel@gmail.com

rm(list=ls())

dev.off()

dir <- 'D:/Martes_Meteoro/'

pacman::p_load(cutoffR, tidyverse, ggplot2)

data <- read.csv('3_Salida/data_pp_2016.csv', header = T, sep = ',')

matrix <- xts(data[,-1], order.by= as.Date(data[,1]))

xyplot(matrix)

info <- matrix$ACOBAMBA[300:nrow(matrix)]

matrix$ACOBAMBA[300:nrow(matrix)] = NA

xyplot(matrix)

obs <- data.frame(matrix,date=index(matrix),check.names = FALSE)

comm <- cutoff(obs, method = c("correlation"), corr = "spearman",
               cutoff = 0.7)

plot.zoo(comm, col = 'blue', main = 'Serie de tiempo completada')

xyplot(as.zoo(comm), xlab = 'Años', main = 'Completados')

df <- data.frame(data[,1], comm) %>% as_tibble()

names(df)[names(df) == "data...1."] <- "Fecha"

write.csv(df, file = '3_Salida/data_completada.csv', row.names = F, quote = F)



# Usando la funcion  ------------------------------------------------------

source('5_Scripts/Functions.R')

csv <- '3_Salida/data_pp_2021_sc.csv'

output <- '3_Salida/data_completada_2021.csv'

Completacion(csv = csv)
