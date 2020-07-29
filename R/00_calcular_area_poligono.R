####################################################################################
### Calcular área de um polígono shp
### Dados de uso do solo: ANA 2016
### Por: Danielle de Oliveira Moreira
### criado em: 11/06/2020
### Atualizado em: 
####################################################################################


## load rgdal package
library(rgdal)
library(raster)

## load your polygone shapefile
uso_solo <- readOGR(dsn = "./data/ANA", layer = "uso_solo_bhrd_ANA")

#################################

#Adicionar uma nova coluna do arquivo acima ($area2), para calcular a área do polígono (area(uso_solo))
uso_solo$area_ha <- area(uso_solo)/10000
uso_solo$area_km2 <- area(uso_solo)/1e+6

head(uso_solo)

#Salvar o arquivo com a nova coluna
writeOGR(uso_solo, "./data/ANA", "uso_solo_bhrd_ANA", driver="ESRI Shapefile", overwrite_layer = TRUE)
