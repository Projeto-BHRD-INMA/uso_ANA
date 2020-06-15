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
uso_solo <- readOGR(dsn = "F:/Dados_ArcGis/BHRD/Uso_Ocupao_do_Solo_Bacia_do_Rio_Doce", layer = "uph_sao_jose_uso")

#################################

#Adicionar uma nova coluna do arquivo acima ($area2), para calcular a área do polígono (area(uso_solo))
uso_solo$area2 <- area(uso_solo)

head(uso_solo)

#Salvar o arquivo com a nova coluna
writeOGR(uso_solo, "F:/Dados_ArcGis/BHRD/Uso_Ocupao_do_Solo_Bacia_do_Rio_Doce", "uph_sao_jose_uso2", driver="ESRI Shapefile")
