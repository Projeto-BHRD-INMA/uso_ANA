####################################################################################
### Explorando os dados de uso do solo para a Bacia Hidrográfica do Rio Doce (BHRD)
### Dados de uso do solo: ANA 2016
### Por: Danielle de Oliveira Moreira
### criado em: 11/06/2020
### Atualizado em: 
####################################################################################

# Carregando pacotes
library(raster)
library(rgdal) #trabalhar com shp

# Abrir os arquivos shp de uso do solo para cada uph (unidade de planejamento hidrográfico) da BHRD

uph_caratinga <- readOGR(dsn = "./data/ANA", layer = "uph_caratinga_uso2")


plot(uph_caratinga)

#-------------------------------------------------------------------------------------
#Explorando os dados

#Para transformar de SpatialPolygonsDataFrame para apenas data frame 
caratinga <- as.data.frame(uph_caratinga)

View(caratinga) #mostra a tabela
head(caratinga) #mostra as primeiras 6 colunas e 6 linhas do dataframe
attributes(caratinga@data)
caratinga@data # mostra a tabela (quase) toda do data frame
caratinga$Uso #mostra todos os atributos (linhas) de uma coluna específica do data frame
caratinga@data[["Uso"]] # O mesmo que o anterior: para ver os nomes das linhas de uma coluna do shp
table(caratinga$Uso)

unique(caratinga$Uso) #mostra dados únicos de uma coluna de um data frame
classe <- subset(caratinga, Uso=="VegetaÃ§Ã£o Nativa") #para selecionar uma classe específica de uma coluna específica

#Renomear os nomes dos campos de uma coluna
levels(caratinga$Uso)

levels(caratinga$Uso) <- c('Água', 'Áreas abertas', 'Agricultura', 'Mineração', 'Reflorestamento', 'Áreas urbanas', 'Afloramento rochoso', 'Pastagem', 'Rodovias', 'Vegetação nativa')


#------------------------------------------------------------------------------
##Estatística descritiva das áreas de cada classe

#transformar a informação de m2 para ha da coluna area2
#dividir por 1e+6 para converter de m2 para km2 OU
#dividir por 10000 para converter de m2 para ha
caratinga$area_ha <- caratinga$area2/10000

View(caratinga)

sum(caratinga$area_ha) #soma total da área de todas as classes

#juntar todos os campos iguais da coluna classe para uma única linha
##### com tapply
tapply(X = caratinga$area_ha, INDEX = caratinga$Uso, FUN = sum)

##### com dplyr
library(dplyr)
barra_caratinga <- caratinga %>% group_by(Uso) %>% summarise(Sum_Area_ha = sum(area_ha))

#Gráfico de colunas
barplot(height = barra_caratinga$Sum_Area_ha, names = caratinga$Uso)


library(ggplot2)
library(plyr)

caratinga$tamanho <- c(rep("tam", 3535))
View(caratinga)

uph_1 <- ddply(caratinga, c("Uso", "tamanho"), summarise,
               N    = length(area),
               mean = mean(area),
               sd   = sd(area),
               se   = sd / sqrt(N)
)

#está dando erro
#------------------------------------------------------------------------------















