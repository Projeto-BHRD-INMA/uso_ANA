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

uph_caratinga@data # mostra a tabela (quase) toda do data frame (funciona apenas com o shp)
uph_caratinga@data[["Uso"]] # O mesmo que o anterior: para ver os nomes das linhas de uma coluna do shp

#Para transformar de SpatialPolygonsDataFrame para apenas data frame 
caratinga <- as.data.frame(uph_caratinga)

View(caratinga) #mostra a tabela
head(caratinga) #mostra as primeiras 6 colunas e 6 linhas do dataframe
attributes(caratinga@data)
caratinga$Uso #mostra todos os atributos (linhas) de uma coluna específica do data frame
table(caratinga$Uso) # mostra o número de atributos (linhas) para cada tipo de uso

unique(caratinga$Uso) #mostra dados únicos de uma coluna de um data frame
classe <- subset(caratinga, Uso=="VegetaÃ§Ã£o Nativa") #para selecionar uma classe específica de uma coluna específica

#Renomear os nomes dos campos de uma coluna
levels(caratinga$Uso)

levels(caratinga$Uso) <- c('Água', 'Áreas abertas', 'Agricultura', 'Mineração', 'Reflorestamento', 'Áreas urbanas', 'Afloramento rochoso', 'Pastagem', 'Rodovias', 'Vegetação nativa')


#------------------------------------------------------------------------------
##Estatística descritiva das áreas de cada classe

###transformar a informação de m2 para ha da coluna area2. Cria-se uma nova coluna chamada area_ha
###dividir por 1e+6 para converter de m2 para km2 OU
###dividir por 10000 para converter de m2 para ha
caratinga$area_ha <- caratinga$area2/10000

View(caratinga)

sum(caratinga$area_ha) #soma total da área de todas as classes

#juntar todos os campos iguais da coluna classe para uma única linha
##### com tapply
tapply(X = caratinga$area_ha, INDEX = caratinga$Uso, FUN = sum)

##### com dplyr (faz a mesma coisa que o anterior)
library(dplyr)
caratinga %>% group_by(Uso) %>% summarise(Sum_Area_ha = sum(area_ha))
barra_caratinga <- caratinga %>% group_by(Uso) %>% summarise(Sum_Area_ha = sum(area_ha))

#Gráfico de colunas
barplot(height = barra_caratinga$Sum_Area_ha, names = caratinga$Uso)

library(ggplot2)

ggplot(data=caratinga, aes(x=Uso, y=area_ha, fill=Uso)) +
  geom_bar(stat="identity") +
  xlab("Classe de uso") + ylab("Área (ha)")

#--------------------------------------------------------------------------
#Função para estatísticas descritivas (cálculo do desvio, média, e intervalo de 95% de confiança) - summarySE. Primeiro temos que rodar a função e depois chamá-la.

#Mais informação: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#------------------------------------------------------------------------------

caratinga_est <- summarySE(caratinga, measurevar="area_ha", groupvars=c("Uso"))

caratinga_est

# The palette colors:
Palette <- c("#124cc0", "#bde871", "#aed332", "#e5368b", "#729b6f", "#de0e0b", "#8f9393", "#fde163", "#030000", "#4c6d05")

c('Água', 'Áreas abertas', 'Agricultura', 'Mineração', 'Reflorestamento', 'Áreas urbanas', 'Afloramento rochoso', 'Pastagem', 'Rodovias', 'Vegetação nativa')

###Gráfico de barras
# Error bars represent standard error of the mean
ggplot(caratinga_est, aes(x=Uso, y=area_ha, fill=Uso)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=area_ha-se, ymax=area_ha+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

#A finished graph might look like this.
ggplot(caratinga_est, aes(x=Uso, y=area_ha, fill=Uso)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.4) +      # Thinner lines
  geom_errorbar(aes(ymin=area_ha-se, ymax=area_ha+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  guides(fill=FALSE) +
  scale_fill_manual(values=Palette) + 
  xlab("Uso do solo") +
  ylab("Area (ha)") +
  ggtitle("Uso do solo na sub-bacia Caratinga") +
  theme_bw()
