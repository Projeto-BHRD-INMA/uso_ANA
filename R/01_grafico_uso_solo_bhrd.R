####################################################################################
### Explorando os dados de uso do solo para a Bacia Hidrográfica do Rio Doce (BHRD)
### E como fazer gráficos para esse tipo de dado
### Dados de uso do solo: ANA 2016
### Por: Danielle de Oliveira Moreira
### criado em: 11/06/2020
### Atualizado em: 
####################################################################################

# Carregando pacotes
library(raster)
library(rgdal) #trabalhar com shp

# Abrir os arquivos shp de uso do solo para cada uph (unidade de planejamento hidrográfico) da BHRD

uph_caratinga <- readOGR(dsn = "./data/ANA", layer = "uph_caratinga_uso2", encoding = 'UTF-8')
# o encoding = 'UTF-8' serve para manter os caracteres especiais dos termos em português

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
classe <- subset(caratinga, Uso=="Vegetação nativa") #para selecionar uma classe específica de uma coluna específica

#------------------------------------------------------------------------------
#Estatística descritiva das áreas de cada classe de uso

###transformar a informação de m2 para ha ou km2 da coluna area2. Cria-se uma nova coluna chamada area_ha ou area_km2
###dividir por 1e+6 para converter de m2 para km2 OU
###dividir por 10000 para converter de m2 para ha
####caratinga$area_ha <- caratinga$area2/10000

##juntar todos os campos iguais da coluna classe para uma única linha
##### com tapply
tapply(X = caratinga$area_ha, INDEX = bf$Uso, FUN = sum)

##### com dplyr (faz a mesma coisa que o anterior, mas em um formato de tabela)
library(dplyr)
caratinga %>% group_by(Uso) %>% summarise(Sum_area_ha = sum(area_ha))

## calcular estatística descritiva
library(doBy)

cdata <- summaryBy(area_ha ~ Uso, data=caratinga, FUN=c(length,sum,mean,sd))
cdata

#Salvar os resultados em uma tabela
write.csv(cdata,"./results/caratinga.csv", row.names = TRUE)

#-----------------------------------------------------------------------------------

#Gráfico de colunas básico com o ggplot2
library(ggplot2)

ggplot(data=caratinga, aes(x=Uso, y=area_ha, fill=Uso)) +
  geom_bar(stat="identity") +
  xlab("Classe de uso") + ylab("Área (ha)")

#Gráfico mais sofisticado usando a função estatística descritiva
# The palette colors para o c('Água', 'Áreas abertas', 'Agricultura', 'Mineração', 'Reflorestamento', 'Áreas urbanas', 'Afloramento rochoso', 'Pastagem', 'Rodovias', 'Vegetação nativa')
Palette <- c("#124cc0", "#bde871", "#aed332", "#e5368b", "#729b6f", "#de0e0b", "#8f9393", "#fde163", "#030000", "#4c6d05")


###Gráfico de barras colorido
# Error bars represent standard error of the mean
ggplot(caratinga_est, aes(x=Uso, y=area_ha, fill=Uso)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=area_ha-se, ymax=area_ha+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

###A finished graph might look like this.
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


##Um gráfico em escala de cinza, sem legendas e sem grids.

ggplot(data = caratinga_est, aes(x=Uso, y=area_ha, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=area_ha-se, ymax=area_ha+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Área (ha)") +
  scale_y_continuous(limits = c(0, 2000),breaks=0:2000*500) +
 # scale_x_discrete(limits=c("Aeroporto","Afloramento Rochoso", "Água", "Áreas Abertas (Úmidas + Secas)", "Áreas de Reflorestamento", "Áreas Urbanas", "Pastagem", "Rodovias", "Vegetação Nativa" ),
#                   labels=c("Aeroporto","Afloramento Rochoso", "Água", "Áreas Abertas", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"
#                   ))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="none")
