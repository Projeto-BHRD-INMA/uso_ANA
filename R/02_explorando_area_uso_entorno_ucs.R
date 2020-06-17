####################################################################################
### Explorando os dados de uso do solo no entorno das UCs da Bacia Hidrográfica do Rio
### Doce (BHRD)
### Dados de uso do solo: ANA 2016
### Por: Danielle de Oliveira Moreira
### criado em: 16/06/2020
### Atualizado em: 
####################################################################################

# Carregando pacotes
library(raster)
library(rgdal) #trabalhar com shp

# Abrir os arquivos shp de uso do solo para cada uph (unidade de planejamento hidrográfico) da BHRD

uso <- readOGR(dsn = "./data/ANA", layer = "buffer_uso_ucs_5km", encoding = 'UTF-8')
# o encoding = 'UTF-8' serve para manter os caracteres especiais dos termos em português


plot(uso)

#-------------------------------------------------------------------------------------
#Explorando os dados
##Para transformar de SpatialPolygonsDataFrame para apenas data frame 
uso_df <- as.data.frame(uso)

View(uso_df) #mostra a tabela

unique(uso_df$buffer)

#Selecionar uma região para calcular estatísticas básicas. Neste caso, está na coluna buffer
## para plotar o mapa. Aqui, não uso o novo data frame, mas sim o objeto anterior
mapa <- subset(uso, buffer=="Rio Corrente") 

plot(mapa)

## Com o data frame
bf <- uso_df[uso_df$buffer == "Rio Corrente", ]
###ou ainda
#  filter(uso_df, buffer == "Pedra do Monjolo")

View(bf)

#Buscar os nomes únicos de classes
unique(bf$Uso)


#------------------------------------------------------------------------------
##Estatística descritiva das áreas de cada classe

###transformar a informação de m2 para ha ou km2 da coluna area2. Cria-se uma nova coluna chamada area_ha ou area_km2
###dividir por 1e+6 para converter de m2 para km2 OU
###dividir por 10000 para converter de m2 para ha
####caratinga$area_ha <- caratinga$area2/10000

#juntar todos os campos iguais da coluna classe para uma única linha
##### com tapply
#tapply(X = bf$area_ha, INDEX = bf$Uso, FUN = sum)

##### com dplyr (faz a mesma coisa que o anterior, mas em um formato de tabela)
#library(dplyr)
#bf %>% group_by(Uso) %>% 
#  summarise(Sum_area_ha = sum(area_ha))

## calcular estatística descritiva com o pacote doBy
library(doBy)

cdata <- summaryBy(area_ha ~ Uso, data=bf, FUN=c(length,sum,mean,sd))
cdata

#Salvar os resultados em uma tabela
write.csv(cdata,"./results/uso_ucs_RioCorrente.csv", row.names = TRUE)
#------------------------------------------------------------------------------

#Gráfico de colunas básico com o ggplot2
library(ggplot2)

ggplot(data=bf_df, aes(x=Uso, y=area_ha, fill=Uso)) +
  geom_bar(stat="identity") +
  xlab("Classe de uso") + ylab("Área (ha)")

#Salvar os resultados em uma tabela
write.csv(cdata,"./results/uso_ucs_AltoMucuri.csv", row.names = TRUE)

# The palette colors para o c('Água', 'Áreas abertas', 'Agricultura', 'Reflorestamento', 'Áreas urbanas', 'Afloramento rochoso', 'Pastagem', 'Vegetação nativa')
Palette <- c("#124cc0", "#bde871", "#aed332", "#729b6f", "#de0e0b", "#8f9393", "#fde163", "#4c6d05")


###Gráfico de barras colorido
# Error bars represent standard error of the mean
ggplot(cdata, aes(x=Uso, y=area_ha.sum, fill=Uso)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=area_ha.sum-area_ha.sd, ymax=area_ha.sum+area_ha.sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

###A finished graph might look like this.
ggplot(cdata, aes(x=Uso, y=area_ha.sum, fill=Uso)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.4) +      # Thinner lines
  geom_errorbar(aes(ymin=area_ha.sum-area_ha.sd, ymax=area_ha.sum+area_ha.sd),
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

ggplot(data = cdata, aes(x=Uso, y=area_ha.sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=area_ha.sum-area_ha.sd, ymax=area_ha.sum+area_ha.sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Área (ha)") +
  scale_x_discrete(limits=c("Água", "Áreas abertas", "Agricultura", "Reflorestamento", "Áreas urbanas", "Afloramento rochoso", "Pastagem", "Vegetação nativa"),
                   labels=c("Água", "Áreas abertas", "Agricultura", "Reflorestamento", "Áreas urbanas", "Afloramento rochoso", "Pastagem", "Vegetação nativa"
                   ))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="none")
