####################################################################################
### Explorando os dados de uso do solo para a Bacia Hidrográfica do Rio Doce (BHRD)
### E como fazer gráficos para esse tipo de dado (segunda seção)
### Dados de uso do solo: ANA 2016
### Por: Danielle de Oliveira Moreira
### criado em: 11/06/2020
### Atualizado em: 27/07/2020
####################################################################################

# Carregando pacotes
library(raster)
library(rgdal) #trabalhar com shp

# Abrir os arquivos shp de uso do solo para cada uph (unidade de planejamento hidrográfico) da BHRD

uso_solo <- readOGR(dsn = "./data/ANA", layer = "uso_solo_bhrd_ANA", encoding = 'UTF-8')
# o encoding = 'UTF-8' serve para manter os caracteres especiais dos termos em português

crs(uso_solo)


#-------------------------------------------------------------------------------------
#Explorando os dados

uso_solo@data # mostra a tabela (quase) toda do data frame (funciona apenas com o shp)
uso_solo@data[["Uso"]] # O mesmo que o anterior: para ver os nomes das linhas de uma coluna do shp

#Para transformar de SpatialPolygonsDataFrame para apenas data frame 
uso_solo2 <- as.data.frame(uso_solo)

View(uso_solo2) #mostra a tabela

head(uso_solo2) #mostra as primeiras 6 colunas e 6 linhas do dataframe
attributes(uso_solo2@data)
uso_solo2$Uso #mostra todos os atributos (linhas) de uma coluna específica do data frame
table(uso_solo2$Uso) # mostra o número de atributos (linhas) para cada tipo de uso

unique(uso_solo2$Uso) #mostra dados únicos de uma coluna de um data frame
classe <- subset(uso_solo2, Uso=="Vegetação nativa") #para selecionar uma classe específica de uma coluna específica

#------------------------------------------------------------------------------
#Estatística descritiva das áreas de cada classe de uso

###transformar a informação de m2 para ha ou km2 da coluna area2. Cria-se uma nova coluna chamada area_ha ou area_km2
###dividir por 1e+6 para converter de m2 para km2 OU
###dividir por 10000 para converter de m2 para ha
####caratinga$area_ha <- caratinga$area2/10000

##juntar todos os campos iguais da coluna classe para uma única linha
##### com tapply
#tapply(X = uso_solo2$area_ha, INDEX = bf$Uso, FUN = sum)

##### com dplyr (faz a mesma coisa que o anterior, mas em um formato de tabela)
#library(dplyr)
#uso_solo2 %>% group_by(Uso) %>% summarise(Sum_area_ha = sum(area_ha))

## calcular estatística descritiva
library(doBy)

cdata <- summaryBy(area_km2 ~ Uso, data=uso_solo2, FUN=c(length,sum,mean,sd))
cdata


#Salvar os resultados em uma tabela
#write.csv(cdata,"./results/uso_solo_bhrd.csv", row.names = TRUE)

#-----------------------------------------------------------------------------------
# Retirar o formato científico dos números
options(scipen = 999)


#----------------------------------------------------------------------------------
#################################### GRAFICOS #####################################
#----------------------------------------------------------------------------------

# 1.Usando um arquivo csv já com a informação da porcentagem para colocar no gráfico
#Salvar os resultados resumidos com ao função doBy em uma tabela
write.csv(cdata,"./results/uso_solo_bhrd.csv", row.names = TRUE)

#Abrir a tabela depois de modificada no excel
pdata <- read.csv("./results/uso_solo_bhrd_percent.csv", header = TRUE, sep = ";")

View(pdata)

# The palette colors para o c('Afloramento Rochoso', Agricultura', 'Água', 'Áreas Abertas', 'Áreas edificadas', 'Mineração', 'Reflorestamento', 'Pastagem', 'Vegetação Nativa')
Palette <- c("#8f9393", "#aed332", "#1660f3", "#bde871", "#e31a1c",  "#e5368b", "#fde163", "#729b6f", "#4c6d05")

library(ggplot2)

#Salvar figura
png(file="./figs/grafico_classes_bhrd.png", height=15, width=20, unit="cm", res=300)

ggplot(pdata, aes(x=Uso, y=Area_perc, fill=Uso, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position=position_dodge(), stat="identity",
           size=.5) +      # Thinner lines
  scale_fill_manual(values=Palette) + 
  xlab("") +
  ylab("Area (%)") +
  theme_classic() +
  theme (axis.text = element_text(size = 12), axis.title=element_text(size=12),
         axis.text.x=element_text(size = 12, angle = 0, hjust = 1), #hjust serve para ajustar o texto abaixo do eixo x
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0)) +
  theme(legend.position="none") +
  geom_text(aes(label=Area_perc), vjust=-0.3, size=3.5) #para inserir os valores das porcentagens em cima das barras
dev.off()

#Salvar figura


#--------------------------------------------------------------------------------------

# 2. Gráfico de colunas básico com o ggplot2 usando a tabela não modificada no Excel

ggplot(data=uso_solo2, aes(x=Uso, y=area_ha, fill=Uso)) +
  geom_bar(stat="identity") +
  xlab("Classe de uso") + ylab("Área (ha)")

#Gráfico mais sofisticado usando a função estatística descritiva
# The palette colors para o c('Aeroporto','Afloramento Rochoso', 'Água', 'Áreas Abertas', 'Áreas Agricolas', 'Mineração', 'Áreas de Reflorestamento', 'Áreas Urbanas', 'Pastagem', 'Rodovias', 'Vegetação Nativa')
Palette <- c("#3a3a3a", "#8f9393", "#1660f3", "#bde871", "#aed332", "#e5368b", "#729b6f", "#de0e0b", "#fde163", "#030000", "#4c6d05")


## 3. Um gráfico em escala de cinza, sem legendas e sem grids.

ggplot(data = cdata, aes(x=Uso, y=area_km2.sum, width=.4)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=area_km2.sum-area_km2.sd, ymax=area_km2.sum+area_km2.sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 2000),breaks=0:2000*500) +
 scale_x_discrete(limits=c("Aeroporto","Afloramento Rochoso", "Água", "Áreas Abertas (Úmidas + Secas)", "Áreas de Reflorestamento", "Áreas Urbanas", "Pastagem", "Rodovias", "Vegetação Nativa" ),
                   labels=c("Aeroporto","Afloramento Rochoso", "Água", "Áreas Abertas", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"
                   ))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="none")


