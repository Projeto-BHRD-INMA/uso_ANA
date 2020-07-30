###############################################################
## Maps with Package tmap
## From the book: Geocomputation with R / Chapter 8
## https://geocompr.robinlovelace.net/index.html
##
## By Danielle de O. Moreira
## 29 jul 2020
## uptadeted in:
##############################################################


#LOADIND SHP
library(rgdal) #trabalhar com shp

# Land cover of sub bacia Guandu
guandu <- readOGR(dsn = "./data/ANA", layer = "uph_guandu_uso2", encoding = 'UTF-8')
guandu@data

# Basic polygon of bacia hidrográfica do rio Doce (Doce River basin)
bhrd <- readOGR(dsn = "./data", layer = "bhrd_Albers_dissol", encoding = 'UTF-8')

#Using tmap package
library(tmap)    # for static and interactive maps

## to open quick maps for visualization (similar to basic "plot")
qtm(guandu)
qtm(bhrd)
qtm(bhrd) + qtm(guandu)


# creating first part of the map
legend_title <- expression("Classes de uso do solo")     # legend
map_g <- tm_shape(guandu) +                              # view a shape
  tm_fill(col = "Uso", title = legend_title) +           # specify a column
  tm_borders()                                           # Add border layer
map_g                                                    # print a map

#usando o tm_polygons no lugar do tm_fill, também dá o mesmo mapa: as bordas ficam

#With no border
map_g1 <- tm_shape(guandu) + 
  tm_fill(col = "Uso", title = legend_title)
map_g1

#Colors
## "BuGn" shades of green
## YlOrBr shades of brown
## RdYlGn divergents colors

map_g2 <- tm_shape(guandu) + 
  tm_fill(col = "Uso", palette = "RdYlGn", style = "cat")
map_g2
# style cat was designed to represent categorical values and assures that each category receives a unique color. Se tirar dessa função acima, não faz diferença

## To load option of colors
tmaptools::palette_explorer()

# Adding a Layout
map_g2 +
  tm_compass(type = "8star", position = c("right", "top")) +  # compass
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5)        # scale bar. Can be with no breaks too (default)

### For More information of layouts and legends
args(tm_layout)
?tm_layout
?tm_legend


## Other examples
map_g2 +
  tm_style("classic", frame.lwd=10) +               # using predefined styles for the map
  tm_compass(type = "4star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5) +
  tm_layout(frame.double.line = FALSE, asp = 0)
  #tm_layout(bg.color = "grey", frame.double.line = FALSE, asp = 0)  #cor de fundo, tamanho do layout
  

# INSERTING A GRID
##  By default, tm_grid draws horizontal and vertical lines acording to the coordinate system of the (master) shape object. Latitude and longitude graticules are drawn with tm_graticules.
## Both, tm_grid() and tm_graticules() could be placed above or below the main spatial data.

#An example, arranging in three maps boxes
tmap_arrange(
  qtm(guandu) + tm_grid(),                # first box, with grids
  qtm(guandu) + tm_graticules(),          # second box, with graticules
  tm_shape(guandu) + tm_graticules() +    # third box, with graticules behind the main map
  tm_fill() + tm_borders()
)

# THIS IS MY MAP

## assign a new palette
Palette <- c("#e31a1c", "#8f9393", "#1660f3", "#bde871", "#91fd63", "#e31a1c", "#fde163", "#e31a1c","#4c6d05")

## For the legend title
legend_title <- expression("Classes de uso do solo")

## Now the map
tm_shape(guandu) + 
  tm_graticules(labels.rot = c(0, 90)) +
  tm_fill(col = "Uso", palette = Palette, title = legend_title) +
  tm_compass(type = "4star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5) +
  tm_layout(frame.double.line = FALSE, asp = 0) +
  tm_legend(bg.color = "white")
  

# INSET MAPS (mapas internos)
## The first step is to define the area of interest, which can be done by creating a new spatial object. To know the exact area of interest, I need to find the coordinantes range:
qtm(bhrd) + qtm(guandu) + tm_grid()
## the map shows me the area of my interest is between the coordinates xmin = 1820000, xmax = 1925000, ymin = 1249000, ymax = 1350000

##Creating a new spatial object
library(sf)      # For st_bbox function

guandu_region <- st_bbox(c(xmin = 1820000, xmax = 1925000,
                      ymin = 1249000, ymax = 1350000),
                    crs = st_crs(guandu)) %>% 
  st_as_sfc()

## Second step
guandu_map <- tm_shape(guandu) + 
  tm_graticules(labels.rot = c(0, 90)) +
  tm_fill(col = "Uso", palette = Palette, title = legend_title) +
  tm_compass(type = "4star", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.8) +
  tm_layout(asp = 0, frame.lwd = 5.0) +
  tm_legend(bg.color = "white")

## Third step
bhrd_map <- tm_shape(bhrd) + tm_polygons() +
  tm_shape(guandu_region) + tm_borders(lwd = 3, col = "red") 
  
## Forth step
library(grid)
guandu_map
print(bhrd_map, vp = viewport(0.85, 0.3, width = 0.25, height = 0.25))

