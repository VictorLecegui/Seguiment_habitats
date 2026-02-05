library(sf)
library(spsurvey)
library(vegan)
library(tidyverse)
library(leaflet)
library(mapSpain)
library(units)

source("scripts/utils.r")


boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp")

boscos <- boscos |> mutate(COD_HIC_tf = make.names(COD_HIC))

### Pas 1: Generar malla sobre la que retallar els polígons

cat <- esp_get_ccaa("Catalunya")
cat <- cat$geometry

# Projectar igual que el shape de boscos
cat <- st_transform(cat, 25831)

st_crs(cat)==st_crs(boscos)

## Construir la malla:
    # ATENCIÓ!!!!!
    # cellsize indica la distància entre els centres dels hexàgons. 
        # A l'ajuda de la funció expliquen que per obtenir el costat s'ha de multiplicar per sqrt(3)

# Per obtenir que els vèrtex estiguin separats entre ells 100m:
mida_costat <- 100
cellsize_param <- mida_costat * sqrt(3)

cat_grid <- st_make_grid(cat, cellsize = cellsize_param, square = FALSE)
cat_grid <- st_intersection(cat_grid, cat)

saveRDS(cat_grid, "results/Malla_100m_cat.rds")
# Càlcul del centroide dels hexàgons
hex_cent <- st_centroid(cat_grid)

# Càlcul dels vèrtex dels hexàgons
hex_vert <- cat_grid |> 
                st_cast("MULTILINESTRING") |> 
                st_cast("POIINT")

hex_vert <- st_union(hex_vert) |> 
                st_cast("POINT")

# Ajuntem centroides i vèrtex
points_grid <- c(hex_cent, hex_vert)
points_grid <- st_sf(geometry = points_grid)

# Retallem per Catalunya
points_cat <- st_intersection(points_grid, cat)
