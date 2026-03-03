
##### MARK: Packages i funcions

library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)
library(pbapply)

# Funcions personalitzades

source("scripts/utils.r")

##### MARK: Dades

# Mapa d'habitats
boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

# Punts ja mostejats
punts_mostrejats <- read_sf("data/Parcelles_fetes_20260213/Parcelles_fetes_20260213.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

 # Filtrar punt duplicat punts_mostrejats

    dup_pts <- duplicated(st_geometry(punts_mostrejats))
    punts_mostrejats <- punts_mostrejats[!dup_pts,]
 # Eliminar dimensió z

    punts_mostrejats <- st_cast(punts_mostrejats, "POINT")
    punts_mostrejats <- st_zm(punts_mostrejats)


punts_mostrejats <- st_set_geometry(punts_mostrejats, "geometry")

# Malla de punts
points_cat <- readRDS("results/01_Generar_malla_Catalunya/Malla_Catalunya.rds")
points_cat <- st_set_geometry(points_cat, "geometry")

# Comprovar que tenen la mateixa projecció

st_crs(points_cat) == st_crs(boscos)
st_crs(punts_mostrejats) == st_crs(boscos)


##### MARK: Preparació pel Loop: Taula de decisió


boscos_flt <- boscos[st_area(boscos) > set_units(200, m2),]
boscos_flt <- st_make_valid(boscos_flt)

### Resum per polígon
summary_pol <- boscos_flt |>
  group_by(COD_HIC_tf, RegioHIC) |>
  summarise(
    n_pol = n()
  )

## Resum per punts de intersecció

intersections <- st_intersection(points_cat, boscos_flt)
