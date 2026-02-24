##### MARK: Packages i funcions

library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)



##### Exemples

# Situació 1: Malla + GRTS

# HIC 9130

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))


pol <- boscos |> filter(COD_HIC== 9130 & RegioHIC == "MED")

pts_malla <- readRDS("results/02_Loop_punts_HIC/Malla_per_HIC/Malla_X9130_MED.rds")
pts_grts <- readRDS("results/02_Loop_punts_HIC/Punts_mostreig/Punts_X9130_MED.rds")

pts_malla_wgs <- st_transform(pts_malla, 4326)
pts_grts_wgs <- st_transform(pts_grts$punts$sites_base, 4326)
pol_wgs <- st_transform(pol, 4326)


map_malla_grts <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addPolygons(
    data = pol_wgs, 
    color = "green",
    fillOpacity = 0.8 
    ) |> 
  addCircleMarkers(
    data = pts_malla_wgs,
    radius = 1,
    color = "white",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "Malla 100m"
  )  |> 
  addCircleMarkers(
    data = pts_grts_wgs,
    radius = 3,
    color = "red",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "Punts GRTS"
  )  |> 
   addControl(
    "<h3>Selecció punts HIC 9130</h3>",
    position = "topright"
  )

map_malla_grts


saveWidget(map, "results/09_Mapes/Mapa_HIC_9130.html", 
            selfcontained = FALSE)


# HIC 9380


pol <- boscos |> filter(COD_HIC== 9380 & RegioHIC == "MED")

pts_malla <- readRDS("results/02_Loop_punts_HIC/Malla_per_HIC/Punts_centre_X9380_MED.rds")
pts_grts <- readRDS("results/02_Loop_punts_HIC/Punts_mostreig/Punts_X9380_MED.rds")

pts_malla_wgs <- st_transform(pts_malla, 4326)
pts_grts_wgs <- st_transform(pts_grts$punts$sites_base, 4326)
pol_wgs <- st_transform(pol, 4326)


map_pol_grts <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addPolygons(
    data = pol_wgs, 
    color = "green",
    fillOpacity = 0.8 
    ) |> 
  addCircleMarkers(
    data = pts_malla_wgs,
    radius = 1,
    color = "white",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "Malla 100m"
  )  |> 
  addCircleMarkers(
    data = pts_grts_wgs,
    radius = 3,
    color = "red",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "Punts GRTS"
  )  |> 
   addControl(
    "<h3>Selecció punts HIC 9380</h3>",
    position = "topright"
  )

map_pol_grts


saveWidget(map_pol_grts, "results/09_Mapes/Mapa_HIC_9380.html", 
            selfcontained = FALSE)
