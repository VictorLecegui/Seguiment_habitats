
### Loop per fer trobar els punts de GRTS a Catalunya pels HIC. 


##### MARK: Packages i funcinos

library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)

# Funcions personalitzades

source("scripts/utils.R")

##### MARK: Dades

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/Malla_Catalunya.rds")

radi <- read.csv("data/HICS_Radi.csv")

# punts_mostrejats <- read_sf("data/Punts_mostrejats.shp")


##### MARK: Preparació pel Loop

hics <- unique(boscos$COD_HIC_tf)
hics <- hics[-2] # Polígons sense hics

regions <- unique(boscos$RegioHIC)


j = 1
i = 1



for(j in seq_along(regions)){

regio <- regions[j]

for(i in seq_along(hics))

hic <- hics[i]

#### Pas 1: Filter polygon



pol <- filter_polygons(boscos, 
                        field = "COD_HIC_tf", 
                        value = hic,
                        buff_dist = "Radi_buffe") |> 
                filter(RegioHIC == regio) |> 
                st_as_sf()





}