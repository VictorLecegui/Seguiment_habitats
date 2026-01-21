#### Script per trobar els punts de mostreig pel seguiment del hàbitats

#### Packages

library(tidyverse)
library(sf)
#install.packages(c("vegclust", "mapSpain"))
library(vegclust)
library(units)
library(leaflet)

##### Dades

boscos <- read_sf("data/MHTCv3_boscos/MHTCv3_boscos.shp")

#### Funcions

source("scripts/utils.R")

corines <- unique(boscos$COD_CORINE)
corines_sample <- corines[1:10]

####################################
#### MARK: Part 1: Crear 50 punts aleatòris amb distància 200m entre ells dins els polígons
###################################
names(boscos)
unique(boscos$COD_HIC)
##### Exemple per un únic CORINE ######

pts <- sample_points(
      polygons = boscos,
      field = "COD_HIC",
      value = "9240",
      n_points = 50,
      dist_primary = 200,
      dist_fallback = 100,
      oversample_factor = 10
    )

######################################
#### MARK: Part 2: Seleccionar 30 punts amd distància màxima però variància mínima per evitar agafar els extrems. 
######################################

D <- as.matrix(st_distance(pts))

hcr_res <- hcr(d=D, nout = 30, nsampl = 1000)
#pts_hcr2 <- hcr(d=D, nout = 30, nsampl = 10000)

pts_hcr <- pts[hcr_res,]


######################################
##### MARK: Part 3: Mapa per visualitzar resultats
######################################

pol_flt <- filter_polygons(boscos,
                           field = "COD_HIC",
                           value = "9240") |>
  st_make_valid() |>
  dplyr::filter(!st_is_empty(geometry))


# Work with wgs to be able to plot in leaflet

pol_flt_wgs <- st_transform(pol_flt, 4326)

# Transform your points too
pts_wgs <- st_transform(pts, 4326)
pts_hcr_wgs <- st_transform(pts_hcr, 4326)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = pol_flt_wgs, color = "green", weight = 2, fillOpacity = 0.3) %>%
  addCircleMarkers(data = pts_wgs, color = "black", radius = 3) %>%
  addCircleMarkers(data = pts_hcr_wgs, color = "red", radius = 3)

st_crs(pol_flt)
