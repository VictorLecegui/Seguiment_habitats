library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)


source("scripts/utils.R")

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/Malla_Catalunya.rds")

# Pas 1: Obtenir polígon 

hic_pi <- filter_polygons(boscos, 
                            field = "COD_HIC_tf",
                            value = "X9540",
                            buff_dist = "Radi_buffe") |> 
            filter(RegioHIC == "MED") |> 
            st_as_sf()

hic_pi <- hic_pi[!st_is_empty(hic_pi),]

# Pas 2: Retallar el polígon de la malla

st_crs(points_cat)==st_crs(hic_pi)

points_cat_crop <- st_crop(points_cat, st_bbox(hic_pi))

hic_points <- st_join(points_cat_crop, hic_pi,
                        join = st_within, left = FALSE)


# Fer grts sobre els punts de dins del polígon amb distància mínima de 200m

set.seed(1998)

hic_30p_grts <- grts(hic_points, n_base = 30, mindis = 200)

hic_30p_rand <- irs(hic_points, mindis = 200, n_base = 30)



### Visualització

hic_pi_ll        <- st_transform(hic_pi, 4326)
points_crop_ll  <- st_transform(hic_points, 4326)
points_30_grts_ll    <- st_transform(hic_30p_grts$sites_base, 4326)
points_30_rand_ll    <- st_transform(hic_30p_rand$sites_base, 4326)



map_pi <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>

  # # Habitat polygon
  # addPolygons(
  #   data = hic_pi_ll,
  #   fillColor = "darkgreen",
  #   fillOpacity = 0.4,
  #   color = "black",
  #   weight = 1,
  #   group = "Polígon HIC 9540"
  # ) |>

  # # Cropped grid
  # addCircleMarkers(
  #   data = points_crop_ll,
  #   radius = 1.5,
  #   color = "grey",
  #   fillOpacity = 0.6,
  #   stroke = FALSE,
  #   group = "Malla de 100m"
  # ) |>

  # GRTS 30 points
  addCircleMarkers(
    data = points_30_grts_ll,
    radius = 5,
    color = "red",
    fillOpacity = 1,
    stroke = TRUE,
    weight = 1,
    group = "GRTS 30 punts"
  ) |>
  # Random 30 points
  addCircleMarkers(
    data = points_30_rand_ll,
    radius = 5,
    color = "blue",
    fillOpacity = 1,
    stroke = TRUE,
    weight = 1,
    group = "Random 30 punts"
  ) |>

  addLayersControl(
    overlayGroups = c("Random 30 punts", "GRTS 30 punts"),
    options = layersControlOptions(collapsed = FALSE)
  )

map_pi

saveWidget(map_pi, "hic_9540_Malla_grts_vs_rand.html", selfcontained = TRUE)


balance_df <- bind_rows(
                sp_balance(hic_30p_grts$sites_base, hic_points),
                sp_balance(hic_30p_rand$sites_base, hic_points)) |> 
        mutate(Mehtod = c("GRTS", "Random"))

write.csv(balance_df, "results/9540/HIC_9540_balance.csv", row.names = FALSE)
