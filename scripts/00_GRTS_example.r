library(sf)
library(spsurvey)
library(ggplot2)
library(dplyr)

library(leaflet)
library(htmlwidgets)

source("scripts/utils.r")

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp")

#### Exemple del procediment

boscos$COD_HIC_tf <- make.names(boscos$COD_HIC)

cbind(unique(boscos$COD_HIC), unique(boscos$COD_HIC_tf))

hics <- unique(boscos$COD_HIC_tf)
hics <- hics[-2]

#### Exemple amb l'HIC 9540 (Pinedes mediterrànies)
hic_med <- filter_polygons(boscos, field = "COD_HIC_tf", 
                            value = "X9540", buff_dist = "Radi_buffe") |> 
            filter(RegioHIC == "MED") |> 
            st_union()


points_50 <- generate_points(polygon = hic_med)

punts_30 <- grts(points_50, n_base = 30)


#### Visualització

hic_med_wgs   <- st_transform(hic_med, 4326)
points_50_wgs <- st_transform(points_50, 4326)
punts_30_wgs  <- st_transform(punts_30$sites_base, 4326)

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Polygon layer
  # First point layer
  addCircleMarkers(
    data = points_50_wgs,
    radius = 4,
    color = "#d95f0e",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "50 punts aleatoris"
  ) %>%
  
  # Second point layer
  addCircleMarkers(
    data = punts_30_wgs,
    radius = 4,
    color = "#1a9850",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "30 punts GRTS"
  ) %>%
   addControl(
    "<h3>Selecció punts HIC 9540</h3>",
    position = "topright"
  ) |> 
  # Layer control
  addLayersControl(
    overlayGroups = c("50 punts aleatoris"
                        , "30 punts GRTS"),
    options = layersControlOptions(collapsed = FALSE)
  ) 
 
map
saveWidget(map, "hic_9540_med_mapa.html", selfcontained = TRUE)

