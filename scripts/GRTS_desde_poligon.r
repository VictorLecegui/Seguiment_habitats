library(sf)
library(spsurvey)
library(ggplot2)
library(dplyr)
library(units)
library(leaflet)
library(htmlwidgets)

source("scripts/utils.r")

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                    mutate(COD_HIC_tf = make.names(COD_HIC))

#### Exemple del procediment

df_hics <- cbind(unique(boscos$COD_HIC), unique(boscos$COD_HIC_tf))
df_hics <- as.data.frame(df_hics)
df_hics$Radi <- c("")

df_hics <- as.data.frame(boscos) |>
              filter(COD_HIC!="-") |>
              group_by(COD_HIC) |> 
              summarise(Area = sum(Shape_Area)) |> 
              arrange(desc(Area))
df_hics

#write.csv(df_hics, "results/01_GRTS_HIC/df_HICS.csv", row.names = FALSE)

hics_test <- make.names(df_hics$COD_HIC)[1:10]


#### Punts base de de la funció grts

n_size <- as.integer(ceiling(pmin(1e9, 10 * (30))))

sfpts <- st_sample(hic_teix, size = 60, type = "hexagonal", exact = TRUE)



#### Exemple amb l'HIC 9540 (Pinedes mediterrànies)

log_file <- "results/01_GRTS_HIC/Punts_grts_HIC.log"
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
# HIC de prova, fagedes
hic = "X9130"

for(i in seq_along(hics_test)){
  hic = hics_test[1]

  log_msg("INFO", paste("Començant a processar HIC:", hic))



hic_pol <- filter_polygons(boscos, field = "COD_HIC_tf", 
                            value = hic, buff_dist = "Radi_buffe") |> 
            filter(RegioHIC == "MED") |> 
            st_as_sf()

hic_pol <- hic_pol[!st_is_empty(hic_pol), ]

  log_msg("INFO", paste("Començant GRTS HIC:", hic))

punts_30 <- grts(hic_pol, n_base = 30, mindis = 200, pt_density = 10)
punts_30_area <- grts(hic_pol, n_base = 30, mindis = 200, 
                  seltype = "proportional", 
                  aux_var = "Shape_Area", pt_density = 10)


  
  
  log_msg("INFO", paste("GRTS HIC", hic, "acabat"))


punts_dist <- st_distance(punts_30_area$sites_base)
diag(punts_dist)<-Inf
min(punts_dist)


if(min(punts_dist) < set_units(200, m)){
  log_msg("ERROR", paste("Hi ha punts a menys de 200m"))
}

saveRDS(punts_30, paste0("results/01_GRTS_HIC/", hic, "_grts_30p_MED.rds"))

log_msg("INFO", paste(hic, "ACABAT"))

}
### genrerar punts de reserva amb hexagons

make_hexagon <- function(center, radius = 200) {
  
  angles <- seq(0, 2 * pi, length.out = 7)[-7]  # 6 vertices
  
  coords <- cbind(
    st_coordinates(center)[1] + radius * cos(angles),
    st_coordinates(center)[2] + radius * sin(angles)
  )
  
  st_polygon(list(rbind(coords, coords[1, ])))
}


hex <- make_hexagon(punts_30$sites_base[1,])
plot(hex)
plot(punts_30$sites_base[1,], add = TRUE)


## coordenades de reserva
st_coordinates(hex)

plot(st_geometry(hex), col = NA, border = "red")
plot(st_geometry(punts_30$sites_base), add = TRUE, pch = 19)
plot(st_geometry(st_coordinates(hex)), add = TRUE, pch = 19, col = "blue")




#### Mapa de visualització 

n_size <- as.integer(ceiling(pmin(1e9, 10 * (30))))

sfpts <- st_sample(hic_pol, size = n_size, type = "hexagonal", exact = TRUE)



punts_50_random <- readRDS("results/01_Loop_punts/Punts_aleatoris/X9130_MED_50p.rds")
punts_30_random <- readRDS("results/01_Loop_punts/Punts_seleccionats/X9130_MED_30p.rds")
  
  


hic_pol_wgs   <- st_transform(hic_pol, 4326)
punts_30_wgs <- st_transform(punts_30$sites_base, 4326)
punts_30_area_wgs <- st_transform(punts_30_area$sites_base, 4326)
punts_30_options_wgs <- st_transform(sfpts, 4326)

punts_50_wgs <- st_transform(punts_50_random, 4326)
punts_30_rand_wgs <- st_transform(punts_30_random$sites_base, 4326)


map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addPolygons(
    data = hic_pol_wgs, 
    color = "green",
    fillOpacity = 0.8 
    ) |> 
      addCircleMarkers(
    data = punts_30_options_wgs,
    radius = 4,
    color = "#0890bd",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "300 Punts base polígon"
  ) |> 
  
  # Polygon layer
  # First point layer
  addCircleMarkers(
    data = punts_30_wgs,
    radius = 4,
    color = "#1b1b7a",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "30 punts GRTS sobre polígon"
  )  |> 
  # addCircleMarkers(
  #   data = punts_30_area_wgs,
  #   radius = 4,
  #   color = "#0890bd",
  #   fillOpacity = 0.8,
  #   stroke = FALSE,
  #   group = "30 punts GRTS area"
  # )  |>  
  addCircleMarkers(
    data = punts_50_wgs,
    radius = 4,
    color = "#da7a7a",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "50 punts aleatoris SSI"
  )  |>  
  addCircleMarkers(
    data = punts_30_rand_wgs,
    radius = 4,
    color = "#db1919",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "30 punts GRTS sobre punts aleatoris"
  )  |>  
   addControl(
    "<h3>Selecció punts HIC 9130</h3>",
    position = "topright"
  ) |> 
  addLayersControl(
    overlayGroups = c("30 punts GRTS sobre punts aleatoris","50 punts aleatoris SSI"
                        , "30 punts GRTS sobre polígon", "300 Punts base polígon"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

map

saveWidget(map, "hic_9130_med_mapa.html", selfcontained = TRUE)
