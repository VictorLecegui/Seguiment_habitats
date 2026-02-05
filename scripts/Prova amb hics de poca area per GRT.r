#### Prova amb hics de poca area per GRTS


hic_teix <- filter_polygons(boscos, field = "COD_HIC_tf", 
                                value = "X9580.") |> 
                                st_as_sf()

hic_teix <- hic_teix[!st_is_empty(hic_teix), ]


#### Visualització del polígon


hic_teix_wgs   <- st_transform(hic_teix, 4326)
#punts_30_wgs <- st_transform(punts_30$sites_base, 4326)
#punts_30_area_wgs <- st_transform(punts_30_area$sites_base, 4326)
punts_30_options_wgs <- st_transform(sfpts, 4326)


map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addPolygons(
    data = hic_teix_wgs, 
    color = "green",
    fillOpacity = 0.8 
    ) |> 
  addCircleMarkers(
    data = punts_30_options_wgs,
    radius = 4,
    color = "white",
    fillOpacity = 0.8,
    stroke = FALSE,
    group = "30 punts GRTS"
  )  |> 
   addControl(
    "<h3>Selecció punts HIC 9130</h3>",
    position = "topright"
  )

map




system.time(
punts_30_teix <- grts(hic_teix, n_base = 30, mindis = 200, 
                        seltype = "proportional", 
                        aux_var = "Shape_Area",
                        pt_density = 3)
)
