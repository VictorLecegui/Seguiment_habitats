#### Script per calcular la probabilitat de tenir com a  mínim 1 punts en el polígon per atzar

### Utilitzo les fagedes d'exemple


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

colnames(boscos)

hic_fg <- filter_polygons(boscos, 
                            field = "COD_HIC_tf",
                            value = "X9130",
                            buff_dist = "Radi_buffe") |> 
            filter(RegioHIC == "MED") |> 
            st_as_sf()



# Pas 2: Set up the polygon and points for simulations

hic_fg <- hic_fg[!st_is_empty(hic_fg),]

hic_fg <- hic_fg |> 
  mutate(poly_id = as.character(row_number()))


st_crs(points_cat)==st_crs(hic_fg)

points_cat_crop <- st_crop(points_cat, st_bbox(hic_fg))

hic_points <- st_join(points_cat_crop, hic_fg,
                        join = st_within, left = FALSE)


# Pas 3: Monte Carlo permutation amb les nostres condicions

set.seed(1998)

n_sim <- 500
n_points <- 30
min_dist <- 200

sim_results <- purrr::map_dfr(1:n_sim, function(i) {

  samp <- irs(hic_points, mindis = min_dist, n_base = n_points)
    samp <- samp$sites_base    

    samp_sum <- samp |> group_by(poly_id) |> 
                summarise(N = n())

  hic_fg |> 
    mutate(has_point = ifelse(poly_id %in% samp_sum$poly_id, 1, 0)) |> 
    mutate(sim = i)
})

# Pas 4: Probabilitat per polígon

unique(sim_results$has_point)

prob_df <- sim_results |> 
  group_by(poly_id) |> 
  summarise(
    prob_point = mean(has_point),
    .groups = "drop"
  ) |> 
  st_as_sf()

# Join back to polygons
hic_fg_prob <- hic_fg |> 
  st_join(prob_df, by = "poly_id")

### MAP

pal <- colorNumeric(
  palette = "viridis",
  domain = hic_fg_prob$prob_point,
  na.color = "transparent"
)

hic_fg_ll_prob <- st_transform(hic_fg_prob, 4326)

map_prob <- leaflet(hic_fg_ll_prob) |>
  addProviderTiles(providers$CartoDB.Positron) |>

  addPolygons(
    fillColor = ~pal(prob_point),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    label = ~paste0(
      "Probabilitat ≥1 punt: ",
      round(prob_point, 2)
    )
  ) |>

  addLegend(
    pal = pal,
    values = ~prob_point,
    title = "Probabilitat\n(≥1 punt)",
    position = "bottomright"
  )
map_prob

saveWidget(map_prob, "hic_9130_Probabilitat_poligons_random.html", selfcontained = TRUE)

##### MARK: Exemple de GRTS #####


# Pas 3: Monte Carlo permutation amb les nostres condicions

set.seed(1998)

n_sim <- 500
n_points <- 30
min_dist <- 200

sim_results_grts <- purrr::map_dfr(1:n_sim, function(i) {

  samp <- grts(hic_points, mindis = min_dist, n_base = n_points)
    samp <- samp$sites_base    

    samp_sum <- samp |> group_by(poly_id) |> 
                summarise(N = n())

  hic_fg |> 
    mutate(has_point = ifelse(poly_id %in% samp_sum$poly_id, 1, 0)) |> 
    mutate(sim = i)
})

# Pas 4: Probabilitat per polígon

unique(sim_results_grts$has_point)

prob_df_grts <- sim_results_grts |> 
  group_by(poly_id) |> 
  summarise(
    prob_point = mean(has_point),
    .groups = "drop"
  ) |> 
  st_as_sf()

# Join back to polygons
hic_fg_prob_grts <- hic_fg |> 
  st_join(prob_df_grts, by = "poly_id")

### MAP

pal_grts <- colorNumeric(
  palette = "viridis",
  domain = hic_fg_prob_grts$prob_point,
  na.color = "transparent"
)

hic_fg_ll_prob_grts <- st_transform(hic_fg_prob, 4326)

map_prob_grts <- leaflet(hic_fg_ll_prob_grts) |>
  addProviderTiles(providers$CartoDB.Positron) |>

  addPolygons(
    fillColor = ~pal_grts(prob_point),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    label = ~paste0(
      "Probabilitat ≥1 punt: ",
      round(prob_point, 2)
    )
  ) |>

  addLegend(
    pal = pal_grts,
    values = ~prob_point,
    title = "Probabilitat\n(≥1 punt)",
    position = "bottomright"
  )

map_prob_grts

saveWidget(map_prob, "hic_9130_Probabilitat_poligons_random.html", selfcontained = TRUE)
