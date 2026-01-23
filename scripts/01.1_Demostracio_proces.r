### Demostració del procés que fa la funció per dins: 

library(tidyverse)
library(vegclust)
library(leaflet)
library(sf)


#install.packages(c("spatstat.geom", "spatstat.random"))
#install.packages("spsurvey")
library(spsurvey)
library(spatstat.geom)
library(spatstat.random)

### dades

boscos <- read_sf("data/MHTCv3_boscos/MHTCv3_boscos.shp")
names(boscos)

# Funcions

source("scripts/utils.R")


#### Procediment 

# 1. Filtre de poligons per camp i valor. En aquest punt s'aplica el buffer si es posa el nom de la columna que conté el buffer

### Exemple de com s'utilitza: 

pol_flt <- filter_polygons(boscos, field = "COD_HIC", value = "9240") |> 
                st_make_valid()
pol_flt_buff <- filter_polygons(boscos, field = "COD_HIC", value = "9240", 
                                buff_dist = "Radi_buffe") |> 
                st_make_valid()


pol_flt_wgs <- st_transform(pol_flt, 4326)
pol_flt_buff_wgs <- st_transform(pol_flt_buff, 4326)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = pol_flt_wgs, color = "blue", weight = 2, fillOpacity = 0.3) %>%
  addPolygons(data = pol_flt_buff_wgs, color = "red", weight = 2, fillOpacity = 0.3)

### 2. Ara podem buscar els 50 punts dins dels polígons amb el buffer aplicat

# 2.1 Podem determinar si els punts cabran o no segons la següent formula
    # Es basa en Diggle 2013: Simple Sequential Inhibition (docs folder)

50*pi*200^2 < 0.6*sum(pol_flt_buff$Shape_Area)

n_points <- 100
dist_primary <- 200
dist_fallback <- 100
A <- sum(pol_flt_buff$Shape_Area)

# Primer comprovem que els punts hi càpiguen: 

if (n_points * pi * dist_primary^2 < 0.6 * A) {
    radi = 200
} else if (n_points * pi * dist_fallback^2 < 0.6 * A) {
   radi = 100
} else {
  print("stop")
}

# 2.2 Apliquem SSI per tobar els punts 



# A la taula hi ha objectes sense geometry, cal eliminar per convertir a win
any(st_is_empty(pol_flt)==FALSE)

pol_flt <- pol_flt[!st_is_empty(pol_flt), ]

# Converteixo a win
pol_win <- as.owin(pol_flt)



pts_ppp <- rSSI(
  r = min_dist,
  n = n_points,
  win = pol_win
)

## Convertir a sf per visualitzar

pts_sf <- st_as_sf(pts_ppp)
# Filtre dels punts perquè manté el poligon del qual ha fet el ranodm
pts_sf <- pts_sf[st_geometry_type(pts_sf) == "POINT", ]
st_crs(pts_sf) <- st_crs(pol_flt)


pts_thin <- grts(pts_sf, n_base = 30)
pts_thin_xy <- pts_thin$sites_base
st_crs(pts_thin_xy) <- st_crs(pts_sf) 


plot(pts_thin, pts_sf, key.width = lcm(3))
class(pts_thin)
pts_thin$sites_base


### Visualize with map

# poligons and points need transformation

pol_flt_wgs <- st_transform(pol_flt, 4326)
pol_flt_buff_wgs <- st_transform(pol_flt_buff, 4326)

# Transform your points too
pts_wgs <- st_transform(pts_sf, 4326)
pts_thin_wgs <- st_transform(pts_thin$sites_base, 4326)



leaflet() %>%
  addTiles() %>%
  addPolygons(data = pol_flt_wgs, color = "blue", weight = 2, fillOpacity = 0.3) %>%
  addPolygons(data = pol_flt_buff_wgs, color = "red", weight = 2, fillOpacity = 0.3) %>%
  addCircleMarkers(data = pts_wgs, color = "black", radius = 3) |> 
  addCircleMarkers(data = pts_thin_wgs, color = "green", radius = 3)


#### Extreure els valors dels punts generats

##### GRTS per variable categòrica


pts_pol <- st_intersection(x = pts_sf, y = pol_flt_buff)
st_crs(pts_thin)==st_crs(pol_flt_buff)

pts_pol |> group_by(COD_CORINE) |> summarise(n())

strata_n <- c('41.7713' = 30, '41.774' = 5 )

pts_pol_grts <- grts(pts_pol, n_base = strata_n, stratum_var = "COD_CORINE" ) 

plot(
  pts_pol_grts,
  formula = siteuse ~ COD_CORINE,
  pts_pol,
  key.width = lcm(3)
)


