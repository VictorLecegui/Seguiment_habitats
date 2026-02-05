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


hic_teix <- filter_polygons(boscos, field = "COD_HIC_tf", 
                            value = "X9580.") |> 
            filter(RegioHIC == "MED") 



### Function to buffer


select_polygons_200m <- function(sf_obj, dist = 200) {
  
  remaining <- sf_obj
  selected <- list()
  
  while(nrow(remaining) > 0) {
    
    # randomly pick one polygon
    i <- sample(seq_len(nrow(remaining)), 1)
    chosen <- remaining[i, ]
    
    selected[[length(selected) + 1]] <- chosen
    
    # buffer chosen polygon
    exclusion_zone <- st_buffer(chosen, dist)
    
    # remove intersecting polygons
    remaining <- remaining[!st_intersects(remaining, exclusion_zone, sparse = FALSE)[,1], ]
  }
  
  do.call(rbind, selected)
}


results <- lapply(1:100, function(x) select_polygons_200m(hic_teix, 200))
best <- results[[which.max(sapply(results, nrow))]]


dist_mat <- st_distance(best)
diag(dist_mat) <- Inf
min(dist_mat)
nrow(best)
