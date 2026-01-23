#### Prcediment generació de punts

# Categoria: HIC
# Metode generació punts aleatòris: rSSI
# Mètode treure 30/50 punts: grst estratificat


#### Packages

#install.packages(c("spatstat.geom", "spatstat.random", "spsurvey"))

library(tidyverse)
library(sf)
library(leaflet)
library(spsurvey)
install.packages("spatstat.utils")
library(spatstat.geom)
library(spatstat.random)
#library(terra)
#library(raster)


# install remotes if not installed
install.packages("remotes")

# install specific compatible versions
remotes::install_version("spatstat.utils", version = "3.1.2")
remotes::install_version("spatstat.data", version = "3.1.2")
remotes::install_version("spatstat.random", version = "3.1.2")
remotes::install_version("spatstat.geom", version = "3.1.2")
remotes::install_version("spatstat.core", version = "3.1.2")

source("scripts/utils.R")

boscos <- read_sf("data/MHTCv3_boscos/MHTCv3_boscos.shp") |> st_make_valid()

hics <- unique(boscos$COD_HIC)
hics <- hics[-2]

hic <- "9540"


n_points <- 50
dist_primary <- 200
dist_fallback <- 100

for(i in seq_along(hics)){

    hic <- hics[i]
    print(paste0("Processing HIC = ", hic))

    pol_flt <- filter_polygons(boscos, field = "COD_HIC", value = hic, 
                                buff_dist = "Radi_buffe") |> 
                    st_make_valid()

    A <- sum(pol_flt$Shape_Area)

    if (n_points * pi * dist_primary^2 < 0.6 * A) {
    radi = 200
    } else if (n_points * pi * dist_fallback^2 < 0.6 * A) {
    radi = 100
    } else {
   {
    msg <- paste0(System.time(), " - No es poden posar 50 punts en l'HIC ", hic, "\n")
    cat(msg, file = "results/01_Punts_aleat_rSSI/debug_rSSI.txt", append = TRUE)
    print(msg)
    next
}
    }

    pol_flt <- pol_flt[!st_is_empty(pol_flt), ]

    # Converteixo a win
    pol_union <- st_union(pol_flt)  |> 
                        st_make_valid()  # merges all polygons
    
    pol_win <- as.owin(pol_union)
    
    pts_ppp <- rSSI(
        r = min_dist,
        n = n_points,
        win = pol_win
    )

    pts_sf <- st_as_sf(pts_ppp)
    # Filtre dels punts perquè manté el poligon del qual ha fet el ranodm
    pts_sf <- pts_sf[st_geometry_type(pts_sf) == "POINT", ]
    st_crs(pts_sf) <- st_crs(pol_flt)
    
    pts_pol <- st_intersection(x = pts_sf, y = pol_flt) |> 
                mutate(min_sp_dist = radi)

    saveRDS(pts_pol, paste0("results/01_Punts_aleat_rSSI/rSSI_", hic, ".rds"))

    print(paste0("HIC = ", hic, " FINISHED"))

}
