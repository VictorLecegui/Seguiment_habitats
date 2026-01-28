#### Prcediment generació de punts

# Categoria: HIC
# Metode generació punts aleatòris: rSSI
# Mètode treure 30/50 punts: grst estratificat


#### Packages

library(tidyverse)
library(sf)
#install.packages(c("vegclust", "mapSpain"))
library(leaflet)
library(spsurvey)

source("scripts/utils.R")

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp")


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
    pol_win <- as.owin(pol_flt)

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
