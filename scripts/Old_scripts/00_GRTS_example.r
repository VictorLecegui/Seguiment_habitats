library(sf)
library(spsurvey)
library(ggplot2)
library(dplyr)

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

plot(punts_30, points_50, pch=19, key.width = lcm(3),
        main = "Punts seleccionats HIC 9540")