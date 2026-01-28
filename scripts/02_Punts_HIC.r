### Script per combinar els resultats generats a 01_Base_Loop_punts.r

# Objectiu 1: Extreure els punts dels objectes rds a les carpetes amb 30 punts
# Objectiu 2: Donar valors als punts a partir del polígon boscos amb un intersect

library(tidyverse)
library(sf)
library(leaflet)

# Part 1: Obtenir tots els punts junts



files <- list.files("results/01_Loop_punts/Punts_seleccionats")

files_30 <- lapply(files, function(x){
   a <- readRDS(paste0(getwd(), "/results/01_Loop_punts/Punts_seleccionats/", x))
   if("sp_design" %in% class(a)){
    return(a$sites_base)
   } 
    return(a)
})

all_points_hic <- bind_rows(files_30)

nrow(all_points_hic)
st_crs(all_points_hic)


# Part 2: Extreure valors dels polígons a partir dels punts

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
               st_make_valid()

st_crs(all_points_hic)==st_crs(boscos)

plot(all_points_hic$geometry)

punts_boscos <- st_join(x = all_points_hic, y=boscos,
                         join = st_within ,left = FALSE)  |> 
                        distinct(geometry, .keep_all = TRUE)

unique(punts_boscos$COD_HIC)



punts_summary <- punts_boscos  |> 
                   group_by(COD_HIC, RegioHIC) |> 
                   summarise(N = n()) |> 
                   as.data.frame() |> 
                   select(-geometry)

#### Write the results

st_write(punts_boscos, "results/02_Punts_HIC/Punts_HIC.shp")
write.csv(punts_summary, "results/02_Punts_HIC/Punts_HIC_resum.csv", row.names = FALSE)


