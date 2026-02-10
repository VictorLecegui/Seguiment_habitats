
### Loop per fer trobar els punts de GRTS a Catalunya pels HIC. 


##### MARK: Packages i funcinos

library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)

# Funcions personalitzades

source("scripts/utils.R")

##### MARK: Dades

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/Malla_Catalunya.rds")

# Comprovar que tenen la mateixa projecció

st_crs(points_cat)==st_crs(boscos)

# punts_mostrejats <- read_sf("data/Punts_mostrejats.shp")


##### MARK: Preparació pel Loop

hics <- unique(boscos$COD_HIC_tf)
hics <- hics[-2] # Polígons sense hics

regions <- unique(boscos$RegioHIC)


j = 1
i = 1

#### Fitxer on emmagatzemar el procés

# dir.create("results/02_Loop_punts_HIC")
log_file <- "results/02_Loop_punts_HIC/Log_HIC.txt"


#### Directoris on desar fitxer

pol_inf_30 <- "results/02_Loop_punts_HIC/N_pol_inf_30/"






#### MARK: Loop per regió i HIC

for(j in seq_along(regions)){

    regio <- regions[j]

for(i in seq_along(hics))

    hic <- hics[i]

#### Pas 1: Filter polygon

    log_msg(msg = paste("Començant a processar:", hic, regio))


    pol <- filter_polygons(boscos, 
                        field = "COD_HIC_tf", 
                        value = hic,
                        buff_dist = "Radi_buffe") |> 
                filter(RegioHIC == regio) |> 
                st_as_sf()

# Si no hi ha 30 poligons passar al següent
    if(nrow(pol) < 30){
        log_msg(level = "ERROR", paste("Menys de 30 polígons per:", hic, regio))
        log_msg(level = "ERROR", paste("Passant al següent HIC"))

        saveRDS(pol, paste0(pol_inf_30, "Poligons_inf_30_", hic, "_", regio, ".rds"))

        next
    }


#### Part 2: Retallar els punts de la mall dins del polígons

    points_cat_crop <- st_crop(points_cat, st_bbox(pol))

    hic_points <- st_join(points_cat_crop, pol,
                        join = st_within, left = FALSE)

    n_punts <- nrow(hic_points)

    if(n_punts < 30){
        log_msg(level = "WARNING", paste("Menys de 30 punts per:", hic, regio))

        results <- lapply(1:100, function(x) select_polygons_200m(pol, 200))
        best <- results[[which.max(sapply(results, nrow))]]

        


    }





}
