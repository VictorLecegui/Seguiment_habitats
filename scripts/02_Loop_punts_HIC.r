
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

source("scripts/utils2.R")

##### MARK: Dades

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/Malla_Catalunya.rds")

radi <- read.csv2("data/HICs_radi.csv") |> 
                mutate(COD_HIC_tf = make.names(HIC))

# Comprovar que tenen la mateixa projecció

st_crs(points_cat)==st_crs(boscos)

# punts_mostrejats <- read_sf("data/Punts_mostrejats.shp")


##### MARK: Preparació pel Loop

hics <- unique(boscos$COD_HIC_tf)
hics <- hics[-2] # Polígons sense hics

regions <- unique(boscos$RegioHIC)

# HICS que s'han de fer el buffer
unique(radi$Radi)
hics_radis <- radi |> filter(Radi=="Si") |> pull(COD_HIC_tf)

j = 1
i = 1

#### Fitxer on emmagatzemar el procés

# dir.create("results/02_Loop_punts_HIC")
log_file <- "results/02_Loop_punts_HIC/Log_HIC.txt"


#### Directoris on desar fitxer

dir_pol_inf_30 <- "results/02_Loop_punts_HIC/N_pol_inf_30/"
#dir.create(dir_pol_inf_30)

dir_punts_mostreig <- "results/02_Loop_punts_HIC/Punts_mostreig/"
#dir.create(dir_punts_mostreig)

dir_malla <- "results/02_Loop_punts_HIC/Malla_per_HIC/"
#dir.create(dir_malla)

dir_poligons <- "results/02_Loop_punts_HIC/Poligons_shapes/"
#dir.create(dir_poligons)


#### MARK: Loop per regió i HIC

for(j in seq_along(regions)){

    regio <- regions[j]

for(i in seq_along(hics)){

    hic <- hics[i]

#### Pas 1: Filter polygon

    log_msg(level = "START", msg = paste("Començant a processar:", hic, regio))

    if(hic %in% hics_radis){
         pol <- filter_polygons(boscos, 
                        field = "COD_HIC_tf", 
                        value = hic,
                        buff_dist = "Radi_buffe") |> 
                filter(RegioHIC == regio) |> 
                st_as_sf()
    } else {
         pol <- filter_polygons(boscos, 
                        field = "COD_HIC_tf", 
                        value = hic) |> 
                filter(RegioHIC == regio) |> 
                st_as_sf()
    }
   

# Si no hi ha 30 poligons passar al següent
    if(nrow(pol) < 30){
        log_msg(level = "ERROR", msg = paste("Menys de 30 polígons per:", hic, regio))
        log_msg(level = "ERROR", msg = paste("Passant al següent HIC"))

        saveRDS(pol, paste0(dir_pol_inf_30, "Poligons_inf_30_", hic, "_", regio, ".rds"))

        next
    }


#### Part 2: Retallar els punts de la mall dins del polígons

    points_cat_crop <- st_crop(points_cat, st_bbox(pol))

    hic_points <- st_join(points_cat_crop, pol,
                        join = st_within, left = FALSE)

    n_punts <- nrow(hic_points)

#### Part 3: Situar els punts i fer GRTS

    if(n_punts < 30){
        log_msg(level = "ATENCIÓ", msg = paste("Menys de 30 punts d'intersecció amb la malla per:", hic, regio))

        results <- lapply(1:100, function(x) select_polygons_200m(pol, 200))
        best <- results[[which.max(sapply(results, nrow))]]

        if (nrow(best) == 0) {
           log_msg(level = "ERROR", msg = paste("Cap polígon seleccionat a més de 200m:", hic, regio))
        
        next
            }

        if(nrow(best)<30){
            log_msg(msg = paste("Menys de 30 polígons separats 200m entre ells, utilitzant el punt dins del polígon:", hic, regio))
            points <- st_point_on_surface(best)
        } else{
            log_msg(msg = paste("Més de 30 polígons amb distància mínima 200m, aplicant GRTS per:", hic, regio))
            points <- st_point_on_surface(best)
            points <- grts(points, n_base = 30, mindis = 200)
        }
    } else {
        log_msg(msg = paste("Més de 30 punts a la malla, aplicant GRTS:", hic, regio))

        points <- grts(hic_points, n_base = 30, mindis = 200)
    }

#### Part 4: Desar els punts, poligons i malla per cada HIC i regió

saveRDS(points, paste0(dir_punts_mostreig, "Punts_", hic, "_", regio, ".rds"))
saveRDS(hic_points, paste0(dir_malla, "Malla_", hic, "_", regio, ".rds"))
write_sf(pol, paste0(dir_poligons, "Pol_", hic, "_", regio, ".shp"))

log_msg(level = "END", "Acabat:", hic, regio)

}}
