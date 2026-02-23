
### Loop per fer trobar els punts de GRTS a Catalunya pels HIC. 


##### MARK: Packages i funcions

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

punts_mostrejats <- read_sf("data/Parcelles_fetes_20260213/Parcelles_fetes_20260213.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/01_Generar_malla_Catalunya/Malla_Catalunya.rds")

radi <- read.csv2("data/HICs_radi.csv") |> 
                mutate(COD_HIC_tf = make.names(HIC))

# Comprovar que tenen la mateixa projecció

st_crs(points_cat) == st_crs(boscos)
st_crs(punts_mostrejats) == st_crs(boscos)

#### Eliminar punts duplicats en els ja mostrejats: per l'HIC 9120 hi ha 2 punts duplicats

dup_all <- duplicated(st_geometry(punts_mostrejats)) |
           duplicated(st_geometry(punts_mostrejats), fromLast = TRUE)

punts_mostrejats[dup_all, ] |> View() # Simplement hi ha un dia de diferència, ja es seleccionarà quin es el bo. 

dup_pts <- duplicated(st_geometry(punts_mostrejats))

punts_mostrejats <- punts_mostrejats[!dup_pts,]

##### MARK: Preparació pel Loop

hics <- unique(boscos$COD_HIC_tf)
hics <- hics[-2] # Polígons sense hics


hics_most <- unique(punts_mostrejats$COD_HIC_tf)
setdiff(hics_most, hics)

regions <- unique(boscos$RegioHIC)

# HICS que s'han de fer el buffer
unique(radi$Radi)
hics_radis <- radi |> 
                filter(Radi == "Si") |> 
                pull(COD_HIC_tf)



#### Generem un fitxer per enregistrar el procediment intern del Loop. 
log_file <- "results/02_Loop_punts_HIC/HIC_9120_dupl.txt"



#### Directoris on desar els fitxers generats
# dir.create("results/02_Loop_punts_HIC")


dir_punts_mostreig <- "results/02_Loop_punts_HIC/Punts_mostreig/"
#dir.create(dir_punts_mostreig)

dir_malla <- "results/02_Loop_punts_HIC/Malla_per_HIC/"
#dir.create(dir_malla)

dir_poligons <- "results/02_Loop_punts_HIC/Poligons_shapes/"
#dir.create(dir_poligons)



#### MARK: Loop per regió i HIC


regio <- regions[1]
hic <- hics[3]

for(j in seq_along(regions)){

    regio <- regions[j]

for(i in seq_along(hics)){

    hic <- hics[i]

#### Pas 1: Filter polygon

    log_msg(level = "START", msg = paste("Començant a processar:", hic, regio))

# Si l'HIC ha de tenir buffer o no 

# SI HIC té buffer
    if(hic %in% hics_radis){
         pol <- filter_polygons(boscos, 
                        field = "COD_HIC_tf", 
                        value = hic,
                        buff_dist = "Radi_buffe") |> 
                filter(RegioHIC == regio) |> 
                st_as_sf()
# NO HIC no se li ha d'aplicar buffer.                 
    } else {
         pol <- filter_polygons(boscos, 
                        field = "COD_HIC_tf", 
                        value = hic) |> 
                filter(RegioHIC == regio) |> 
                st_as_sf()
    }

# Si no hi ha polígons per aquest HIC o regió passar al següent. 
   if(nrow(pol)==0){
        log_msg(level = "ATENCIÓ!", msg = paste("No hi ha polígons de:", hic, "a", regio))
            next
   }

#### Extreure punts mostrejats

punts_hic_regio <- punts_mostrejats |> 
                        filter(COD_HIC_tf == hic & RegioHIC == regio)

# Eliminar dimensió z de punts
if(!is.null(punts_hic_regio) && nrow(punts_hic_regio)>0){
    punts_hic_regio <- st_cast(punts_hic_regio, "POINT")
    punts_hic_regio <- st_zm(punts_hic_regio)
}

#### Generar els punts (funció a utils.r). Segons si ja hi ha punts o no. 

# Posem l'objecte punts_hic_regio, si es null internament la funció ja l'ignora

resultat_punts <- generate_points_hic(pol, 
                                        points_cat, 
                                        hic, 
                                        regio, 
                                        legacy_points = punts_hic_regio,
                                        n_target = 30, 
                                        min_dist = 200, 
                                        n_iter = 100)



if (is.null(resultat_punts)) {
    log_msg(level="ERROR",
            msg=paste("No s'han pogut generar punts per:", hic, regio))
    next
}



#### Part 4: Desar els punts, poligons i malla per cada HIC i regió

saveRDS(list(punts = resultat_punts$points, 
                method = resultat_punts$method), 
                paste0(dir_punts_mostreig, "Punts_", hic, "_", regio, ".rds"))

if(nrow(resultat_punts$hic_points)>1){
saveRDS(resultat_punts$hic_points, paste0(dir_malla, "Malla_", hic, "_", regio, ".rds"))
}

if(!is.null(resultat_punts$points_centre)){
    saveRDS(resultat_punts$points_centre, paste0(dir_malla, "Punts_centre_", hic, "_", regio, ".rds"))
}

# Escric un shape amb el poligon filtrat per HIC i Regió
write_sf(pol, paste0(dir_poligons, "Pol_", hic, "_", regio, ".shp"))

log_msg(level = "END", msg = paste("Acabat:", hic, regio))

}}


