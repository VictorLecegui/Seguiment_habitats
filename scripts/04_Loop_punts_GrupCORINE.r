
### Loop per fer trobar els punts de GRTS a Catalunya pels Grups de CORINE 
    # Considerant els punts ja mostrejats pels CORINE 


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

#### MARK: Dades

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/01_Generar_malla_Catalunya/Malla_Catalunya.rds")

## Punts ja mostrejats + Punts seleccionats per HIC

punts_mostrejats <- read_sf("data/Parcelles_fetes_20260213/Parcelles_fetes_20260213.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

    # Filtrar punt duplicat punts_mostrejats

    dup_pts <- duplicated(st_geometry(punts_mostrejats))
    punts_mostrejats <- punts_mostrejats[!dup_pts,]

punts_hic <- read_sf("results/03_Llegir_resultats_HIC/Punts_HIC.gpkg") |> 
                mutate(Selected_for = "HIC")

punts_mostrejats_no_HIC <- punts_mostrejats |> filter(COD_HIC=="-")

# Comprovació que tenim tots els punts

punts_hic |> filter(Point_origin=="Legacy")  |> nrow()
nrow(punts_mostrejats)

punts_mostrejats_no_HIC |> nrow()
punts_hic |> nrow()

# Assegurar-nos que utilitzen la mateixa geometry abans d'ajuntar-los
punts_mostrejats_no_HIC <- st_set_geometry(punts_mostrejats_no_HIC, "geometry")
punts_hic <- st_set_geometry(punts_hic, "geometry")

punts_fets <- bind_rows(punts_mostrejats_no_HIC, punts_hic)

nrow(punts_fets) == nrow(punts_mostrejats_no_HIC) + nrow(punts_hic)


# Eliminar dimensió z de punts
punts_fets <- st_cast(punts_fets, "POINT")
punts_fets <- st_zm(punts_fets)


# Comprovar que tenen la mateixa projecció

st_crs(points_cat) == st_crs(boscos)
st_crs(punts_fets) == st_crs(boscos)

##### MARK: Preparació pel Loop
colnames(boscos)
grups <- unique(boscos$Codi_grup)
make.names(grups)==grups # Els noms estan bé per escriure fitxers etc. 

colnames(punts_fets)

#### Generem un fitxer per enregistrar el procediment intern del Loop. 
log_file <- "results/04_Loop_punts_GrupCORINE/Log_Grups_20260223.txt"


#### Directoris on desar els fitxers generats
# dir.create("results/02_Loop_punts_HIC")


dir_punts_mostreig <- "results/04_Loop_punts_GrupCORINE/Punts_mostreig/"
#dir.create(dir_punts_mostreig)

dir_malla <- "results/04_Loop_punts_GrupCORINE/Malla_per_HIC/"
#dir.create(dir_malla)

dir_poligons <- "results/04_Loop_punts_GrupCORINE/Poligons_shapes/"
#dir.create(dir_poligons)



for(i in seq_along(grups)){

    grup <- grups[i]

    log_msg(level = "START", msg = paste("Començant a processar:",grup))

#### Extreure punts mostrejats i comprovar si ja n'hi ha 30

punts_grup_fets <- punts_fets |> 
                        filter(Codi_grup == grup)

#### COMPROVACIÓ SI JA TENIM ELS PUNTS

if(nrow(punts_grup_fets)>=30){
   points <- punts_grup_fets 
   method <- "Punts aprofitats HIC"

    saveRDS(list(points = points, 
                method = method), 
                paste0(dir_punts_mostreig, "Punts_", grup, ".rds"))

    log_msg(level = "INFO", msg = paste("30 Punts ja mostrejats, aprofitats dels HIC, pel grup:", grup))

    log_msg(level = "END", msg = paste("Acabat grup:", grup))
    print(paste(i, "/", length(grups)))


    next
}


# SI encara no tenim els 30 punts Filtre Grup i aplicar buffer

    pol <- filter_polygons(boscos, 
                        field = "Codi_grup", 
                        value = grup,
                        buff_dist = "Radi_buffe") |> 
                st_as_sf()

# Si no hi ha polígons per aquest HIC o regió passar al següent. 
   if(nrow(pol)==0){
        log_msg(level = "ATENCIÓ!", msg = paste("No hi ha polígons de:", grup))
            next
   }

# Si hi ha poligons buits (per haver fet el buffer) eliminar-los

pol <- pol[!st_is_empty(pol),]

#### Generar els punts (funció a utils.r). Segons si ja hi ha punts o no. 

# Posem l'objecte punts_hic_regio, si es null internament la funció ja l'ignora

resultat_punts <- generate_points_grups(pol, 
                                        points_cat, 
                                        grup, 
                                        legacy_points = punts_grup_fets,
                                        n_target = 30, 
                                        min_dist = 200, 
                                        n_iter = 100)


if (is.null(resultat_punts)) {
    log_msg(level="ERROR",
            msg=paste("No s'han pogut generar punts per:", grup))
    next
}

#### Part 4: Desar els punts, poligons i malla per cada HIC i regió

saveRDS(resultat_punts, 
         paste0(dir_punts_mostreig, "Punts_", grup, ".rds"))



if(nrow(resultat_punts$grup_points)>1){
saveRDS(resultat_punts$grup_points, paste0(dir_malla, "Malla_", grup, ".rds"))
}

if(!is.null(resultat_punts$points_centre)){
    saveRDS(resultat_punts$points_centre, paste0(dir_malla, "Punts_centre_", grup, ".rds"))
}

# Escric un shape amb el poligon filtrat per HIC i Regió
write_sf(pol, paste0(dir_poligons, "Pol_", grup, ".shp"))

log_msg(level = "END", msg = paste("Acabat:", grup))

print(paste(i, "/", length(grups)))

}


