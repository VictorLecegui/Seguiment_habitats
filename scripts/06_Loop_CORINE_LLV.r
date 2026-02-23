
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

    # Eliminar dimensió z

    punts_mostrejats <- st_cast(punts_mostrejats, "POINT")
    punts_mostrejats <- st_zm(punts_mostrejats)

punts_mostrejats <- st_set_geometry(punts_mostrejats, "geometry")

# dels punts mostrejats quins son Llista Vermella
punts_mostrejats_LV <- punts_mostrejats |> filter(Llista_ver=="LV")
unique(punts_mostrejats_LV$Codi_grup)


# Punts ja seleccionats per mostejar per HIC o Grup de CORINE
punts_hic_grup <- read_sf("results/05_Llegir_punts_Grup/Punts_HIC_GrupCORINE.gpkg") 
punts_hic_grup <- st_set_geometry(punts_hic_grup, "geometry")

sum(duplicated(st_geometry(punts_hic_grup)))


# Comprovació que els punts ja mostrejats, estan inclosos als punts

st_crs(punts_mostrejats_LV)==st_crs(punts_hic_grup)

punts_comb <- bind_rows(punts_hic_grup, punts_mostrejats_LV) 

punts_comb[duplicated(st_geometry(punts_comb)),] # 15 punts = nrow(punts_mostrejats_LV)


# Treballem directament amb els ja seleccionats

# Comprovar que tenen la mateixa projecció

st_crs(points_cat) == st_crs(boscos)
st_crs(punts_hic_grup) == st_crs(boscos)


##### MARK: Preparació pel Loop
colnames(boscos)


boscos <- boscos |> mutate(COD_CORINE_tf = make.names(COD_CORINE))

# Afegir els CORINE amb els nou noms als punts fets

punts_hic_grup <- punts_hic_grup |> 
    mutate(COD_CORINE_tf = make.names(COD_CORINE))

corines_llv <- punts_hic_grup |> 
    filter(Llista_ver=="LV") |> 
    group_by(COD_CORINE_tf) |> 
    summarise(N = n()) |>  
    filter(N<30) |> 
    pull(COD_CORINE_tf)

corines_llv # CORINES amb < 30 punts ja mostejats i inclosos en la llista vermells

#### Generem un fitxer per enregistrar el procediment intern del Loop. 
log_file <- "results/06_Loop_CORINE_LLV/Log_LlistaVermella_20260223.txt"



#### Directoris on desar els fitxers generats
# dir.create("results/02_Loop_punts_HIC")


dir_punts_mostreig <- "results/06_Loop_CORINE_LLV/Punts_mostreig/"
#dir.create(dir_punts_mostreig)

dir_malla <- "results/06_Loop_CORINE_LLV/Malla_per_HIC/"
#dir.create(dir_malla)

dir_poligons <- "results/06_Loop_CORINE_LLV/Poligons_shapes/"
#dir.create(dir_poligons)


corines_llv

corine <- "X44.461." # COIRNE que genra punts duplicats
rm(corine)
for(i in seq_along(corines_llv)){

    corine <- corines_llv[i]

    log_msg(level = "START", msg = paste("Començant a processar:", corine))

#### Extreure punts mostrejats i comprovar si ja n'hi ha 30

punts_lv_fets <- punts_hic_grup |> 
                        filter(COD_CORINE_tf == corine)

#### COMPROVACIÓ SI JA TENIM ELS PUNTS

if(nrow(punts_lv_fets)>=30){
   points <- punts_lv_fets 
   method <- "Punts aprofitats HIC + Grup CORINE"

    saveRDS(list(points = points, 
                method = method), 
                paste0(dir_punts_mostreig, "Punts_", corine, ".rds"))

    log_msg(level = "INFO", msg = paste("30 Punts ja mostrejats, aprofitats dels HIC i GRUP per:", corine))

    log_msg(level = "END", msg = paste("Acabat CORINE:", corine))
    print(paste(i, "/", length(corines_llv)))

    next
}


# SI encara no tenim els 30 punts Filtre Grup i aplicar buffer

    pol <- filter_polygons(boscos, 
                        field = "COD_CORINE_tf", 
                        value = corine) |> 
                st_as_sf()

# Si no hi ha polígons per aquest HIC o regió passar al següent. 
   if(nrow(pol)==0){
        log_msg(level = "ATENCIÓ!", msg = paste("No hi ha polígons de:", corine))
            next
   }

# Si hi ha poligons buits (per haver fet el buffer) eliminar-los

pol <- pol[!st_is_empty(pol),]

#### Generar els punts (funció a utils.r). Segons si ja hi ha punts o no. 

# Posem l'objecte punts_hic_regio, si es null internament la funció ja l'ignora

resultat_punts <- generate_points_LV(pol, 
                                        points_cat, 
                                        corine, 
                                        legacy_points = punts_lv_fets,
                                        n_target = 30, 
                                        min_dist = 200, 
                                        n_iter = 100)


if (is.null(resultat_punts)) {
    log_msg(level="ERROR",
            msg=paste("No s'han pogut generar punts per:", corine))
    next
}

#### Part 4: Desar els punts, poligons i malla per cada HIC i regió

saveRDS(resultat_punts, 
         paste0(dir_punts_mostreig, "Punts_", corine, ".rds"))



if(nrow(resultat_punts$corine_points)>1){
saveRDS(resultat_punts$corine_points, paste0(dir_malla, "Malla_", corine, ".rds"))
}

if(!is.null(resultat_punts$points_centre)){
    saveRDS(resultat_punts$points_centre, paste0(dir_malla, "Punts_centre_", corine, ".rds"))
}


log_msg(level = "END", msg = paste("Acabat:", corine))

print(paste(i, "/", length(corines_llv)))

}

sort(corines_llv)
