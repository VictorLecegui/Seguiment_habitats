
### Loop per fer trobar els punts de GRTS a Catalunya pels HIC. 


##### MARK: Packages i funcions

library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)
library(pbapply)

# Funcions personalitzades

source("scripts/utils2.R")

##### MARK: Dades

# Mapa d'habitats
boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC_20260306/MHTCv3_boscos_RegionsHIC_20260306.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

  # Filtrem els polígons amb àrea < 200m2 (superfície de l'inventari)

  boscos_flt <- boscos[st_area(boscos) > set_units(250, m2),]
  boscos_flt <- st_make_valid(boscos_flt)
  min(st_area(boscos_flt))
  min(st_area(boscos))

# Punts ja mostejats
punts_mostrejats <- read_sf("data/Parcelles_fetes_20260306/Parcelles_fetes_20260306.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))
punts_mostrejats |>
  filter(COD_HIC!="-") |> 
  group_by(COD_HIC, RegioHIC) |> 
  summarise(N=n())
 
 # Filtrar punt duplicat punts_mostrejats

    dup_pts <- duplicated(st_geometry(punts_mostrejats))
    punts_mostrejats <- punts_mostrejats[!dup_pts,]
 # Eliminar dimensió z

    punts_mostrejats <- st_cast(punts_mostrejats, "POINT")
    punts_mostrejats <- st_zm(punts_mostrejats)


punts_mostrejats <- st_set_geometry(punts_mostrejats, "geometry")

# Malla de punts
points_cat <- readRDS("results/01_Generar_malla_Catalunya/Malla_Catalunya.rds")
points_cat <- st_set_geometry(points_cat, "geometry")

# Comprovar que tenen la mateixa projecció

st_crs(points_cat) == st_crs(boscos_flt)
st_crs(punts_mostrejats) == st_crs(boscos_flt)


##### MARK: Preparació pel Loop

hics <- unique(boscos_flt$COD_HIC_tf)
hics <- hics[-2] # Polígons sense hics


hics_most <- unique(punts_mostrejats$COD_HIC_tf)
setdiff(hics_most, hics)

regions <- unique(boscos_flt$RegioHIC)

# HICS que s'han de fer el buffer

#### Generem un fitxer per enregistrar el procediment intern del Loop. 
log_file <- "results/02_Loop_punts_HIC/Log_poligons_HIC_20260309.txt"


#### Directoris on desar els fitxers generats
# dir.create("results/02_Loop_punts_HIC")


dir_punts_mostreig <- "results/02_Loop_punts_HIC/Punts_mostreig/"
#dir.create(dir_punts_mostreig)

dir_malla <- "results/02_Loop_punts_HIC/Malla_per_HIC/"
#dir.create(dir_malla)

dir_poligons <- "results/02_Loop_punts_HIC/Poligons_shapes/"
#dir.create(dir_poligons)



#### MARK: Loop per regió i HIC



#### Loop 1: Buscar quines combinacions de HIC i regió tenen polígons

for(j in seq_along(regions)){

    regio <- regions[j]

for(i in seq_along(hics)){

    hic <- hics[i]

#### Pas 1: Filter polygon

    log_msg(level = "START", msg = paste("Començant a processar:", hic, regio))

# Filtrem la capa d'habitats per l'HIC i regió que volem: 

    pol <- filter_polygons(boscos_flt, 
                    field = "COD_HIC_tf", 
                    value = hic) |> 
            filter(RegioHIC == regio) |> 
            st_as_sf()

# Filtre dels polígons de menys de 200m2 (inventari no hi entra)

# Si no hi ha polígons per aquest HIC o regió passar al següent. 
   if(nrow(pol)==0){
        log_msg(level = "ATENCIÓ!", msg = paste("No hi ha polígons de:", hic, "a", regio))
        log_msg(level = "END", msg = paste("Acabat:", hic, "a", regio))


        next    
   }


st_write(pol, paste0(dir_poligons, "Pol_", hic, "_", regio, ".gpkg"))
log_msg(level = "END", msg = paste("Poligons guardats per: ", hic, "a", regio))

}}






### Loop 2: Comprovar el nombre de punts 

## Llegir els polígons generats en el loop anterior (HICS i Regions amb poligons)

pol_files <- list.files(dir_poligons, pattern = "\\.gpkg$", full.names = FALSE)
pol_clean <- tools::file_path_sans_ext(pol_files)

split_mat <- do.call(rbind, strsplit(pol_clean, "_"))

files_df <- data.frame(
  prefix = split_mat[,1],
  HIC    = split_mat[,2],
  Region = split_mat[,3],
  row.names = NULL
)

log_file <- "results/02_Loop_punts_HIC/Test_random.txt"


k=3

for(k in 1:nrow(files_df)) {

  hic   <- files_df$HIC[k]
  regio <- files_df$Region[k]

log_msg(level = "START", msg = paste("Compençant a processar punts per: ", hic, "a", regio, 
                                        k, "/", nrow(files_df)))

#### Extreure punts mostrejats

punts_hic_regio <- punts_mostrejats |> 
                        filter(COD_HIC_tf == hic & RegioHIC == regio)


pol_flt <- st_read(paste0(dir_poligons, "/", pol_files[k]))

#### Generar els punts (funció a utils.r). Segons si ja hi ha punts o no. 

# Posem l'objecte punts_hic_regio, si es null internament la funció ja l'ignora

resultat_punts <- generate_points_hic(pol_flt, 
                                        points_cat, 
                                        hic, 
                                        regio, 
                                        legacy_points = punts_hic_regio,
                                        n_target = 30, 
                                        min_dist = 199,
                                        n_reserva = 6, 
                                        n_iter = 999)

resultat_punts

#### Part 4: Desar els punts, poligons i malla per cada HIC i regió

saveRDS(resultat_punts, 
                paste0(dir_punts_mostreig, "Punts_", hic, "_", regio, ".rds"))

if(nrow(resultat_punts$hic_points)>1){
saveRDS(resultat_punts$hic_points, paste0(dir_malla, "Malla_", hic, "_", regio, ".rds"))
}


log_msg(level = "END", msg = paste("Acabat:", hic, regio))


}

log_msg(level = "END LOOP", msg = paste("Acabat tot el loop: Punts generats per HIC"))


