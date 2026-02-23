library(tidyverse)
library(sf)
library(units)

# Folder path
path_punts <- "results/04_Loop_punts_GrupCORINE/Punts_mostreig"

# List all RDS files (important fix)
punts_grup <- list.files(path_punts, pattern = "\\.rds$", full.names = TRUE)

file_path<-"results/04_Loop_punts_GrupCORINE/Punts_mostreig/Punts_GAg.rds"


# Function to read and process each file
read_points_grts_grup <- function(file_path){
    print(file_path)
  one_grup <- readRDS(file_path)


# SI els punts ja estaven mostejats per aquest grup
if(one_grup$method == "Punts aprofitats HIC"){

    punts <- one_grup$punts  |> 
                mutate(Method_grup = one_grup$method)
    
    return(punts)

} else {

  punts_obj <- one_grup$points

# Si els punts han sigut generats amb GRTS:
if("sp_design" %in% class(punts_obj)){

  # Combine GRTS and Legacy correctly
  if (!is.null(punts_obj$sites_legacy) && !is.null(punts_obj$sites_base)) {

    punts_grts <- punts_obj$sites_base |> 
      mutate(Point_origin = "GRTS")

    punts_legacy <- punts_obj$sites_legacy |> 
      mutate(Point_origin = "Legacy")

    punts <- bind_rows(punts_grts, punts_legacy)

  } else {
    if(nrow(punts_obj$sites_base) == 30){
        
         punts <- punts_obj$sites_base |> 
          mutate(Point_origin = "GRTS")
    } else{
        if(nrow(punts_obj$sites_legacy) == 30){
            punts <- punts_obj$sites_legacy  |> 
                mutate(Point_origin = "Legacy")
        }
    }

  }
} else {
  
  # SI els punts s'han generat amb el centroide del polígon etc. 
   punts <- punts_obj
}


  # Add generation method 
  punts <- punts |> 
    mutate(Method_grup = one_grup$method)

  # ---- Minimum distance check ----
  min_dist_matrix <- st_distance(punts)

  diag(min_dist_matrix) <- set_units(Inf, "m")

  min_dist_value <- min(min_dist_matrix)

  punts <- punts |> 
    mutate(min_dist_200m_grup = ifelse(min_dist_value < set_units(200, "m"),
                                  "Inf_200m",
                                  "Sup_200m"))

  return(punts)

}}

# Apply to all files
punts_ls <- lapply(punts_grup, read_points_grts_grup)


punts_grups <- bind_rows(punts_ls)
nrow(punts_grups)

str(punts_grups)

punts_grups |> 
        group_by(COD_HIC) |> 
        summarise(N = n()) |> 
        arrange(N)
punts_grups |> 
        group_by(Codi_grup) |> 
        summarise(N = n()) |> 
        arrange(N)

boscos |> filter(Codi_grup=="GJt") |> plot()
punts_grups |> filter(Codi_grup=="GJt") |> plot()



st_write(punts_grups, "results/05_Llegir_punts_Grup/Punts_HIC_Grup.gpkg")


#### Afegir columna de si els putns son HIC + Grup o Grup

punts_hic <- st_read("results/03_Llegir_resultats_HIC/punts_mostreig.gpkg")

punts_grups <- st_read("results/05_Llegir_punts_Grup/Punts_HIC_Grup.gpkg")

## Comprovar si hi ha punts duplicats: 

dup_geom <- duplicated(st_geometry(punts_hic))

sum(dup_geom) 

  punts_hic[dup_geom, ]
nrow(punts_grups)
# comprovo si en eliminar-lo s'ha perdut algun punt del diseny

punts_grups |> filter(COD_HIC == "9120" & RegioHIC == "MED") |> nrow()

close_to_legacy <- st_is_within_distance(
                             punts_grups,
                             punts_hic,
                            dist = set_units(10,m))
        

punts_exclusius_grup <- punts_grups[lengths(close_to_legacy) == 0, ]

punts_exclusius_grup <- punts_exclusius_grup |> 
  mutate(Categoria = "Grup_CORINE")

punts_hic <- punts_hic |> 
  mutate(Categoria = "HIC + Grup_CORINE")

punts_totals <- bind_rows(punts_hic, punts_exclusius_grup)
nrow(punts_totals)

nrow(punts_grups) - nrow(punts_hic)

nrow(punts_exclusius_grup) + nrow(punts_hic) == nrow(punts_grups)



punts_grups |> 
  filter(Llista_ver == "LV") |> 
  group_by(COD_CORINE) |> 
  summarise(N = n()) |> 
  arrange(N) 

