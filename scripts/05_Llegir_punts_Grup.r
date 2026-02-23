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

    punts <- one_grup$points  |> 
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



st_write(punts_grups, "results/05_Llegir_punts_Grup/Punts_Grup.gpkg")


#### Afegir columna de si els punts son HIC + Grup o Grup

punts_hic <- st_read("results/03_Llegir_resultats_HIC/Punts_HIC.gpkg")

punts_grups <- st_read("results/05_Llegir_punts_Grup/Punts_Grup.gpkg")

## Comprovar si hi ha punts duplicats: 

sum(duplicated(st_geometry(punts_grups))) # No hi ha punts duplicats




close_to_legacy <- st_is_within_distance(
                             punts_grups,
                             punts_hic,
                            dist = set_units(10,m))
        

punts_exclusius_grup <- punts_grups[lengths(close_to_legacy) == 0, ]

punts_exclusius_grup <- punts_exclusius_grup |> 
  mutate(Sel_GrupCORINE = 1,
          Sel_HIC = 0)

punts_hic <- punts_hic |> 
  mutate(Sel_GrupCORINE = 1,
          Sel_HIC = 1)

punts_totals <- bind_rows(punts_hic, punts_exclusius_grup)
nrow(punts_totals)

unique(punts_totals$Sel_GrupCORINE)
unique(punts_totals$Sel_HIC)

punts_totals |> filter(Sel_HIC == 1 & Sel_GrupCORINE ==1) |> nrow() # 1068 = nrow(Punts_hic)
punts_totals |> filter(Sel_HIC == 1) |> nrow()
punts_totals |> filter(Sel_GrupCORINE == 1) |> nrow()
punts_totals |> filter(Sel_HIC == 0) |> nrow() # Grups seleccionats només per Grup



nrow(punts_exclusius_grup) + nrow(punts_hic) == nrow(punts_totals)

### esciure punts per separat

#dir.create("results/05_Llegir_punts_Grup/Capes_separades")
st_write(punts_hic, "results/05_Llegir_punts_Grup/Capes_separades/Punts_exclusius_HIC.gpkg")
st_write(punts_exclusius_grup, "results/05_Llegir_punts_Grup/Capes_separades/Punts_exclusius_Grup.gpkg")

sum(duplicated(st_geometry(punts_totals)))

st_write(punts_totals, "results/05_Llegir_punts_Grup/Punts_HIC_GrupCORINE.gpkg")



#### Ara seguir i fer el mateix pels punts de la llista vermella. 


