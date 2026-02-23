library(tidyverse)
library(sf)
library(units)

# Folder path
path_punts <- "results/06_Loop_CORINE_LLV/Punts_mostreig"

# List all RDS files (important fix)
punts_corine <- list.files(path_punts, pattern = "\\.rds$", full.names = TRUE)

# file_path<-"results/04_Loop_punts_GrupCORINE/Punts_mostreig/Punts_GAg.rds"


# Function to read and process each file
read_points_grts_LV <- function(file_path){
    print(file_path)
  one_corine <- readRDS(file_path)


# SI els punts ja estaven mostejats per aquest grup
if(one_corine$method == "Punts aprofitats HIC + Grup CORINE"){

    punts <- one_corine$points  |> 
                mutate(Method_LV = one_corine$method)
    
    return(punts)

} else {

  punts_obj <- one_corine$points

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
    mutate(Method_LV = one_corine$method)

  # ---- Minimum distance check ----
  min_dist_matrix <- st_distance(punts)

  diag(min_dist_matrix) <- set_units(Inf, "m")

  min_dist_value <- min(min_dist_matrix)

  punts <- punts |> 
    mutate(min_dist_200m_corine = ifelse(min_dist_value < set_units(200, "m"),
                                  "Inf_200m",
                                  "Sup_200m"))

  return(punts)

}}

punts_ls <- lapply(punts_corine, read_points_grts_LV)

punts_LV <- bind_rows(punts_ls)
nrow(punts_LV)

punts_LV |> 
    filter(Llista_ver=="LV") |> 
    group_by(COD_HIC) |> 
    summarise(N = n()) |> 
    arrange(N)



#st_write(punts_LV, "results/07_Llegir_punts_LlistaVermella/Punts_LV.gpkg")

sum(duplicated(st_geometry(punts_LV))) # Encara surten duplicats

# Elimino punts duplicats
punts_LV2 <- punts_LV[!duplicated(st_geometry(punts_LV)),]
nrow(punts_LV2)

punts_LV[duplicated(st_geometry(punts_LV)),] |> 
  distinct(COD_CORINE)

punts_LV2 |> group_by(COD_CORINE) |> 
    summarise(N = n()) |> 
    arrange(N, COD_CORINE)


#### Afegir punts de si els punts son exclusius de LV

punts_hic_grup <- st_read("results/05_Llegir_punts_Grup/Punts_HIC_GrupCORINE.gpkg")

punts_hic_grup <- punts_hic_grup |> mutate(Sel_LV = ifelse(Llista_ver=="LV", 
                            1,
                            0)) |> 
                            mutate(Sel_LV = ifelse(is.na(Llista_ver), 0, Sel_LV))


punts_hic_grup |> filter(Sel_LV == 1) |> nrow() 


close_to_seleccionats <- st_is_within_distance(
                             punts_LV2,
                             punts_hic_grup,
                            dist = set_units(1,m))
        

punts_exclusius_LV <- punts_LV[lengths(close_to_seleccionats) == 0, ]

punts_exclusius_LV <- punts_exclusius_LV |> 
    mutate(Sel_LV = 1, Sel_HIC = 0, Sel_GrupCORINE = 0)


punts_hic_grup <- st_set_geometry(punts_hic_grup, "geometry")
punts_exclusius_LV <- st_set_geometry(punts_exclusius_LV, "geometry")

###### PUNTS FINALS #####

punts_finals <- bind_rows(punts_hic_grup, punts_exclusius_LV)

st_write(punts_finals, "results/07_Llegir_punts_LlistaVermella/Punts_finals.gpkg")


