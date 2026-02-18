#### Script per llegir els resultats del loop per HIC


library(tidyverse)
library(sf)
library(units)

# Folder path
path_punts <- "results/02_Loop_punts_HIC/Punts_mostreig"

# List all RDS files (important fix)
punts_hic <- list.files(path_punts, pattern = "\\.rds$", full.names = TRUE)

file_path<-"results/02_Loop_punts_HIC/Punts_mostreig/Punts_X3240_ALP.rds"
# Function to read and process each file
read_points_grts <- function(file_path){
    print(file_path)
  one_hic <- readRDS(file_path)

  punts_obj <- one_hic$punts

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
   punts <- punts_obj
}

# SI els punts s'han generat amb el centroide del polígon etc. 

  # Add generation method 
  punts <- punts |> 
    mutate(Method = one_hic$method)

  # ---- Minimum distance check ----
  min_dist_matrix <- st_distance(punts)

  diag(min_dist_matrix) <- set_units(Inf, "m")

  min_dist_value <- min(min_dist_matrix)

  punts <- punts |> 
    mutate(min_dist_200m = ifelse(min_dist_value < set_units(200, "m"),
                                  "Inf 200m",
                                  "Sup 200m"))

  return(punts)
}

# Apply to all files
punts_ls <- lapply(punts_hic, read_points_grts)

punts_mostreig <- bind_rows(punts_ls)
nrow(punts_mostreig) # Nombre de punts a mostrejar

# Nombre de punts aprofitats
punts_mostreig  |> filter(Point_origin=="Legacy") |> nrow()


st_write(punts_mostreig, 
          "results/03_Llegir_resultats_HIC/Punts_mostreig_HIC.shp")
