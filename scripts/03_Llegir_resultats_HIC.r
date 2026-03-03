#### Script per llegir els resultats del loop per HIC


library(tidyverse)
library(sf)
library(units)

# Folder path
path_punts <- "results/02_Loop_punts_HIC/Punts_mostreig"

# List all RDS files (important fix)
punts_hic <- list.files(path_punts, pattern = "\\.rds$", full.names = TRUE)

file_path<-"results/02_Loop_punts_HIC/Punts_mostreig/Punts_X9340_MED.rds"
# Function to read and process each file
read_points_grts <- function(file_path){
  print(file_path)
  
  one_hic <- readRDS(file_path)

  punts_obj <- one_hic$points

  return(punts_obj)
}

read_metadata_grts <- function(file_path){
  print(file_path)
  
  one_hic <- readRDS(file_path)

  meta_obj <- one_hic$meta_points

  return(meta_obj)
}

# Apply to all files
punts_ls <- lapply(punts_hic, read_points_grts)
metadata_ls <- lapply(punts_hic, read_metadata_grts)


punts_mostreig <- bind_rows(punts_ls)
nrow(punts_mostreig) # Nombre de punts a mostrejar

colnames(punts_mostreig)
head(punts_mostreig)

metadata_punts <- bind_rows(metadata_ls)

# Nombre de punts aprofitats

# st_write(punts_mostreig, "results/03_Llegir_resultats_HIC/Punts_HIC.gpkg")

# st_write(punts_mostreig, 
#           "results/03_Llegir_resultats_HIC/Punts_mostreig_HIC.shp")

#### LLegir i resumir els punts de mosteig seleccionats. 


punts_mostreig <- read_sf("results/03_Llegir_resultats_HIC/Punts_HIC.gpkg")

sum(duplicated(st_geometry(punts_mostreig))) # cap punt duplicat


punts_mostreig |> group_by(COD_HIC) |> summarise(N=n()) |> arrange(N)
colnames(punts_mostreig_esri)

punts_mostreig |> 
    group_by(COD_HIC_tf, RegioHIC) |> 
    summarise(N = n()) |> 
    arrange(N)


metadata_punts |>
  filter(variables=="sp_balance_SES") |> 
  filter(values < 0)
# Seguent nivell: Grup de CORINEs

# Grups pels quals s'haurà de buscar punts
punts_mostreig |> 
    group_by(Codi_grup) |> 
    summarise(N = n()) |>
    filter(N < 30) |>  
    arrange(desc(N))

# Grups que ja tenen 30 punts
punts_mostreig |> 
    group_by(Codi_grup) |> 
    summarise(N = n()) |>
    filter(N >= 30) |>  
    arrange(desc(N))
length(unique(punts_mostreig$Codi_grup))
