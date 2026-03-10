#### Script per llegir els resultats del loop per HIC


library(tidyverse)
library(sf)
library(units)

# Folder path
path_punts <- "results/02_Loop_punts_HIC/Punts_mostreig_parallel"

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

punts_mostreig |> 
  group_by(COD_HIC, RegioHIC) |> 
  summarise(N = n()) |> 
  arrange(N)

punts_mostreig |> 
  group_by(COD_HIC_tf, RegioHIC) |> 
  summarise(N = n()) |> 
  arrange(desc(N))


#### Arreglem les columnes mal fetes de la Ruta 4
punts_mostreig2 <- punts_mostreig |> 
    mutate(Origen_punts = coalesce(Point_origin, Points_origin)) |> 
    select(-c(Point_origin, Points_origin)) |> 
    rename(Point_origin = Origen_punts)

### Hi ha un HIC (91D0*) que s'ha fet per polígons i té >36 punts. 
### Aplicar GRTS per aquest HIC

hic_mol <- punts_mostreig2 |> filter(COD_HIC_tf == "X91D0." & RegioHIC == "ALP")
nrow(hic_mol)


punts_mostrejats <- read_sf("data/Parcelles_fetes_20260306/Parcelles_fetes_20260306.shp") 
unique(punts_mostrejats$COD_HIC) # No té punts mostejats

hic_points <- grts(hic_mol,
                       n_base = 30,
                       n_over = 6,
                       mindis = 199,
                       maxtry = 100,
                       DesignID = "X91D0._ALP"
        )

hic_points <- sp_rbind(hic_points)

  # Eliminar punts anteriors i substituir pels de GRTS

  punts_mostreig3 <- punts_mostreig2 |> filter(!(COD_HIC_tf == "X91D0." & RegioHIC == "ALP"))

  punts_mostreig4 <- bind_rows(punts_mostreig3, hic_points)

nrow(punts_mostreig4)

###### AFEGIR US DELS PUNTS ######
colnames(punts_mostreig4)
unique(punts_mostreig4$Llista_ver)

punts_mostreig5 <- punts_mostreig4 |> 
    mutate( Util_HIC = "SI", 
            Util_GrupCORINE = "SI", 
            Util_LLV = if_else(!is.na(Llista_ver) & Llista_ver == "LV", "SI", "NO"))

punts_mostreig5 |> group_by(COD_HIC_tf, RegioHIC) |> 
summarise(N = n()) |> 
arrange(N)


###### METADATA FILE #######

metadata_punts <- bind_rows(metadata_ls) |> 
select( variables, values, HIC, Regio)

metadata_punts

metadata_df <- metadata_punts |> 
filter(!(variables %in% c("HIC", "Regio"))) |>  
    pivot_wider(id_cols = c(HIC, Regio), 
                names_from = variables, 
                values_from = values)

metadata_df |> filter(Ruta == 4)

punts_mostreig

# Nombre de punts aprofitats

st_write(punts_mostreig5, "results/03_Llegir_resultats_HIC/Punts_HIC.gpkg")
write.csv(metadata_df, "results/03_Llegir_resultats_HIC/Punts_metadata.csv", row.names = FALSE)
# st_write(punts_mostreig, 
#           "results/03_Llegir_resultats_HIC/Punts_mostreig_HIC.shp")

#### LLegir i resumir els punts de mosteig seleccionats. 


punts_mostreig <- read_sf("results/03_Llegir_resultats_HIC/Punts_HIC.gpkg")

sum(duplicated(st_geometry(punts_mostreig))) # cap punt duplicat

metadata_punts |> 
    filter(variables == "Ruta" & values ==4)


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

unique(punts_mostreig$siteuse)
