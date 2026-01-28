#### Prcediment generació de punts

# Categoria: HIC
# Metode generació punts aleatòris: rSSI
# Mètode treure 30/50 punts: grst estratificat


#### Packages

#install.packages(c("spatstat.geom", "spatstat.random", "spsurvey"))

library(tidyverse)
library(sf)
library(leaflet)
library(spsurvey)
library(spatstat.geom)
library(spatstat.random)

library(lwgeom)
library(mapview)

#library(terra)
#library(raster)

source("scripts/utils.R")

boscos <- read_sf("data/MHTCv3_boscos/MHTCv3_boscos.shp") 

# Alguns polígons no estan tancats i això dona problemes

valid <- st_is_valid(boscos)

boscos_bad <- boscos[!valid, ]
boscos_ok  <- boscos[valid, ]

plot(st_geometry(boscos_ok), col = "grey90", border = "grey70")
plot(st_geometry(boscos_bad), col = "red", add = TRUE)

plot(st_geometry(boscos_bad[1, ]))


hics <- unique(boscos$COD_HIC)
hics <- hics[-2]

hic <- "9380"

run_rSSI_one_hic <- function(
  shp_data,
  value,
  field = "COD_HIC",
  buff_dist = "Radi_buffe",
  n_points = 50,
  dist_primary = 200,
  dist_fallback = 100,
  out_dir
) {

    log_file = paste0(out_dir, "/log_file_rSSI_", field, "_", value, ".txt")

  cat(paste0(Sys.time(), " - Processing " ,field, " = ", value, "\n"),
      file = log_file, append = TRUE)

  tryCatch({

    pol_flt <- filter_polygons(
      shp_data,
      field = field,
      value = value,
      buff_dist = buff_dist
    ) |>
      st_make_valid()

    pol_flt <- pol_flt[!st_is_empty(pol_flt), ]

    if (nrow(pol_flt) == 0) {
      stop("Filtered polygon is empty after st_is_empty()")
    }

    # Union + conversion to owin (danger zone)
    pol_union <- st_union(pol_flt) |>
      st_make_valid()

    pol_win <- as.owin(pol_union)

    # ---- rSSI with fallback ----
    pts_ppp <- tryCatch(
      {
        rSSI(
          r   = dist_primary,
          n   = n_points,
          win = pol_win
        )
      },
      error = function(e1) {

        cat(paste0(
          Sys.time(),
          " - rSSI failed with dist_primary (",
          dist_primary,
          ") for ", 
          field,
          " = ",
          value,
          ": ",
          conditionMessage(e1),
          "\n"
        ),
        file = log_file, append = TRUE)

        rSSI(
          r   = dist_fallback,
          n   = n_points,
          win = pol_win
        )
      }
    )

    pts_sf <- st_as_sf(pts_ppp)
    pts_sf <- pts_sf[st_geometry_type(pts_sf) == "POINT", ]
    st_crs(pts_sf) <- st_crs(pol_flt)

    pts_pol <- st_intersection(pts_sf, pol_flt) |>
      mutate(
        min_sp_dist = ifelse(
          length(pts_ppp) > 0 && min(nndist(pts_ppp)) >= dist_primary,
          dist_primary,
          dist_fallback
        )
      )

    saveRDS(
      pts_pol,
      file.path(out_dir, paste0("rSSI_", field, "_", value, ".rds"))
    )

    cat(paste0(Sys.time(), " - ",field," = ", value, " FINISHED\n"),
        file = log_file, append = TRUE)

    return(TRUE)

  }, error = function(e) {

    cat(paste0(
      Sys.time(),
      " - ERROR in ", field, " ",
      value,
      ": ",
      conditionMessage(e),
      "\n"
    ),
    file = log_file, append = TRUE)

    return(FALSE)
  })
}

# 1. Generació dels punts aleatoris

out_dir_hic <- "results/01_Punts_aleat_rSSi"

run_rSSI_one_hic(shp_data=boscos_ok, hic = "9150", field = "COD_HIC", out_dir=out_dir_hic )

