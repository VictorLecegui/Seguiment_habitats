#### Script per automatitizar el procés i fer-ho per tots els camp 


#### Packages

library(tidyverse)
library(sf)
#install.packages(c("vegclust", "mapSpain"))
library(vegclust)
library(mapSpain)
library(units)

##### Dades

boscos <- read_sf("data/MHTCv3_boscos/MHTCv3_boscos.shp")

#### Funcions

source("scripts/utils.R")

corines <- unique(boscos$COD_CORINE)
corines_sample <- corines[1:10]

####################################
#### MARK: Part 1: Crear 50 punts aleatòris amb distància 200m entre ells dins els polígons
###################################

### Funció per fer-ho


#dir.create("results/01_Punts_aleatoris")
# dir.create("results/01_Punts_aleatoris/RDS")
# dir.create("results/01_Punts_aleatoris/errors", showWarnings = FALSE)


dir.exists(output_path)
output_path <- "results/01_Punts_aleatoris/"

for (cc in corines_sample) {

  message("Processing CORINE ", cc)

  tryCatch({

    pts <- sample_points(
      polygons = boscos,
      field = "COD_CORINE",
      value = cc,
      n_points = 50,
      dist_primary = 200,
      dist_fallback = 100,
      oversample_factor = 10
    )

    if (is.null(pts)) {

      # ---- logical failure (not an error)
      err <- list(
        corine = cc,
        type = "constraint_failure",
        message = "Could not allocate 50 points at 200 or 100 m",
        time = Sys.time()
      )

      saveRDS(
        err,
        file = file.path(paste0(output_path, "/errors"),
                         paste0("error_CORINE_", cc, ".rds"))
      )

      message("  -> FAILED (constraints)")
      next
    }

    # ---- success → write to disk immediately
    saveRDS(
      pts,
      file = file.path(paste0(output_path, "/RDS"),
                       paste0("pts_CORINE_", cc, ".rds"))
    )

    # ---- free memory explicitly
    rm(pts)
    gc()

    message("  -> DONE")

  }, error = function(e) {

    # ---- unexpected error → log & continue
    err <- list(
      corine = cc,
      type = "runtime_error",
      message = conditionMessage(e),
      time = Sys.time()
    )

    saveRDS(
      err,
      file = file.path(paste0(output_path, "/errors"),
                       paste0("error_CORINE_", cc, ".rds"))
    )

    message("  -> ERROR (see log)")
  })
}

