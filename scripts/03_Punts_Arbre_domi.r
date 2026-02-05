####  Punts arbre domi
# Package

library(tidyverse)
library(spsurvey)
library(units)
library(sf)


##### Llegir les dades

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
               st_make_valid() |> 
               mutate(Arbre_domi_tf = make.names(Arbre_domi))

punts_hic <- read_sf("results/02_Punts_HIC/Punts_HIC.shp") |> 
               st_make_valid() |> 
               mutate(Arbre_domi_tf = make.names(Arbre_domi))


arbre_domi <- punts_hic |> 
    group_by(Arbre_domi_tf) |> 
    summarise(N = n())


arbres_no_hic <- setdiff(unique(boscos$Arbre_domi_tf),
        unique(punts_hic$Arbre_domi_tf))

arbres_sup_30 <- arbre_domi |> filter(N>=30) |> pull(Arbre_domi_tf)
arbres_inf_30 <- arbre_domi |> filter(N<30) |> pull(Arbre_domi_tf)

setdiff(unique(boscos$Arbre_domi_tf), c(arbres_no_hic, 
                                        arbres_sup_30, 
                                        arbres_inf_30))

### Procediment: 
    # Arbres no hic: Tot el procés
    # Arbres sup 30: Ja mostrejats amb els HIC, no cal generar punts
    # Arbres inf 30: Generar punts tenint en compte els que ja tenim

## MARK: Arbres inf 30 però ja amb punts

# fitxer per enregistrar el que va passant en el loop:
log_file <- "results/03_Punts_Arbre_domi/punts_arbre_domi.log"
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

# Loop per tots els arbres que ja tenen punts

for (i in 1:3) {

  arbre <- arbres_inf_30[i]

  log_msg("INFO", paste("Començant a processar Arbre domi:", arbre))

    # Punts ja mostrejats: 

  punts_mostrejats <- punts_hic |> 
    filter(Arbre_domi_tf == arbre)

    # Filtre dels polígon pel nom de Arbre dominant

  pol_arbre <- tryCatch(
    {
      filter_polygons(
        boscos,
        field = "Arbre_domi_tf",   # FIX
        value = arbre,
        buff_dist = "Radi_buffe"
      ) |> 
        st_union()
    },
    error = function(e) {
      log_msg("ERROR", paste("Error generant polígon per", arbre, ":", e$message))
      return(NULL)
    }
  )

  if (is.null(pol_arbre)) {
    log_msg("ERROR", paste("Polígon NULL per", arbre, "- saltant"))
    next
  }

  # Generar 50 punts dins dels polígons

  punts_50 <- tryCatch(
    generate_points(polygon = pol_arbre),
    error = function(e) {
      log_msg("ERROR", paste("Error generant punts per", arbre, ":", e$message))
      return(NULL)
    }
  )

# Si no s'han pogut generar punts
  if (is.null(punts_50) || !inherits(punts_50, "sf") || nrow(punts_50) == 0) {
    log_msg("ERROR", paste("No s'han pogut generar punts per", arbre))
    next
  }

# Si hi ha menys de 30 punts
  if (nrow(punts_50) < 30) {
    log_msg(
      "WARNING",
      paste("Només", nrow(punts_50), "punts generats per", arbre)
    )
  }

  saveRDS(
    punts_50,
    paste0(
      "results/03_Punts_Arbre_domi/Punts_aleatoris/",
      arbre, "_50p.rds"
    )
  )

  # Selecció 30 punts per GRTS tenint en compte els ja mostrejats

  if (nrow(punts_50) > 30) {

    punts_30 <- grts(
      punts_50,
      n_base = 30,
      legacy_sites = punts_mostrejats
    )

    saveRDS(
      punts_30,
      paste0(
        "results/03_Punts_Arbre_domi/Punts_seleccionats/",
        arbre, "_30p.rds"
      )
    )

  } else {

    punts_30 <- punts_50

    saveRDS(
      punts_30,
      paste0(
        "results/03_Punts_Arbre_domi/Punts_seleccionats/",
        arbre, "_", nrow(punts_30), "p.rds"
      )
    )
  }

  log_msg("INFO", paste("Arbre", arbre, "acabat"))
}
