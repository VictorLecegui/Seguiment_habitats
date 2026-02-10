#### Prcediment generació de punts

# Categoria: HIC
# Metode generació punts aleatòris: rSSI
# Mètode treure 30/50 punts: grst estratificat


#### Packages

library(tidyverse)
library(sf)
#install.packages(c("vegclust", "mapSpain"))
library(leaflet)
library(spsurvey)
library(units)

source("scripts/utils.R")

### DADES: Shape de boscos

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp")

# He de canviar els noms perquè siguin segurs a l'hora d'escriure fitxers
boscos$COD_HIC_tf <- make.names(boscos$COD_HIC)

cbind(unique(boscos$COD_HIC), unique(boscos$COD_HIC_tf))

hics <- unique(boscos$COD_HIC_tf)
hics <- hics[-2]

regions <- unique(boscos$RegioHIC)


for(j in seq_along(regions)){

    regio <- regions[j]
    message("Inici regió = ", regio)

    for(i in seq_along(hics)){

        hic <- hics[i]
#### Part 1: Filtrar el polígon segons els camps i els valors requerits

        message("Començant a processar HIC: ", hic)

  hic_regio_raw <- filter_polygons(
      boscos,
      field = "COD_HIC_tf",
      value = hic,
      buff_dist = "Radi_buffe"
    ) |>
      filter(RegioHIC == regio)

    # ── SIMPLE CONDITION: SKIP INVALID COMBINATIONS ──
    if (nrow(hic_regio_raw) == 0) {
      message(" HIC ", hic, " no present a la regió ", regio, ". Saltant.")
      next
    }

    # Only union if valid
    hic_regio <- st_union(hic_regio_raw)

#### Part 2: Generar 50 punts aleatòris dins del polígon
        # Opció 1: 50 pùnts a 200m de distància
        # Opció 2: 50 punts a 100m de distància
        # Opció 3: Tots els punts que hi capiguen separats 100 entre ells
        # Opció 4: Si no s'ha pogut generar retornar error 

    punts_50 <- tryCatch(
       generate_points(polygon = hic_regio),
          error = function(e) {
           message(
           " Error generant punts per HIC ", hic,
            " a la regió ", regio, ": ",
           e$message
             )
         return(NULL)
        }
        )

if (is.null(punts_50) || nrow(punts_50) == 0) {
  message(
    " HIC ", hic,
    " no s'han pogut generar punts aleatoris a la regió = ",
    regio, ". Saltant."
  )
  next
}
        saveRDS(punts_50, paste0("results/01_Loop_punts/Punts_aleatoris/", 
                            hic,"_", regio, "_50p.rds"))

#### Part 3: Generar els 30 punts amb GRTS

        if(nrow(punts_50)>30){

             punts_30<- grts(punts_50, n_base = 30)

             saveRDS(punts_30, paste0("results/01_Loop_punts/Punts_seleccionats/", 
                            hic, "_",regio, "_30p.rds"))

} else {

punts_30 <- punts_50

saveRDS(punts_30, paste0("results/01_Loop_punts/Punts_seleccionats/", 
                            hic, "_",regio, "_", nrow(punts_30),"p.rds"))
}
        message("HIC = ", hic, " acabat")

        }

    message("Regió: ", regio, " acabada")

}



