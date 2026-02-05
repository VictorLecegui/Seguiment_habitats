#### Arbre domi un exemple

####  Punts arbre domi
# Package

library(tidyverse)
library(spsurvey)
library(units)
library(sf)
library(mapSpain)

source("scripts/utils.r")

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

## Arbres inf 30 però ja amb punts


    arbre <- arbres_inf_30[1]


    punts_mostrejats <- punts_hic |> filter(Arbre_domi_tf == arbre)


    pol_arbre <- filter_polygons(boscos, 
                                field = "Arbre_domi_tf",
                                value = "Fagus.sylvatica", 
                                buff_dist = "Radi_buffe") |> 
                                st_union() |> 
                                st_as_sf()

    # punts_50 <- irs(pol_arbre, 
    #                 n_base = 50, 
    #                 mindis = 200, 
    #                 legacy_sites = punts_mostrejats )

### Generar els 50 punts aleatoris

    punts_50 <- generate_points(polygon = pol_arbre)

# Seleccionar 30 punts dels 50 amb GRTS

    punts_30<- grts(punts_50, n_base = 30, 
                            legacy_sites = punts_mostrejats)
    st_crs(punts_30$sites_base)=st_crs(punts_mostrejats)

plot(punts_30)


### Visualitzacio resultats:

cat_poly <- esp_get_ccaa("Catalunya") |> 
            st_transform(25831)


st_crs(punts_50)==st_crs(punts_mostrejats)
st_crs(punts_30$sites_base)==st_crs(punts_mostrejats)

plot(cat_poly$geometry)
plot(punts_50,add = TRUE, col = "black", pch = 19)
plot(punts_mostrejats, add=TRUE, col = "red", pch = 19)
plot(punts_30$sites_base, add=TRUE, col = "green", pch = 19)

legend("bottomright", 
        legend = c("Punts aleatoris", 
                    "Punts mostrejats amb HIC",
                    "Punts mostrejats per Arbre domi" ),
        col = c("black", "red", "green"),
        pch = 19)


st_crs(punts_30$sites_base)==st_crs(boscos)

punts_arbre_domi <- st_join(x = punts_30$sites_base, y=boscos,
                         join = st_within ,left = FALSE) 

punts_arbre_domi
