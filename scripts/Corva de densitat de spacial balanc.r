#### Corva de densitat de spacial balance

#### Script per calcular la probabilitat de tenir com a  mínim 1 punts en el polígon per atzar

### Utilitzo les fagedes d'exemple


library(sf)
library(tidyverse)
library(vegan)
library(units)
library(spsurvey)
library(leaflet)
library(htmlwidgets)


source("scripts/utils.R")

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp") |> 
                mutate(COD_HIC_tf = make.names(COD_HIC))

points_cat <- readRDS("results/Malla_Catalunya.rds")


# Pas 1: Obtenir polígon 

colnames(boscos)

hic_fg <- filter_polygons(boscos, 
                            field = "COD_HIC_tf",
                            value = "X9130",
                            buff_dist = "Radi_buffe") |> 
            filter(RegioHIC == "MED") |> 
            st_as_sf()

hic_fg <- hic_fg[!st_is_empty(hic_fg),]

st_crs(points_cat)==st_crs(hic_fg)

points_cat_crop <- st_crop(points_cat, st_bbox(hic_fg))

hic_points <- st_join(points_cat_crop, hic_fg,
                        join = st_within, left = FALSE)

# Pas 2: Monte Carlo permutation amb les nostres condicions

set.seed(1998)

n_sim <- 500
n_points <- 30
min_dist <- 200

sim_results_aleat <- purrr::map_dbl(1:n_sim, function(i) {

  samp <- irs(hic_points, mindis = min_dist, n_base = n_points)
   
   balance <- sp_balance(samp$sites_base, hic_points)
   balance$value
   
})

sim_results_grts <- purrr::map_dfr(1:n_sim, function(i) {

  samp <- grts(hic_points, mindis = min_dist, n_base = n_points)
   
   balance <- sp_balance(samp$sites_base, hic_points)
   balance$value

})

density(sim_results_aleat)
?map_vec

balance_aleat <- data.frame(Metode = "Aleatori",
                            Valor = sim_results_aleat)

plot_dens <- ggplot(balance_aleat, aes(x = Valor)) +
            geom_density()+
            geom_vline(aes(xintercept=bal_grts$value), lty = 2 ,col = "red") +
            annotate(geom = "text",
                    x = bal_grts$value - 0.005, y = 17,
                    label = "1.6%") +
            theme_bw()+
            ggtitle("Corba de densitat balanç espacial valors aleatoris",
                        subtitle = "HIC 9130")
ggsave(plot_dens, filename = "results/Gràfic valors aleatoris.pdf", width = 8, height = 6)            

bal_grts <- grts(hic_points, n_base = 30, mindis = 200)

bal_grts <- sp_balance(bal_grts$sites_base, hic_points)

# Proporció de valors aleatoris inferior o iguals als aconseguits amb GRTS

p_empirical <- mean(abs(bal_grts$value) >= abs(balance_aleat$Valor))
# resultat
p_empirical*100 # 1.6%
