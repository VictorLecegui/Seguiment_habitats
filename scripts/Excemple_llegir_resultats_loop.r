prova <- readRDS("results/02_Loop_punts_HIC/Punts_mostreig/Punts_X9430_ALP.rds")
prova

prova_pol <- st_read("results/02_Loop_punts_HIC/Poligons_shapes/Pol_X9560._ALP.gpkg")
prova_pol


    m <- st_distance(punts_hic_regio)
        diag(m)<-Inf
        min(m)
        dist_sup_200 <- ifelse(min(m) > set_units(min_dist,m),
                TRUE,
                FALSE)