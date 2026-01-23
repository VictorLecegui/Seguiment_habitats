##### 30 punts ben distribuïts espaialment

library(spsurvey)

hics_50 <- list.files("results/01_Punts_aleat_rSSI")

hics_50_ls <- lapply(paste0("results/01_Punts_aleat_rSSI/", hics_50), readRDS)


hics_bined <- bind_rows(hics_50_ls)

hics_name <- unique(hics_bined$COD_HIC)
n_sample <- 30

n_strat <- c(rep(30, length(hics_name)))
names(n_strat) <- hics_name

pts_thin <- grts(hics_bined, n_base = n_strat, stratum_var = "COD_HIC") 


plot(
  pts_thin,
  formula = siteuse ~ COD_HIC,
  hics_bined,
  key.width = lcm(3)
)

### Spatial Balance

sp_balance(pts_thin$sites_base, hics_bined, stratum_var = "COD_HIC")
