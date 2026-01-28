#### procediment per seguir amb altres nivells de calssificació dels habitats

library(units)

results_hic <- readRDS("results/02_Selection_GRTS_HIC/results_hic.rds")

arbres_mostrejats <- results_hic |> 
    group_by(Arbre_domi)  |>
    summarise(N=n()) 

arbres_list <- split(arbres_mostrejats, arbres_mostrejats$Arbre_domi)

fraxinus <- arbres_list$'Fraxinus angustifolia'


run_rSSI_one_hic(shp_data=boscos_ok, value = "Fraxinus angustifolia", field = "Arbre_domi", out_dir=out_dir_hic )

pol_flt <- filter_polygons(
      boscos_ok,
      field = "Arbre_domi",
      value =  "Fraxinus angustifolia",
      buff_dist = "Radi_buffe"
    )

pol_unified <- pol_flt |> st_union()

# multipolygon (must be projected, meters)

# target_n <- 50
# min_dist  <- set_units(200, m)

# pts <- st_sfc(crs = st_crs(pol))
# tries <- 0
# max_tries <- 1e6

# while (length(pts) < target_n && tries < max_tries) {
#   tries <- tries + 1

#   p <- st_sample(pol_unified, size=1)

#   if (length(pts) == 0) {
#     pts <- p
#   } else {
#     d <- st_distance(p, pts)
#     if (min(d) >= min_dist) {
#       pts <- c(pts, p)
#     }
#   }
# }

# pts <- st_sf(geometry = pts)
# plot(pts)

pts_pol <- st_intersection(pts, pol_flt)

fraxinus_pts <- fraxinus |> st_cast("POINT") 

fraxinus_grts <- grts(pts_pol, n_base = 30, legacy_sites = fraxinus_pts)

plot(pts_pol$geometry)
plot(fraxinus_pts, col="green", add = TRUE)
plot(fraxinus_grts$sites_base, col = "blue", add = TRUE)

plot(fraxinus_grts, pch=19)
sp_balance(fraxinus_grts$sites_base, pts_pol)
sp_balance(fraxinus_grts$sites_legacy, pts_pol)
