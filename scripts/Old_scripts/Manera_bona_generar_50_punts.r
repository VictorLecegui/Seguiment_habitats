### Procés manual de generar els 50 punts però bo!

### Igual SSI però tenint en compte el multypolygon, cosa que rSSI a spatstat no fa i per això no entren els punts. 

pol_flt <- filter_polygons(
      boscos_ok,
      field = "Arbre_domi",
      value =  "Fraxinus angustifolia",
      buff_dist = "Radi_buffe"
    ) 

# multipolygon (must be projected, meters)

target_n <- 50
min_dist  <- set_units(200, m)

pts <- st_sfc(crs = st_crs(hic_9340_med))
tries <- 0
max_tries <- 1e6



while (length(pts) < target_n && tries < max_tries) {
  tries <- tries + 1

  p <- st_sample(hic_sf, size=1)
  st_crs(p) <- st_crs(hic_9340_med)
  if (length(pts) == 0) {
    pts <- p
  } else {
    d <- st_distance(p, pts)
    if (min(d) >= min_dist) {
      pts <- c(pts, p)
    }
  }
}

pts <- st_sf(geometry = pts)
plot(pts)


