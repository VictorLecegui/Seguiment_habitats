### Procés manual de generar els 50 punts però bo!

### Igual SSI però tenint en compte el multypolygon, cosa que rSSI a spatstat no fa i per això no entren els punts. 

pol_flt <- filter_polygons(
      boscos_ok,
      field = "Arbre_domi",
      value =  "Fraxinus angustifolia",
      buff_dist = "Radi_buffe"
    )  |> 
    st_union()

# multipolygon (must be projected, meters)

target_n <- 50
min_dist  <- set_units(200, m)

pts <- st_sfc(crs = st_crs(pol))
tries <- 0
max_tries <- 1e6

while (length(pts) < target_n && tries < max_tries) {
  tries <- tries + 1

  p <- st_sample(pol_flt, size=1)

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


