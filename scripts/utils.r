##### Funcions en el procés de selecció dels punts de mostreig dels hàbitats

#### Part 1: Trobar 50 punts distanciats entre ells 

    # La distància ha de ser en METRES

### Funció 1: Utilitzada per filtrar el polígon per camp i el valor que volem
### tambe fa el buffer de la capa segons el valor a Radi Buffe

###3 VIGILAR! Si no hi ha Buffer la funció no funcionarà, si els poligons ja tenen el buffer fet crear una columna amb aquest nom i 0 com a valor. 

filter_polygons <- function(polygons, field, value, buff_dist = NULL) {

  if (is.null(field) || is.null(value)) {
    stop("You must provide both 'field' and 'value'")
  }

  if (!field %in% names(polygons)) {
    stop("Field '", field, "' not found in polygons")
  }

  polys_flt <- polygons |>
    dplyr::filter(!is.na(.data[[field]])) |>
    dplyr::filter(.data[[field]] == value) 

  if(!is.null(buff_dist)){
    polys_flt <- polys_flt |> 
                  dplyr::mutate(geometry = st_buffer(.data[["geometry"]], dist = -.data[[buff_dist]]))
  }

  if (nrow(polys_flt) == 0) {
    message(field, " = ", value, ": no polygons found")
    return(NULL)
  }

  polys_flt
}

# Funció 2: Procés de generar punts dins dels polígons i que estiguin separats per X metres

try_points <- function(polygon, 
                       target_n, 
                       min_dist, 
                       max_tries) {

  min_dist <- units::set_units(min_dist, m)
  pts <- st_sfc(crs = st_crs(polygon))
  tries <- 0

  while (length(pts) < target_n && tries < max_tries) {
    tries <- tries + 1
    p <- st_sample(polygon, size = 1)

    if (length(pts) == 0) {
      pts <- p
    } else {
      d <- st_distance(p, pts)
      if (min(d) >= min_dist) {
        pts <- c(pts, p)
      }
    }
  }

  st_sf(geometry = pts)
}

# Funció 3: Comprovar si els punts caben dins del polígon

min_required_area <- function(n, min_dist) {
  packing_efficiency <- pi / sqrt(12)  # hexagonal packing
  n * pi * (min_dist / 2)^2 / packing_efficiency
}

can_fit_points <- function(polygon, n, min_dist) {
  poly_area <- as.numeric(st_area(polygon))
  min_area  <- min_required_area(n, min_dist)
  poly_area >= min_area
}



# Funció 4: Genera els punts aleatoris a partir de Funció 2 segons els requeriments


generate_points <- function(polygon,
                            target_n = 50,
                            dist_primary = 200,
                            dist_fallback = 100,
                            max_tries = 1e6) {

  poly_area <- sum(as.numeric(st_area(polygon)))

  area_200 <- min_required_area(target_n, dist_primary)
  area_100 <- min_required_area(target_n, dist_fallback)

  # ── DECISION TREE ─────────────────────────────

  if (poly_area >= area_200) {
    message("Àrea suficient per ", dist_primary, " m. Intentant SSI...")
    
    pts <- try_points(polygon, target_n, dist_primary, max_tries)
    
    if (nrow(pts) == target_n) {
      pts$method <- "random_200m"
      return(pts)
    }
  }

  if (poly_area >= area_100) {
    message("Només possible amb ", dist_fallback, " m. Intentant SSI...")
    
    pts <- try_points(polygon, target_n, dist_fallback, max_tries)
    
    if (nrow(pts) == target_n) {
      pts$method <- "random_100m"
      return(pts)
    }
  }

  message("SSI impossible. Generant malla regular...")

 grid <- st_make_grid(
 polygon,
 cellsize = dist_fallback,
 what = "centers",
 square = FALSE
 )
 
grid <- st_sf(geometry = grid)

inside <- lengths(st_within(grid, polygon)) > 0
grid <- grid[inside, ] 

 message("Malla generada amb ", nrow(grid), " punts")
 
 grid$method <- "grid_100m"
 return(grid)
                            }
