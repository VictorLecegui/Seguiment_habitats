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
try_sampling <- function(
  polys,
  n_points,
  min_dist,
  oversample_factor
) {

  min_dist <- units::set_units(min_dist, "m")

  # ---- Geometry safety
  polys <- sf::st_make_valid(polys)
  polys <- polys[!sf::st_is_empty(polys), ]

  if (nrow(polys) == 0) return(NULL)

  cand <- tryCatch(
    {
      sf::st_sample(
        polys,
        size = n_points * oversample_factor,
        type = "random"
      ) |> sf::st_as_sf()
    },
    error = function(e) NULL
  )

  if (is.null(cand) || nrow(cand) == 0) return(NULL)

  selected <- cand[0, ]

  for (i in seq_len(nrow(cand))) {

    if (nrow(selected) == 0) {
      selected <- cand[i, ]
    } else {
      d <- sf::st_distance(cand[i, ], selected)
      if (min(d) >= min_dist) {
        selected <- rbind(selected, cand[i, ])
      }
    }

    if (nrow(selected) == n_points) break
  }

  if (nrow(selected) == n_points) selected else NULL
}


# Funció 3: La funció que utilitza les altres funcions i genera els punts

sample_points <- function(
  polygons,
  field,
  value,
  n_points = 50,
  dist_primary = 200,
  dist_fallback = 100,
  oversample_factor = 10
) {

  # ---- Filter polygons
  polys_flt <- filter_polygons(polygons, field, value)
  if (is.null(polys_flt)) return(NULL)

  # ---- Try primary distance
  pts <- try_sampling(
    polys = polys_flt,
    n_points = n_points,
    min_dist = dist_primary,
    oversample_factor = oversample_factor
  )

  if (!is.null(pts)) {
    attr(pts, "used_distance") <- dist_primary
    return(pts)
  }

  # ---- Try fallback
  pts <- try_sampling(
    polys = polys_flt,
    n_points = n_points,
    min_dist = dist_fallback,
    oversample_factor = oversample_factor
  )

  if (!is.null(pts)) {
    attr(pts, "used_distance") <- dist_fallback
    return(pts)
  }

  message(
    sprintf(
      "%s = %s: cannot place %d points at %d or %d m",
      field, value, n_points, dist_primary, dist_fallback
    )
  )

  return(NULL)
}
