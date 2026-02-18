#### Funcions noves per treballar amb la malla


# Filtre del shape gran per valor i camp que volem + buffer

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

# Funció per registrar warnings i errors

log_msg <- function(level = "INFO", msg) {
  line <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    "[", level, "] ",
    msg
  )
  write(line, file = log_file, append = TRUE)
  message(line)
}

### Funció per seleccionar polígons a 200m

select_polygons_200m <- function(sf_obj, dist = 200) {
  
  if (st_is_longlat(sf_obj)) {
  stop("Polygons must be projected in meters (not lon/lat)")
  }

  remaining <- sf_obj
  selected <- list()
  
  while(nrow(remaining) > 0) {
    
    # randomly pick one polygon
    i <- sample(seq_len(nrow(remaining)), 1)
    chosen <- remaining[i, ]
    
    selected[[length(selected) + 1]] <- chosen
    
    # buffer chosen polygon
    exclusion_zone <- st_buffer(chosen, dist)
    
    # remove intersecting polygons
    remaining <- remaining[!st_intersects(remaining, exclusion_zone, sparse = FALSE)[,1], ]
  }
  
  do.call(rbind, selected)
}

#### Function with the process of selecting points considering already sampled points: 

generate_points_hic <- function(pol,
                                points_cat,
                                hic,
                                regio,
                                legacy_points = NULL,
                                n_target = 30,
                                min_dist = 200,
                                n_iter = 100) {
  
  #### Inici filtrem malla pel polígon de l'HIC
  
  points_cat_crop <- st_crop(points_cat, st_bbox(pol))
  
  hic_points <- st_join(points_cat_crop, pol,
                        join = st_within, left = FALSE)
  
  # Considerem punts mostrejats per tenir el nombre de punts que tenim
  # SI tenim punts mostrejats
  if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
    
    log_msg(msg = paste(nrow(legacy_points),
                        "Punts ja mostrejats inclosos per l'HIC",
                        hic, "a", regio))
    
    n_punts <- nrow(hic_points) + nrow(legacy_points)
    
  } 
  # NO tenim punts mostejats
  else {
    
    log_msg(msg = paste("0 Punts ja mostrejats inclosos per l'HIC",
                        hic, "a", regio))
    
    n_punts <- nrow(hic_points)
  }
  
  
  # Comprovem si el nombre de punts disponibles es inferior o superior als que volem

  # Menys punts disponibles dels que volem: 
  if (n_punts < n_target) {
    
    log_msg(level = "ATENCIÓ!",
            msg = paste("Menys de", n_target,
                        "punts d'intersecció amb la malla per:",
                        hic, regio))
    # Busquem combinacions de polígons separats 200m entre ells
    results <- lapply(1:n_iter,
                      function(x) select_polygons_200m(pol, min_dist))
    
    # Millor combinació de polígons a 200m entre ells
    best <- results[[which.max(sapply(results, nrow))]]
    
    # Si no hi ha polígons separats 200m entre ells Aturar el procés
    if (nrow(best) == 0) {
      log_msg(level = "ERROR",
              msg = paste("Cap polígon seleccionat a més de",
                          min_dist, "m:", hic, regio))
      return(NULL)
    }
    
    # SI hi ha pol separats 200m entre ells

    # Extreure poligons que coincidien de la malla
    idx <- st_contains(best, hic_points)
    pol_with_points <- best[lengths(idx) > 0, ]
    
    # Poligons sense punts 
    pol_no_points   <- best[lengths(idx) == 0, ]
    
    # Menys de 30 poligons separats 200m entre ells
    if (nrow(best) < n_target) {
      
      log_msg(msg = paste("Menys de", n_target,
                          "polígons separats", min_dist,
                          "m entre ells:", hic, regio))
      # Generem un punt per polígon dels que no en teníen
      points_pol <- st_point_on_surface(pol_no_points)
      
      # SI hih a punts mostrejats els afegim
      if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
        points <- rbind(points_pol, hic_points, legacy_points)
        method <- "Centroide polígon (<30 pol & <30 points)"
      } 
      # NO hi ha punts mostejats (punts malla + punts centre)
      else {
        points <- rbind(points_pol, hic_points)
        method <- "Centroide polígon (<30 pol & <30 points)"
      }

     if(nrow(points) > n_target){
      # SI hih a punts mostrejats els afegim
      if (!is.null(legacy_points) && nrow(legacy_points) > 0) { 
        points <- grts(points, 
                    n_base = n_target,
                    mindis = min_dist,
                    legacy_sites = legacy_points)
        method <- "Centroide polígon (<30 pol & >30 points)"
      } 
      # NO hi ha punts mostejats (punts malla + punts centre + GRTS)
      else {
        points <- grts(points, 
                    n_base = n_target, 
                    mindis = min_dist)
        method <- "Centroide polígon (<30 pol & >30 points)"
      }
     }

     # Més de 30 polígons separats 200m entre ells 
    } else {
      
      log_msg(msg = paste("Més de", n_target,
                          "polígons amb distància mínima",
                          min_dist, "m, aplicant GRTS per:",
                          hic, regio))
      
      # Afegim un punt central en els polígons que no tenen punts
      points_pol <- st_point_on_surface(pol_no_points)
      # Ajuntem punts centroide + punts malla
      points_pol <- rbind(hic_points, points_pol)

      # SI hih a punts mostejats
      if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
        
        # Calculem punts que queden per mostrejat
        n_remaining <- n_target - nrow(legacy_points)
        
        # SI queden punts per mostrejar
        if (n_remaining > 0) {

          points <- grts(points_pol,
                         n_base = n_target,
                         legacy_sites = legacy_points,
                         mindis = min_dist)
          method <- "Centroide polígon + GRTS (>30 pol)"

        } else {
          points <- legacy_points

          method <- "30 punts ja mostrejats"
        }
  

      # NO hi ha punts mostrejats
      } else {
        
        points <- grts(points_pol,
                       n_base = n_target,
                       mindis = min_dist)
        method <- "Centroide polígon + GRTS (>30 pol)"
      }
    }

# Més de 30 punts en la intersecció amb la malla

  } else {
    
    log_msg(msg = paste("Més de", n_target,
                        "punts a la malla, aplicant GRTS:",
                        hic, regio))

   # SI hi ha punts mostrejats 
    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
      
      n_remaining <- n_target - nrow(legacy_points)
      
      if (n_remaining > 0) {
        points <- grts(hic_points,
                       n_base = n_target,
                       legacy_sites = legacy_points,
                       mindis = min_dist)
        method <- "Malla + GRTS"
      } else {
        points <- legacy_points
        method <- "30 punts ja mostrejats"
      }
  
  # NO hi ha punts mostrejats    
    } else {
      
      points <- grts(hic_points,
                     n_base = n_target,
                     mindis = min_dist)
      method <- "Malla + GRTS"
    }
  }
  
##### Guardem els resultats: 

log_msg(level = "MÈTODE", msg = method)

if(exists("points_pol")){
    return(list(
    points = points,
    hic_points = hic_points,
    points_centre = points_pol,
    method_points = method  ))
}

  return(list(
    points = points,
    hic_points = hic_points,
    method_points = method  ))
}
