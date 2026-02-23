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
    
    # Menys de 30 poligons separats 200m entre ells i menys de 30 punts a la malla
    if ((nrow(pol_no_points) + nrow(hic_points)) < n_target) {
      
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
# Si una vegada ajuntem els polígons + punts malla etc. >30 punts
     if(nrow(points) > n_target){
      # SI hih a punts mostrejats els afegim
      if (!is.null(legacy_points) && nrow(legacy_points) > 0) { 
        points_close_legacy <- st_is_within_distance(
                             points,
                             legacy_points,
                            dist = set_units(min_dist,m))
        

        points_no_legacy <- points[lengths(points_close_legacy) == 0, ]
       
        points <- grts(points_no_legacy, 
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
          points_close_legacy <- st_is_within_distance(
                             points_pol,
                             legacy_points,
                            dist = set_units(min_dist,m))
        

          points_no_legacy <- points_pol[lengths(points_close_legacy) == 0, ]

          points <- grts(points_no_legacy,
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
        points_close_legacy <- st_is_within_distance(
                             hic_points,
                             legacy_points,
                            dist = set_units(min_dist,m))
        

        points_no_legacy <- hic_points[lengths(points_close_legacy) == 0, ]

        points <- grts(points_no_legacy,
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


### MARK: Funció per generar punts pels grups de CORINE



generate_points_grups <- function(pol,
                                points_cat,
                                grup,
                                legacy_points = NULL,
                                n_target = 30,
                                min_dist = 200,
                                n_iter = 100) {
  
  #### Inici filtrem malla pel polígon del grup

  
  grup_points <- st_join(points_cat, pol,
                        join = st_within, left = FALSE)


# Filtrar els punts de la malla que estan a <200m dels punts ja mostejats

 if (!is.null(legacy_points) && nrow(legacy_points) > 0) {

    close_to_legacy <- st_is_within_distance(
                             grup_points,
                             legacy_points,
                            dist = set_units(min_dist,m))

    grup_points <- grup_points[lengths(close_to_legacy) == 0, ]

}
  
  # Considerem punts mostrejats per tenir el nombre de punts que tenim
  
  # SI tenim punts mostrejats
  if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
    
    log_msg(msg = paste(nrow(legacy_points),
                        "Punts ja mostrejats inclosos pel grup",
                        grup))
    
    n_punts <- nrow(grup_points) + nrow(legacy_points)
    
  } 
  # NO tenim punts mostejats
  else {
    
    log_msg(msg = paste("0 Punts ja mostrejats inclosos pel grup",
                        grup))
    
    n_punts <- nrow(grup_points)
  }
  
  
  # Comprovem si el nombre de punts disponibles es inferior o superior als que volem

  # Menys punts disponibles dels que volem: 
  if (n_punts < n_target) {
    
    log_msg(level = "ATENCIÓ!",
            msg = paste("Menys de", n_target,
                        "punts d'intersecció amb la malla per:",
                        grup))
    # Busquem combinacions de polígons separats 200m entre ells
    results <- lapply(1:n_iter,
                      function(x) select_polygons_200m(pol, min_dist))
    
    # Millor combinació de polígons a 200m entre ells
    best <- results[[which.max(sapply(results, nrow))]]
    
    # Si no hi ha polígons separats 200m entre ells Aturar el procés
    if (nrow(best) == 0) {
      log_msg(level = "ERROR",
              msg = paste("Cap polígon seleccionat a més de",
                          min_dist, "m:", grup))
      return(NULL)
    }
    
    # SI hi ha pol separats 200m entre ells

    # Extreure poligons que coincidien de la malla
    idx <- st_contains(best, grup_points)
    pol_with_points <- best[lengths(idx) > 0, ]
    
    # Poligons sense punts 
    pol_no_points   <- best[lengths(idx) == 0, ]
    
    # Menys de 30 poligons separats 200m entre ells i menys de 30 punts a la malla
    if ((nrow(pol_no_points) + nrow(grup_points)) < n_target) {
      
      log_msg(msg = paste("Menys de", n_target,
                          "polígons separats", min_dist,
                          "m entre ells:", grup))
      # Generem un punt per polígon dels que no en teníen
      points_pol <- st_point_on_surface(pol_no_points)
      
      # SI hih a punts mostrejats els afegim
      if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
          points_pol <- st_set_geometry(points_pol, "geometry")
          grup_points <- st_set_geometry(grup_points, "geometry")
          legacy_points <- st_set_geometry(legacy_points, "geometry")


        points <- bind_rows(points_pol, grup_points, legacy_points)

        if(nrow(points) > n_target){
            points <- grts(points, 
                    n_base = n_target,
                    mindis = min_dist,
                    legacy_sites = legacy_points)
            
             method <- "Centroide polígon (<30 pol & >30 points) - GRTS"
        } else {
             method <- "Centroide polígon (<30 pol & <30 points)"
        }

      } 
      # NO hi ha punts mostejats (punts malla + punts centre)
      else {
        
        points_pol <- st_set_geometry(points_pol, "geometry")
        grup_points <- st_set_geometry(grup_points, "geometry")
        
        points <- bind_rows(points_pol, grup_points)

        if(nrow(points) > n_target){
             points <- grts(points, 
                    n_base = n_target, 
                    mindis = min_dist)

            method <- "Centroide polígon (<30 pol & >30 points) - GRTS"

        }
        method <- "Centroide polígon (<30 pol & <30 points)"
      }

     # Més de 30 polígons separats 200m entre ells 
    } else {
      
      log_msg(msg = paste("Més de", n_target,
                          "polígons amb distància mínima",
                          min_dist, "m, aplicant GRTS per:",
                          grup))
      
      # Afegim un punt central en els polígons que no tenen punts
      points_pol <- st_point_on_surface(pol_no_points)
      # Ajuntem punts centroide + punts malla
      points_pol <- rbind(grup_points, points_pol)

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
                        grup))

   # SI hi ha punts mostrejats 
    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
      
      n_remaining <- n_target - nrow(legacy_points)
      
      if (n_remaining > 0) {
        points <- grts(grup_points,
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
      
      points <- grts(grup_points,
                     n_base = n_target,
                     mindis = min_dist)
      method <- "Malla + GRTS"
    }
  }
  
##### Guardem els resultats: 

log_msg(level = "MÈTODE", msg = method)

if(exists("points_pol")){
    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
     result_list <- list(
        points = points,
        grup_points = grup_points,
        points_centre = points_pol,
        method_points = method,
        n_legacy = nrow(legacy_points)  )

    return(result_list)
    } else {
         result_list <- list(
        points = points,
        grup_points = grup_points,
        points_centre = points_pol,
        method_points = method)

    return(result_list)

    }

} else {

    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
     result_list <- list(
        points = points,
        grup_points = grup_points,
        method_points = method,
        n_legacy = nrow(legacy_points)  )

    return(result_list)
    } else {
         result_list <- list(
        points = points,
        grup_points = grup_points,
        method_points = method)
        
    return(result_list)

    }


}
}

### MARK: Funció generar punts LLista vermella

generate_points_LV <- function(pol,
                                points_cat,
                                corine,
                                legacy_points = NULL,
                                n_target = 30,
                                min_dist = 200,
                                n_iter = 100) {
  
  # Check if there are duplicated legacy points

if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
  
  dup_leg <- duplicated(st_geometry(legacy_points))
  
  if (any(dup_leg)) {
    stop(paste("Duplicated points in Legacy points in", corine))
  }
}

  #### Inici filtrem malla pel polígon del grup

  
  corine_points <- st_join(points_cat, pol,
                        join = st_within, left = FALSE)


# Filtrar els punts de la malla que estan a <200m dels punts ja mostejats

 if (!is.null(legacy_points) && nrow(legacy_points) > 0) {

    close_to_legacy <- st_is_within_distance(
                             corine_points,
                             legacy_points,
                            dist = set_units(100,m))

    corine_points <- corine_points[lengths(close_to_legacy) == 0, ]

}
  
  # Considerem punts mostrejats per tenir el nombre de punts que tenim
  
  # SI tenim punts mostrejats
  if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
    
    log_msg(msg = paste(nrow(legacy_points),
                        "Punts ja mostrejats inclosos pel CORINE:",
                        corine))
    
    n_punts <- nrow(corine_points) + nrow(legacy_points)
    
  } 
  # NO tenim punts mostejats
  else {
    
    log_msg(msg = paste("0 Punts ja mostrejats inclosos pel CORINE",
                        corine))
    
    n_punts <- nrow(corine_points)
  }
  
  
  # Comprovem si el nombre de punts disponibles es inferior o superior als que volem

 log_msg(level = "INFO",
            msg = paste(n_punts,
                        " disponibles per:",
                        corine))
  # Menys punts disponibles dels que volem: 
  if (n_punts < n_target) {
    
    log_msg(level = "ATENCIÓ!",
            msg = paste("Menys de", n_target,
                        "punts d'intersecció amb la malla per:",
                        corine))
    # Busquem combinacions de polígons separats 200m entre ells
    results <- lapply(1:n_iter,
                      function(x) select_polygons_200m(pol, min_dist))
    
    # Millor combinació de polígons a 200m entre ells
    best <- results[[which.max(sapply(results, nrow))]]
    
    # Si no hi ha polígons separats 200m entre ells Aturar el procés
    if (nrow(best) == 0) {
      log_msg(level = "ERROR",
              msg = paste("Cap polígon seleccionat a més de",
                          min_dist, "m:", corine))
      return(NULL)
    }
    
    # SI hi ha pol separats 200m entre ells

    # Extreure poligons que coincidien de la malla
    idx <- st_contains(best, corine_points)
    pol_with_points <- best[lengths(idx) > 0, ]
    
    # Poligons sense punts 
    pol_no_points   <- best[lengths(idx) == 0, ]
    
    # Menys de 30 poligons separats 200m entre ells i menys de 30 punts a la malla
    if ((nrow(pol_no_points) + nrow(corine_points)) < n_target) {
      
      log_msg(msg = paste("Menys de", n_target,
                          "polígons separats", min_dist,
                          "m entre ells:", corine))
      # Generem un punt per polígon dels que no en teníen
      points_pol <- st_point_on_surface(pol_no_points)
      
      # SI hih a punts mostrejats els afegim
      if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
          points_pol <- st_set_geometry(points_pol, "geometry")
          corine_points <- st_set_geometry(corine_points, "geometry")
          legacy_points <- st_set_geometry(legacy_points, "geometry")


        points <- bind_rows(points_pol, corine_points, legacy_points)

        dup_pts <- duplicated(st_geometry(points))
        points <- points[!dup_pts,]


        if(nrow(points) > n_target){
            points <- grts(points, 
                    n_base = n_target,
                    mindis = min_dist,
                    legacy_sites = legacy_points)

             method <- "Centroide polígon (<30 pol & >30 points) - GRTS"
        } else {
             method <- "Centroide polígon (<30 pol & <30 points)"
        }

      } 
      # NO hi ha punts mostejats (punts malla + punts centre)
      else {
        
        points_pol <- st_set_geometry(points_pol, "geometry")
        corine_points <- st_set_geometry(corine_points, "geometry")
        
        points <- bind_rows(points_pol, corine_points)

        dup_pts <- duplicated(st_geometry(points))
        points <- points[!dup,]


        if(nrow(points) > n_target){
             points <- grts(points, 
                    n_base = n_target, 
                    mindis = min_dist)

            method <- "Centroide polígon (<30 pol & >30 points) - GRTS"

        }
        method <- "Centroide polígon (<30 pol & <30 points)"
      }

     # Més de 30 polígons separats 200m entre ells 
    } else {
      
      log_msg(msg = paste("Més de", n_target,
                          "polígons amb distància mínima",
                          min_dist, "m, aplicant GRTS per:",
                          corine))
      
      # Afegim un punt central en els polígons que no tenen punts
      points_pol <- st_point_on_surface(pol_no_points)
      # Ajuntem punts centroide + punts malla
      corine_points <- st_set_geometry(corine_points, "geometry")
      points_pol <- st_set_geometry(points_pol, "geometry")

      points_pol <- bind_rows(corine_points, points_pol)

      dup_pts <- duplicated(st_geometry(points_pol))
      points_pol <- points_pol[!dup_pts,]

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
                        corine))

   # SI hi ha punts mostrejats 
    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
      
      n_remaining <- n_target - nrow(legacy_points)

      points_disp <- bind_rows(corine_points, legacy_points)


      if (n_remaining > 0) {
        points <- grts(points_disp,
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
      
      points <- grts(corine_points,
                     n_base = n_target,
                     mindis = min_dist)
      method <- "Malla + GRTS"
    }
  }
  
##### Guardem els resultats: 

log_msg(level = "MÈTODE", msg = method)

if(exists("points_pol")){
    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
     result_list <- list(
        points = points,
        corine_points = corine_points,
        points_centre = points_pol,
        method_points = method,
        n_legacy = nrow(legacy_points)  )

    return(result_list)
    } else {
         result_list <- list(
        points = points,
        corine_points = corine_points,
        points_centre = points_pol,
        method_points = method)

    return(result_list)

    }

} else {

    if (!is.null(legacy_points) && nrow(legacy_points) > 0) {
     result_list <- list(
        points = points,
        corine_points = corine_points,
        method_points = method,
        n_legacy = nrow(legacy_points)  )

    return(result_list)
    } else {
         result_list <- list(
        points = points,
        corine_points = corine_points,
        method_points = method)
        
    return(result_list)

    }

}
}

