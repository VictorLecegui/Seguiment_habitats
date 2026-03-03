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

#### MARK: Balanç espacial aleatori
points_random <- function(p_malla, 
                               n_target,
                               n_reserva, 
                               legacy_points = NULL, 
                               min_dist,
                               maxtry = 10){

    rand_points <- irs(p_malla, 
                       n_base = n_target, 
                       n_over = n_reserva,
                       legacy_sites = legacy_points,
                       mindis = min_dist,
                       maxtry = maxtry)

    points_bined <- sp_rbind(rand_points)
    bal <- sp_balance(points_bined, p_malla)

    return(bal$value)
}




#### MARK: HIC Function with the process of selecting points considering already sampled points: 

generate_points_hic <- function(pol_flt,
                                points_cat,
                                hic,
                                regio,
                                legacy_points = NULL,
                                n_target = 30,
                                n_reserva = 6, 
                                min_dist = 199,
                                n_iter = 999) {
  
  #### Inici filtrem malla pel polígon de l'HIC
  
  
  hic_points <- st_join(points_cat, pol_flt,
                        join = st_within, left = FALSE)

 if (!is.null(legacy_points) && nrow(legacy_points) > 0) {

# Filtrem punts a <99 m de la malla respecte els punts ja fets. 

  close_to_legacy <- st_is_within_distance(
                             hic_points,
                             legacy_points,
                            dist = set_units(99,m))

  hic_points <- hic_points[lengths(close_to_legacy) == 0, ]

# Nombre de punts totals 

  log_msg(msg = paste(nrow(legacy_points),
                        "Punts ja mostrejats inclosos per l'HIC",
                        hic, "a", regio))

  n_punts <- nrow(hic_points) + nrow(legacy_points)

  legacy <- TRUE # Variable pel metadata final

 } else {

  log_msg(msg = paste("0 Punts ja mostrejats inclosos per l'HIC",
                        hic, "a", regio))
    
  n_punts <- nrow(hic_points)

  legacy = FALSE # Variable pel metadata final

                                }

# Comprovem si el nombre de punts disponibles es inferior o superior als que volem

# Més punts disponibles dels que volem: 

if (n_punts >= n_target + n_reserva) {
    
    log_msg(msg = paste("Punts disponibles:", n_punts, ">=", n_target + n_reserva,
                        ", aplicant GRTS:",
                        hic, regio))

   # SI hi ha punts mostrejats 
    if (legacy == TRUE) {
      
      n_remaining <- (n_target + n_reserva) - nrow(legacy_points)
      # Si encara queden punts per mostrejar

      if (n_remaining > 0) {
 
        points <- grts(hic_points,
                       n_base = n_target,
                       legacy_sites = legacy_points,
                       n_over = n_reserva,
                       mindis = min_dist,
                       maxtry = 100,
                       DesignID = paste0(hic,"_",regio)
        )

        points_bined <- sp_rbind(points) # combinem:
          # Punts legacy
          # Punts base
          # Punts reserva

        points_bal <- sp_balance(points_bined, hic_points)
        
        method <- "Malla_i_GRTS_amb_Legacy"
        path <- 1

        # Comprovem dist min: 
      
        m <- st_distance(points_bined)
        diag(m)<-Inf
        dist_sup_200 <- ifelse(min(m) > set_units(min_dist,m),
                TRUE,
                FALSE)

      } 
      else 
      # Si ja hi ha 36 punts mostrejats per HIC  
      {
        points <- legacy_points

        points_bal <- sp_balance(points, hic_points)

        method <- "Legacy_sup_30"
        path <- 2
         m <- st_distance(points)
        diag(m)<-Inf
        dist_sup_200 <- ifelse(min(m) > set_units(min_dist,m),
                TRUE,
                FALSE)        
      }

      #### Compute p-val of the spatial balance

      rand_bal <- pbreplicate(
                    n_iter,
                    points_random(
                      p_malla = hic_points,
                      n_target = n_target,
                      n_reserva = n_reserva,
                      legacy_points = legacy_points,
                      min_dist = min_dist
                    )
                  )

      p_value <- (sum(rand_bal <= points_bal$value) + 1) / (n_iter + 1)
      ses <- (points_bal$value - mean(rand_bal))/sd(rand_bal)
      mean_rand <- mean(rand_bal)

  # NO hi ha punts mostrejats    
    } else {
      
       points <- grts(hic_points,
                       n_base = n_target,
                       n_over = n_reserva,
                       mindis = min_dist,
                       maxtry = 100,
                       DesignID = paste0(hic,"_",regio))

      method <- "Malla_i_GRTS_no_legacy"
      path  <- 3

      points_bined <- sp_rbind(points)

      points_bal <- sp_balance(points_bined, hic_points)

      m <- st_distance(points_bined)
        diag(m)<-Inf
        dist_sup_200 <- ifelse(min(m) > set_units(min_dist,m),
                TRUE,
                FALSE)  
      
         rand_bal <- pbreplicate(
                    n_iter,
                    points_random(
                      p_malla = hic_points,
                      n_target = n_target,
                      n_reserva = n_reserva,
                      legacy_points = NULL,
                      min_dist = min_dist
                    )
                  )

      p_value <- (sum(rand_bal <= points_bal$value) + 1) / (n_iter + 1)
      ses <- (points_bal$value - mean(rand_bal))/sd(rand_bal)
      mean_rand <- mean(rand_bal)

    }

### Results when n_points >30 


log_msg(level = paste(hic, regio), msg = paste(hic, regio, legacy, method, points_bal$value, p_value, dist_sup_200, path))

n_legacy <- if (!is.null(legacy_points)) nrow(legacy_points) else 0

meta_results <- c(hic, regio, legacy, n_legacy, method, points_bal$value, mean(rand_bal), ses, p_value, dist_sup_200, path, n_punts)
names(meta_results) <- c("HIC", "Regio", "Legacy", "N_legacy", "Metode", "sp_balance_grts", "sp_balance_rand","sp_balance_SES", "sp_balance_p-val", "Dist_sup_200", "Ruta", "N_punts_disp") 

meta_tbl <- tibble(values = meta_results,
                      variables = names(meta_results),
                      HIC = hic, 
                      Regio = regio)
  return(list(
    points = points_bined,
    hic_points = hic_points,
    meta_points = meta_tbl))
}


### Si no hi ha 30 punts a la malla

   else {

    log_msg(level = "ATENCIÓ", msg = paste("Falta de punts per GRTS", hic, regio, ":", n_punts, "<", n_target + n_reserva))
    
### Generar punt central si un polígon no té punt: 

 idx <- st_intersects(pol_flt, hic_points)
 pol_no_points   <- pol_flt[lengths(idx) == 0, ]
 points_pol <- st_point_on_surface(pol_no_points)
  points_pol <- st_set_geometry(points_pol, "geometry")


  hic_points2 <- hic_points |> mutate(Point_origin = "Malla")
  points_pol <- points_pol |> mutate(Points_origin = "Centroid")
  
  points_sample <- bind_rows(hic_points2, points_pol)
###

path  <- 4
method <- "Malla_i_pol_sense_punts"

### Results when n_points < 30

n_legacy <- if (!is.null(legacy_points)) nrow(legacy_points) else 0

meta_results <- c(hic, regio, legacy, n_legacy, method, NA, NA, NA, NA, NA,path, nrow(points_sample))
names(meta_results) <- c("HIC", "Regio", "Legacy", "N_legacy", "Metode", "sp_balance_grts", "sp_balance_rand","sp_balance_SES","sp_balance_p-val", "Dist_sup_200", "Ruta", "N_punts_disp") 

log_msg(level = paste(hic, regio), msg = paste(hic, regio, legacy, method, dist_sup_200, path, nrow(points_sample)))


meta_tbl <- tibble(values = meta_results,
                      variables = names(meta_results),
                      HIC = hic, 
                      Regio = regio) 


return(list(
  points = points_sample,
  hic_points = hic_points,
  meta_points = meta_tbl
))

  }
                                }


### MARK: Funció per generar punts pels grups de CORINE



generate_points_grups <- function(pol,
                                points_cat,
                                grup,
                                legacy_points = NULL,
                                n_target = 30,
                                n_base = 6, 
                                min_dist = 199,
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

