select_hic_petits <- function(x){
     
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

}