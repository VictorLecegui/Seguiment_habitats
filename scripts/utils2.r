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



