create_lagged_df <- function(dataset, vars, h) {
  # Verificar que las variables existan
  missing_vars <- setdiff(vars, colnames(dataset))
  if (length(missing_vars) > 0) {
    stop(paste("Las siguientes variables no están en el dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  # Verificar que 'Date' exista
  if (!"Date" %in% colnames(dataset)) {
    stop("El dataset debe contener una columna llamada 'Date'")
  }
  
  # Inicializar lista para almacenar columnas rezagadas
  lagged_data <- list(Date = dataset$Date)
  
  for (var in vars) {
    for (i in 1:h) {
      col_name <- paste0(var, "_L", i)
      lagged_data[[col_name]] <- dplyr::lag(dataset[[var]], n = i)
    }
  }
  
  # Convertir a data.frame
  lagged_df <- as.data.frame(lagged_data)
  return(lagged_df)
}

create_leads_df <- function(dataset, vars, h = 1, all_leads = TRUE) {
  # Validar que las variables existan
  missing_vars <- setdiff(vars, colnames(dataset))
  if (length(missing_vars) > 0) {
    stop(paste("Las siguientes variables no están en el dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  # Verificar que exista la columna "Date"
  if (!"Date" %in% colnames(dataset)) {
    stop("El dataset debe contener una columna llamada 'Date'")
  }
  
  # Inicializar con la columna Date
  leads_data <- list(Date = dataset$Date)
  
  for (var in vars) {
    if (all_leads) {
      for (i in 1:h) {
        lead_col <- dplyr::lead(dataset[[var]], n = i)
        col_name <- paste0(var, "_lead", i)
        leads_data[[col_name]] <- lead_col
      }
    } else {
      lead_col <- dplyr::lead(dataset[[var]], n = h)
      col_name <- paste0(var, "_lead", h)
      leads_data[[col_name]] <- lead_col
    }
  }
  
  # Devolver como data frame
  return(as.data.frame(leads_data))
}


convert_to_numeric <- function(df) {
  df <- as.data.frame(df)  # Asegura que sea data.frame
  
  for (col in names(df)) {
    # Intentar convertir a numérico
    suppressWarnings({
      num_col <- as.numeric(df[[col]])
      
      # Solo reemplaza si la conversión no produce NAs nuevos
      if (sum(!is.na(num_col)) >= sum(!is.na(df[[col]]))) {
        df[[col]] <- num_col
      }
    })
  }
  
  return(df)
}