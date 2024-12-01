
# Función para extraer código R de un archivo .Rmd
fn_extract_r_code <- function(rmd_file, chunk_names = NULL) {
  # Leer el archivo .Rmd
  lines <- readLines(rmd_file)

  # Inicializar variables
  in_chunk <- FALSE
  chunk_code <- list()
  current_chunk <- NULL

  # Recorrer las líneas del archivo
  for (line in lines) {
    # Detectar el inicio de un chunk
    if (grepl("^```\\{r", line)) {
      in_chunk <- TRUE
      current_chunk <- sub("^```\\{r\\s*(\\w*).*", "\\1", line)
      chunk_code[[current_chunk]] <- c()
    }

    # Detectar el final de un chunk
    if (grepl("^```$", line) && in_chunk) {
      in_chunk <- FALSE
      current_chunk <- NULL
    }

    # Si estamos dentro de un chunk, agregar el código
    if (in_chunk && !grepl("^```\\{r", line)) {
      chunk_code[[current_chunk]] <- c(chunk_code[[current_chunk]], line)
    }
  }

  # Filtrar los chunks si se especificaron nombres de chunks
  if (!is.null(chunk_names)) {
    chunk_code <- chunk_code[chunk_names]
  }

  return(chunk_code)
}

