fn_extract_r_chunks <- function(file_path, start_chunk = NULL, end_chunk = NULL) {
  # Leer el contenido del archivo .qmd
  file_content <- readLines(file_path)

  # Identificar los chunks de código R
  chunk_start_indices <- grep("^```\\{r", file_content)
  chunk_end_indices <- grep("^```$", file_content)

  # Si no se especifican start_chunk y end_chunk, extraer todos los chunks
  if (is.null(start_chunk) && is.null(end_chunk)) {
    start_chunk <- 1
    end_chunk <- length(chunk_start_indices)
  }

  # Verificar que los índices de inicio y fin sean válidos
  if (length(chunk_start_indices) < start_chunk || length(chunk_end_indices) < end_chunk) {
    stop("Los números de chunk especificados están fuera del rango.")
  }

  # Extraer el código de los chunks especificados
  extracted_code <- character()
  for (i in start_chunk:end_chunk) {
    start_index <- chunk_start_indices[i]
    end_index <- chunk_end_indices[which(chunk_end_indices > start_index)[1]]
    extracted_code <- c(extracted_code, file_content[(start_index + 1):(end_index - 1)])
  }

  # Unir el código extraído en una sola cadena de texto
  extracted_code <- paste(extracted_code, collapse = "\n")

  return(extracted_code)
}
