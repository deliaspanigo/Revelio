
fn_replace_code_in_rmd <- function(rmd_file, old_code, new_code_vector) {
  # Leer el contenido del archivo .Rmd
  lines <- readLines(rmd_file, warn = FALSE)

  # Unir el vector en un formato adecuado (usualmente usando \n para saltos de línea)
  new_code <- paste(new_code_vector, collapse = "\n")  # Unir el vector con saltos de línea

  # Buscar y reemplazar el texto especificado
  lines <- gsub(old_code, new_code, lines)

  # Guardar los cambios en el mismo archivo .Rmd
  writeLines(lines, rmd_file)

  message("El archivo ", rmd_file, " ha sido actualizado: '", old_code, "' reemplazado por el nuevo código.")
}

