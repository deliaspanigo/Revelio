

# Asegúrate de que los paquetes necesarios estén instalados y cargados
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}

library(openxlsx)
library(usethis)

# Comprobar si el directorio existe
if (!dir.exists("./data-raw")) {
  stop("El directorio './data-raw' no existe.")
}

# Listar los archivos en el directorio especificado
vector_paths <- list.files("./data-raw", full.names = TRUE, recursive = TRUE)

# Filtrar los archivos que terminan en .xlsx
vector_check_xlsx <- grepl("\\.xlsx$", vector_paths)
vector_xlsx_files <- vector_paths[vector_check_xlsx]

# Iterar sobre los archivos .xlsx
for (x in seq_along(vector_xlsx_files)) {

  # x <- 1
  the_path <- vector_xlsx_files[x]

  # Leer el archivo .xlsx
  base_cargada <- openxlsx::read.xlsx(xlsxFile = the_path, sheet = 1)

  # Obtener el nombre del objeto sin la extensión
  nombre_en_R <- tools::file_path_sans_ext(basename(the_path))

  # Asignar el objeto a un nombre en el entorno global
  armado01 <- paste0("assign('", nombre_en_R, "', base_cargada, envir = .GlobalEnv)")

  eval(parse(text = armado01))

  # Utilizar usethis::use_data para registrar el objeto en el paquete
  armado02 <- paste0("usethis::use_data(list = ", nombre_en_R, ",overwrite = TRUE)")
  eval(parse(text = armado02))

  # Limpiar el entorno
  rm(list = nombre_en_R, envir = .GlobalEnv)
}

# Mensaje de éxito
cat("Todos los archivos .xlsx se han procesado correctamente.\n")
