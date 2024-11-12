
# install.packages("devtools")
# install.packages("roxygen2")
# Activar la libreria para instalar
library("remotes")

# Instalar Rscience desde la librería desde GitHub
remotes::install_github("deliaspanigo/Revelio", force = T)

# Estoy adentro de la carpeta del nuevo paquete!

# Indico que aqui desarrollare mi paquete
# devtools::create(".")

dir.create("tests")
cat("
library(testthat)
test_that('miFuncion multiplica correctamente', {
  expect_equal(miFuncion(2), 4)
  expect_equal(miFuncion(3), 6)
})
", file = "tests/test-miFuncion.R")


dir.create("www")
dir.create("inst")
dir.create("inst/rstudio")
dir.create("inst/www")
dir.create("inst/extdata")
dir.create("data")
dir.create("data-raw")
dir.create("resources")

library("devtools")
# Construir y revisar el paquete
devtools::load_all()

# devtools::document()

devtools::build()

devtools::check()

devtools::install()
