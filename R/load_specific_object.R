load_specific_object <- function(file, object) {
  env <- new.env()
  load(file, envir = env)
  return(env[[object]])
}
