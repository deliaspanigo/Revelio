
fn_import_special_yml <- function(selected_yml){

  selected_yml <- "menu01_summary.yml"
  the_package_name <- "Revelio"
  special_folder_package <- file.path("yml_menu")
  special_folder_local <- file.path("inst", "yml_menu")

  input_path_package <- base::system.file(package = the_package_name)
  input_folder_package <- file.path(input_path_package, special_folder_package)
  input_folder_local <- file.path(getwd(), special_folder_local)

  input_path_file_package <- file.path(input_folder_package, selected_yml)
  input_path_file_local <-   file.path(input_folder_local, selected_yml)

  selected_input_path_file <- c()
  if(file.exists(input_path_file_package)) selected_input_path_file <- input_path_file_packages else
    if(file.exists(input_path_file_local)) selected_input_path_file <- input_path_file_local


  output_yml <- yaml::read_yaml(selected_input_path_file)
  return(output_yml)

}
