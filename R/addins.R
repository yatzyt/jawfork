#' io_sas_path_to_syntax
#'
#' @return TODO

io_sas_path_to_syntax <- function() {
  my_string <- clipr::read_clip()
  my_string <- gsub("\\\\", "/", my_string)
  my_string <- paste0(gsub(".sas7bdat\"", "", basename(my_string), fixed = T), " <- as.data.frame(haven::read_sas(data_file = ", my_string, "))")
  rstudioapi::insertText(my_string)
  message("Syntax inserted")
}
