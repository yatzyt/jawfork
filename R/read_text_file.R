#' read_text_file
#'
#' @param file_path TODO
#'
#' @return TODO

read_text_file <- function(file_path) {
  x <- readChar(file_path, file.info(file_path)$size)
  return(gsub("\r\n", "\n", x))
}
