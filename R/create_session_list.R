



#' create_session_list
#'
#' @param totem TODO
#' @param sas_file_path TODO
#'
#' @return TODO

create_session_list <- function(totem, sas_file_path) {
  session_list <- list()

  ls_content <- ls(name = .GlobalEnv)
  if ((sas_file_path %in% ls_content) == F) {
    session_list$sas_file_path <- sas_file_path
    session_list$sas_file_basename <- basename(sas_file_path)
  } else {
    session_list$sas_file_path <- sas_file_path
    session_list$sas_file_basename <- sas_file_path
  }

  session_list$e <- new.env()
  session_list$data1_contents <- NULL
  session_list$data1 <- NULL
  session_list$data2 <- NULL
  session_list$data3 <- NULL
  session_list$data4 <- NULL
  session_list$data_row_num <- NULL

  return(session_list)
}
