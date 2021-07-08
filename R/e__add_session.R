#' e__add_session
#'
#' @param sas_file_path TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_session <- function(sas_file_path,outer_env=totem) {
  session_name <- gsub(":", "_", paste0(Sys.time()))
  totem$all_sessions <- c(totem$all_sessions, session_name)
  outer_env[[session_name]] <- create_session_list(totem, sas_file_path)
  return(session_name)
}
