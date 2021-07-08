#' logger
#'
#' @param msg TODO
#' @param jaw_e TODO
#' @param append TODO
#'
#' @return TODO

logger <- function(msg, jaw_e, append = T) {
  cat(paste0(Sys.time(), "\t", msg, "\n"), file = jaw_e$jaw_log_path, append = append)
}
