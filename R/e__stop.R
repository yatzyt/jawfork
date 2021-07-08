#' e__stop
#'
#' @param outer_env TODO
#'
#' @return TODO

e__stop <- function(outer_env=totem) {
  all_sessions <- outer_env$all_sessions
  for (session_name in all_sessions) {
    outer_env$close_all_windows(session_name)
  }
}
