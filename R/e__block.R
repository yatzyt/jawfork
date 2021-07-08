#' e__block
#'
#' @param loop_sleep TODO
#' @param outer_env TODO
#'
#' @return TODO

e__block <- function(loop_sleep = .01,outer_env=totem) {
  outer_env$while_loop_running <- T
  while (outer_env$while_loop_running) {

    Sys.sleep(loop_sleep)
  }

}
