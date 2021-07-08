#' e__all_menu_events
#'
#' @param mi TODO
#' @param data TODO
#' @param ... TODO
#'
#' @return TODO

e__all_menu_events <- function(mi, data, ...) {
  config_i <- data[[1]]
  item_i <- data[[2]]
  outer_env <- data[[3]]
  inner_env <- data[[4]]
  session_name <- data[[5]]
  event_mapping <- data[[6]]
  event <- data[[7]]


  if (config_i %in% names(outer_env$all_event_functions) ) {
    if (item_i %in% names(outer_env$all_event_functions[[config_i]]) ) {

    view_objects <- list(event_mapping = event_mapping, event = event)
    outer_env$all_event_functions[[config_i]][[item_i]](session_name, inner_env$table_objects_list$current_row, view_objects, outer_env = outer_env, obj_env = inner_env)

    }
  }
}
