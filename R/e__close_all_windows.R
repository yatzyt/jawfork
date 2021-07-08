#' e__close_all_windows
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

e__close_all_windows <- function(session_name,outer_env=totem) {
  totem$all_sessions <- setdiff(totem$all_sessions, session_name)
  if (length(totem$all_sessions) == 0) {
    # try({
    #   gSourceRemove(totem$loop_function_obj)
    #   totem$loop_function_obj <- F
    #   message("Jaw shut down")
    # })
    # assign("totem_running", F, envir = .GlobalEnv)
    outer_env$while_loop_running <- F
  }
  # outer_env[[session_name]]$dialog$main_content_window$destroy()


  fd_allocation <- RGtk2::gtkWidgetGetAllocation(outer_env[[session_name]]$data_view_list$bottom_tables_box)$allocation


  main_position <- RGtk2::gtkPanedGetPosition(outer_env[[session_name]]$data_view_list$main_paned)

  totem$settings_list$default_sizes$main_pane <- main_position


  top_position <- RGtk2::gtkPanedGetPosition(outer_env[[session_name]]$data_view_list$top_paned)

  totem$settings_list$default_sizes$top_pane <- top_position


  slot_position <- RGtk2::gtkPanedGetPosition(outer_env[[session_name]]$data_view_list$paned)

  totem$settings_list$default_sizes$slot_pane <- slot_position



  allocation <- RGtk2::gtkWidgetGetAllocation(outer_env[[session_name]]$windows$main_window)$allocation
  totem$settings_list$default_sizes$window <- c(allocation$width, allocation$height)

  RGtk2::gtkWidgetDestroy(outer_env[[session_name]]$windows$main_window)
  RGtk2::gtkWidgetDestroy(outer_env[[session_name]]$past_code_window)
  outer_env[[session_name]] <- NULL
  save_settings(totem)
}
