

#' e__df_view
#'
#' @param df TODO
#' @param title TODO
#' @param height TODO
#' @param width TODO
#' @param outer_env TODO
#'
#' @return TODO

e__df_view <- function(df, title, height = 300, width = 500,outer_env=totem) {
  w <- RGtk2::gtkWindow(show = FALSE)
  w["title"] <- title

  table_box <- RGtk2::gtkVBox()
  RGtk2::gtkContainerAdd(w, table_box)


  RGtk2::gtkWidgetSetSizeRequest(w, width, height)

  custom_table <- outer_env$u__df_tree(
    session_name = "df view",
    passed_box = table_box,
    rows_length = 500,
    event_mapping = NULL,
    style_list = list(value = RGtk2::pangoFontDescriptionFromString("bold 10")),
    is_value_table = F, is_meta_table = F, is_data_code_table = F
  )
  custom_table$update(df)

  RGtk2::gtkWidgetShow(w)
}
