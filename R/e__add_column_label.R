#' e__add_column_label
#'
#' @param treeviewcolumn TODO
#' @param label TODO
#' @param j TODO
#' @param var_class TODO
#' @param tool_tip TODO
#' @param sec_label TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__add_column_label <- function(treeviewcolumn, label, j, var_class = NULL, tool_tip = NULL, sec_label,outer_env=totem,obj_env=inner_env) {
  color_header_1 <- "#9bb5f5"
  font.str <- "Serif, bold 9"
  font <- RGtk2::pangoFontDescriptionFromString(font.str)


  evb <- RGtk2::gtkEventBox()
  hb <- RGtk2::gtkVBox()
  RGtk2::gtkContainerAdd(evb, hb)


  if (is.null(var_class) == F) {
    if (var_class == "numeric") {
      x <- RGtk2::gtkLabel(paste0(label, " [n] "))
    } else if (toupper(var_class) == toupper("Date")) {
      x <- RGtk2::gtkLabel(paste0(label, " [d] "))
    } else if (toupper(var_class) == toupper("character")) {
      x <- RGtk2::gtkLabel(paste0(label, " [c] "))
    } else if (toupper(var_class) == toupper(paste0(class(as.POSIXct(Sys.time())), collapse = "/"))) {
      x <- RGtk2::gtkLabel(paste0(label, " [t] "))
    } else if (toupper(var_class) == toupper(paste0(class(as.POSIXlt(Sys.time())), collapse = "/"))) {
      x <- RGtk2::gtkLabel(paste0(label, " [t] "))
    } else {
      x <- RGtk2::gtkLabel(paste0(label, " "))
    }
    x$xalign <- 0
    RGtk2::gtkWidgetModifyFont(x, font)
    RGtk2::gtkBoxPackStart(hb, x, T, T, padding = 1)
    y <- RGtk2::gtkLabel(sec_label)
    RGtk2::gtkBoxPackStart(hb, y, F, F, padding = 1)
  } else {
    y <- RGtk2::gtkLabel("")
    x <- RGtk2::gtkLabel(paste0(label, " "))
    x$xalign <- 0
    RGtk2::gtkWidgetModifyFont(x, font)
    RGtk2::gtkBoxPackStart(hb, x, T, T, padding = 1)
  }


  if (is.null(var_class) == F) {
    if (var_class == "numeric") {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    } else {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    }
  } else {
    if ((j %% 2) == 0) {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    } else {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    }
  }

  if (is.null(tool_tip) == F) {
    RGtk2::gtkWidgetSetTooltipText(evb, tool_tip)
  }

  RGtk2::gtkTreeViewColumnSetWidget(treeviewcolumn, widget = evb)

  return(list(evb = evb, y = y))
}
