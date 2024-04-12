
#' e__table_obj_function_df2
#'
#' @param df TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__table_obj_function_df2 <- function(df, outer_env = totem,obj_env=inner_env) {



  if (nrow(df) == 0) {
    df2 <- matrix("#ffffff", ncol = 2, nrow = nrow(df))
    colnames(df2) <- c("f___1", "f___2")
    return(df2)
  }


  df2 <- matrix("#ffffff", ncol = 2, nrow = nrow(df))


  if ("format_by_entry" %in% names(outer_env[[session_name]])) {
    format_var <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$format_by_entry)
  } else {
    format_var <- "USUBJID"
  }



  if (format_var %in% colnames(df)) {
    usubjid_levels <- as.numeric(as.factor(df[, format_var, drop = T]))
    usubjid_levels[is.na(usubjid_levels)] <- -98

    usubjid_levels2 <- c(-99, usubjid_levels[1:(length(usubjid_levels) - 1)])
    usubjid_levels <- cumsum((usubjid_levels != usubjid_levels2) * 1)



    tryCatch(
      {
        df2[, 2] <- ifelse((usubjid_levels %% 2) == 0, ifelse((1:nrow(df) %% 2) == 0, "#fcf7e8", "#f4efe1"),
          ifelse((1:nrow(df) %% 2) == 0, "#e8edfc", "#e1e5f4")
        )
      },
      error = function(e) {
        df2[, 2] <- ifelse((1:nrow(df) %% 2) == 0, "#F1F1F1", "#FFFFFF")
      }
    )
  } else {
    df2[, 2] <- ifelse((1:nrow(df) %% 2) == 0, "#F1F1F1", "#FFFFFF")
  }
  df2[, 1] <- "#9bb5f5"


  colnames(df2) <- c("f___1", "f___2")
  return(df2)
}


#' e__table_obj_function
#'
#' @param box TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__table_obj_function <- function(box, outer_env = totem,obj_env=inner_env) {

  obj_env$table_objects_list <- list()
  obj_env$table_objects_list$current_row <- NA


  obj_env$table_objects_list$inner_box <- RGtk2::gtkVBox()
  RGtk2::gtkBoxPackStart(box, obj_env$table_objects_list$inner_box, T, T)


  obj_env$table_objects_list$current_columns <- c("x")
  obj_env$table_objects_list$raw_df <- data.frame("x" = character())
  obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(obj_env$table_objects_list$raw_df)
  obj_env$table_objects_list$view <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)
  obj_env$table_objects_list$allColumns <- vector("list", 1)

  update_table <- function(df) {


    if ((paste0(obj_env$table_objects_list$current_columns, collapse = "|") == paste0(colnames(df), collapse = "|")) == F) {
      obj_env$table_objects_list$current_columns <- colnames(df)

      df2 <- obj_env$table_obj_function_df2(df)
      df <- cbind(df, df2)

      RGtk2::gtkWidgetDestroy(obj_env$table_objects_list$inner_box)
      obj_env$table_objects_list$inner_box <- RGtk2::gtkVBox()
      RGtk2::gtkBoxPackStart(box, obj_env$table_objects_list$inner_box, T, T)

      obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(df)

      obj_env$table_objects_list$view <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)

      # selection <- RGtk2::gtkTreeViewGetSelection(view)
      # RGtk2::gtkTreeSelectionSetMode(selection, 'multiple')


      # RGtk2::gtkTreeViewSetGridLines(view, "horizontal")

      RGtk2::gtkTreeViewSetFixedHeightMode(obj_env$table_objects_list$view, F)
      obj_env$table_objects_list$allColumns <- vector("list", ncol(df) - 2)
      for (j in seq_len(ncol(df) - 2)) {
        tmp <- obj_env$new_tree_view_column(df, j)
        RGtk2::gtkTreeViewAppendColumn(obj_env$table_objects_list$view, tmp$column)
        obj_env$table_objects_list$allColumns[[j]] <- tmp
      }


      selectedColor <- RGtk2::as.GdkColor(c(198, 213, 253) * 256) # Linux

      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], "#e7e3cd")
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], "#e7e3cd")
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], RGtk2::as.GdkColor("black"))
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], RGtk2::as.GdkColor("black"))


      ## basic GUI
      sw <- RGtk2::gtkScrolledWindow()

      RGtk2::gtkScrolledWindowSetPolicy(sw, "automatic", "automatic")
      RGtk2::gtkContainerAdd(sw, obj_env$table_objects_list$view)

      RGtk2::gtkBoxPackStart(obj_env$table_objects_list$inner_box, sw, T, T)



      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "button-press-event", obj_env$tree_view_column_btn_press, data = obj_env)
    } else {
      obj_env$table_objects_list$raw_df <- df




      df2 <- obj_env$table_obj_function_df2(df)


      df <- cbind(df, df2)


      obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(df)


      RGtk2::gtkTreeViewSetModel(obj_env$table_objects_list$view, obj_env$table_objects_list$model)
      RGtk2::gtkTreeViewColumnsAutosize(obj_env$table_objects_list$view)
      if (is_full_data_table) {
        for (j in setdiff(seq_len(ncol(df) - 2), 1)) {
          data3 <- outer_env[[session_name]]$data3
          my_row <- data3[j - 1, ]
          my_tool_tip <- paste0(
            my_row[, "label"], "\nLength: ", my_row[, "length"],
            "\nClass: ", my_row[, "class"], "\nDistinct: ", my_row[, "distinct"],
            "\nUnique: ", my_row[, "unique"], "\nMissing: ", my_row[, "missing"],
            "\nBlank: ", my_row[, "blank"]
          )

          RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0("U: ", my_row[, "unique"]))
          RGtk2::gtkWidgetSetTooltipText(obj_env$table_objects_list$allColumns[[j]]$evt$evb, my_tool_tip)
          if (my_row[, "class"] == "numeric") {
            RGtk2::gtkWidgetModifyBg(object = obj_env$table_objects_list$allColumns[[j]]$evt$evb, state = "normal", color = "#FFFFFF")
          } else {
            RGtk2::gtkWidgetModifyBg(object = obj_env$table_objects_list$allColumns[[j]]$evt$evb, state = "normal", color = "#FFFFFF")
          }
        }
      }
    }
  }





  clear_filters <- function() {
    obj_env$filter_obj$clean_inner()
  }


  clear_arrange <- function() {
    obj_env$order_by_obj$clean_inner()
  }

  clear_select <- function() {
    obj_env$select_obj$clean_inner()
  }

  add_select <- function(txt) {
    obj_env$select_obj$add(txt)
  }





  get_current_row <- function() {
    return(obj_env$table_objects_list$current_row)
  }
  hide <- function() {
    RGtk2::gtkWidgetHide(obj_env$table_objects_list$inner_box)
    return(T)
  }
  show <- function() {
    RGtk2::gtkWidgetShow(obj_env$table_objects_list$inner_box)
    return(T)
  }


  return(list(
    update_table = update_table,
    clear_filters = clear_filters, clear_arrange = clear_arrange, clear_select = clear_select, add_select = add_select, get_current_row = get_current_row, hide = hide, show = show
  ))
}
