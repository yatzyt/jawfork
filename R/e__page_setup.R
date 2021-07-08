



#' e__page_setup
#'
#' @param header_table TODO
#' @param row_1 TODO
#' @param obj_env TODO
#'
#' @return TODO

e__page_setup <- function(header_table, row_1,obj_env=inner_env) {
  page <- RGtk2::gtkHBox()

  RGtk2::gtkTableAttach(header_table,
    child = page, left.attach = 0, right.attach = 20,
    top.attach = row_1, bottom.attach = row_1 + 1, xoptions = 5,
    yoptions = 5, xpadding = 0, ypadding = 0
  )



  u__button(
    box = page,
    start = T, padding = 1,
    stock_id = "gtk-goto-first",
    tool_tip = "First record.",
    call_back_fct = function(widget, event, data) {
      obj_env<- data
      obj_env$page_obj$to_page(1)
      obj_env$df_obj$draw_table()
      return(T)
    },
    data = obj_env
  )


  u__button(
    box = page,
    start = T, padding = 1,
    stock_id = "gtk-go-back",
    tool_tip = "Back",
    call_back_fct = function(widget, event, data) {
      obj_env<- data
      obj_env$page_obj$prev_page()
      obj_env$df_obj$draw_table()
      return(T)
    },
    data = obj_env
  )


  page_entry <- RGtk2::gtkEntry()
  RGtk2::gtkEntrySetText(page_entry, "1")
  RGtk2::gtkBoxPackStart(page, page_entry, T, T, padding = 5)


  u__button(
    box = page,
    start = T, padding = 1,
    stock_id = "gtk-go-forward",
    tool_tip = "Forward",
    call_back_fct = function(widget, event, data) {
      obj_env<- data
      obj_env$page_obj$next_page()
      obj_env$df_obj$draw_table()
      return(T)
    },
    data = obj_env
  )


  u__button(
    box = page,
    start = T, padding = 1,
    stock_id = "gtk-goto-last",
    tool_tip = "Last record.",
    call_back_fct = function(widget, event, data) {
      obj_env<- data
      obj_env$page_obj$last_page()
      obj_env$df_obj$draw_table()
      return(T)
    },
    data = obj_env
  )


  set_dim <- function(txt) {
    RGtk2::gtkLabelSetLabel(dim_label, txt)
  }


  to_page <- function(num) {
    RGtk2::gtkEntrySetText(page_entry, paste0(num))
  }


  last_page <- function(obj_env=inner_env) {
    x <- as.integer(RGtk2::gtkEntryGetText(page_entry))
    y <- max(1, obj_env$df_obj$get_filtered_df_nrow())
    to_page(y)
  }

  next_page <- function(obj_env=inner_env) {
    x <- as.integer(RGtk2::gtkEntryGetText(page_entry))
    y <- max(1, min(obj_env$df_obj$get_filtered_df_nrow(), x + rows_length))
    to_page(y)
  }


  prev_page <- function(obj_env=inner_env) {
    x <- as.integer(RGtk2::gtkEntryGetText(page_entry))
    y <- max(1, x - rows_length)
    to_page(y)
  }


  clean_page <- function(obj_env=inner_env) {
    x <- as.integer(RGtk2::gtkEntryGetText(page_entry))
    if (is.na(x) | is.null(x)) {
      x <- 1
      to_page(x)
    }
    y <- max(1, min(obj_env$df_obj$get_filtered_df_nrow() , x))
    if (x != y) {
      to_page(y)
    }
  }

  get_page <- function() {
    return(as.numeric(RGtk2::gtkEntryGetText(page_entry)))
  }

  hide <- function() {
    RGtk2::gtkWidgetHide(page)
  }

  show <- function() {
    RGtk2::gtkWidgetShow(page)
  }


  RGtk2::gSignalConnect(page_entry, "activate", function(menu, obj_env=inner_env) {
    obj_env$page_obj$clean_page()
    obj_env$df_obj$draw_table()
    return(T)
  })


  return(list(
    to_page = to_page, last_page = last_page, next_page = next_page,
    prev_page = prev_page, clean_page = clean_page, get_page = get_page, set_dim = set_dim, hide = hide, show = show
  ))
}
