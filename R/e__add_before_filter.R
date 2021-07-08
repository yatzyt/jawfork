
#' e__add_before_filter_full_data_bucket
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data_bucket <- function(session_name, current_row, exclude = F, outer_env = totem) {
  cross_tab_names <- current_row$column

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    temp_string <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$status_bar$box_bucket_entry)
    my_title[[i]] <- paste0(x, " %in% c(", temp_string, ")")




    i <- i + 1
  }

  if (exclude) {
    cmd <- paste0("df <- filter(df, (", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- filter(df, ", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, cmd)
}


#' e__add_before_filter_full_data_column
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data_column <- function(session_name, current_row, df_obj, exclude = F, outer_env = totem) {
  filtered_data <- df_obj$current_data()
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- current_row$column

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% c(\"", paste0(sort(unique(filtered_data[, x, drop = T])), collapse = "\", \""), "\")")
    } else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% c(", paste0(sort(unique(filtered_data[, x, drop = T])), collapse = ", "), ")")
    } else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% as.Date(c(\"", paste0(as.character(sort(unique(filtered_data[, x, drop = T]))), collapse = "\", \""), "\"))")
    } else if (lubridate::is.timepoint(temp_df[[x]])) {
      my_title[[i]] <- paste0("as.character(", x, ") %in% c(\"", paste0(as.character(sort(unique(filtered_data[, x, drop = T]))), collapse = "\", \""), "\")")
    }



    i <- i + 1
  }

  if (exclude) {
    cmd <- paste0("df <- filter(df, (", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- filter(df, ", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, cmd)
}



#' e__add_before_filter_full_data
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data <- function(session_name, current_row, exclude = F, outer_env = totem) {
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- current_row$column

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% c(\"", current_row$row[, x, drop = T], "\")")
    } else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% c(", current_row$row[, x, drop = T], ")")
    } else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
    } else if (lubridate::is.timepoint(temp_df[[x]])) {
      my_title[[i]] <- paste0("as.character(", x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
    }




    i <- i + 1
  }

  if (exclude) {
    cmd <- paste0("df <- filter(df, (", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- filter(df, ", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, cmd)
}

#' e__add_before_filter
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter <- function(session_name, current_row, exclude = F, outer_env = totem) {
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines"))

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% c(\"", current_row$row[, x, drop = T], "\")")
    } else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% c(", current_row$row[, x, drop = T], ")")
    } else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
    } else if (lubridate::is.timepoint(temp_df[[x]])) {
      my_title[[i]] <- paste0("as.character(", x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
    }
    i <- i + 1
  }


  if (exclude) {
    cmd <- paste0("df <- filter(df, (", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- filter(df, ", paste0(my_title, collapse = " & "), ")")
  }


  outer_env$u__append_before_code(session_name, cmd)
}



#' e__add_count_to_df_summary
#'
#' @param session_name TODO
#' @param cross_tab_names TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_count_to_df_summary <- function(session_name, cross_tab_names, outer_env = totem) {
  cmd <- paste0("df$n__1 <- add_cross_counts(df, c(\"", paste0(cross_tab_names, collapse = "\", \""), "\"))")
  outer_env$u__append_before_code(session_name, cmd)
}

#' e__append_before_code
#'
#' @param session_name TODO
#' @param cmd TODO
#' @param outer_env TODO
#'
#' @return TODO

e__append_before_code <- function(session_name, cmd, outer_env = totem) {
  source_file <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$file_source_cb)
  if (source_file == T) {
    outer_env$u__code_r_add_cmd(session_name, cmd)
  } else {
    u__text_area_append_text(outer_env[[session_name]]$text_area_1, cmd)
  }
}

#' e__set_before_code
#'
#' @param session_name TODO
#' @param cmd TODO
#' @param outer_env TODO
#'
#' @return TODO

e__set_before_code <- function(session_name, cmd, outer_env = totem) {
  u__text_area_set_text(outer_env[[session_name]]$text_area_1, cmd)
}
