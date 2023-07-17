#' e__get_summary
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__get_summary <- function(session_name, current_row,outer_env=totem) {
  temp_df <- outer_env[[session_name]]$data2
  temp_df$random_char_str <- "a"
  temp_df2 <- as.matrix(temp_df)
  temp_df <- temp_df[, -ncol(temp_df)]
  temp_df2 <- temp_df2[, -ncol(temp_df2)]

  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines"))

  my_filter <- rep(T, nrow(temp_df))
  my_title <- ""

  for (x in cross_tab_names) {
    my_title <- paste0(my_title, "| ", x, "==", current_row$row[, x, drop = T])
    my_filter <- my_filter & (temp_df2[, x, drop = T] %in% current_row$row[, x, drop = T])
  }
  
  # Begin JNEFF code, I do not even want to touch anything above

  #y <- temp_df[my_filter, , drop = F]
  col <- temp_df[[current_row$column]]
  Statistic <- c("Mean")
  Value <- c(mean(col, na.rm = T))
  y <- data.frame(Statistic, Value)
  y <- rbind(y, c("SD", sd(col, na.rm = T))
  y <- rbind(y, c("Median", median(col, na.rm = T))
  

  outer_env$u__df_view(y,
    paste0("Summary: ", outer_env[[session_name]]$sas_file_basename, " (", nrow(y), " x ", ncol(y), ")", my_title, "|", as.character(Sys.time())),
    height = 300, width = 500
  )
}

