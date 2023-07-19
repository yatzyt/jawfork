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

  #y <- temp_df[my_filter, , drop = F]
  
  # Begin JNEFF code, I do not even want to touch anything above
  col <- temp_df[[current_row$column]]
  Statistic <- c("N", "Mean (SD)", "Median", "(Q1, Q3)", "Min, Max")
  quantiles <- quantile(col, prob = c(0.50, 0.25, 0.75, 0.00, 1.00), type = 1, na.rm = T, names = F)
  Value <- as.character(c(sum(!is.na(col)), paste0(round(mean(col, na.rm = T), digits = 4), " (", round(sd(col, na.rm = T), digits = 4), ")"), quantiles[1], paste0("(", quantiles[2], ", ", quantiles[3], ")"), paste0(quantiles[4], ", ", quantiles[5])))
  
  group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)
  
  if (group_by_entry != "") {
    clipr::write_clip(group_by_entry)
    Output <- temp_df %>% group_by(group_by_entry) %>% summarise(Mean = mean(current_row$column, na.rm = T)
    
    y <- data.frame(Output)
  } else {
    Groupby <- c("Empty")
    
    y <- data.frame(Statistic, Value, Groupby)
  }
  
  

  outer_env$u__df_view(y,
    paste0(current_row$column, " Summary: ", outer_env[[session_name]]$sas_file_basename, " | ", as.character(Sys.time())),
    height = 300, width = 500
  )
}

