
#' e__copy_if_then
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then <- function(session_name, current_row, df_obj,outer_env=totem) {
  column_classes <- df_obj$get_column_classes()
  column_values <- df_obj$get_column_values(current_row$column)
  if (column_classes[current_row$column] == "numeric") {
    sep <- ""
  } else {
    sep <- "\""
  }

  string_builder <- rep(NA, length(column_values) + 2)
  string_builder[1] <- paste0("if      missing(", current_row$column, ") then <var>=\"\";")
  j <- 2
  for (i in column_values) {
    string_builder[j] <- paste0("else if ", current_row$column, "=", sep, i, sep, " then <var>=\"\";")

    j <- j + 1
  }
  string_builder[j] <- paste0("else do;\n    err_msg=catx(\"|\",\"Error: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;")



  utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
}

#' e__copy_if_then_do
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then_do <- function(session_name, current_row, df_obj,outer_env=totem) {
  column_classes <- df_obj$get_column_classes()
  column_values <- df_obj$get_column_values(current_row$column)
  if (column_classes[current_row$column] == "numeric") {
    sep <- ""
  } else {
    sep <- "\""
  }

  string_builder <- rep(NA, length(column_values) + 2)
  string_builder[j] <- paste0("if      missing(", current_row$column, ") then do;\n    <var>=\"\";\nend;")
  j <- 2
  for (i in column_values) {
    string_builder[j] <- paste0("else if ", current_row$column, "=", sep, i, sep, " then do;\n    <var>=\"\";\nend;")

    j <- j + 1
  }
  string_builder[j] <- paste0("else do;\n    err_msg=catx(\"|\",\"Error: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;")


  utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
}
