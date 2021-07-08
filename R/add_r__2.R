#' add_r__2
#'
#' @param df TODO
#' @param var_name TODO
#'
#' @return TODO

add_r__2 <- function(df, var_name) {
  df[[var_name]] <- seq_len(nrow(df))
  df <- df[c(var_name, setdiff(colnames(df), var_name))]

  return(df)
}
