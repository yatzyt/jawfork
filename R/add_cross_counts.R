#' add_cross_counts
#'
#' @param df TODO
#' @param group_by_vars TODO
#'
#' @return TODO

add_cross_counts <- function(df, group_by_vars) {
  fcount_df <- df %>%
    group_by(!!!syms(group_by_vars)) %>%
    summarise(u_________n = n())

  df <- left_join(df, fcount_df, by = group_by_vars)


  return(df$u_________n)
}
