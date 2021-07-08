#' df_meta
#'
#' @param df  TODO
#'
#' @return TODO

df_meta <- function(df) {
  meta <- as.data.frame(matrix(NA, nrow = ncol(df), ncol = 6))
  for (i in seq_len(ncol(df))) {
    meta[i, 1] <- colnames(df)[i]
    if (length(sjlabelled::get_label(df[[colnames(df)[i]]])) > 0) {
      meta[i, 2] <- sjlabelled::get_label(df[[colnames(df)[i]]])
    }
    meta[i, 3] <- paste0(class(df[[colnames(df)[i]]]), collapse = "/")
    meta[i, 4] <- length(unique(df[[colnames(df)[i]]]))
    meta[i, 5] <- sum(is.na(df[[colnames(df)[i]]]))
    meta[i, 6] <- sum(df[[colnames(df)[i]]] %in% "")
  }
  colnames(meta) <- c("variable", "label", "class", "unique", "missing", "blank")
  return(meta)
}


#' df_meta_contents
#'
#' @param df  TODO
#' @param contents  TODO
#'
#' @return TODO

df_meta_contents <- function(df, contents) {
  meta <- df_meta(df)
  # meta <- meta[meta$variable !="f__",]

  meta <- dplyr::left_join(meta, contents[c("variable", "length")], by = "variable")
  meta$distinct <- (nrow(df) == meta$unique) * 1
  meta <- meta[c("variable", "label", "class", "length", "distinct", "unique", "missing", "blank")]

  return(meta)
}
