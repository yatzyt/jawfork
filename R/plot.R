# hist_plot <- function(x, var_name="", var_label=""){




#   if ((("character" %in% class(x)) & length(unique(x)) < 50) | ("character" %in% class(x))==F) {



#       if ((is.numeric(x)) & length(unique(x)) > 15) {

#         methods::show(ggplot2::ggplot(data.frame(xvar = as.numeric(x)), ggplot2::aes(xvar)) +
#           ggplot2::geom_histogram(color = "black", fill = "#583474", bins = 15,
#           na.rm = T) + ggplot2::ggtitle(var_name) + ggplot2::xlab(paste(strwrap(var_label,
#           width = 0.7 * getOption("width")), collapse = "\n")))
#       } else {

#         x <- as.factor(x)
#         x <- forcats:::fct_explicit_na(x, na_level = "(Missing)")

#         methods::show(ggplot2::ggplot(data.frame(xvar = x), ggplot2::aes(xvar)) +
#           ggplot2::geom_bar(color = "black", fill = "#583474", na.rm = F) +
#           ggplot2::ggtitle(var_name) + ggplot2::xlab(paste(strwrap(var_label,
#           width = 0.7 * getOption("width")), collapse = "\n")) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -15,
#           vjust = 1, hjust = 0)) + ggplot2::scale_x_discrete(limits = levels(x)))
#       }




#   }




# }
