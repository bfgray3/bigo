#################
### plot.bigo ###
#################

#' @export
plot.bigo <- function(x, y, ...) {

  df <- x[["runtimes"]]

  if (ncol(df) == 2) {

    var_name <- setdiff(names(df), "elapsed")
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = var_name, y = "elapsed")) +
           ggplot2::geom_line() +
           ggplot2::labs(title = "",
                         x = "",
                         y = "",
                         subtitle = "")

    return(p)

  } else if (ncol(df) == 3) {

    var_names <- setdiff(names(df), "elapsed")
    p <- ggplot2::ggplot(df, ggplot2::aes(x = var_names[[1]], y = var_names[[2]], z = "elapsed")) +
      ggplot2::geom_contour() +
      ggplot2::labs(title = "",
                    x = "",
                    y = "",
                    subtitle = "")

    return(p)

  } else {

    stop("Cannot plot complexity when there are more than two variables.",
         call. = FALSE)

  }

}

###