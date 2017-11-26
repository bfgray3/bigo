##################
### print.bigo ###
##################

#' @export
print.bigo <- function(x, ...) {

  cat("Runtime for ", x[["function_name"]], ":\n\n", sep = "")
  print(x[["runtimes"]])

}

###