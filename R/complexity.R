############
### time ###
############

.time <- function(.expr) {
  elapsed <- system.time(.expr)
  elapsed[["elapsed"]]
}

###############
### .f_time ###
###############

.f_time <- function(..., .fun) {
  .time(.fun(...))
}

############
### bigo ###
############

#' Big-O
#'
#' Analyze the algorithmic complexity of a function with respect to arguments.
#'
#' This function returns an object of class \code{bigo}, which contains a
#' tibble of runtime results, the name of the function being measured, and
#' the number of runs over which the results are averaged.
#'
#' @param f The function to be investigated.
#' @param ... One or more numeric or integer vectors, passed as named
#'   arguments, where the names are arguments of \code{f}.
#' @param num_runs The number of experiments over which to average results.
#' @return A tibble of class \code{bigo} with containing the results of the
#'   runtime experiments
#' @examples
#' bigo(f = function(n) if (n < 3) 1 else { Recall(n - 1) + Recall(n - 2) },
#'      n = 1:15)
#' @export
bigo <- function(f, ..., num_runs = 1) {

  ################
  ### FUNCTION ###
  ################

  f <- match.fun(f)
  f_args <- names(formals(f))
  f_char <- deparse(substitute(f))

  #################
  ### ARGUMENTS ###
  #################

  arg_ranges <- list(...)
  if (! length(arg_ranges)) {
    stop("Must pass at least one named argument for complexity evaluation.",
         call. = FALSE)
  }

  passed_args <- names(arg_ranges)
  if (any(passed_args == "") || is.null(passed_args)) {
    stop("All arguments for complexity evaluation must be named.",
         call. = FALSE)
  }

  #######################################
  ### CHECK FOR VALIDITY OF ARGUMENTS ###
  #######################################

  invalid_args <- setdiff(passed_args, f_args)

  if (length(invalid_args)) {
    stop("Passed ", paste(invalid_args, collapse = ", "),
         " to be evaluated for complexity, but ",
         if (length(invalid_args) > 1) "they are " else "it is ",
         "not an argument of `", f_char, "`.", call. = FALSE)
  }

  #######################
  ### FIND COMPLEXITY ###
  #######################

  runtime_results <- arg_ranges %>%
                       purrr::map(floor) %>%
                       expand.grid() %>%
                       mutate(elapsed = purrr::pmap_dbl(., .f_time, .fun = f))

  results <- list(runtimes = runtime_results,
                  function_name = f_char,
                  num_runs = num_runs)

  class(results) <- c("bigo")

  results

}

###