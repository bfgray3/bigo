context("many_vars")

#############
### SETUP ###
#############

f1 <- function(n, m) {
  for(i in 1:n) {
    for (j in 1:m) {
      next
    }
  }
}

N <- 5
M <- 10

### NAMED FUNCTION ###
r1 <- bigo(f = f1, n = seq_len(N), m = seq_len(M))
time1 <- r1[["runtimes"]]
name1 <- r1[["function_name"]]
runs1 <- r1[["num_runs"]]

### ANONYMOUS FUNCTION ###
r2 <- bigo(f = function(n, m) for (i in seq_len(N)) { for (j in seq_len(M)) { next } },
           n = seq_len(N), m = seq_len(M))
time2 <- r2[["runtimes"]]
name2 <- r2[["function_name"]]
runs2 <- r2[["num_runs"]]

###############
### RUNTIME ###
###############

test_that("Runtime tibble is right.", {

  expect_is(time1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(time1), c(N * M, 3))
  expect_equal(time1[["n"]], rep(seq_len(N), times = M))
  expect_equal(time1[["m"]], rep(seq_len(M), each = N))
  expect_identical(names(time1), c("n", "m", "elapsed"))

  expect_is(time2, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(time2), c(N * M, 3))
  expect_equal(time2[["n"]], rep(seq_len(N), times = M))
  expect_equal(time2[["m"]], rep(seq_len(M), each = N))
  expect_identical(names(time2), c("n", "m", "elapsed"))

})

######################
### NUMBER OF RUNS ###
######################

test_that("Number of runs is right.", {

  expect_is(runs1, "numeric")
  expect_equal(runs1, 1)

  expect_is(runs2, "numeric")
  expect_equal(runs2, 1)

})

###################
### BIGO OBJECT ###
###################

test_that("Overall bigo object is right.", {

  expect_is(r1, "bigo")
  expect_equal(length(r1), 3)
  expect_identical(names(r1), c("runtimes", "function_name", "num_runs"))

  expect_is(r2, "bigo")
  expect_equal(length(r2), 3)
  expect_identical(names(r2), c("runtimes", "function_name", "num_runs"))

})

#####################
### FUNCTION NAME ###
#####################

test_that("Function name is right.", {

  expect_is(name1, "character")
  expect_identical(name1, "f1")

  expect_is(name2, "character")
  expect_identical(name2, "anonymous")

})

#############
### PLOTS ###
#############

test_that("Plot is right.", {

  p1 <- plot(r1)
  expect_is(p1, c("gg", "ggplot"))

  p2 <- plot(r2)
  expect_is(p2, c("gg", "ggplot"))

})

###