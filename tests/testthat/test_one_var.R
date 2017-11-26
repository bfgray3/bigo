context("one_var")

#############
### SETUP ###
#############

f1 <- function(n) {
  if (n < 3) {
    return(1)
  } else {
    Recall(n - 1) + Recall(n - 2)
  }
}

### NAMED FUNCTION ###
r1 <- bigo(f = f1, n = 1:15)
time1 <- r1[["runtimes"]]
name1 <- r1[["function_name"]]
runs1 <- r1[["num_runs"]]

### ANONYMOUS FUNCTION ###
r2 <- bigo(f = function(n) if (n < 3) 1 else { Recall(n - 1) + Recall(n - 2) },
           n = 1:15)
time2 <- r2[["runtimes"]]
name2 <- r2[["function_name"]]
runs2 <- r2[["num_runs"]]

###############
### RUNTIME ###
###############

test_that("Runtime tibble is right.", {

  expect_is(time1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(time1), c(15, 2))
  expect_equal(time1[["n"]], 1:15)
  expect_identical(names(time1), c("n", "elapsed"))

  expect_is(time2, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(time2), c(15, 2))
  expect_equal(time2[["n"]], 1:15)
  expect_identical(names(time2), c("n", "elapsed"))

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

###