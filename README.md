
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bigo

[![Build
Status](https://travis-ci.org/bfgray3/bigo.svg?branch=master)](https://travis-ci.org/bfgray3/bigo)

`bigo` (big-O) is an R package designed to make it quick and easy to
measure and visualize the runtime complexity of algorithms in your code.
It emphasizes empirical runtime measurements–in numerical values and
plots–rather than theoretical complexity analysis. This functionality is
useful for researchers evaluating their algorithms, students learning
computer science concepts, and many other R users.

## Installation

You can install `bigo` from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("bfgray3/bigo")
```

## Example

Below we deomonstrate the most basic functionality of `bigo` with a
simple Fibonacci function.

``` r
library(bigo)

fib <- function(n) {
 
  if (n < 1) {
    stop("`n` must be a positive integer.", call. = FALSE)
  } else if (n < 3) {
    return(1)
  } else {
    return(Recall(n - 1) + Recall(n - 2))
  }

}

plot(bigo(f = fib, n = seq(from = 2, to = 30, by = 2)))
```

![](man/figures/README-example-1.png)<!-- -->
