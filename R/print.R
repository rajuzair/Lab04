#' Print for a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' print(fit)
#' @export

print.linreg = function(x, ...) {

  cat("Call:\n")
  print(x[["call"]])
  cat("\nCoefficients: \n")
  print(x[["regression_coefficient"]])

}

print(linreg(Petal.Length~Species, data = iris))
