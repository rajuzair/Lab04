#' Get fitted values from a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' pred(fit)
#' @export

pred <- function(x, ...) {
  return(x[["fitted_values"]])
}
#pred(linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris))
