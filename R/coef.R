#' Get regression coefficients from a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' coef(fit)
#' @export

coef.linreg <- function(x, ...) {

  return(x[["regression_coefficient"]])
}

