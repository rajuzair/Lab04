#' Get residuals from a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @description Issue with the resid function: Does not work as resid.linreg for some odd reason. Must mask from stats package
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' resid(fit)
#' @export

residuals.linreg <- function(x, ...) {

  return(x[["residuals"]])
}

resid(linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris))

