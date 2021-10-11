#' Residual()
#'
#' @param results Arguments
#' @param ... Additional Arguments
#'
#' @export
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' resid(mod)

resid<-function(results) UseMethod("resid")


resid<- function(results, ...) {

  return(results[["Residuals"]])
}
