#' Pred()
#'
#' @param results Aurgument
#' @param ... Additional Argument
#'
#' @return
#' @export
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' pred(mod)

pred<-function(results) UseMethod("pred")


pred <- function(results, ...) {
  return(results[["Fitted_values"]])
}
