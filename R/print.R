#' print
#'
#' @param results Argument
#' @param ... Additional Argument
#'
#' @export
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' print(mod)


print<- function(results) UseMethod("print")

print.linreg <- function(results, ...) {
  cat("Call: \n")
  print(results$Call)
  cat("\nCoefficients: \n")
  print(results$Regression_coefficient)
}


# print.summary.linreg <- function(x, ...)
# {
#   cat("Call:\n")
#   print(x$Call)
#   cat("\n")
#   printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE)
#   res_std_error <- cat("Residual standard error: ", sqrt(sum(x$residuals^2)/x$degree_of_freedom), "on ",
#                        x$degree_of_freedom, "degrees of freedom \n")
# }
