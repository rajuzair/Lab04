#' summary()
#'
#' @param results Argument
#' @param ... Additional Argument
#'
#' @return Return the summary of linreq
#'
#' @export
#'
#' @examples
#' mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' summary(mod)

summary<-function(results,...) UseMethod("summary")
#' @param results
#' @title summary
#' @param ...
#' @return summary
#' @export

summary.linreg <- function(object, ...) {

  #print(results$vcov)
   std_error <- sqrt(diag(object$vcov))
   std_errr<-unname(std_error)
   #print(std_error)
   regression_coefficient<-unname(object$Regression_coefficient)
   t_values<- unname(object$t_values)
   p_values<-unname(object$p_values)

   coefficient = matrix(c(regression_coefficient, std_error, t_values, p_values),
                        nrow = 3,
                        ncol = 4,
                        dimnames = list(c("(Intercept)", "Speciesversicolor", "Speciesvirginica"),
                                        c("Estimate", "Std. Error", "t values", "p values")))
   print(coefficient)

   res_std_error <- cat("Residual standard error: ", sqrt(sum(object$Residuals^2)/object$Degree_of_freedom), "on ",
                        object$Degree_of_freedom, "degrees of freedom \n")



  }
