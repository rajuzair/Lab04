#' Get a summary of a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' summary(fit)
#' @export
#'
#
summary.linreg <- function(object, ...) {

   smry<- as.data.frame(object[["regression_coefficient"]])
   smry[,1]<-round(as.numeric(object[["regression_coefficient"]]), 5)
   smry[,2]<-round(as.numeric(object[["std_error"]]), 5)
   smry[,3]<-round(as.numeric(object[["t_values"]]), 3)
   smry[,4]<-round(as.numeric(object[["p_values"]]), 3)
   smry[,5]<-sapply(object[["p_values"]],
                    function(x) if(x<0.001) {"***"}
                    else if (x<0.01) {"**"}
                    else if (x<0.05) {"*"}
                    else if (x<0.1) {"."}
                    else {""})
   cat("Call: \n")
   print(object[["call"]])
   colnames(smry)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)", " ")
   cat("\nCoefficients: \n")
   print(smry)
   cat("---\n")
   cat(paste("Residual standard error: ", round(object[["se"]], 7), "on ",
       object[["degree_of_freedom"]], "degrees of freedom \n"))

}

summary(linreg(formula = Petal.Length ~ Species, data = iris))
