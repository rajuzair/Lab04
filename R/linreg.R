#' @title Linreg
#'
#' @param results object list

linreg1 <- function(results = list()){
structure(results, class("linreg"))
# class(results)<-"linreg"
}

#' @param formula Model Formula
#' @param data Data Set
#'
#' @return linreg class object
#'
#' @export

linreg<-function(formula, data, ...){

  X<-model.matrix(formula, data)
  y_var<-all.vars(formula)[1]
  y<-as.numeric(data[[y_var]])

  QR<-qr(X)
  Q<-qr.Q(QR)
  R<-qr.R(QR)


  # calculate the regressions coefficients (beta_hat)
  # r_coeff<-as.numeric(solve.qr(QR, y))
  r_coeff<-qr.coef(QR, y)


  # calculate the fitted values (y_hat)
  f_values<-as.vector(X%*%r_coeff)

  # calculate the residuals (e_hat)
  residuals<-as.numeric(qr.resid(QR,y))

  # calculate the degrees of freedom (df)
  n<-nrow(data)
  p<-ncol(X)
  df<-n-p

  sigma2<-sum((y - X%*%r_coeff)^2)/df
  vcov<-(sigma2*chol2inv(QR$qr))

  # calculate the residual variance (sigma_hat_squared)
  residual_var<-as.numeric(t(residuals)%*%residuals/df)

  # calculate the variance of the regression coefficient (var_beta_hat)
  var_r_coeff<-residual_var*solve(t(R)%*%R)
  # print(var_r_coeff)

  t_values<-as.numeric(r_coeff/sqrt(diag(var_r_coeff)))
  p_values<-(2*pt(-abs(t_values), df))
  std_error <- sqrt(residual_var)
  call<-match.call()


  results<-list(
    "Call" = call,
    "Regression_coefficients" = r_coeff,
    "Residuals" = residuals,
    "Fitted_values" = f_values,
    "Degree_of_freedom" = df,
    "Residual_variance" = residual_var,
    "t_values" = t_values,
    "variance_of_regression_coefficient" = var_r_coeff,
    "p_values" = p_values,
    "vcov" = vcov,
    "Residual_std_error"= std_error,
    "Formula" = formula,
    "Dataname" = deparse(substitute(data))

  )
  # attr(results, "class") <- "linreg"
  # class(results)<-"linreg"

  return(linreg1(results))
}

#fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

#coef(fit)
#pred(mod)
#pred(fit)
#print(fit)
#print(fit)
#plot(fit)

#summary(mod)
