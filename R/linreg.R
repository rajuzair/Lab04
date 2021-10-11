#' Linear Regression Model using QR decomposition
#'
#' @param formula A model formula equation
#' @param data A data set
#' @description It compute multiple regression model values to determine statistical parameters.
#' @return Multiple regression coefficients
#' @examples
#' lm<- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' @import stats
#' @export

linreg <- function(formula, data) {

  ##Model Matrix
  X<-model.matrix(formula, data)
  y_var<-all.vars(formula)[1]
  y<-data[[y_var]]


  # QR <-qr(x = X) # Compute the QR decomposition
  # Q <- qr.Q(qr = QR) # Returns the Q from our decomposition
  # R <- qr.R(qr = QR) # Returns the R from out decomposition

  QR<-qr(X)
  Q<-qr.Q(QR)
  R<-qr.R(QR)

  # Model coefficients
  # beta_hat <- as.numeric(backsolve(r = R, x = crossprod(Q, Y)))
  # names(beta_hat) <- colnames(X) # Name the vector for use in print() and other functions

  #calculating the regressions coefficients (beta_hat)
  #r_coeff<-solve.qr(QR, y)
  r_coeff<-qr.coef(QR, y)

  # calculate the fitted values (y_hat)
  f_values<-as.numeric(X%*%r_coeff)
  # print(f_values)


  # Fitted values
  # fit_val <- as.numeric(X %*% beta_hat)
  # names(fit_val) <- as.character(1:length(Y))

  # calculating the residuals (e_hat)
  residuals<-as.numeric(qr.resid(QR,y))


  # Residuals, degrees of freedom and variance of the residuals
  residual <- as.numeric(y - X %*% r_coeff)
  # names(residual) <- as.character(1:length(Y))

  # calculate the degrees of freedom (df)
  n<-nrow(data)
  p<-ncol(X)
  df<-n-p

  #df <- nrow(data) - ncol(X)

  residual_var<- as.numeric((t(residuals) %*% residuals) / df)
  #residual_var<-residual_var[1,1]
  #resid_Std_E<- sqrt(residual_var)

  resid_var <- as.numeric((t(residual) %*% residual) / df)
  resid_SE <- sqrt(resid_var)

  residual_std <- residuals / sqrt((1/(nrow(X)-1)) * sum(residuals^2))
  residual_std_sqrt <- sqrt(abs(residual_std))

  # calculate the variance of the regression coefficient (var_beta_hat)



  beta_var <- residual_var * chol2inv(R)
  beta_SE <- sqrt(diag(beta_var))

  #var_r_coeff<-residual_var*solve(t(R)%*%R)
  beta_hat <- as.numeric(backsolve(r = R, x = crossprod(Q, y)))
  t_values <- beta_hat / beta_SE
  p_values <- 2 * pt(q = -abs(t_values), df)
  # t_values<-r_coeff/sqrt(diag(var_r_coeff))
  # p_values<- pt(-abs(t_values), df)

  sigma2<-sum((y - X%*%r_coeff)^2)/df
  vcov<-sigma2*chol2inv(QR$qr)
  std_error <- sqrt(diag(vcov))

  call <- match.call()

  results <- list(
    "call" = call,
    "regression_coefficient" = r_coeff,
    "fitted_values" = f_values,
    "residuals" = residuals,
    "degree_of_freedom" = df,
    "residual_variance" = residual_var,
    "Coefficient variance" = beta_var,
    "Coefficient standard deviation" = beta_SE,
    "t_values" = t_values,
    "p_values" = p_values,
    "Model matrix" = X,
    "Dependent variable" = y,
    "formula" = formula,
    "data" = data,
    "Sqrt_std_residuals" = residual_std_sqrt,
    "std_error" = std_error,
    "se" = resid_SE
  )
  class(results) <- "linreg"
  return(results)
}
