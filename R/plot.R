#' Plots for a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' plot(fit)
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab ggtitle coord_cartesian theme theme_void annotation_custom theme_classic element_rect element_text stat_summary_bin
#' @importFrom stats median
#' @importFrom grid roundrectGrob unit
#' @importFrom here here
#' @export


plot.linreg<- function(results, ...) {


  df_1 <- data.frame(results$Fitted_values, results$Residuals)

  p1 <- ggplot(df_1, aes(x = df_1[,1], y = df_1[,2])) +  # ggplot2::ggplot
    geom_point(shape = 1, size = 5) +
    stat_summary_bin(fun = median, geom = "line", color = "red")+
    xlab(paste0("Fitted values \n", "linreg(",deparse(results[["Formula"]]),")" )) + ylab("Residuals") +
    ggtitle("Residuals vs Fitted")+
    theme(plot.title= element_text(hjust = 0.5), axis.line = element_line((color='black')), plot.background = element_blank(),
          panel.grid = element_blank())



  # Standardized residual
  std_res <- results$Residuals/sqrt(results$Residual_variance)

  df_2 <- data.frame(results$Fitted_values, sqrt(abs(std_res)))
  p2 <- ggplot(df_2, aes(x = df_2[,1], y = df_2[,2])) +
    geom_point(shape = 1, size = 5) +
    stat_summary_bin(fun = mean, geom = "line", color = "red") +
    xlab(paste0("Fitted values \n", "linreg(",deparse(results[["Formula"]]),")" )) + ylab(expression(sqrt("Standarized Residuals"))) +
    ggtitle("Scale-Location")+
    theme(plot.title= element_text(hjust = 0.5), axis.line = element_line((color='black')), plot.background = element_blank(),
          panel.grid = element_blank())

  graph <- list(p1,p2)
  return(graph)

}


