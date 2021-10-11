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

plot.linreg <- function(x, ...){
  df<- data.frame(x[["residuals"]],
                  x[["fitted_values"]],
                  x[["Sqrt_std_residuals"]])

  p1<- ggplot2::ggplot(df, ggplot2::aes(y = df[,1], x = df[,2])) +
    ggplot2::geom_point(shape = 1, size = 5) +
    ggplot2::stat_summary_bin(fun = median, geom = "line", color = "red")+
    ggplot2::xlab(paste0("Fitted values \n", "linreg(",deparse(x[["formula"]]),")" )) + ggplot2::ylab("Residuals") +
    ggplot2::ggtitle("Residuals vs Fitted") + ggplot2::theme(plot.title = element_text(hjust = 0.5))

  t<- grid::roundrectGrob()

  p2<- ggplot2::ggplot(df, ggplot2::aes(y = df[,3], x = df[,2])) +
    ggplot2::geom_point(shape = 1, size = 5) +
    ggplot2::stat_summary_bin(fun = mean, geom = "line", color = "red") +
    ggplot2::xlab(paste0("Fitted values \n", "linreg(",deparse(x[["formula"]]),")" )) + ggplot2::ylab(expression(sqrt("Standardized residuals"))) +
    ggplot2::ggtitle("Scale-Location") + ggplot2::theme(plot.title = element_text(hjust = 0.5))

    gridExtra::grid.arrange(p1, p2, heights = c(0.55, 0.55, 0.06))
}

#plot(linreg(formula = Petal.Length ~ Species, data = iris))
