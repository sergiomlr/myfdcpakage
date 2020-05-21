#' Exceedance Probability Equation
#'
#' This function estimates the flow duration curve. It assumes the input is
#' an atomic vector containting values of a variable i.e. discharge

#' @import ggplot2
#' @param infile a numeric list or vector input file
#' @return A dataframe with variable and probability

#' @export
fdc <- function (infile) {
  df <- data.frame(infile)
  df$q <- sort(df$infile, decreasing = TRUE)
  df$i <- 1:length(df$q)
  df$p <- df$i/(length(df$q)+1)
  df[c("q","p")]
}

#' @export
fdc_plot <- function(df){
  ggplot(df)+aes(p, q)+geom_line()+scale_y_continuous(trans='log2')
}
