#' Ekbohm's MLE based test under homoscedasticity
#' @description
#' Performs a two sample test using the Student's t distribution and MLE statistic under homoscedasticity.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a(non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided"(default), "greater", or"less".
#' @return
#' \item{Z_E}{the value of MLE based Z-statistic.}
#' \item{p_value}{the p-value for the test.}
#' @examples
#' x <- c(150,210,NA,350,315,NA,NA,98,170,165)
#' y <- c(NA,150,305,180,NA,90,80,206,111,43)
#' ekbohm(x,y,alternative="two.sided") #Z_E=1.355812 p_value=0.2331778
#' ekbohm(x,y,alternative="greater") #Z_E=1.355812  p_value=0.1165889
#' ekbohm(x,y,alternative="less") #Z_E=1.355812 p_value=0.8834111
#' @export

ekbohm <- function(x, y, alternative = "two.sided") {
  # Determining necessary values.
  l1 <- length(x)
  l2 <- length(y)
  x0 <- x[!is.na(x) == !is.na(y)]
  x1 <- x[!is.na(x) & is.na(y)]
  y0 <- y[!is.na(x) == !is.na(y)]
  y1 <- y[is.na(x) & !is.na(y)]
  n1 <- length(x0)
  n2 <- length(x1)
  n3 <- length(y1)
  n4 <- length(y0)
  # Dealing with data that isn't partially matched.
  if (length(x) != length(y)) {
    print("Samples have different length, proceeding with two sample T-test.")
    t.test(x, y, alternative = alternative, paired = F)
  }
  else{
    # Dealing with data that isn't partially matched.
    if (n2==0 & n3==0) {
      print("Samples are all matched, proceeding with paired T-test.")
      t.test(x, y, alternative = alternative,paired = T)
    }
    # Dealing with data that isn't partially matched.
    else if (n1==0 | n4==0) {
      print("Samples are not matched, proceeding with two sample T-test.")
      t.test(x, y, alternative = alternative)
    }
    # Carrying out Ekbohm's MLE based Test.
    else{
      x1_bar <- mean(x1)
      y1_bar <- mean(y1)
      s_x1 <- sd(x1)
      s_y1 <- sd(y1)
      x0_bar <- mean(x0)
      y0_bar <- mean(y0)
      s_x0 <- sd(x0)
      s_y0 <- sd(y0)
      S <- cov(x0, y0)
      r <- S / (s_x0 * s_y0)
      f_star <- n1 * (n1 + n3 + n2 * r) * (((n1 + n2) * (n1 + n3) - n2 * n3 * (r ^2)) ^ (-1))
      g_star <- n1 * (n1 + n2 + n3 * r) * (((n1 + n2) * (n1 + n3) - n2 * n3 * (r ^2)) ^ (-1))
      sigma <- ((s_x0^2)*(n1-1)+(s_y0^2)*(n1-1)+(1+(r^2))*((s_x1^2)*(n2-1)+(s_y1^2)*(n3-1)))/(2*(n1-1)+(1+(r^2))*(n2+n3-2))
      v1_star <- sigma*((2 * n1 * (1 - r) + (n2 + n3) * (1 - r ^ 2)) / ((n1 + n2) * (n1 + n3) - n2 * n3* (r^2)))
      Z_E = (f_star * (x0_bar - x1_bar) - g_star * (y0_bar - y1_bar) + x1_bar - y1_bar) / sqrt(v1_star)

      if (alternative == "greater") {
        p_value = pt(Z_E, df=n1, lower.tail = F)
      }
      else if (alternative == "less") {
        p_value = pt(Z_E, df=n1, lower.tail = T)
      }
      else if (alternative == "two.sided") {
        p_value = 2 * pt(abs(Z_E), df=n1, lower.tail = F)
      }
      else{
        return('Error: alternative must be two.sided, less, or greater.')
      }
      return(list(Z_E = Z_E, p_value = p_value))
    }
  }
}
