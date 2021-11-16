#' Lin and Stivers's MLE based test under heteroscedasticity
#' @description
#' Performs a two sample test using the Student's t distribution and MLE statistic under heteroscedasticity.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a(non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided"(default), "greater", or"less".
#' @return
#' \item{Z_LS}{the value of MLE based Z-statistic.}
#' \item{p_value}{the p-value for the test.}
#' @examples
#' x <- c(150,210,NA,350,315,NA,NA,98,170,165)
#' y <- c(NA,150,305,180,NA,90,80,206,111,43)
#' lin_stivers(x,y,alternative="two.sided") #Z_LS=1.584755 p_value=0.1738763
#' lin_stivers(x,y,alternative="greater") #Z_LS=1.584755  p_value=0.08693813
#' lin_stivers(x,y,alternative="less") #Z_LS=1.584755 p_value=0.9130619
#' @export

lin_stivers <- function(x, y, alternative = "two.sided") {
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
  if (l1 != l2) {
    print("Samples have different length, proceeding with two sample T-test.")
    t.test(x, y, alternative = alternative, paired = F)
  }
  else{
    # Dealing with data that isn't partially matched.
    if (n2 == 0 & n3 == 0) {
      print("Samples are all matched, proceeding with paired T-test.")
      t.test(x, y, alternative = alternative, paired = T)
    }
    # Dealing with data that isn't partially matched.
    else if (n1 == 0 | n4 ==0) {
      print("Samples are not matched, proceeding with two sample T-test.")
      t.test(x, y, alternative = alternative)
    }
    # Carrying out Lin and Stivers's MLE based Test
    else{
      x1_bar <- mean(x1, na.rm = T)
      y1_bar <- mean(y1, na.rm = T)
      x0_bar <- mean(x0)
      y0_bar <- mean(y0)
      s_x0 <- sd(x0)
      s_y0 <- sd(y0)
      S <- cov(x0, y0)
      r <- S / (s_x0 * s_y0)
      f <- n1 * (n1 + n3 + n2 * S / s_x0^2) * ((n1 + n2) * (n1 + n3) - n2 * n3 * ((r) ^ 2)) ^ (-1)
      g <- n1 * (n1 + n2 + n3 * S / s_y0^2) * ((n1 + n2) * (n1 + n3) - n2 * n3 * ((r) ^ 2)) ^ (-1)
      v1 <- ((f^2/n1+(1-f)^2/n2)*(s_x0^2)*(n1-1)+(g^2/n1 + (1-g)^2/n3) * s_y0^2 * (n1-1)-2*f*g*S*(n1-1)/n1)/(n1-1)
      Z_LS <- (f * (x0_bar - x1_bar) - g * (y0_bar - y1_bar) + x1_bar - y1_bar) / sqrt(v1)

      if (alternative == "greater") {
        p_value = pt(Z_LS, df = n1, lower.tail = F)
      }
      else if (alternative == "less") {
        p_value = pt(Z_LS, df = n1, lower.tail = T)
      }
      else if (alternative == "two.sided") {
        p_value = 2 * pt(abs(Z_LS), df = n1, lower.tail = F)
      }
      else{
        return('Error: alternative must be two.sided, less, or greater.')
      }
      return(list(Z_LS = Z_LS, p_value = p_value))
    }
  }
}
