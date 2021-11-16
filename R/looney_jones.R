#' Looney and Jones's corrected Z-test
#' @description
#' Performs a two sample Z-test based on a modified variance estimation of the standard Z-test.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a(non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided"(default), "greater", or"less".
#' @return
#' \item{Z_corr}{the value of the corrected Z-test statistic.}
#' \item{p_value}{the p-value for the test.}
#' @examples
#' x <- c(150,210,NA,350,315,NA,NA,98,170,165)
#' y <- c(NA,150,305,180,NA,90,80,206,111,43)
#' looney_jones(x,y,alternative="two.sided") #Z_stat=1.430195 p_value=0.152661
#' looney_jones(x,y,alternative="greater") #Z_stat=1.430195  p_value=0.07633049
#' looney_jones(x,y,alternative="less") #Z_stat=1.430195 p_value=0.9236695
#' @export

looney_jones <- function(x,y,alternative = "two.sided"){
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
  # Dealing with data which isn't partially matched.
  if (l1 != l2) {
    print("Samples have different length, proceeding with two sample T-test.")
    t.test(x, y, alternative = alternative, paired = F)
  }
  else{
    # Dealing with data which isn't partially matched.
    if (n2==0 & n3==0){
      print("Samples are all matched, proceeding with paired T-test.")
      t.test(x, y, alternative = alternative, paired = TRUE)
    }
    # Dealing with data which isn't partially matched.
    else if (n1==0 | n4==0){
      print("Samples are not matched, proceeding with two sample T-test.")
      t.test(x, y, alternative = alternative)
    }
    # Carrying out corrected Z-test
    else{
      xstar <- c(x0, x1)
      ystar <- c(y0, y1)
      S <- cov(x0, y0)
      xvar <- var(xstar, na.rm = T)
      yvar <- var(ystar, na.rm = T)
      xmean <- mean(xstar, na.rm = T)
      ymean <- mean(ystar, na.rm = T)
      z_corr<-(xmean-ymean)/(sqrt((xvar/(n1+n2))+(yvar/(n1+n3))-(2*n1*S)/((n1+n2)*(n1+n3))))
      if (alternative == "two.sided") {
        p_value = 2 * pnorm(abs(z_corr), lower.tail = F)
      }
      else if (alternative == "greater") {
        p_value = pnorm(z_corr, lower.tail = F)
      }
      else if (alternative == "less") {
        p_value = pnorm(z_corr, lower.tail = T)
      }
      else{
        return('Error: alternative must be two.sided, less, or greater.')
      }
      return(list(Z_corr = z_corr, p_value = p_value))
    }
  }
}
