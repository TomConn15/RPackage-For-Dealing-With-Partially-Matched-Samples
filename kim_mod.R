#' Kim et al's Modified T-test
#' @description
#' Performs a two sample t-test using the Kim et al's modified t-statistic.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided"(default), "greater", or "less".
#' @return
#' \item{t3}{the value of the modified t-statistic.}
#' \item{p_value}{the p-value for the test.}
#' @examples
#' x <- c(150,210,NA,350,315,NA,NA,98,170,165)
#' y <- c(NA,150,305,180,NA,90,80,206,111,43)
#' kim_mod(x,y,alternative="two.sided") #t3=1.358102 p_value=0.1744315
#' kim_mod(x,y,alternative="greater") #t3=1.358102  p_value=.08721573
#' kim_mod(x,y,alternative="less") #t3=1.358102 p_value=0.9127843
#' @export

kim_mod = function (x, y, alternative = "two.sided") {
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
  D <- x0 - y0
  D_bar <- mean(D,na.rm = T)
  x1_bar <- mean(x1, na.rm = T)
  y1_bar <- mean(y1,na.rm = T)
  nH <- 2 / ((1/n2)+(1/n3))
  # Dealing with data that isn't partially matched.
  if (l1 != l2) {
    print('Samples have different lengths, proceeding with two sample T-test.')
    t.test(x, y, alternative = alternative, paired = F)
  }
  else {
    # Dealing with data that isn't partially matched.
    if (n2==0 & n3==0){
      print('Samples are all matched, proceeding with paired T-test.')
      t.test(x, y, alternative = alternative, paired = T)
    }
    # Dealing with data that isn't partially matched.
    else if (n1 == 0 | n4==0) {
      print('Samples are not matched, proceeding with two sample T-test.')
      t.test(x, y, alternative = alternative)
    }
    # Carrying out modified t-test.
    else {
      SD <- var(D,na.rm = T)
      S1 <- var(x1,na.rm = T)
      S2 <- var(y1,na.rm = T)
      t3 <- (n1 * D_bar + nH * (x1_bar - y1_bar)) / sqrt(n1 * SD + (nH^2) * ((S2 /n3) + (S1 / n2)))
      if (alternative == "two.sided") {
        p_value <- 2 * pnorm(abs(t3), lower.tail = F)
      }
      else if (alternative == "greater") {
        p_value <- pnorm(t3, lower.tail = F)
      }
      else if (alternative == "less") {
        p_value <- pnorm(t3, lower.tail = T)
      }
      else{
        return('Error: alternative must be two.sided, less, or greater.')
      }
      return(list(t3 = t3, p_value = p_value))
    }
  }
}
