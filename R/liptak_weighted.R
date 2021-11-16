#' Liptak's Weighted Z-test
#' @description
#' Performs a two sample weighted Z-test using a pooled p-value.
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided", "greater"(default), or "less".
#' @return
#' \item{Z1}{the value of the Z1 test statistic.}
#' \item{Z2}{the value of the Z2 test statistic.}
#' \item{p_value}{the p-value for the test.}
#' @examples
#' x <- c(150,210,NA,350,315,NA,NA,98,170,165)
#' y <- c(NA,150,305,180,NA,90,80,206,111,43)
#' liptak_weighted(x,y,alternative="two.sided") #Z1=0.6227665 Z2=-0.150289 p_value=0.6732314
#' liptak_weighted(x,y,alternative="greater") #Z1=1.11065 Z2=0.5832401 p_value=0.1068279
#' liptak_weighted(x,y,alternative="less") #Z1=-1.11065 Z2=-0.5832401 p_value=0.8931721
#' @export

liptak_weighted <- function(x, y, alternative = "two.sided") {
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
  # Dealing with samples which aren't partially matched
  if (l1 != l2) {
    print("Samples have different length, proceeding with two sample T-test.")
    t.test(x, y, alternative = alternative,paired = F)
  }
  else{
    # Dealing with data which isn't partially matched.
    if (n2 == 0 & n3 == 0) {
      print("Samples are all matched, proceeding with paired T-test.")
      t.test(x, y, alternative = alternative, paired = T)
    }
    # Dealing with data which isn't partially matched.
    else if (n1 == 0 | n4 ==0) {
      print("Samples are not matched, proceeding with two sample T-test.")
      t.test(x, y, alternative = alternative)
    }
    # Carrying out weighted Z-test.
    else{
      if(alternative != "greater" & alternative != "less" & alternative != "two.sided"){
        return("Error: alternative must be greater, less, or two.sided")
      }
      else{
        w1 <- sqrt(2 * n1)
        w2 <- sqrt(n2 + n3)
        p1 <- t.test(x0, y0, alternative = alternative, paired = TRUE)$p.value
        p2 <- t.test(x1, y1, alternative = alternative)$p.value
        Z1 <- qnorm(1 - p1)
        Z2 <- qnorm(1 - p2)
        pci <- 1 - pnorm((w1 * Z1 + w2 * Z2) / sqrt((w1 ^ 2) + (w2 ^ 2)))
        if (alternative == "two.sided"){
          if (pci < 0.5){
            pval <- 2*pci
          }
          else {
            pval <- 2*(1-pci)
          }
          return(list(Z1 = Z1, Z2 = Z2, p_value = pval))
        }
        else{
          return(list(Z1 = Z1, Z2 = Z2, p_value = pci))
        }
      }
    }
  }
}
