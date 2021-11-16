## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PMConnolly)

## ----eval=FALSE---------------------------------------------------------------
#  liptak_weighted(x,y,alternative="two.sided")

## -----------------------------------------------------------------------------
x <- c(150,210,NA,350,315,NA,NA,98,170,165)
y <- c(NA,150,305,180,NA,90,80,206,111,43)
liptak_weighted(x,y,alternative="two.sided")
liptak_weighted(x,y,alternative="greater")
liptak_weighted(x,y,alternative="less")

## ----eval = F-----------------------------------------------------------------
#  kim_mod(x,y,alternative="two.sided")

## -----------------------------------------------------------------------------
x <- c(150,210,NA,350,315,NA,NA,98,170,165)
y <- c(NA,150,305,180,NA,90,80,206,111,43)
kim_mod(x,y,alternative="two.sided")
kim_mod(x,y,alternative="greater")
kim_mod(x,y,alternative="less")

## ----eval = F-----------------------------------------------------------------
#  looney_jones(x,y,alternative="two.sided")

## -----------------------------------------------------------------------------
x <- c(150,210,NA,350,315,NA,NA,98,170,165)
y <- c(NA,150,305,180,NA,90,80,206,111,43)
looney_jones(x,y,alternative="two.sided")
looney_jones(x,y,alternative="greater")
looney_jones(x,y,alternative="less")

## ----eval = F-----------------------------------------------------------------
#  lin_stivers(x,y,alternative="two.sided")

## -----------------------------------------------------------------------------
x <- c(150,210,NA,350,315,NA,NA,98,170,165)
y <- c(NA,150,305,180,NA,90,80,206,111,43)
lin_stivers(x,y,alternative="two.sided")
lin_stivers(x,y,alternative="greater")
lin_stivers(x,y,alternative="less")

## ----eval = F-----------------------------------------------------------------
#  ekbohm(x,y,alternative="two.sided")

## -----------------------------------------------------------------------------
x <- c(150,210,NA,350,315,NA,NA,98,170,165)
y <- c(NA,150,305,180,NA,90,80,206,111,43)
ekbohm(x,y,alternative="two.sided")
ekbohm(x,y,alternative="greater")
ekbohm(x,y,alternative="less")

