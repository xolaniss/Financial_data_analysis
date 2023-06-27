# Description
# portfolio returns - Xolani Sibande 15 June 2022

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

options(scipen = 999)

# Functions ---------------------------------------------------------------
source(here("Functions", "general_function.R"))

# Import -------------------------------------------------------------
AMZN_xts <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AMZN_xts)
GOOG_xts <- getSymbols("GOOG", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(GOOG_xts)
AAPL_xts <- getSymbols("AAPL", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AAPL_xts)
SPY_xts <- getSymbols("SPY", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(SPY_xts)

# Returns -----------------------------------------------------------------
AMZN_ret_xts <- Delt(AMZN_xts$AMZN.Adjusted)
GOOG_ret_xts <- Delt(GOOG_xts$GOOG.Adjusted)
AAPL_ret_xts <- Delt(AAPL_xts$AAPL.Adjusted)

returns <- 
  cbind(AMZN_ret_xts,
      GOOG_ret_xts,
      AAPL_ret_xts)

names(returns) <- c("AMZN", "GOOG", "AAPL")
returns <- returns[-1, ]

head.tail(returns)

# Weights -----------------------------------------------------------------
i.amzn <- 50000
i.goog <- 30000
i.aapl <- 20000
i.total <- i.amzn + i.goog + i.aapl

w.amzn <- i.amzn / i.total
w.goog <- i.goog / i.total
w.aapl <- i.aapl / i.total

# Portfolio return --------------------------------------------------------
port.ret <-  1 + returns
head.tail(port.ret)

cum.ret <- cumprod(port.ret)
head.tail(cum.ret)
cum.ret <- cum.ret[nrow(cum.ret )] - 1

cum.pot <- as.numeric(
  w.amzn*cum.ret$AMZN + 
  w.goog*cum.ret$GOOG +
  w.aapl*cum.ret$AAPL)

# Matrix approach ---------------------------------------------------------
weight <- c(w.amzn, w.goog, w.aapl)
mat.weight <- matrix(weight, 1)
mat.returns <- matrix(cum.ret, 3)
port.ret2 <- as.numeric(mat.weight %*% mat.returns)


# Quarterly rebalancing ----------------------------------------------------

## Q1 ----------------------------------------------------------------------
ret.q1 <- returns["2019-01-01/2019-03-31"]
head.tail(ret.q1)
grets.q1 <- 1 + ret.q1
head.tail(grets.q1)
crets.q1 <- apply(grets.q1, 2, cumprod)
head.tail(crets.q1)
crets.q1 <- crets.q1[nrow(crets.q1), ] - 1

## Q2 ----------------------------------------------------------------------
ret.q2 <- returns["2019-04-01/2019-06-30"]
head.tail(ret.q2)
grets.q2 <- 1 + ret.q2
head.tail(grets.q2)
crets.q2 <- apply(grets.q2, 2, cumprod)
head.tail(crets.q2)
crets.q2 <- crets.q2[nrow(crets.q2), ] - 1

## Q3 ----------------------------------------------------------------------
ret.q3 <- returns["2019-07-01/2019-09-30"]
head.tail(ret.q3)
grets.q3 <- 1 + ret.q3
head.tail(grets.q3)
crets.q3 <- apply(grets.q3, 2, cumprod)
head.tail(crets.q3)
crets.q3 <- crets.q3[nrow(crets.q3), ] - 1

## Q4 ----------------------------------------------------------------------
ret.q4 <- returns["2019-10-01/2019-12-31"]
head.tail(ret.q4)
grets.q4 <- 1 + ret.q4
head.tail(grets.q4)
crets.q4 <- apply(grets.q4, 2, cumprod)
head.tail(crets.q4)
crets.q4 <- crets.q4[nrow(crets.q4), ] - 1

# Another approach --------------------------------------------------------
prc.amzn <- AMZN_xts$AMZN.Adjusted
qrt.amzn <- to.quarterly(prc.amzn)
head.tail(qrt.amzn)

prc.goog <- GOOG_xts$GOOG.Adjusted
qrt.goog <- to.quarterly(prc.goog)
head.tail(qrt.goog)

prc.aapl <- AAPL_xts$AAPL.Adjusted
qrt.aapl <- to.quarterly(prc.aapl)
head.tail(qrt.aapl)

rqrt <- cbind(
  Delt(qrt.amzn[, 4]),
  Delt(qrt.goog[, 4]),
  Delt(qrt.aapl[, 4])
)

names(rqrt) <- c("AMZN", "GOOG", "AAPL")
rqrt <- rqrt[-1, ]
tail(rqrt)

# Equal weights approach -----------------------------------------------------------

## Q1 --------------------------------------------------------------------
ew.io <- 1000
eq.i1 <- ew.io * (1 + mean(crets.q1))
eq.i1

## Q2 ---------------------------------------------------------------------
eq.i2 <- eq.i1 * (1 + mean(crets.q2))
eq.i2

## Q3 --------------------------------------------------------------------
eq.i3 <- eq.i2 * (1 + mean(crets.q3))
eq.i3

## Q4 --------------------------------------------------------------------
eq.i4 <- eq.i3 * (1 + mean(crets.q4))
eq.i4

# Value-weighted approach -------------------------------------------------


## Weights 1 ---------------------------------------------------------------
mc1.amzn <- 737.47
mc1.goog <- 720.32
mc1.aapl <- 746.08
mc1.tot <- mc1.amzn + mc1.goog + mc1.aapl

w1.amzn <-  mc1.amzn / mc1.tot
w1.goog <- mc1.goog / mc1.tot
w1.aapl <- mc1.aapl / mc1.tot

## Q1 --------------------------------------------------------------------
vw.i0 <- 1000
vw.i0.amzn <- vw.i0 * w1.amzn
vw.i0.goog <- vw.i0 * w1.goog
vw.i0.aapl <- vw.i0 * w1.aapl

vw.i1.amzn <- vw.i0.amzn * (1 + crets.q1[1])
vw.i1.goog <- vw.i0.goog * (1 + crets.q1[2])
vw.i1.aapl <- vw.i0.aapl * (1 + crets.q1[3])
vw.i1 <- sum(vw.i1.amzn, vw.i1.goog, vw.i1.aapl)

## Q2 --------------------------------------------------------------------
mc2.amzn <- 876.22
mc2.goog <- 815.67
mc2.aapl <- 895.67
mc2.tot <- mc2.amzn + mc2.goog + mc2.aapl

w2.amzn <-  mc2.amzn / mc2.tot
w2.goog <- mc2.goog / mc2.tot
w2.aapl <- mc2.aapl / mc2.tot

vw.i1.amzn <- vw.i1 * w2.amzn
vw.i1.goog <- vw.i1 * w2.goog
vw.i1.aapl <- vw.i1 * w2.aapl

vw.i2.amzn <- vw.i1.amzn * (1 + crets.q2[1])
vw.i2.goog <- vw.i1.goog * (1 + crets.q2[2])
vw.i2.aapl <- vw.i1.aapl * (1 + crets.q2[3])
vw.i2 <- sum(vw.i2.amzn, vw.i2.goog, vw.i2.aapl)

## Q3 ----------------------------------------------------------------------
mc3.amzn <- 939.29
mc3.goog <- 750.42
mc3.aapl <- 910.64
mc3.tot <- mc3.amzn + mc3.goog + mc3.aapl

w3.amzn <-  mc3.amzn / mc3.tot
w3.goog <- mc3.goog / mc3.tot
w3.aapl <- mc3.aapl / mc3.tot

vw.i2.amzn <- vw.i2 * w3.amzn
vw.i2.goog <- vw.i2 * w3.goog
vw.i2.aapl <- vw.i2 * w3.aapl

vw.i3.amzn <- vw.i2.amzn * (1 + crets.q3[1])
vw.i3.goog <- vw.i2.goog * (1 + crets.q3[2])
vw.i3.aapl <- vw.i2.aapl * (1 + crets.q3[3])
vw.i3 <- sum(vw.i3.amzn, vw.i3.goog, vw.i3.aapl)

## Q4 ----------------------------------------------------------------------
mc4.amzn <- 859.28
mc4.goog <- 842.21
mc4.aapl <- 995.15
mc4.tot <- mc4.amzn + mc4.goog + mc4.aapl

w4.amzn <-  mc4.amzn / mc4.tot
w4.goog <- mc4.goog / mc4.tot
w4.aapl <- mc4.aapl / mc4.tot

vw.i3.amzn <- vw.i3 * w4.amzn
vw.i3.goog <- vw.i3 * w4.goog
vw.i3.aapl <- vw.i3 * w4.aapl

vw.i4.amzn <- vw.i3.amzn * (1 + crets.q4[1])
vw.i4.goog <- vw.i3.goog * (1 + crets.q4[2])
vw.i4.aapl <- vw.i3.aapl * (1 + crets.q4[3])
vw.i4 <- sum(vw.i4.amzn, vw.i4.goog, vw.i4.aapl)

# Daily equal weighted returns -----------------------------------------------------------

## Q1 --------------------------------------------------------------------
ew.val1 <- ew.io/3 * cumprod(grets.q1)
head.tail(ew.val1)

ew.val1$tot <- rowSums(ew.val1)
head.tail(ew.val1$tot)

## Q2 --------------------------------------------------------------------
ew.val2 <- as.numeric(ew.val1[nrow(ew.val1), 4]) /3 * cumprod(grets.q2)
head.tail(ew.val2)

ew.val2$tot <- rowSums(ew.val2)
head.tail(ew.val2$tot)

## Q3--------------------------------------------------------------------
ew.val3 <- as.numeric(ew.val2[nrow(ew.val2), 4]) /3 * cumprod(grets.q3)
head.tail(ew.val3)

ew.val3$tot <- rowSums(ew.val3)
head.tail(ew.val3$tot)

## Q4--------------------------------------------------------------------
ew.val4 <- as.numeric(ew.val3[nrow(ew.val3), 4]) /3 * cumprod(grets.q4)
head.tail(ew.val4)

ew.val4$tot <- rowSums(ew.val4)
head.tail(ew.val4$tot)


ew.pot <- rbind(ew.val1, ew.val2, ew.val3, ew.val4)
head.tail(ew.pot)


# Daily value weighted returns --------------------------------------------
vw.val1 <-  cumprod(grets.q1)
vw.val1$AMZN <-  vw.val1$AMZN * vw.i0.amzn
vw.val1$GOOG <-  vw.val1$GOOG * vw.i0.goog
vw.val1$AAPL <-  vw.val1$AAPL * vw.i0.aapl


## Q1 --------------------------------------------------------------------
vw.val1$tot <-  rowSums(vw.val1)
head.tail(vw.val1)

## Q2 --------------------------------------------------------------------
vw.val2 <-  cumprod(grets.q2)
vw.i1tot <- as.numeric(vw.val1[nrow(vw.val1), 4])
vw.val2$AMZN <- vw.val2$AMZN * vw.i1tot * (mc2.amzn/ mc2.tot)
vw.val2$GOOG <- vw.val2$GOOG * vw.i1tot * (mc2.goog/ mc2.tot)
vw.val2$AAPL <- vw.val2$AAPL * vw.i1tot * (mc2.aapl/ mc2.tot)

vw.val2$tot <- rowSums(vw.val2)
head.tail(vw.val2)


## Q3 --------------------------------------------------------------------
vw.val3 <- cumprod(grets.q3)
vw.i2tot <- as.numeric(vw.val2[nrow(vw.val2), 4])
vw.val3$AMZN <- vw.val3$AMZN * vw.i2tot * (mc3.amzn/ mc3.tot)
vw.val3$GOOG <- vw.val3$GOOG * vw.i2tot * (mc3.goog/ mc3.tot)
vw.val3$AAPL <- vw.val3$AAPL * vw.i2tot * (mc3.aapl/ mc3.tot)

vw.val3$tot <- rowSums(vw.val3)
head.tail(vw.val3)

## Q4 --------------------------------------------------------------------
vw.val4 <- cumprod(grets.q4)
vw.i3tot <- as.numeric(vw.val3[nrow(vw.val3), 4])

vw.val4$AMZN <- vw.val4$AMZN * vw.i3tot * (mc4.amzn/ mc4.tot)
vw.val4$GOOG <- vw.val4$GOOG * vw.i3tot * (mc4.goog/ mc4.tot)
vw.val4$AAPL <- vw.val4$AAPL * vw.i3tot * (mc4.aapl/ mc4.tot)

vw.val4$tot <- rowSums(vw.val4)
head.tail(vw.val4)

vw.port <- rbind(vw.val1, vw.val2, vw.val3, vw.val4)

portfolios <- cbind(ew.pot$tot, vw.port$tot)
names(portfolios) <- c("EW", "VW")
head.tail(portfolios)


# Time weighted rate of return ----------------------------------------------------------------

dates <-  ymd(c("2018−12−31", "2019−03−31", "2019−06−30", "2019−07−31", "2019−09−30", "2019−12−31")) 
mv <-  c(2000000, 1950000, 2000000, 2220000, 2400000, 2500000)
cf <- c(0, 0, 0, 20000, 0, -5000) 
cbind(data.frame(dates), mv, cf)

hpr <- rep(0, length(cf))
for (i in (2 : length(cf))) {
  hpr[i] <- (mv[i] - mv[i-1] + cf[i]) / mv[i-1]
}
cbind(data.frame(dates), mv, cf, hpr)

gross.ret <-  1 + hpr
cum.ret <- cumprod(gross.ret)
cum.ret[length(cum.ret)] - 1


# Money weighted rate of return -------------------------------------------

pv <- function(cf, dates, r ){
  t <-  as.numeric((dates - dates[1]) / 365)
  pv_factor <- (1 + r)^-t
  pv <- cf * pv_factor
  value <- sum(pv)
  return(value)
}

mwrr <-function(cf, dates, guess){
  delta.x <- 0.01
  tol <- 0.0000001
  cur.x <- guess
  iter <- 0
    for(i in 1:1000){
      fx <- pv(cf, dates, cur.x)
      cur.x.delta <- cur.x - delta.x
      fx.delta <- pv(cf, dates, cur.x.delta)
      dx <- (fx - fx.delta) / delta.x
      cur.x <- cur.x - (fx / dx)
      iter <- iter + 1
      cat("At iteration", iter, "MWRR equals", cur.x, "\n")
      if (abs(fx) < tol) break
    }
}

## Examples
cf <- c(-100000, 120000)
dates <-  ymd(c("2018−12−31", "2019−12−31"))
mwrr(cf, dates, 0.1)

cf <- c(-200000, 20000, 220000)
dates <- ymd(c("2018−12−31", "2019−06−30", "2019−12−31"))
mwrr(cf, dates, 0.7)


# Exporting "Hypothetical Portfolio (Daily)" ------------------------------
portfolios
invest.date <- portfolios[1 , 2]
index(invest.date) <- index(invest.date) - 1
invest.date[1, ] <- 1000 
combine <- rbind(invest.date, portfolios[, 2])

hypo.port <- Delt(combine)
head.tail(hypo.port)
hypo.port <- hypo.port[-1, ]
names(hypo.port) <- "Port.Ret"
head.tail(hypo.port)

Date <- as.Date(index(hypo.port))
hypo.port <- data.frame(Date,  hypo.port, row.names = seq(1, nrow(hypo.port), 1))
write.csv(hypo.port, file = here("portfolio_returns", "Hypothetical Portfolio (Daily).csv"), row.names = TRUE)
