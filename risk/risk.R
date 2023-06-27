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
SPY_xts <- getSymbols("SPY", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(SPY_xts)

BIL_xts <- getSymbols("BIL", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(BIL_xts)



# returns -----------------------------------------------------------------
ret.spy <- Delt(SPY_xts$SPY.Adjusted)
ret.bil <- Delt(BIL_xts$BIL.Adjusted)
rets <- cbind(ret.spy, ret.bil)
names(rets) <- c("SPY", "BIL")
head.tail(rets)

# cumulative returns ------------------------------------------------------
rets[1,] <- 0
gross.rets <- 1 + rets
cum.rets <- cumprod(gross.rets)

plot(
  x = index(cum.rets),
  xlab = "Date",
  y = cum.rets$SPY,
  ylab = "Value of investment",
  main = "Value of $1 Invested in the S&P 500 index and T−Bills 2015 − 2019"
)
lines(
  x = index(cum.rets),
  y = cum.rets$BIL
)


plot(
  rets$SPY,
  main = "Volatility of S&P 500 Index and T−BIlls"
)
lines(
  rets$BIL,
  col = "red"
)

# individual risk --------------------------------------------------------------
returns <- cbind(ret.spy[-1, ], ret.bil[-1, ])
names(returns) <- c("SPY", "BIL")
head.tail(returns)

sd.spy <- sd(returns$SPY)
sd.bil <- sd(returns$BIL)

var.spy <- as.numeric(var(returns$SPY))
var.bil <- as.numeric(var(returns$BIL))

Year <- as.Date(index(returns), format = "%Y-%m-%d") %>% format("%Y")
returns <- cbind(returns, Year)


ret.2015 <- subset(returns, returns$Year == "2015")
head.tail(ret.2015)
sd.2015 <- apply(ret.2015[, -3], 2, sd)

ret.2016 <- subset(returns, returns$Year == "2016")
head.tail(ret.2016)
sd.2016 <- apply(ret.2016[, -3], 2, sd)

ret.2017 <- subset(returns, returns$Year == "2017")
head.tail(ret.2017)
sd.2017 <- apply(ret.2017[, -3], 2, sd)

ret.2018 <- subset(returns, returns$Year == "2018")
head.tail(ret.2018)
sd.2018 <- apply(ret.2018[, -3], 2, sd)

ret.2019 <- subset(returns, returns$Year == "2019")
head.tail(ret.2019)
sd.2019 <- apply(ret.2019[, -3], 2, sd)

sd.all <- rbind(sd.2015, sd.2016, sd.2017, sd.2018, sd.2019)

sd.all <- sd.all * sqrt(252) # annualised standard deviation

t.sd <- t(sd.all)

barplot(
  t.sd,
  beside = TRUE,
  main = "Annualized Standard Deviation of SPY and BIL Returns 2015 to 2019",
  legend.text = c("SPY", "BIL"),
  ylim = c(0, 0.2)
)

# Portfolio risk ----------------------------------------------------------
w.spy <- 0.6
w.bil <- 0.4
annual.sd.spy <- sd.spy * sqrt(252)
annual.sd.bil <- sd.bil * sqrt(252)

covar <- as.numeric(cov(returns$SPY, returns$BIL) * 252)
port.var <- w.spy^2 * annual.sd.spy^2  + w.bil^2 * annual.sd.bil^2 + 2 * covar * w.spy * w.bil
port.sd <- sqrt(port.var)


## matrix approach -------------------------------------------------------
weight <- matrix(c(w.spy, w.bil), nrow = 1)
mat.rets <- as.matrix(returns[, -3])
head.tail(mat.rets)
vcov <- cov(mat.rets) * 252
mat.port.var <- weight %*% vcov %*% t(weight)
mat.port.sd <- sqrt(mat.port.var)


# Multiple assets ---------------------------------------------------------

AMZN_xts <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AMZN_xts)

GOOG_xts <- getSymbols("GOOG", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(GOOG_xts)

ret.amzn <- Delt(AMZN_xts$AMZN.Adjusted)
ret.goog <- Delt(GOOG_xts$GOOG.Adjusted)

rets <- cbind(ret.amzn[-1, ], ret.goog[-1, ], ret.spy[-1, ], ret.bil[-1,])
names(rets) <- c("AMZN", "GOOG", "SPY", "BIL")
head.tail(rets)

weight <- matrix(c(.3, .3, .2, .2), 1)
mat.ret <- as.matrix(rets)
vcov <- cov(mat.ret)*252

mat.port.var <- weight %*% vcov %*% t(weight)
mat.port.sd <- sqrt(mat.port.var)

# VaR gauss ---------------------------------------------------------------------
returns <- read_csv(here("portfolio_returns", "Hypothetical Portfolio (Daily).csv"), 
                    col_types = c("n","D", "n"))
rets <- returns[, -1]
str(rets)
head.tail(returns)

port.ret <- mean(rets$Port.Ret)
port.sd <- sd(rets$Port.Ret)

var01.gauss <- abs((port.ret + qnorm(p = 0.01)*port.sd)* 1465801)
var01.gauss

var.05.gauss <- abs((port.ret + qnorm(p = 0.05)*port.sd)* 1465801)
var.05.gauss

# Historical VaR ----------------------------------------------------------

AMZN_xts <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AMZN_xts)
GOOG_xts <- getSymbols("GOOG", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(GOOG_xts)
AAPL_xts <- getSymbols("AAPL", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AAPL_xts)

ret.amzn <- Delt(AMZN_xts$AMZN.Adjusted)
ret.goog <- Delt(GOOG_xts$GOOG.Adjusted)
ret.aapl <- Delt(AAPL_xts$AAPL.Adjusted)

ret.data <- cbind(ret.amzn, ret.goog, ret.aapl)
names(ret.data) <- c("AMZN", "GOOG", "AAPL")
ret.data <- ret.data[-1, ]
head.tail(ret.data)

w.amzn <- 0.318
w.goog <- 0.312
w.aapl <- 0.369
value <- 1465801

v.amzn <- value * w.amzn
v.goog <- value * w.goog
v.aapl <- value * w.aapl

sim.pnl <- v.amzn*ret.data$AMZN + v.goog*ret.data$GOOG + v.aapl*ret.data$AAPL
names(sim.pnl) <- "Pnl"
head.tail(sim.pnl)

var01.hist <- as.numeric(quantile(-sim.pnl$Pnl, 0.99))
var05.hist <- as.numeric(quantile(-sim.pnl$Pnl, 0.95))

ret.d <- density(sim.pnl$Pnl)
x <- seq(min(sim.pnl$Pnl), max(sim.pnl$Pnl), length = 1000)
y <- dnorm(x, mean = mean(sim.pnl$Pnl), sd = sd(sim.pnl$Pnl))

plot(ret.d,
    xlab = "Profit and Loss",
    main = "Density of Simulated Portfolio P&L Over Three Years And 1% and 5% 1−Day Historical Value−at−Risk (VaR)"
    )
abline(v = -quantile(-sim.pnl$Pnl, 0.99), col = "red")
abline(v = -quantile(-sim.pnl$Pnl, 0.95), col = "blue")
lines(x, y, type = "l", col = "darkgreen") 






