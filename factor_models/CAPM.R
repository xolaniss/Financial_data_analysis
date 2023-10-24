# Description

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
data.port <- read_csv(here("factor_models", "Hypothetical Portfolio (Monthly).csv"))
dim(data.port)
head.tail(data.port)
SPY_xts <- getSymbols("SPY", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
TSLA_xts <- getSymbols("TSLA", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
AMZN_xts <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
rf <- load.fred()


# Transform ---------------------------------------------------------------
market <- to.monthly(SPY_xts)
head.tail(SPY_xts)
market <- Delt(market$SPY_xts.Adjusted)
market <- market[-1,]
names(market) <- "Market"
head.tail(market)
dim(market)
rf <- to.monthly(rf)
rf <- (1 + rf$rf.Open / 100) ^(1/12) - 1
head.tail(rf)
rf.sub <- subset(rf, index(rf) >= as.yearmon("Jan 2015") & index(rf) <= as.yearmon("Dec 2019"))
head.tail(rf.sub)

combo <- cbind(market, rf.sub, data.port$Port.Ret)
names(combo) <- c("Market", "Risk.Free", "Port.Ret")
head.tail(combo)

combo$Ex.Ret <- combo$Port.Ret - combo$Risk.Free
combo$Ex.Market <- combo$Market - combo$Risk.Free
head.tail(combo)

# CAPM --------------------------------------------------------------------

## Excess returns form -----------------------------------------------------------
capm <- lm(Ex.Ret ~ Ex.Market, data = combo)
summary(capm)
beta <- summary(capm)$coefficient[2]

## Equitiy risk premium ----------------------------------------------------
ERP <- 0.0818 # From https://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html
rf <- 0.0155 # From https://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html
capm_coe <- rf + beta*ERP

## Market model ------------------------------------------------------------
mkt.model <- lm(Port.Ret ~ Market, data = combo)
summary(mkt.model)


# Rolling window regressions ----------------------------------------------
rets <- Delt(AMZN_xts$AMZN.Adjusted)
rets$SPY <- Delt(SPY_xts$SPY.Adjusted)
names(rets)[1] <- "AMZN"
rets <- rets[-1,]
head.tail(rets)

coeffs <- rollapply(rets,
                    width = 252,
                    function(X){
                      roll.reg = lm(AMZN ~ SPY, data = as.data.frame(X))
                                    return(roll.reg$coef)
                    },
                    by.column = FALSE
                    )
coeffs[249:255]
coeffs <- na.omit(coeffs)
names(coeffs) <- c("Alpha", "Beta")
head.tail(coeffs)

par(mfrow = c(2,1))
plot(
  y = coeffs$Alpha,
  x = index(coeffs),
  main = "AMZN Alpha, 252 Rolling Window",
  xlab = "",
  ylab = "Alpha",
  type = "l", 
  col = "blue"
  )
plot(
  y = coeffs$Beta,
  x = index(coeffs),
  main = "AMZN Beta, 252 Rolling Window",
  xlab = "",
  ylab = "Beta",
  type = "l", 
  col = "red"
)
par(mfrow = c(1,1))

## Betas on different days -----------------------------------------------
start.date <- "2017-12-01"
end.date <- "2019-12-31"
prices <- data.frame(
  "date" = as.Date(index(AMZN_xts), format = "%Y-%m-%d"),
  AMZN_xts$AMZN.Adjusted, 
  SPY_xts$SPY.Adjusted
  )
head.tail(prices)
all.dates <- data.frame("date" = seq(as.Date(start.date), as.Date(end.date), by = "day"))
head(all.dates)
combo <- merge(all.dates, prices, by = "date", all = TRUE)
head.tail(combo)
combo <- na.locf(combo)
tail(combo)
combo$wkday <- weekdays(combo$date)
head.tail(combo)

beta <- data.frame("beta" = rep(999, 5))
row.names(beta) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
head(beta)
day.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")


for (j in 1:5) {
  data <- subset(combo, wkday == paste(day.week[j]))
  stock.ret <- data[, 2]/ Lag(data[, 2], k = 1) -1
  mkt.ret <- data[, 3]/ Lag(data[, 3], k = 1) -1
  
  combo.ret <- cbind(stock.ret, mkt.ret)
  combo.ret <- data.frame(combo.ret)
  colnames(combo.ret) <- c("stock.ret", "mkt.ret")
  combo.ret <-  combo.ret[(nrow(combo.ret) - 104 + 1):nrow(combo.ret), ]
  beta[j, ] <- summary(lm(stock.ret ~ mkt.ret, data = combo.ret))$coeff[2]
}
beta
median(beta$beta)

# Fama French  ------------------------------------------------------------
data.ff <- read.csv("F-F_Research_Data_Factors.csv", skip = 3, header = TRUE)
str(data.ff )
ff.sub <- data.ff %>%
  mutate(X = parse_date_time(data.ff$X, orders = "Ym")) %>% 
  rename(Date = X) %>% 
  mutate(across(.cols = 2:5, .fns = ~as.numeric(.x) / 100)) %>% 
  filter(Date >= "2015-01-01" & Date <= "2019-12-31")

ff.sub <- cbind(ff.sub, data.port[,2])
ff.sub$Port.RF <-  ff.sub$Port.Ret - ff.sub$RF
head.tail(ff.sub)

ff.reg <- lm(Port.RF ~ Mkt.RF + SMB + HML, data = ff.sub)
summary(ff.reg)

capm.reg <- lm(Port.RF ~ Mkt.RF, data = ff.sub)
summary(capm.reg)

# Event Studies -----------------------------------------------------------
event.dt <- as.Date("2019-04-24")
estper.start <- event.dt -365
data.firm <- TSLA_xts
data.market <- SPY_xts
rets <- cbind(diff(log(data.firm$TSLA.Adjusted)), diff(log(data.market$SPY.Adjusted)))
rets <- rets[-1, ]
names(rets) <- c("Firm", "Market")
head.tail(rets)
window <- paste0(estper.start, "/", event.dt)
rets <- rets[window]
head.tail(rets)

## Single step -----------------------------------------------------------
rets$Dummy <- ifelse(index(rets) == "2019-04-24", 1, 0)
head.tail(rets)

event <-  lm(Firm ~ Market + Dummy, data = rets)
summary(event)

## Two stage -------------------------------------------------------------
event.window <- rets["2019-04-24", -3]
event.window
est.period <- rets["2018-04-24/2019-04-23", -3]
est.period

event2 <- lm(Firm ~ Market, data = est.period)
summary(event2)
alpha <- summary(event2)$coeff[1]
alpha
beta <- summary(event2)$coeff[2]
beta
rmse <- summary(event2)$sigma
rmse

event.window$Pred <- alpha + beta*event.window$Market
event.window$Ab.Ret <- event.window$Firm - event.window$Pred
event.window$tsat <- event.window$Ab.Ret/ rmse
event.window

## Sample quantiles ------------------------------------------------------
ab.ret <- est.period$Firm - alpha + beta*est.period$Market
head.tail(ab.ret)
sorted <- data.frame(ab.ret)
sorted <- sorted[order(est.period$Firm),]

alpha <- 0.05
N <- nrow(est.period)
N
cut.off <- ceiling(alpha * N)
cut.off

sorted[cut.off]

quantile(sorted, probs = c(alpha, 1 - alpha))
