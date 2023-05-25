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
AAPL_xts <- getSymbols("AAPL", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)


# Returns ----------------------------------------------------------------
AAPL_close_xts <- AAPL_xts[, 4]
names(AAPL_close_xts) <- "price"
AAPL_close_xts$lag_price <- lag(AAPL_close_xts$price)
AAPL_close_xts$returns <- (AAPL_close_xts$price/ AAPL_close_xts$lag_price) - 1
AAPL_returns_xts <- AAPL_close_xts[-1, 3]
head.tail(AAPL_returns_xts)

# Total returns -----------------------------------------------------------
AAPL_adjusted_xts <- AAPL_xts[, 6]
names(AAPL_adjusted_xts) <- "price"
AAPL_adjusted_xts$lag_price <- lag(AAPL_adjusted_xts)
AAPL_adjusted_xts$tot_returns <- (AAPL_adjusted_xts$price / AAPL_adjusted_xts$lag_price) - 1
AAPL_tot_returns_xts <- AAPL_adjusted_xts[-1, 3]
head.tail(AAPL_tot_returns_xts)

# Log returns -------------------------------------------------------------
AAPL_tot_returns_xts$log_tot_returns <- ROC(AAPL_adjusted_xts$price)[-1]
head.tail(AAPL_tot_returns_xts)

# Winsorization and tructation -----------------------------------------------------------
upper <- as.numeric(quantile(AAPL_tot_returns_xts$tot_returns, 0.995))
lower <- as.numeric(quantile(AAPL_tot_returns_xts$tot_returns, 0.05))

winsorize <- ifelse(AAPL_tot_returns_xts$tot_returns <= lower, lower, 
                    ifelse(AAPL_tot_returns_xts$tot_returns >= upper, upper, AAPL_tot_returns_xts$tot_returns))
hist(winsorize)

truncate <- 
  subset(
    AAPL_tot_returns_xts$tot_returns,
    AAPL_tot_returns_xts$tot_returns <= upper &
      AAPL_tot_returns_xts$tot_returns >= lower
)

hist(truncate)


# Cumulative returns ------------------------------------------------------
AAPL_tot_returns_xts$gross_returns <- 1 + AAPL_tot_returns_xts$tot_returns
head.tail(AAPL_tot_returns_xts)

cum_arith_xts <- cumprod(AAPL_tot_returns_xts$gross_returns)
head.tail(cum_arith_xts)
as.numeric(cum_arith_xts[nrow(cum_arith_xts)] - 1)

cum_log_xts <- sum(AAPL_tot_returns_xts$log_tot_returns)
head.tail(cum_log_xts)

exp(cum_log_xts) - 1 # same as arithmetic


# Price return vs total return --------------------------------------------
AAPL_normalised_xts <- (AAPL_close_xts$price / as.numeric(AAPL_close_xts$price[2]))[-1]
names(AAPL_normalised_xts) <- "Price Return"

AAPL_normalised_xts$`Total Return` <- (AAPL_adjusted_xts$price / as.numeric(AAPL_adjusted_xts$price[2]))[-1]
head.tail(AAPL_normalised_xts)

AAPL_normalised_tbl <- data.frame("Date" = index(AAPL_normalised_xts), AAPL_normalised_xts)

AAPL_normalised_tbl %>% 
  pivot_longer(-Date, names_to = "Type", values_to = "Returns (%)") %>% 
  ggplot(aes(x = Date, y = `Returns (%)`, col = Type)) +
  geom_line() +
  geom_hline(yintercept = 1) +
  labs(title = "Price versus total returns") + 
  theme_bw() 

# Weekly returns ----------------------------------------------------------
AAPL_weekly_xts <- to.weekly(AAPL_xts, name = "wk")
AAPL_adjusted_weekly_xts <- AAPL_weekly_xts[, 6]
names(AAPL_adjusted_weekly_xts) <- "price"

AAPL_adjusted_weekly_xts$total.ret <- Delt(AAPL_adjusted_weekly_xts$price)
AAPL_adjusted_weekly_xts <- AAPL_adjusted_weekly_xts[-1]
head.tail(AAPL_adjusted_weekly_xts)



