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

options(digits = 2)

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

# Combining -----------------------------------------------------------------
close_xts <- cbind(AMZN_xts$AMZN.Close, GOOG_xts$GOOG.Close, AAPL_xts$AAPL.Close, SPY_xts$SPY.Close)

# Normalizing -------------------------------------------------------------
norm_close_xts <- xts()
norm_close_xts$AMZN <- close_xts$AMZN.Close / as.numeric(close_xts$AMZN.Close[1])
norm_close_xts$GOOG <- close_xts$GOOG.Close / as.numeric(close_xts$GOOG.Close[1])
norm_close_xts$AAPL <- close_xts$AAPL.Close / as.numeric(close_xts$AAPL.Close[1])
norm_close_xts$SPY <- close_xts$SPY.Close / as.numeric(close_xts$SPY.Close[1])

head.tail(norm_close_xts)

# Graphing ---------------------------------------------------------------
norm_close_tbl <- data.frame("Date" = as.Date(index(norm_close_xts)), norm_close_xts) 
glimpse(norm_close_tbl)
head.tail(norm_close_tbl)

norm_close_gg <- 
  norm_close_tbl %>% 
  pivot_longer(-Date, names_to = "Ticker", values_to = "Price") %>% 
  ggplot(aes(x = Date, y = Price, col = Ticker)) +
  geom_line() +
  labs(title = "Value of $1 Investment in AMZN, GOOG, AAPL, SPY", y = "Price ($)", x = "") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) +
  theme_bw()




