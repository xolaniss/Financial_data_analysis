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
options(digits = 3)
# Functions ---------------------------------------------------------------
source(here("Functions", "general_function.R"))

# Import -------------------------------------------------------------
AMZN_xts <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AMZN_xts)

# Simple and exponential moving average -----------------------------------------------------------------
AMZN_ma_20_xts <- rollapply(AMZN_xts$AMZN.Close, 20, mean)
AMZN_ema_20_xts <- EMA(AMZN_xts$AMZN.Close, n = 20)

smoothing_xts <- cbind(AMZN_xts$AMZN.Close, AMZN_ma_20_xts, AMZN_ema_20_xts)
names(smoothing_xts) <- c("Price", "MA_20", "EMA_20")
head.tail(smoothing_xts)

# Plot 2019 ------------------------------------------------------------
smoothing_2019_xts <- smoothing_xts["2019-01-01/2019-12-31"]

dt <- index(smoothing_2019_xts)
plot(
  x = dt,
  y = smoothing_2019_xts$Price,
  ylab = "Price",
  ylim = range(smoothing_2019_xts),
  type = "l")
lines(x = dt, y = smoothing_2019_xts$MA_20, col = "blue")
lines(x = dt, y = smoothing_2019_xts$EMA_20, col = "red")
legend("bottomright", 
       c("AMZN", "MA", "EMA"),
       lwd = c(2, 2, 1),
       col = c("black", "blue", "red"))


# Candle stick -------------------------------------------------
ohlc <- to.monthly(AMZN_xts, name = "ohlc")[-1,-6] 
head.tail(ohlc)

AMZN_ohlc <- as.quantmod.OHLC(ohlc, col.name = c("Open", "High", "Low", "Close", "Volume"))
head.tail(AMZN_ohlc)

chartSeries(AMZN_ohlc,
            theme = "white",
            name = "AMZN OHLC")

# Alternative Graph ------------------------------------------------------
AMZN_price_vol_xts <- AMZN_xts[-1, c(4,5)]
head.tail(AMZN_price_vol_xts)

AMZN_price_vol_tbl <- data.frame("Date" = as.Date(index(AMZN_price_vol_xts)), AMZN_price_vol_xts )[-1,] %>% 
  mutate(AMZN.Volume = AMZN.Volume / 1000000)
head.tail(AMZN_price_vol_tbl)
glimpse(AMZN_price_vol_tbl)

AMZN_price_vol_tbl %>% 
  # pivot_longer(-Date) %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = AMZN.Close), col = "black") +
  geom_area(aes(y = AMZN.Volume / 5), fill = "red", alpha = 0.5) +
  ggtitle("AMZN Price and Volume 2015-2019") +
  
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Volume",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*5, name="Price")
  ) 

