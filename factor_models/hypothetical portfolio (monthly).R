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

# Functions ---------------------------------------------------------------
source(here("Functions", "general_function.R"))

# Import -------------------------------------------------------------
AMZN_xts <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AMZN_xts)
GOOG_xts <- getSymbols("GOOG", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(GOOG_xts)
AAPL_xts <- getSymbols("AAPL", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(AAPL_xts)

# Creating monthly hypothetical returns -----------------------------------
data.amzn <- to.monthly(AMZN_xts)
ret.amzn <-  Delt(data.amzn[, 6])
data.goog <- to.monthly(GOOG_xts)
ret.goog <- Delt(data.goog[, 6])
data.aapl <- to.monthly(AAPL_xts)
ret.aapl <- Delt(data.aapl[, 6])

port <- cbind(ret.amzn, ret.goog, ret.aapl)
port <- port[-1,]
names(port) <- c("AMZN", "GOOG", "AAPL")
head.tail(port)
hypo.port <- data.frame(rowMeans(port))
names(hypo.port) <- "Port.Ret"
hypo.port <- cbind(data.frame(index(port)), hypo.port)
names(hypo.port)[1] <- "Date"
head.tail(hypo.port)

write.csv(hypo.port, here("factor_models", "Hypothetical Portfolio (Monthly).csv"), row.names = FALSE)
