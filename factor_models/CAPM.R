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
data.port <- read_csv(here("factor_models", "Hypothetical Portfolio (Monthly).csv"))
SPY_xts <- getSymbols("SPY", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
DGS3MO <- load.fred()

# Transform ---------------------------------------------------------------
market <- to.monthly(SPY_xts)
head.tail(SPY_xts)
market <- Delt(market$SPY_xts.Adjusted)
market <- market[-1,]
names(market) <- "Market"
head.tail(market)
dim(market)

