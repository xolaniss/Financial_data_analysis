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

# Import and clean -------------------------------------------------------------
hypo <- read.csv(here("factor_models", "Hypothetical Portfolio (Monthly).csv"))
dim(hypo)
head.tail(hypo)
Port <- hypo[,2]

SPYG_xts <- getSymbols("SPYG", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
head.tail(SPYG_xts)
data.spyg <- to.monthly(SPYG_xts)
altport <- Delt(data.spyg$SPYG_xts.Adjusted)
altport <- altport[-1,]
names(altport) <- "Alt.Port" 
dim(altport)
head.tail(altport)

SPY_xts <- getSymbols("SPY", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE)
spy <- to.monthly(SPY_xts)
benchmark <- Delt(spy$SPY_xts.Adjusted)
benchmark <- benchmark[-1, ]
names(benchmark) <- "Benchmark"
dim(benchmark)
head.tail(benchmark)

# Combined -----------------------------------------------------------------
port.rets <- cbind( benchmark, Port, altport)
head.tail(port.rets)

# Sharpe Ratio --------------------------------------------------------
r <- 0.0155 # Risk free rate DGS3MO
ret.p <- mean(port.rets$Port) * 12 # annual average return
sd.p <- sd(port.rets$Port) * sqrt(12)

sharpe_ratio.p <- (ret.p - r) / sd.p
sharpe_ratio.p

ret.alt <- mean(port.rets$Alt.Port) * 12 # annual average return
sd.alt <- sd(port.rets$Alt.Port) * sqrt(12)

sharpe_ratio.alt <- (ret.alt - r) / sd.alt
sharpe_ratio.alt

# Roy's safety first ---------------------------------------------------------------

MAR <-  0.022 # Based on a real rate return of at least zero (inflation expectations)
roysf.p <- (ret.p - MAR) / sd.p
roysf.p
roysf.alt <- (ret.alt - MAR) / sd.alt
roysf.alt 

# Treynor ratio ---------------------------------------------------------------
reg <- lm(Port ~ Benchmark, data = port.rets)
beta.p <- coef(reg)[2]
treynor.p <- (ret.p - r) / beta.p
treynor.p

reg.alt <- lm(Alt.Port ~ Benchmark, data = port.rets)
beta.alt <- coef(reg.alt)[2]
treynor.alt <- (ret.alt - r) / beta.alt
treynor.alt

# Information ratio ---------------------------------------------------------------
active.p <- port.rets$Port - port.rets$Benchmark
head.tail(active.p)
alpha.p <- mean(active.p) * 12
alpha.p
tracking.p <- sd(active.p) * sqrt(12)
tracking.p
ir.port <- alpha.p / tracking.p

active.alt <- port.rets$Alt.Port - port.rets$Benchmark
alpha.alt <- mean(active.alt) * 12
alpha.alt
tracking.alt <- sd(active.alt) * sqrt(12)
tracking.alt
ir.alt <- alpha.alt / tracking.alt
ir.alt

# Combined results --------------------------------------------------------
port <- c(sharpe_ratio.p, roysf.p, treynor.p, ir.port)
port
alt.port <- c(sharpe_ratio.alt, roysf.alt, treynor.alt, ir.alt)
alt.port
risk.adj.ret <- rbind(port, alt.port)
risk.adj.ret
colnames(risk.adj.ret) <- c("Sharpe", "Roy SF", "Treynor", "Info Ratio")
risk.adj.ret
rownames(risk.adj.ret) <- c("Portfolio", "Alt Portfolio")
risk.adj.ret
