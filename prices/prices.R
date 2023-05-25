# Packages ----------------------------------------------------------------
library(tidyverse)
library(quantmod)
library(here)
library(xts)
library(timetk)

# Functions ---------------------------------------------------------------
source(here("Functions", "general_function.R"))


# Import ------------------------------------------------------------------
(AMZN <- read_csv("AMZN_Yahoo.csv"))

# Checking ----------------------------------------------------------------
head.tail(AMZN) # start date 2014-12-31 end date 2019-12-30 

# Cleaning ----------------------------------------------------------------

  # tibble ---------------------------------------------------------
AMZN_tbl <- 
  AMZN %>% 
  mutate(Date = as.Date(Date)) %>%  # date conversion
  rename("Adjusted" = `Adj Close`) %>% 
  relocate(Volume, .before = Adjusted)
glimpse(AMZN_tbl)

  # xts ---------------------------------------------------------------------
AMZN_xts <- xts(AMZN_tbl %>% select(-Date), order.by = AMZN_tbl$Date)
names(AMZN_xts) <- paste("AMZN", colnames(AMZN_xts), sep = ".")
head.tail(AMZN_xts)

# Alternative method ------------------------------------------------------
(AMZN_getSym <- getSymbols("AMZN", from = "2014-12-31", to = "2019-12-31", auto.assign = FALSE))


# Plotting ----------------------------------------------------------------
plot(AMZN_xts$AMZN.Close)
str(AMZN_xts)

# Summary -----------------------------------------------------------------
summary(AMZN_xts$AMZN.Close)

# Freq conversion ---------------------------------------------------------
head.tail(to.weekly(AMZN_xts, name = "AMZN.wk"))
head.tail(to.monthly(AMZN_xts, name = "AMZN.mo"))


