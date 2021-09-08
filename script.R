# ------------------------------------------------------------------------------------------
# Author: Eric Wang
# Data:
#  1. House sales: https://www.zillow.com/research/data/ (Zillow: "Sales Count and Price Cuts")
#  2. Private residential construction spending in millions: https://fred.stlouisfed.org/series/PRRESCONS (Federal Reserve)
# Academic reference:
#  1. Hyndman and Athanasopoulos's Forecasting: Principles and Practice, 2nd edition (2018)
# ------------------------------------------------------------------------------------------
# Header -----------------------------------------------------------------------------------
# Path
rm(list = ls())
DATAROOT <-  "Documents/GitHub datasets/Forecast housing market/"

# Options
options(scipen = 999)
options(stringsAsFactors = FALSE)
options(mc.cores = 4)

# Packages
library(DBI)
library(tidycensus)
library(readr)
library(readxl)
library(parallel)
library(car)
library(openxlsx)
library(magrittr)
library(stringr)
library(forecast)
library(lubridate)
library(rlang)
library(purrr)
library(stringdist)
library(tidyr)
library(glue)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(XML)
library(methods)
library(haven)
library(janitor)
library(dataCompareR)
library(furrr)
library(scales)
library(vctrs)
library(zoo)
library(fredr)
library(wbstats)
library(forcats)

# Code -------------------------------------------------------------------------------------
# House sales and construction spending are both major macroeconomic indicators.
# For example, construction spending can help serve as a proxy variable for the
# economic effects of the COVID epidemic.

# Import, format fields, and filter to NY
sales_df <- read_csv(paste0(DATAROOT, "Metro_sales_count_now_uc_sfrcondo_month.csv")) %>% 
  clean_names() %>% 
  filter(region_name == 'New York, NY') %>% 
  pivot_longer(cols = starts_with('x'), names_to = 'date', values_to = 'sales') %>% 
  mutate(date = as.yearmon(str_replace_all(date, c('x' = '', '_' = '-')))) %>% 
  select(date, sales)

spending_df <- read_csv(paste0(DATAROOT, "PRRESCONS.csv")) %>% 
  clean_names() %>% 
  rename(spending = prrescons) %>% 
  mutate(date = as.yearmon(date))

market_df <- sales_df %>% 
  left_join(spending_df, by = "date") 

# Check that the dates are complete
check <- tibble(date = seq(min(market_df$date), max(market_df$date), 1/12))
stopifnot(all(check$date == market_df$date))

# Check for and handle missing values using linear interpolation
print(summary(market_df))

market <- market_df %>% 
  # Only the sales field has some missing values
  mutate(interpolated_sales = na.interp(sales)) %>% 
  arrange(date) %>% 
  # Reshape data into a time series matrix; zoo package facilitated this
  # Confine date range to 2009-2019
  ts(start = c(2009, 1), end = c(2019, 12), frequency = 12)

rm(market_df, sales_df, spending_df)

# Quickly plot interpolated house sales to verify their seasonal variance
autoplot(market, facets = TRUE)

# Quickly check correlation between house sales and construction spending
# As expected, these variables are strongly correlated
plot(as.vector(market[, 'interpolated_sales']), as.vector(market[, 'spending']))
cor(market[, 'interpolated_sales'], market[, 'spending'])

# Create a training set. I hope to forecast one year ahead and the convention
# is that the test set should be around 20% the size of the training set
plot(market[, 'interpolated_sales'])
train <- window(market, start = c(2013, 1), end = c(2018, 12))
next_steps <- 1*12

# Choose the optimal dynamic regression model with ARIMA errors
model1 <- auto.arima(train[, 'interpolated_sales'], 
                    xreg = train[, 'spending'], 
                    ic = "aicc",
                    stepwise = FALSE,
                    approximation = FALSE)

# Check fitted model's residuals using the Ljung-Box test
# P-value approaches 0.05. The residuals of this model exhibit autocorrelation, 
# indicating significant lack of fit. The automatically chosen model minimizes 
# the AICc among possible other models but is not necessarily the most valid model.
checkresiduals(model1)

# Plot the ACF of the differenced data to determine a better lag
# The lag parameter is fine
ggAcf(diff(train[, 'interpolated_sales']))

# Try adjusting the training set so that it is roughly stable and starts and 
# concludes at the beginning and ending of a cycle
train <- window(market, start = c(2013, 2), end = c(2018, 1))
plot(train[, 'interpolated_sales'])

# Rerun the algorithm and check again
# This passes the residuals test
model2 <- auto.arima(train[, 'interpolated_sales'], 
                    xreg = train[, 'spending'], 
                    ic = "aicc",
                    stepwise = TRUE,
                    approximation = FALSE)
checkresiduals(model2)

# Create the forecast
  
  # First, forecast spending using ARIMA, which will be used as a model input
  prelim_forecast <- auto.arima(train[, 'spending'], ic = "aicc",
                                stepwise = FALSE, approximation = FALSE) %>% 
    forecast(h = next_steps)
  
  # This passes the residuals test
  checkresiduals(prelim_forecast)
  
forecast <- forecast(model2, xreg = prelim_forecast$mean)

# Check the forecast's accuracy. The mean absolute percentage error (MAPE) may 
# be the easiest metric for most people to interpret, although it requires
# that all of the data be non-zero. There are many critiques of MAPE.
accuracy <- accuracy(forecast, market[, 'interpolated_sales'])
summary(model2)
stopifnot(all(market[, 'interpolated_sales'] > 0))

# Create forecast plot with summary for sharing
font_size <- 14

plot <- autoplot(forecast
  ) + 
  autolayer(window(market[, 'interpolated_sales'], start = c(2009, 1), end = c(2019, 1)), series = "actual"
  ) +
  scale_y_continuous(
    labels = number_format(big.mark = ",")
  ) +
  theme(
    text = element_text(family = 'Helvetica', size = font_size, colour = "black"),
    
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 0.5, size = font_size + 4, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    plot.margin = unit(rep(1.5, 4), "cm"),
    
    panel.background = element_rect(fill = "white"),
    
    axis.line = element_line(),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.title.y = element_text(angle = 0, vjust = 1.05, hjust = 0, colour = "black"),
    
    legend.text = element_text(size = font_size),
    legend.position = "right",
    legend.key = element_rect(fill = "white")
  ) +
  labs(
    title = "Forecast of NY House Sales (February 2018 to January 2019)",
    subtitle = paste0('Sales'),
    y = '',
    x = 'Year',
    caption = paste0("Note: Dynamic regression with ARIMA(2,0,0)(0,1,0)[12] errors. First, I regressed house sales in NY onto",
    " private residential construction spending \nusing ARIMA errors. Then, I used that model to forecast house sales twelve months", 
    " ahead from February 2018 to January 2019 using \nanother ARIMA forecast of construction spending. My final forecast is in blue",
    " with shaded 95% prediction intervals. The forecast's MAPE was ", percent((100 - accuracy[2, 5])/100, accuracy = 0.1) , ".")
  )
plot
golden_ratio <- (1 + sqrt(5))/2
ggsave(plot, path = 'Documents/GitHub/Housing forecasts', filename = "overlay.pdf", height = 8, width = 8 * golden_ratio, encoding = "ISOLatin9.enc")
