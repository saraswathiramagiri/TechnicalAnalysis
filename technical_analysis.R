# BDA400 Assignment 2 - Technical Analysis using R

library(quantmod)
library(TTR)
# Read stock symbols from portfolio.txt
portfolio <- readLines("portfolio.txt")

# Display the portfolio
portfolio
# Function to load stock data
load_stock_data <- function(symbols) {
  stock_data <- list()
  
  for (symbol in symbols) {
    data <- getSymbols(symbol, src = "yahoo",
                       from = "2025-01-01",
                       auto.assign = FALSE)
    stock_data[[symbol]] <- data
  }
  
  return(stock_data)
}
# Load stock data
stocks <- load_stock_data(portfolio)

# Display loaded stock symbols
names(stocks)
# Calculate summary statistics for each stock
for (symbol in names(stocks)) {
  cat("\n", symbol, "\n")
  print(summary(Cl(stocks[[symbol]])))
}
# Calculate 20-day Simple Moving Average for each stock
for (symbol in names(stocks)) {
  stocks[[symbol]]$SMA20 <- SMA(Cl(stocks[[symbol]]), n = 20)
}
# Calculate 50-day Simple Moving Average for each stock
for (symbol in names(stocks)) {
  stocks[[symbol]]$SMA50 <- SMA(Cl(stocks[[symbol]]), n = 50)
}
# Calculate RSI for each stock
for (symbol in names(stocks)) {
  stocks[[symbol]]$RSI <- RSI(Cl(stocks[[symbol]]), n = 14)
}
# Calculate MACD for each stock
for (symbol in names(stocks)) {
  macd_values <- MACD(Cl(stocks[[symbol]]))
  stocks[[symbol]]$MACD <- macd_values[, 1]
  stocks[[symbol]]$Signal <- macd_values[, 2]
}
# Calculate Bollinger Bands for each stock
for (symbol in names(stocks)) {
  bb_values <- BBands(Cl(stocks[[symbol]]), n = 20)
  stocks[[symbol]]$BB_Upper <- bb_values[, "up"]
  stocks[[symbol]]$BB_Middle <- bb_values[, "mavg"]
  stocks[[symbol]]$BB_Lower <- bb_values[, "dn"]
}
colnames(stocks[["AAPL"]])
# Plot AAPL stock price
chartSeries(stocks[["AAPL"]],
            name = "AAPL Stock Price",
            theme = chartTheme("white"))
addSMA(n = 20)
addSMA(n = 50)
addRSI(n = 14)
addMACD()
addBBands(n = 20)
# Function to calculate basic statistics
calculate_statistics <- function(stock_data) {
  close_prices <- as.numeric(Cl(stock_data))
  
  mode_value <- as.numeric(
    names(sort(table(close_prices), decreasing = TRUE)[1])
  )
  
  statistics <- list(
    Moving_Average = mean(tail(close_prices, 20), na.rm = TRUE),
    Mean = mean(close_prices, na.rm = TRUE),
    Mode = mode_value,
    Median = median(close_prices, na.rm = TRUE),
    Standard_Deviation = sd(close_prices, na.rm = TRUE)
  )
  
  return(statistics)
}

# Calculate statistics for all stocks
stock_statistics <- lapply(stocks, calculate_statistics)

# Display calculated statistics
stock_statistics