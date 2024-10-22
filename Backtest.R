# Install necessary packages


# Load the libraries
library(quantmod)
library(TTR)
library(PerformanceAnalytics)

# Get Apple stock data
getSymbols("AAPL", from = "2010-01-01", to = Sys.Date())

# Calculate moving averages
short_ma <- SMA(Cl(AAPL), n = 50)
long_ma <- SMA(Cl(AAPL), n = 100)

# Create a signal vector
signal <- ifelse(short_ma > long_ma, 1, 0)
signal <- Lag(signal)  # Lag the signal to avoid look-ahead bias

# Handle NA values
signal <- na.locf(signal, na.rm = FALSE)  # Forward fill NA values in signals
signal[is.na(signal)] <- 0  # Replace remaining NA values with 0
returns <- dailyReturn(Cl(AAPL))
returns <- na.omit(returns)  # Remove NA values in returns

# Calculate strategy returns
strategy_returns <- returns * signal

# Plot the cumulative returns
plot(cumsum(returns), main = "Cumulative Returns", col = "blue", type = "l")
lines(cumsum(strategy_returns), col = "red")
legend("topright", legend = c("Buy and Hold", "Strategy"), col = c("blue", "red"), lty = 1)

# Print performance summary
strategy_performance <- Return.cumulative(strategy_returns)
cat("Strategy Cumulative Returns: ", strategy_performance * 100, "%\n")

