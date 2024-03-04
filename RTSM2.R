# Required Packages
packages = c('quantmod') 

# Install all Packages with Dependencies
install.packages(packages, dependencies = TRUE) 

install.packages("tseries")
library(tseries)


# Load all Packages
lapply(packages, require, character.only = TRUE)

# Required Packages
packages = c('tseries', 'forecast')

# Load all Packages
lapply(packages, require, character.only = TRUE)

# Required Packages
packages = c('FinTS', 'rugarch')

# Install all Packages with Dependencies
install.packages(packages, dependencies = TRUE)


# Load all Packages
lapply(packages, require, character.only = TRUE)

# Create an environment to store the data
stock_data <- new.env()

library(forecast)

# Define the stock symbol
stock_symbol <- 'WIPRO.NS'  #WIPRO.NS is the ticker symbol for Wipro Ltd BSE

# Define the start and end dates
start_date <- as.Date('2019-01-01')
end_date <- as.Date('2023-12-31')

# Fetch the stock data
getSymbols(Symbols = stock_symbol, src = 'yahoo', from = start_date, to = end_date, env = stock_data)


# Extract closing prices
rel_prices <- Cl(stock_data[[stock_symbol]])

# Remove missing values
rel_prices <- na.omit(rel_prices)

# View the resulting closing prices
head(rel_prices)


# Check the class of the stock_price object
class(rel_prices)


# Augmented Dickey-Fuller (ADF) Test for Stationarity with Adani Enterprises Ltd on BSE Data
# *******************************************************************

# Perform Augmented Dickey-Fuller (ADF) Test
adf_result <- adf.test(rel_prices)

# Print the ADF test result
print(adf_result)

#In this case, since the p-value (0.4087) is greater than a typical significance
#level like 0.05, we fail to reject the null hypothesis.
#Therefore, we do not have enough evidence to conclude that the time series is stationary.

rel_ds = diff(log(rel_prices)); plot(rel_prices) #Reliance Ltd  (First)return  #Difference Time-Series

rel_ds=na.omit(rel_ds)
adf_test_rel_ds = adf.test(rel_ds); adf_test_rel_ds

# Ljung-Box Test for Autocorrelation -  Data
# ***********************************************

lb_test_rel_ds = Box.test(rel_ds)# Inference : reliance Difference (Stationary)
lb_test_rel_ds

#In this case, since the p-value (4.294e-05) is much smaller than a typical significance level like 0.05,

#we reject the null hypothesis. Therefore, we have evidence to suggest that there is significant autocorrelation in the time series. | NULL: No Auto correlation | Alternate: Auto Correlation

#Autocorrelation Function (ACF) | Partial Autocorrelation Function (PACF)
# *****************************************************************************

acf(rel_prices) # ACF of adani Series

pacf(rel_prices) # PACF of adani Series

acf(rel_ds) # ACF of adani Series

pacf(rel_ds) # PACF of adani Difference (Stationary) Series

arma_pq_rel_ds = auto.arima(rel_ds); arma_pq_rel_ds

rel_ds_fpq = forecast(arma_pq_rel_ds, h = 500)
plot(rel_ds_fpq)

lb_test_arma_pq_rel_ds = Box.test(arma_pq_rel_ds$residuals); 
lb_test_arma_pq_rel_ds

# Test for Volatility Clustering or Heteroskedasticity: Box Test
rel_ret_sq = arma_pq_rel_ds$residuals^2 # Residual Variance (Since Mean Returns is approx. 0)
plot(rel_ret_sq)

rel_ret_sq_box_test = Box.test(rel_ret_sq,lag = 1 ) # H0: Return Variance Series is Not Serially Correlated
rel_ret_sq_box_test # Inference : Return Variance Series is Autocorrelated (Has Volatility Clustering)

# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
#rel_ret_arch_test <- arch.test(arma_pq_rel_ds$residuals^2, lags = 1)
 # H0: No ARCH Effects
arch_test_result <-tseries:: arch.test(arma_pq_rel_ds$residuals^2, lags = 1)

rel_ret_arch_test # Inference : Return Series is Heteroskedastic (Has Volatility Clustering)

# GARCH Model
garch_model1 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,1), include.mean = TRUE))
rel_ret_garch1 = ugarchfit(garch_model1, data = arma_pq_rel_ds$residuals^2); rel_ret_garch1

# GARCH Forecast
rel_ret_garch_forecast1 = ugarchforecast(rel_ret_garch1, n.ahead = 500); rel_ret_garch_forecast1

plot(rel_ret_garch_forecast1, which = 1)

# Plot the specified aspect of the GARCH forecast
plot(rel_ret_garch_forecast1, which = 3)




