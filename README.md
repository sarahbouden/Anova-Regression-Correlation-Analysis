# 📈 Time Series Analysis & Forecasting of Industrial Electronics Production

## Project Overview 🎯

This project performs a comprehensive time series analysis on industrial electronics production data from January 2009 to March 2024. The primary goal is to dissect the data's underlying structure, identify key patterns like trends and seasonality, and build a robust forecasting model to predict future production output.

The analysis follows a classic statistical modeling workflow, starting from data exploration and ending with model validation and forecasting, all implemented in R.

## The Dataset 📊
Source: Industrial production data, specifically for the electronics sector (IPB53122N).

Timeframe: January 2009 - March 2024

Frequency: Monthly

The dataset captures the cyclical and long-term growth patterns of the electronics industry over more than a decade.

## Analysis Workflow & Methodology 🛠️
The project was structured into several key stages, from initial data inspection to final prediction.

### 1. Data Preparation & Exploration

The first step involved loading the data and converting it into a time series object in R. Initial visualizations immediately revealed a strong upward trend, indicating consistent growth in production over the years.

### 2. Stationarity Testing

A crucial step in time series analysis is to ensure the data is stationary (i.e., its statistical properties do not change over time).

Augmented Dickey-Fuller (ADF) Test: The initial test confirmed that the series was non-stationary, primarily due to the strong trend.

Differencing: To achieve stationarity, the series was differenced, effectively removing the trend and preparing it for modeling.

### 3. Model Identification
 With a stationary series, the next step was to identify the parameters (p, d, q) for an ARIMA (AutoRegressive Integrated Moving Average) model.

Autocorrelation Function (ACF) Plot: Helped identify the order of the Moving Average (MA) component.

Partial Autocorrelation Function (PACF) Plot: Helped identify the order of the AutoRegressive (AR) component.

### 4. Model Building & Selection 🏗️

Several models were constructed and compared to find the best fit for the data:

auto.arima(): An automated algorithm from the forecast package was used to find the optimal ARIMA model.

Manual ARIMA Model: A model was also built manually based on the insights from the ACF/PACF plots.

Alternative Models: Linear, Quadratic, and Harmonic regression models were also fitted to compare against the ARIMA model.

The final model was selected based on performance metrics like AIC (Akaike Information Criterion), BIC (Bayesian Information Criterion), and RMSE (Root Mean Squared Error).

### 5. Model Diagnostics & Validation 🩺
After selecting the best model, a thorough diagnostic check was performed on its residuals (the errors of the model's predictions).

Ljung-Box Test: This test was used to ensure that the residuals were independent and resembled white noise, meaning no leftover patterns were left unexplained by the model.

Normality Test (Shapiro-Wilk): The residuals were tested for normality to ensure the model's assumptions were met.

Histogram of Residuals: A visual check was performed to confirm the distribution of the residuals.

### 6. Forecasting the Future 🔮

With a validated model in hand, the final step was to generate forecasts for future industrial production. The forecast package was used to predict the next several months, complete with confidence intervals to show the range of likely outcomes.

# Conclusion 🏁
This project successfully demonstrated an end-to-end time series analysis workflow. By systematically identifying trends, ensuring stationarity, and selecting an optimal ARIMA model, we developed a reliable tool for forecasting industrial electronics production.

The rigorous validation of the model's residuals confirms its statistical soundness, making it a valuable asset for strategic planning, resource allocation, and decision-making within the electronics industry.

