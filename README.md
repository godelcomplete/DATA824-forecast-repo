library(shiny)
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(fredr)

#---------------------------------------
# DATA RETRIEVALFROM FRED
#---------------------------------------
fredr_set_key("INSERT API HERE") # REMINDER: DO NOT SHARE THIS CODE

# Retrieve CPI (CPIAUCSL)
cpi_data <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2000-01-01"),
  frequency = "m"
)

cpi_data <- cpi_data[order(cpi_data$date), ]
cpi_data$date <- as.Date(cpi_data$date)
start_year_cpi <- year(min(cpi_data$date))
start_month_cpi <- month(min(cpi_data$date))
ts_data_cpi <- ts(cpi_data$value, start = c(start_year_cpi, start_month_cpi), frequency = 12)
hist_dates_cpi <- cpi_data$date
ts_data_cpi_diff <- diff(ts_data_cpi)
hist_dates_cpi_diff <- hist_dates_cpi[-1]

# Retrieve Unemployment Rate (UNRATE)
unrate_data <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("2000-01-01"),
  frequency = "m"
)

unrate_data <- unrate_data[order(unrate_data$date), ]
unrate_data$date <- as.Date(unrate_data$date)
start_year_unrate <- year(min(unrate_data$date))
start_month_unrate <- month(min(unrate_data$date))
ts_data_unrate <- ts(unrate_data$value, start = c(start_year_unrate, start_month_unrate), frequency = 12)
hist_dates_unrate <- unrate_data$date
ts_data_unrate_diff <- diff(ts_data_unrate)
hist_dates_unrate_diff <- hist_dates_unrate[-1]

# Retrieve Real GDP (GDPC1) - Note: GDPC1 is quarterly data. We'll assume monthly frequency is desired?
# Actually, GDPC1 is quarterly. For consistency with monthly ARIMA, let's just proceed, but note frequency=4 for quarterly.
gdp_data <- fredr(
  series_id = "GDPC1",
  observation_start = as.Date("2000-01-01")
  # frequency defaults to quarterly since GDPC1 is quarterly data
)

gdp_data <- gdp_data[order(gdp_data$date), ]
gdp_data$date <- as.Date(gdp_data$date)
start_year_gdp <- year(min(gdp_data$date))
start_quarter_gdp <- (month(min(gdp_data$date)) - 1)/3 + 1  # Convert month to quarter index 1-4
# Note: GDPC1 is quarterly, frequency=4
ts_data_gdp <- ts(gdp_data$value, start = c(start_year_gdp, start_quarter_gdp), frequency = 4)
hist_dates_gdp <- gdp_data$date
ts_data_gdp_diff <- diff(ts_data_gdp)
hist_dates_gdp_diff <- hist_dates_gdp[-1]

#---------------------------------------
# CREATING FUNCTION FOR FORECAST AND EASIER CODING (OVERWHELMING OTHERWISE)
#---------------------------------------
create_forecasts <- function(ts_data, hist_dates, horizon, freq = "month") {
  # Fit ARIMA
  fit <- auto.arima(ts_data)
  fcast <- forecast(fit, h = horizon)
  
  last_hist_date <- max(hist_dates)
  
  # Generate future dates depending on freq
  if (freq == "month") {
    future_dates <- seq(from = last_hist_date %m+% months(1),
                        by = "month", length.out = horizon)
  } else if (freq == "quarter") {
    # For quarterly data, increment by 3 months for each step
    future_dates <- seq(from = last_hist_date %m+% months(3),
                        by = "3 months", length.out = horizon)
  } else {
    stop("Unsupported frequency")
  }
  
  forecast_df <- data.frame(
    ds = future_dates,
    yhat = as.numeric(fcast$mean),
    yhat_lower = as.numeric(fcast$lower[, 2]),
    yhat_upper = as.numeric(fcast$upper[, 2])
  )
  
  hist_df <- data.frame(ds = hist_dates, y = as.numeric(ts_data))
  
  list(hist = hist_df, fcast = forecast_df, last_hist = last_hist_date)
}

#---------------------------------------
# DEFINE UI
#---------------------------------------
ui <- fluidPage(
  titlePanel("Forecasting Dashboard - Levels and Differenced Data for CPI, UNRATE, GDPC1"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("horizon", "Forecast Horizon (periods):",
                  min = 1, max = 36, value = 12),
      
      actionButton("go_forecast", "Update Forecast")
    ),
    
    mainPanel(
      h3("CPI Forecasts"),
      h4("Levels"),
      plotOutput("forecast_plot_cpi"),
      h4("Differenced"),
      plotOutput("forecast_plot_cpi_diff"),
      
      h3("Unemployment Rate (UNRATE) Forecasts"),
      h4("Levels"),
      plotOutput("forecast_plot_unrate"),
      h4("Differenced"),
      plotOutput("forecast_plot_unrate_diff"),
      
      h3("Real GDP (GDPC1) Forecasts (Quarterly)"),
      h4("Levels"),
      plotOutput("forecast_plot_gdp"),
      h4("Differenced"),
      plotOutput("forecast_plot_gdp_diff")
    )
  )
)

#---------------------------------------
# SERVER 
#---------------------------------------
server <- function(input, output, session) {
  
  forecast_data <- eventReactive(input$go_forecast, {
    horizon <- input$horizon
    
    # CPI - monthly
    cpi_levels <- create_forecasts(ts_data_cpi, hist_dates_cpi, horizon, freq = "month")
    cpi_diff <- create_forecasts(ts_data_cpi_diff, hist_dates_cpi_diff, horizon, freq = "month")
    
    # UNRATE - monthly
    unrate_levels <- create_forecasts(ts_data_unrate, hist_dates_unrate, horizon, freq = "month")
    unrate_diff <- create_forecasts(ts_data_unrate_diff, hist_dates_unrate_diff, horizon, freq = "month")
    
    # GDP - quarterly
    # Note: horizon in "periods" is quarterly steps for GDP.
    gdp_levels <- create_forecasts(ts_data_gdp, hist_dates_gdp, horizon, freq = "quarter")
    gdp_diff <- create_forecasts(ts_data_gdp_diff, hist_dates_gdp_diff, horizon, freq = "quarter")
    
    # Filter historical data to start from one year before last hist date for monthly data
    # and one year before (4 quarters before) last hist date for quarterly data.
    # For consistency, let's always filter 12 months back, even for quarterly.
    # This may show more than 4 quarters for gdp if it doesn't align perfectly, but it's okay.
    
    # 1 year before for monthly: last_hist %m-% months(12)
    # 1 year before for quarterly: also use %m-% months(12), this will show a year of data
    
    cpi_one_year_back <- cpi_levels$last_hist %m-% months(12)
    cpi_levels$hist <- cpi_levels$hist %>% filter(ds >= cpi_one_year_back)
    cpi_diff$hist <- cpi_diff$hist %>% filter(ds >= cpi_one_year_back)
    
    unrate_one_year_back <- unrate_levels$last_hist %m-% months(12)
    unrate_levels$hist <- unrate_levels$hist %>% filter(ds >= unrate_one_year_back)
    unrate_diff$hist <- unrate_diff$hist %>% filter(ds >= unrate_one_year_back)
    
    gdp_one_year_back <- gdp_levels$last_hist %m-% months(12)
    gdp_levels$hist <- gdp_levels$hist %>% filter(ds >= gdp_one_year_back)
    gdp_diff$hist <- gdp_diff$hist %>% filter(ds >= gdp_one_year_back)
    
    list(
      cpi_levels = cpi_levels, cpi_diff = cpi_diff,
      unrate_levels = unrate_levels, unrate_diff = unrate_diff,
      gdp_levels = gdp_levels, gdp_diff = gdp_diff
    )
  })
  
  # Plot functions
  output$forecast_plot_cpi <- renderPlot({
    dat <- forecast_data()$cpi_levels
    ggplot() +
      geom_line(data = dat$hist, aes(x = ds, y = y), color = "blue") +
      geom_line(data = dat$fcast, aes(x = ds, y = yhat), color = "red", linetype = "dashed") +
      geom_ribbon(data = dat$fcast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "gray80", alpha = 0.5) +
      labs(title = "CPI Forecast (Levels)", x = "Date", y = "CPI Level") +
      theme_minimal()
  })
  
  output$forecast_plot_cpi_diff <- renderPlot({
    dat <- forecast_data()$cpi_diff
    ggplot() +
      geom_line(data = dat$hist, aes(x = ds, y = y), color = "blue") +
      geom_line(data = dat$fcast, aes(x = ds, y = yhat), color = "red", linetype = "dashed") +
      geom_ribbon(data = dat$fcast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "gray80", alpha = 0.5) +
      labs(title = "CPI Forecast (Differenced)", x = "Date", y = "Differenced CPI") +
      theme_minimal()
  })
  
  output$forecast_plot_unrate <- renderPlot({
    dat <- forecast_data()$unrate_levels
    ggplot() +
      geom_line(data = dat$hist, aes(x = ds, y = y), color = "blue") +
      geom_line(data = dat$fcast, aes(x = ds, y = yhat), color = "red", linetype = "dashed") +
      geom_ribbon(data = dat$fcast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "gray80", alpha = 0.5) +
      labs(title = "Unemployment Rate Forecast (Levels)", x = "Date", y = "UNRATE (%)") +
      theme_minimal()
  })
  
  output$forecast_plot_unrate_diff <- renderPlot({
    dat <- forecast_data()$unrate_diff
    ggplot() +
      geom_line(data = dat$hist, aes(x = ds, y = y), color = "blue") +
      geom_line(data = dat$fcast, aes(x = ds, y = yhat), color = "red", linetype = "dashed") +
      geom_ribbon(data = dat$fcast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "gray80", alpha = 0.5) +
      labs(title = "Unemployment Rate Forecast (Differenced)", x = "Date", y = "Diff UNRATE") +
      theme_minimal()
  })
  
  output$forecast_plot_gdp <- renderPlot({
    dat <- forecast_data()$gdp_levels
    ggplot() +
      geom_line(data = dat$hist, aes(x = ds, y = y), color = "blue") +
      geom_line(data = dat$fcast, aes(x = ds, y = yhat), color = "red", linetype = "dashed") +
      geom_ribbon(data = dat$fcast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "gray80", alpha = 0.5) +
      labs(title = "Real GDP Forecast (Levels)", x = "Date", y = "Real GDP (Billions Chn. 2012 $)") +
      theme_minimal()
  })
  
  output$forecast_plot_gdp_diff <- renderPlot({
    dat <- forecast_data()$gdp_diff
    ggplot() +
      geom_line(data = dat$hist, aes(x = ds, y = y), color = "blue") +
      geom_line(data = dat$fcast, aes(x = ds, y = yhat), color = "red", linetype = "dashed") +
      geom_ribbon(data = dat$fcast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                  fill = "gray80", alpha = 0.5) +
      labs(title = "Real GDP Forecast (Differenced)", x = "Date", y = "Diff GDP") +
      theme_minimal()
  })
}

#---------------------------------------
# RUN THE APP
#---------------------------------------
shinyApp(ui = ui, server = server)
