install.packages(c("tidyverse","lubridate","forecast","tseries",
                   "ggplot2","plotly","reshape2","corrplot"))

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(plotly)
library(reshape2)
library(corrplot)

data <- read.csv("C:/Users/hp/Downloads/weather_forecast_data.csv")

head(data)
str(data)
summary(data)
# Remove NA
data <- na.omit(data)

# Convert Rain into factor
data$Rain <- as.factor(data$Rain)

colSums(is.na(data))

ggplot(data, aes(x=Temperature)) +
  geom_histogram(fill="skyblue", bins=30) +
  theme_minimal() +
  ggtitle("Temperature Distribution")

ggplot(data, aes(x=1:nrow(data), y=Temperature)) +
  geom_line(color="gray") +
  theme_minimal() +
  ggtitle("Temperature Trend")

ggplot(data, aes(x=Rain, y=Temperature, fill=Rain)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Temperature vs Rain")

#Correlation Heatmap
numeric_data <- data %>% select_if(is.numeric)

cor_matrix <- cor(numeric_data)

corrplot(cor_matrix, method="color", type="upper")

#3D Weather Visualization
plot_ly(data,
        x = ~Temperature,
        y = ~Humidity,
        z = ~Wind_Speed,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 3)) %>%
  layout(title = "3D Weather Visualization")

#Time Series Create (Temperature)
ts_temp <- ts(data$Temperature, frequency=12)

plot(ts_temp, main="Time Series of Temperature")

#Stationarity Test
adf.test(ts_temp)

#ARIMA Model
arima_model <- auto.arima(ts_temp)

summary(arima_model)

forecast_arima <- forecast(arima_model, h=30)

plot(forecast_arima, main="ARIMA Forecast")

#ETS MODEL
ets_model <- ets(ts_temp)
summary(ets_model)
forecast_ets <- forecast(ets_model, h=30)
plot(forecast_ets, main="ETS Forecast")

#comparision of the arima and ets model
accuracy(forecast_arima)
accuracy(forecast_ets)

#Combined Forecast Graph
autoplot(ts_temp) +
  autolayer(forecast_arima$mean, series="ARIMA") +
  autolayer(forecast_ets$mean, series="ETS") +
  ggtitle("ARIMA vs ETS Forecast Comparison") +
  theme_minimal()

#Decomposition
decomp <- decompose(ts_temp)
plot(decomp)

#Temperature Categories
data$Temp_Category <- cut(data$Temperature,
                          breaks=c(-Inf,15,25,35,Inf),
                          labels=c("Cold","Normal","Warm","Hot"))
data$Pressure_Level <- ifelse(data$Pressure > mean(data$Pressure),
                              "High","Low")

#2D Scatter Plot (Temp vs Humidity)
ggplot(data, aes(x=Temperature, y=Humidity, color=Rain)) +
  geom_point(size=3) +
  scale_color_manual(values=c("red","blue")) +
  theme_minimal() +
  ggtitle("Temperature vs Humidity (Rain Colored)")

#Multi-Line Plot (Temp, Humidity, Wind)
data_long <- data %>%
  select(Temperature, Humidity, Wind_Speed) %>%
  mutate(Index = 1:n()) %>%
  pivot_longer(cols=-Index)

ggplot(data_long, aes(x=Index, y=value, color=name)) +
  geom_line(size=1) +
  theme_minimal() +
  ggtitle("Multi Weather Parameter Trend")

#density plot
ggplot(data, aes(x=Temperature, fill=Rain)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("orange","green")) +
  theme_minimal() +
  ggtitle("Temperature Density by Rain")

#violin plot
ggplot(data, aes(x=Rain, y=Humidity, fill=Rain)) +
  geom_violin() +
  theme_minimal() +
  ggtitle("Humidity Distribution by Rain")

#3d surface plot
library(plotly)

matrix_data <- matrix(data$Temperature[1:100],
                      nrow=10, ncol=10)

plot_ly(z = ~matrix_data) %>%
  add_surface(colorscale="Viridis") %>%
  layout(title="3D Temperature Surface")

#Forecast Confidence Interval Plot (Colorful)

autoplot(forecast_arima) +
  theme_minimal() +
  ggtitle("ARIMA Forecast with Confidence Interval") +
  scale_colour_brewer(palette="Set1")

#Cluster Visualization
set.seed(123)
weather_scaled <- scale(data[,c("Temperature","Humidity",
                                "Wind_Speed","Cloud_Cover","Pressure")])
kmeans_model <- kmeans(weather_scaled, centers=3)
data$Cluster <- as.factor(kmeans_model$cluster)
ggplot(data, aes(x=Temperature,
                 y=Humidity,
                 color=Cluster)) +
  geom_point(size=3) +
  theme_minimal() +
  ggtitle("Weather Clustering")
plot_ly(data,
        x=~Temperature,
        y=~Humidity,
        z=~Wind_Speed,
        color=~Cluster,
        colors="Set1",
        type="scatter3d",
        mode="markers") %>%
  layout(title="3D Cluster Visualization")