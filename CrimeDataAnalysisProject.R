library(dplyr)
library(lubridate)
library(e1071)
library(caret)
library(ggplot2)

crime_data <- read.csv("C:/Users/Asus/Downloads/BDA/crime_dataset_india.csv", stringsAsFactors = FALSE)

crime_data$Date_Reported <- mdy_hm(crime_data$Date.Reported)
crime_data$Date_of_Occurrence <- mdy_hm(crime_data$Date.of.Occurrence)
crime_data$Date_Case_Closed <- mdy_hm(crime_data$Date.Case.Closed)
crime_data$Month <- month(crime_data$Date_of_Occurrence)
crime_data$Weekday <- weekdays(crime_data$Date_of_Occurrence)
crime_data$Case_Closed_Flag <- ifelse(tolower(crime_data$Case.Closed) == "yes", 1, 0)

model_data <- crime_data %>%
  select(City, Crime.Code, Victim.Age, Victim.Gender, Weapon.Used, Police.Deployed, Case_Closed_Flag) %>%
  na.omit()

model_data$City <- as.factor(model_data$City)
model_data$Crime.Code <- as.factor(model_data$Crime.Code)
model_data$Victim.Gender <- as.factor(model_data$Victim.Gender)
model_data$Weapon.Used <- as.factor(model_data$Weapon.Used)
model_data$Case_Closed_Flag <- as.factor(model_data$Case_Closed_Flag)

set.seed(123)
trainIndex <- createDataPartition(model_data$Case_Closed_Flag, p = 0.8, list = FALSE)
train_data <- model_data[trainIndex, ]
test_data <- model_data[-trainIndex, ]

nb_model <- naiveBayes(Case_Closed_Flag ~ ., data = train_data)
predictions <- predict(nb_model, test_data)
confusionMatrix(predictions, test_data$Case_Closed_Flag)

z_scores <- scale(crime_data$Victim.Age)
crime_data$Z_Anomaly_Age <- ifelse(abs(z_scores) > 3, 1, 0)
anomalies_age <- crime_data[crime_data$Z_Anomaly_Age == 1, ]

Q1 <- quantile(crime_data$Police.Deployed, 0.25, na.rm = TRUE)
Q3 <- quantile(crime_data$Police.Deployed, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val
crime_data$IQR_Anomaly_PD <- ifelse(crime_data$Police.Deployed < lower_bound | crime_data$Police.Deployed > upper_bound, 1, 0)
anomalies_pd <- crime_data[crime_data$IQR_Anomaly_PD == 1, ]

unsolved_by_city <- crime_data %>%
  filter(Case_Closed_Flag == 0) %>%
  count(City, sort = TRUE)

crime_data$Hour <- substr(crime_data$Time.of.Occurrence, 12, 13)
table(crime_data$Hour)

crime_data %>%
  mutate(Year = year(Date_Reported)) %>%
  group_by(Year) %>%
  summarise(Total_Crimes = n()) -> crime_summary

ggplot(crime_summary, aes(x = Year, y = Total_Crimes)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Crime Rate Comparison by Year",
       x = "Year",
       y = "Total Crimes Reported") +
  scale_y_continuous(breaks = seq(0, 5000, by = 500), limits = c(0, 5000)) +
  theme_minimal()

unresolved_cases <- crime_data %>%
  filter(tolower(Case.Closed) == "no") %>%
  group_by(City) %>%
  summarise(Unresolved_Count = n()) %>%
  mutate(Percentage = round(100 * Unresolved_Count / sum(Unresolved_Count), 1),
         Label = paste0(City, " (", Percentage, "%)"))

ggplot(unresolved_cases, aes(x = "", y = Percentage, fill = Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Unresolved Cases by City") +
  theme_void() +
  theme(legend.title = element_blank())

top_crimes_by_city <- crime_data %>%
  group_by(City, Crime.Description) %>%
  summarise(Frequency = n(), .groups = 'drop_last') %>%
  slice_max(order_by = Frequency, n = 1)

ggplot(top_crimes_by_city, aes(x = reorder(City, -Frequency), y = Frequency, fill = Crime.Description)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Frequent Crime in Each City",
       x = "City",
       y = "Number of Crimes",
       fill = "Crime Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
