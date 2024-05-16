#Install and load packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

#Load dataset
data <- read.csv("~/Ife Data/SDG_analysis.csv")

#Carrying out simple exploration of dataset.
head(data) #Shows first 6 rows of dataset.
tail(data) #Shows last 6 rows of dataset.
colnames(data) #Shows column names of dataset.
str(data) #Shows structure of dataset including the data type.
dim(data) #Shows dimension of the dataset, we have 252 rows and 22 columns.
glimpse(data) #Shows column of dataset with some portion of dataset.
view(data) #Shows entire dataset.

#Check for, and drop missing data.
sum(is.na(data)) #There are 105 missing data
data <- data %>% drop_na()
sum(is.na(data)) #All dropped
dim(data) #Dataset new dimension is 342 rows and 12 columns.

#The Year_code and Country_code columns will not be useful for this analysis, hence we remove them
data <- data[-c(2,4)]
view(data)
str(data)

#Exploratory Data Analysis (EDA)
#Check for outliers for GDP
Filtered_country <- data %>%
  filter(Country == "Nigeria") 
ggplot(Filtered_country, aes(x = Country, y = GDP))+ 
  geom_boxplot(fill = "blue") + theme(panel.background = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  labs(x = "Country", y = "GDP") +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))

# Visualization for Population Across Countries
# Sort the dataset by Population in descending order
sorted_data <- data %>% arrange(desc(Population))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2017) 
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries
ggplot(top_10, aes(x = reorder(Country, -Population), y = Population)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Top 10 Countries by Population - 2017",
       x = "Country", y = "Population") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + #Turn the x-axis labels to improve legibility.
  theme(panel.background = element_blank()) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

# Visualization for Population with Access to Electricity Across Countries
# Sort the dataset by Access to electricity in descending order
sorted_data <- data %>% arrange(desc(Access_to_electricity))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2017) 
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries 
ggplot(top_10,      aes(x = reorder(Country, -Access_to_electricity), y = Access_to_electricity)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Top 10 Countries by Access to Electricity - 2017",
       x = "Country", y = "Population with Access to Electricity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales:: percent_format(scale = 1)) + theme(panel.background = element_blank())

# Visualization for Population Using Internet Across Countries
# Sort the dataset by Population Using Internet  in descending order
sorted_data <- data %>% arrange(desc(Population_using_Internet))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2017) 
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries using ggplot
ggplot(top_10, aes(x = reorder(Country, -Population_using_Internet), y = Population_using_Internet)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Countries by Population Using Internet - 2017",
       x = "Country", y = "Population Using Internet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1))+ theme(panel.background = element_blank())

# Visualization for Population with Drinkable water Across Countries
sorted_data <- data %>% arrange(desc(Population_with_drinkable_water))
# Group by a particular year 
grouped_data <- sorted_data %>%
  filter(Year == 2017) 
# Select the top 10 countries
top_10 <- head(grouped_data, 10)
# Plot the top 10 countries using ggplot
ggplot(top_10, aes(x = reorder(Country, -Population_with_drinkable_water), y = Population_with_drinkable_water)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Top 10 Countries by Population with Drinkable water - 2017",
       x = "Country", y = "Population with Drinkable water") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1))+ theme(panel.background = element_blank())

# Visualization for Maternal Mortality Ratio Trend
Filtered_country <- data %>%
  filter(Country == "South Africa") 
ggplot(Filtered_country, aes(x = Year, y = Maternal_mortality_ratio , color = Country)) +
  geom_line() +
  labs(title = "South Africa Maternal Mortality Ratio Trend 2000 to 2017",
       x = "Year", y = "Maternal Mortality Ratio")+ theme(panel.background = element_blank()) +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))


#STATSITICAL ANALYSIS
"We will verify that the data are normally distributed. To do this, we will do the
Shapiro-Wilk (Statistical Test), Q-Q plot (Visual Method), and Kolmogorov-Smirnov 
(Statistical Test) hypothesis tests."
#Install and load datarium and qqplotr

install.packages("datarium")
install.packages("qqplotr")
library(datarium)
library(qqplotr)

#Shapiro Test
#Test for individual indicators. Take Population with drinkable water and Access to electricity for example.
shapiro.test(data$Population_with_drinkable_water)
shapiro.test(data$Access_to_electricity)

"We reject the null hypothesis because the data are not normally distributed, since the p-value 
is less than the significance level (0.05)."

#Perform the Q-Q test
qqnorm(data$Population_with_drinkable_water)
qqline(data$Population_with_drinkable_water, col = 2)
qqnorm(data$Access_to_electricity)
qqline(data$Access_to_electricity, col = 2) 
#These does not imply normalcy because the points do not closely follow the reference line.

#Perform the Kolmogorov-Smirnov test
ks.test(data$Population_with_drinkable_water, "pnorm")
ks.test(data$Access_to_electricity, "pnorm")
"We reject the null hypothesis because the data are not normally distributed, since the p-value 
is less than the significance level (0.05)."

"Since the data does not pass normality test, as we have noticed, we take the data's log to make this right."
#First, we eliminate any non-integer and non-numeric column(s).
data1 <- data[-c(2)]
view(data1)
str(data1)
#Take log to normalize data.
data_norm <- log(data1)
qqnorm(data_norm$Population_with_drinkable_water)
qqline(data_norm$Population_with_drinkable_water, col = 2)
qqnorm(data_norm$Access_to_electricity)
qqline(data_norm$Access_to_electricity, col = 2)
#This procedure is applicable to all non-normal indicators.

summary(data) #Summaries Statistics of the entire dataset

#Compute the mean, median, mode, standard deviation, skewness, and kurtosis.
#Mean, Median, and Standard deviation
mean_values <- apply(data1, 2, mean)
median_values <- apply(data1, 2, median)
std_dev_values <- apply(data1, 2, sd)

#Display the results.
print(mean_values)
print(median_values)
print(std_dev_values) 

#Create a function to calculate the Mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the user function for some columns. 
model_population <- getmode(data1$Population)
mode_water <- getmode(data1$Population_with_drinkable_water)
mode_gdp <- getmode(data1$GDP)
mode_maternal_mortality <- getmode(data1$Maternal_mortality_ratio)
mode_co2 <- getmode(data1$CO2_emissions)
mode_forest <- getmode(data1$Forest_area)
mode_electricity <- getmode(data1$Access_to_electricity)
mode_internet <- getmode(data1$Population_using_Internet)
print(model_population)
print(mode_water)
print(mode_gdp)
print(mode_maternal_mortality)
print(mode_co2)
print(mode_forest)
print(mode_electricity)
print(mode_internet)

#Install and load the e1071 package to calculate Skewness and Kurtosis. 
install.packages("e1071")
library(e1071)

# Calculate skewness
skewness_population <- skewness(data1$Population)
skewness_water <- skewness(data1$Population_with_drinkable_water)
skewness_gdp <- skewness(data1$GDP)
skewness_maternal_mortality <- skewness(data1$Maternal_mortality_ratio)
skewness_co2 <- skewness(data1$CO2_emissions)
skewness_forest <- skewness(data1$Forest_area)
skewness_electricity <- skewness(data1$Access_to_electricity)
skewness_internet <- skewness(data1$Population_using_Internet)


# Print results
print(skewness_population)
print(skewness_water)
print(skewness_gdp)
print(skewness_maternal_mortality)
print(skewness_co2)
print(skewness_forest)
print(skewness_electricity)
print(skewness_internet)

# Calculate kurtosis
kurtosis_population <- kurtosis(data1$Population)
kurtosis_water <- kurtosis(data1$Population_with_drinkable_water)
kurtosis_gdp <- kurtosis(data1$GDP)
kurtosis_maternal_mortality <- kurtosis(data1$Maternal_mortality_ratio)
kurtosis_co2 <- kurtosis(data1$CO2_emissions)
kurtosis_forest <- kurtosis(data1$Forest_area)
kurtosis_electricity <- kurtosis(data1$Access_to_electricity)
kurtosis_internet <- kurtosis(data1$Population_using_Internet)

# Print results
print(kurtosis_population)
print(kurtosis_water)
print(kurtosis_gdp)
print(kurtosis_maternal_mortality)
print(kurtosis_co2)
print(kurtosis_forest)
print(kurtosis_electricity)
print(kurtosis_internet)


#CORRELATION ANALYSIS
#Carryout Pearson and Spearman correlation analysis on dataset

cor_matrix1 <- cor(data1[, -1], method = "pearson")  # Exclude the 'Year' column
cor_matrix2 <- cor(data1[, -1], method = "spearman")  # Exclude the 'Year' column
print(cor_matrix1)
print(cor_matrix2)

#Interpretation
"The correlation coefficient ranges from -1 to 1.
A perfect positive correlation is shown by a correlation of 1.
A perfect negative correlation is represented by a correlation of -1.
There is no correlation when the correlation is 0."

#Visualization
#We can visualize the correlation matrix using corrplot
#Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix1, type = "upper", order = "hclust")
corrplot(cor_matrix2, type = "upper", order = "hclust")

#HYPOTHESIS TEST
#1. T-test
#Objective 1.	To evaluate the state of the SDG implementation in Africa, outlining both the challenges and areas of development.
t_test <- t.test(data1$Population_with_drinkable_water, data1$Year)
print(t_test)

#Since P-value <0.05, we reject the Null Hypothesis and accept the Alternative Hypothesis.
"Null Hypothesis(H0): There is no significant difference in the average SDG scores between African countries.
Alternative Hypothesis(H1): There is a significant difference in the average SDG scores between African countries."


#2. Chi-squared Test
#Objective 2.	To determine the key drivers behind achieving the SDGs.
chi_test <- chisq.test(data1$Year, data1$Access_to_electricity)
print(chi_test)

#Since P-value = 0.4046, i.e.P >0.05, we do not have enough evidence to reject the Null Hypothesis.
"Null Hypothesis(H0): There is no association between the key drivers behind development and the achievement of the SDGs.
Alternative Hypothesis(H1): There is an association between the key drivers behind development and the achievement of the SDGs."


#3.ANOVA
#Objective 3.	To identify efficient tactics and best practices that have sped up the achievement of the SDGs in African nations.
# Perform ANOVA for each variable against 'Year'
for (col in c("Population", "Population_with_drinkable_water", "GDP", 
              "Maternal_mortality_ratio", "CO2_emissions", "Forest_area", 
              "Access_to_electricity", "Population_using_Internet")) 
  {
  anova_result <- aov(as.formula(paste(col, "~ Year")), data = data1)
  print(paste("ANOVA for", col))
  print(summary(anova_result))
  }
#Interpretation
"Population:
  For F-value = 8.462 and p-value = 0.00387 (p<0.05), this indicates a significant difference in population means across years.
Population_with_drinkable_water:
  For F-value = 12.12 and p-value = 0.000563 (p<0.05), this indicates a significant difference in population with drinkable water means across years.
GDP:
  For F-value = 32.7 and p-value = 2.35e-08 (p<0.05), this indicates a significant difference in GDP means across years.
Maternal_mortality_ratio:
  For F-value = 21.32 and p-value = 5.52e-06 (p<0.05), this indicates a significant difference in maternal mortality ratio means across years.
CO2_emissions:
  For F-value = 0 and p-value = 0.999 (p>0.05), this indicates that there is no significant difference in CO2 emissions means across years.
Forest_area:
  For F-value = 0.211 and p-value = 0.646 (p>0.05), this indicates that there is no significant difference in forest area means across years.
Access_to_electricity:
  For F-value = 7.593 and p-value = 0.00618 (p<0.05), this indicates a significant difference in access to electricity means across years.
Population_using_Internet:
  For F-value = 227.2 and p-value <2e-16 i.e (p<0.05), this indicates a significant difference in population using the Internet means across years."

#Since P >0.05 for CO2 Emission and Forest Area, we do not have enough evidence to reject the Null Hypothesis.
#we reject the Null Hypothesis and accept the Alternative Hypothesis for other variables.
"Null Hypothesis(H0): There is no significant difference in the average SDG achievement scores among African nations using different tactics and best practices.
Alternative Hypothesis(H1): There is a significant difference in the average SDG achievement scores among African nations using different tactics and best practices."

#4.Mann-Whitney U test
#Objective 4. To make recommendations for methods to boost SDG performance and quicken the targets' advancement.
# Assume we want to compare 'Access_to_electricity' between two years, such as 2000 and 2017
# Subset the data for the two years
year_2000 <- data1[data1$Year == 2000, "Access_to_electricity"]
year_2017 <- data1[data1$Year == 2017, "Access_to_electricity"]
mann_whitney <- wilcox.test(year_2000, year_2017)
print(mann_whitney)

#Since P-value = 0.03887 i.e. p <0.05, we reject the Null Hypothesis and accept the Alternative Hypothesis.
"Null Hypothesis(H0): There is no method to boost SDG performance and quicken the targets' advancement
Alternative Hypothesis(H1): There are methods to boost SDG performance and quicken the targets' advancement"


#REGRESSION ANALYSIS
#1. Multiple Linear Regression (MLR)
#Objective 1: To evaluate the state of the SDG implementation in Africa.

"Response Variable: SDG achievement score.
Predictor Variables: Population with drinkable water, Access to electricity, Population using Internet
GDP, Maternal mortality ratio and CO2_emissions.
Rationale: Analyzing the effects of several independent variables on the dependent variable (SDG accomplishment score) 
is possible with multiple linear regression. This can help evaluate the state of the SDG implementation in Africa."

#Calculate SDG Achievement score using 3 predictor variables
data1$SDG_Achievement <- (data1$Population_with_drinkable_water + data1$Access_to_electricity 
                          + data1$Population_using_Internet + data1$GDP + data1$Maternal_mortality_ratio 
                          + data1$CO2_emissions) / 6

# Carry out MLR
MLR <- lm(SDG_Achievement ~ Population_with_drinkable_water + Access_to_electricity +
            Population_using_Internet + GDP + Maternal_mortality_ratio +CO2_emissions, data = data1)
summary(MLR)


"All coefficients are significant, and the MLR equation will be:
SDG_Achievement = -2.895e-05 + (1.667e-01) X Population_with_drinkable_water + (1.667e-01)
X Access_to_electricity + (1.667e-01) X Population_using_Internet + (1.667e-01) X GDP + (1.667e-01) X
Maternal_mortality_ratio + (1.667e-01) X CO2_emissions
R-squared is 1."

data.frame(colnames(data1))
#Lets visualize this.

pairs(data1[,c(3,4,5,6,8,9,10)], lower.panel = NULL, pch = 19,cex = 0.2)

#All 3 Predictor variables have a linear relationship with SDG Achievement.

#Check for residual independence.
plot(MLR, 1)
#The correlation is approximately zero(0)

#Check for normality of Residuals
plot(MLR, 2)
#Observations are close to the line, hence we can say it is normal.

#Check for equal variances of the residuals (Homoscedasticity) 
plot(MLR, 3)
#With almost equal variability at all fitted values, the residuals are fairly distributed around the red line.


#Using variance inflation factor,we check for multicollinearity.
#Install and load the Car package.
install.packages("car")
library(car)

vif(MLR)
"All 6 predictor variable values are 8.386052, 8.412712, 1.907005, 1.802504,2.739980 and 2.130300
respectively. Since two values greater then 5, we can conclude there is collinearity between IVs (Xs).
Since all 5 assumptions are not approved, we can fix this by simply removing the variables with values 
greater than 5 and continue prediction"

#Predict SDG Achievement
"# Coefficients
intercept <- -2.895e-05
gdp_coeff <- 1.667e-01
water_coeff <- 1.667e-01
maternal_coeff <- 1.667e-01
internet_coeff <- 1.667e-01

# Values
gdp <- 367
water <- 28
maternal <- 917
internet <- 0.57

# Calculate SDG Achievement
SDG_Achievement <- intercept + (gdp_coeff * gdp) + (water_coeff * water) + (maternal_coeff * maternal) + (internet_coeff * internet)

# Print the result
print(SDG_Achievement)
#218.8"


#2. Simple Linear Regression (SLR).
#Objective 1: 2.	To determine the key drivers behind development achieving the SDGs.

"Response Variable: SDG achievement score.
Predictor Variables: GDP
Rationale: Analyzing the effects of one independent variables on the dependent variable (SDG accomplishment score) 
is possible with simple linear regression. This can help determine the key drivers behind development achieving the SDGs."


SLR <- lm(SDG_Achievement ~ GDP, data = data1)
summary(SLR)


"Both coefficients are significant and the SLR equation will be:
SDG_Achievement = 9.395e+01 + (1.667e-01) × GDP
With 1 as R-squared value, using this regression equation, GDP can forecast 100% of the total 
variability in the SDG Achievement."

#Lets visualize this.
pairs(data1[,c(4,10)], lower.panel = NULL, pch = 19,cex = 0.2)
#GDP have a linear relationship with SDG Achievement.

#Check for residual independence.
plot(SLR, 1)
#The correlation is approximately zero(0)

#Check for normality of Residuals
plot(SLR, 2)
#Observations are close to the line, hence we can say it is normal.

#Check for equal variances of the residuals (Homoscedasticity) 
plot(SLR, 3)
#With almost equal variability at all fitted values, the residuals are fairly distributed around the red line.


#Predict SDG Achievement using GDP
"# Coefficients
intercept <- 9.395e+01
gdp_coeff <- 1.667e-01

# Values
gdp <- 367

# Calculate SDG Achievement
SDG_Achievement <- intercept + (gdp_coeff * gdp)

# Print the result
print(SDG_Achievement)
#61.18
percentage <- (61.18 / 218.8) * 100
print(percentage)
This means GDP can predict 27.96% of SDG Achievement considering our variables"


#3. Linear Regression
#Objective 3.	To make recommendations for methods to boost SDG performance and quicken the targets' advancement.

#Linear Regression
LR <- lm(SDG_Achievement ~ GDP, data = data1)
summary(LR)


#TIME SERIES ANALYSIS
#Install and load ts and forecast packages
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

#TIME SERIES 1. Maternal mortality ratio (MMR)
#Lets filter a country as sample for this analysis.
Filtered_country <- data %>%
  filter(Country == "Nigeria")
MMR_ts <- ts(Filtered_country$Maternal_mortality_ratio, start = 2000,frequency = 1)
plot(MMR_ts, main = "Nigeria Maternal mortality ratio Time Series 2000 to 2017", xlab = "Time",
     ylab = "Maternal mortality ratio")


adf.test(MMR_ts)  # Augmented Dickey-Fuller test for stationarity
"since p-value = 0.4831, i.e p>0.05 we accept the null hypothesis that suggests the time series 
is not stationary."

#Build a model for our forecast
arima_MMR <- Arima(MMR_ts)
print(arima_MMR)
"The MMR_ts time series data appears to suit the ARIMA(0,0,0) model with a non-zero mean quite 
well, with an estimated mean of 1031.83 and a variance of 10224."

# Forecast 5 years ahead
forecast_MMR <- forecast(arima_MMR, h = 5) 
forecast_MMR
plot(forecast_MMR, main = "Nigeria Maternal mortality ratio", xlab = "Time", ylab = "Maternal mortality ratio")

"According to the estimate, 95% of the maternal mortality ratio will fall between 833.6548 and 
1230.012 (light blue box), while 80% will fall between 902.2513 and 1161.415 (tick blue box)."

#lets build another model and use 99% CI.
arima_MMR2 <- auto.arima(MMR_ts)
print(arima_MMR2)
forecast_MMR2 <- forecast(arima_MMR2, h = 10, level = c(80,99))
forecast_MMR2
plot(forecast_MMR2, main = "Nigeria Maternal mortality ratio Forecast", xlab = "Time", ylab = "Maternal mortality ratio")
"According to the estimate, 99% of the maternal mortality ratio will fall between 879.6107 and 
938.3893 (light grey shade)"

#TIME SERIES 2. CO2 Emission
#Lets filter a country as sample for this analysis.
Filtered_country <- data %>%
  filter(Country == "Egypt, Arab Rep.")
CO2_ts <- ts(Filtered_country$CO2_emissions, start = 2000,frequency = 1)
plot(CO2_ts, main = "Egypt CO2 Emission Time Series 2000 to 2017", xlab = "Time",
     ylab = "CO2 Emission")

adf.test(CO2_ts)  # Augmented Dickey-Fuller test for stationarity
"since p-value = 0.7788, i.e p>0.05 we accept the null hypothesis that suggests the time series 
is not stationary."

#Build a model for our forecast
arima_CO2 <- Arima(CO2_ts)
print(arima_CO2)
"The CO2_ts time series data appears to suit the ARIMA(0,0,0) model with a non-zero mean quite 
well, with an estimated mean of 2.2324 and a variance of 0.0821."

# Forecast 5 years ahead
forecast_CO2 <- forecast(arima_CO2, h = 5) 
forecast_CO2
plot(forecast_CO2, main = "Egypt CO2 Emission Forecast", xlab = "Time", ylab = "CO2 Emission")

"According to the estimate, 95% of the maternal mortality ratio will fall between 1.670959 and 
2.793902 (light blue box), while 80% will fall between 1.865304 and 2.599557 (tick blue box)."

#lets build another model and use 99% CI.
arima_CO2_2 <- auto.arima(CO2_ts)
print(arima_CO2_2)
forecast_CO2_2 <- forecast(arima_CO2_2, h = 10, level = c(80,99))
forecast_CO2_2
plot(forecast_CO2_2, main = "Egypt CO2 Emission Forecast", xlab = "Time", ylab = "Egypt CO2 Emission")
"According to the estimate, 99% of the maternal mortality ratio will fall between 2.362332 and 
2.713191 (light grey shade)"
