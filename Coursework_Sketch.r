#load given dataset into R
life_expectency_data <- read.csv("/Users/april/Documents/DA with R/FDA_Coursework1/naing.csv")

#exploring the size and summary of the dataset
View(life_expectency_data)
str(life_expectency_data)
ncol(life_expectency_data)
nrow(life_expectency_data)
colnames(life_expectency_data)
summary(life_expectency_data)

#install & load moments package to calulate random variable moments status
install.packages("moments")
library(moments)

#Exploratory Data Analysis (EDA)
#creating a function to calculate moment status ()
moment_stats<- function(x, column_name){
  mean_val <- mean(x, na.rm = TRUE)
  variance_val <- var(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm=TRUE)
  skewness_val <- skewness(x, na.rm = TRUE)
  kurtosis_val <- kurtosis(x, na.rm = TRUE)
  return(c(
    cat(column_name, "\n"),
    Mean = mean_val,
    Variance = variance_val,
    Standard_Deviation = sd_val,
    Skewness = skewness_val,
    Kurtosis = kurtosis_val))
}

#calculating moments statistics
moment_stats(life_expectency_data$Infant_deaths,"Infant Deaths:")
moment_stats(life_expectency_data$Under_five_deaths,"Under Five Deaths:")
moment_stats(life_expectency_data$Adult_mortality,"Adult Mortality:")
moment_stats(life_expectency_data$Alcohol_consumption,"Alcohol Consumption:")
moment_stats(life_expectency_data$BMI,"Body Mass Index (BMI):")
moment_stats(life_expectency_data$Hepatitis_B,"Hepatitis B:")
moment_stats(life_expectency_data$Measles,"Measles:")
moment_stats(life_expectency_data$Polio,"Polio:")
moment_stats(life_expectency_data$Diphtheria,"Diphtheria:")
moment_stats(life_expectency_data$Incidents_HIV,"Incidents of HIV:")
moment_stats(life_expectency_data$GDP_per_capita,"GPD Per Capita:")
moment_stats(life_expectency_data$Population_mln,"Population in Millions:")
moment_stats(life_expectency_data$Thinness_ten_nineteen_years,"Thinness of Children aged 10-19:")
moment_stats(life_expectency_data$Thinness_five_nine_years,"Thinness of Children aged 5-9:")
moment_stats(life_expectency_data$Schooling,"Schooling:")
moment_stats(life_expectency_data$Life_expectancy,"Life Expectency:")

shapiro.test(life_expectency_data$Infant_deaths)
shapiro.test(life_expectency_data$Under_five_deaths)
shapiro.test(life_expectency_data$Adult_mortality)
shapiro.test(life_expectency_data$Alcohol_consumption)
shapiro.test(life_expectency_data$BMI)
shapiro.test(life_expectency_data$Hepatitis_B)
shapiro.test(life_expectency_data$Measles)
shapiro.test(life_expectency_data$Polio)
shapiro.test(life_expectency_data$Diphtheria)
shapiro.test(life_expectency_data$Incidents_HIV)
shapiro.test(life_expectency_data$GDP_per_capita)
shapiro.test(life_expectency_data$Population_mln)
shapiro.test(life_expectency_data$Thinness_ten_nineteen_years)
shapiro.test(life_expectency_data$Thinness_five_nine_years)
shapiro.test(life_expectency_data$Schooling)
shapiro.test(life_expectency_data$Life_expectancy)

#Examining histograms for the numerical variables in Life Expectency Dataset
hist(life_expectency_data$Infant_deaths)
hist(life_expectency_data$Under_five_deaths, breaks=3)
hist(life_expectency_data$Adult_mortality, breaks=7)
hist(life_expectency_data$Alcohol_consumption, breaks=4)
hist(life_expectency_data$BMI, freq=FALSE)
hist(life_expectency_data$Hepatitis_B, breaks=8)
hist(life_expectency_data$Measles, breaks=3)
hist(life_expectency_data$Polio, breaks=5)
hist(life_expectency_data$Diphtheria, breaks=5)
hist(life_expectency_data$Incidents_HIV, breaks=6)
hist(life_expectency_data$GDP_per_capita, breaks=9)
hist(life_expectency_data$Population_mln, breaks=8)
hist(life_expectency_data$Thinness_ten_nineteen_years)
hist(life_expectency_data$Thinness_ten_nineteen_years)
hist(life_expectency_data$Schooling, breaks=12)
hist(life_expectency_data$Life_expectancy, breaks = 10, freq = FALSE)

#Examining Boxplot for the numerical variables in Life Expectency Dataset
boxplot(life_expectency_data$Infant_deaths)
boxplot(life_expectency_data$Under_five_deaths)
boxplot(life_expectency_data$Adult_mortality)
boxplot(life_expectency_data$Alcohol_consumption)
boxplot(life_expectency_data$BMI)
boxplot(life_expectency_data$Hepatitis_B)
boxplot(life_expectency_data$Measles)
boxplot(life_expectency_data$Polio)
boxplot(life_expectency_data$Diphtheria)
boxplot(life_expectency_data$Incidents_HIV)
boxplot(life_expectency_data$GDP_per_capita)
boxplot(life_expectency_data$Population_mln)
boxplot(life_expectency_data$Thinness_ten_nineteen_years)
boxplot(life_expectency_data$Thinness_five_nine_years)
boxplot(life_expectency_data$Schooling)
boxplot(life_expectency_data$Life_expectancy)

#Examining Density Plot for the numerical variables in Life Expectency Dataset
plot(density(life_expectency_data$Infant_deaths))
plot(density(life_expectency_data$Under_five_deaths, adjust = 3))
plot(density(life_expectency_data$Adult_mortality, adjust = 0.7))
plot(density(life_expectency_data$Alcohol_consumption, adjust = 5))
plot(density(life_expectency_data$BMI, adjust = 0.05))
plot(density(life_expectency_data$Hepatitis_B, adjust = 4))
plot(density(life_expectency_data$Measles, adjust = 10))
plot(density(life_expectency_data$Polio, adjust = 0.001))
plot(density(life_expectency_data$Diphtheria, adjust = 2))
plot(density(life_expectency_data$Incidents_HIV, adjust = 4))
plot(density(life_expectency_data$GDP_per_capita))
plot(density(life_expectency_data$Population_mln, adjust = 0.8))
plot(density(life_expectency_data$Thinness_ten_nineteen_years, adjust = 1))
plot(density(life_expectency_data$Thinness_five_nine_years, adjust = 0.7))
plot(density(life_expectency_data$Schooling, adjust = 5))
plot(density(life_expectency_data$Life_expectancy, adjust = 3))

#Examining Bar Plot for the non-numerical variables in Life Expectency Dataset
barplot(table(life_expectency_data$Continent))
barplot(table(life_expectency_data$Region))
barplot(table(life_expectency_data$Economy_status_Developed))
barplot(table(life_expectency_data$Economy_status_Developing))

#examining the frequencies of different groups of continents
length(table(life_expectency_data$Continent))
#examining the frequencies of different regions
length(table(life_expectency_data$Region))

#install & load DescTools to calculate Mode
install.packages("DescTools")
library(DescTools)
Mode(life_expectency_data$Continent, na.rm = TRUE)
Mode(life_expectency_data$Region, na.rm = TRUE)

#Grouped Visual Analysis
boxplot(Life_expectancy~Region, life_expectency_data)
boxplot(Life_expectancy~Continent, life_expectency_data)
boxplot(Alcohol_consumption~Economy_status_Developing, life_expectency_data)
boxplot(GDP_per_capita~Economy_status_Developed, life_expectency_data)

# calculate moments for life expectency grouped by Regions
# dplyr allows us to group data and calculate by groups
install.packages("dplyr")
library(dplyr)
# create a grouped dataset
life_expectency_byRegion <- group_by(life_expectency_data, Region)
# use grouped dataset to calculate moments
summarise(life_expectency_byRegion,
          mean = mean(Life_expectancy),
          variance = var(Life_expectancy),
          skewness = skewness(Life_expectancy),
          kurtosis = kurtosis(Life_expectancy))

# if we wanted to see individual histograms, we could split the data
life_expectency_Asia <- life_expectency_data[life_expectency_data$Continent == "Asia",]$Life_expectancy
life_expectency_Americas <- life_expectency_data[life_expectency_data$Continent == "Americas",]$Life_expectancy
life_expectency_Oceania<- life_expectency_data[life_expectency_data$Continent == "Oceania",]$Life_expectancy
hist(life_expectency_Asia)
hist(life_expectency_Americas)
hist(life_expectency_Oceania)

#checking if there is an NA value
anyNA(life_expectency_data) 

#Question 1: Does the high value of BMI correspond to shorter life expectency?
#H0: There is no correlation between BMI and life expectancy.
#H1: There is a correlation between BMI and life expectancy.

#correlation_model <- lm(BMI~Life_expectancy, data=life_expectency_data)
plot(life_expectency_data$BMI, life_expectency_data$Life_expectancy)
abline(lm(BMI~Life_expectancy, data=life_expectency_data))
ggplot(life_expectency_data, aes(x = BMI, y = Life_expectancy)) +
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "Average BMI vs Life Expectency", x = "Body Mass Index (BMI)", y = "Average Life Expectency of Population")

# checking data normality
# looking at the data visually
hist(life_expectency_data$BMI)
hist(life_expectency_data$Life_expectancy)

plot(density(life_expectency_data$BMI))
plot(density(life_expectency_data$Life_expectancy))

# checking moments
moment_stats(life_expectency_data$BMI,"Body Mass Index")
moment_stats(life_expectency_data$Life_expectancy,"Average Life Expectency")

# generating the QQ-plot (you need both lines of code)
qqnorm(life_expectency_data$BMI)
qqline(life_expectency_data$BMI)

qqnorm(life_expectency_data$Life_expectancy)
qqline(life_expectency_data$Life_expectancy)

# running the normality test
shapiro.test(life_expectency_data$BMI)
shapiro.test(life_expectency_data$Life_expectancy)

#checking outliers
z_life_expectency <- (life_expectency_data$Life_expectancy - mean(life_expectency_data$Life_expectancy))/sd(life_expectency_data$Life_expectancy)
z_life_expectency[z_life_expectency > 3 | z_life_expectency < -3] #no outliers
z_BMI <- (life_expectency_data$BMI - mean(life_expectency_data$BMI))/sd(life_expectency_data$BMI)
z_BMI[z_BMI > 3 | z_BMI < -3] #no outliers

cor.test(life_expectency_data$BMI, life_expectency_data$Life_expectancy, method = "spearman", exact = FALSE)

#Question 2: Do developing countries experience higher deaths of children aged <5 than developed countries?
#H0: There is no correlation between economy_status_developing and under_five_deaths.
#H1: There is a correlation between economy_status_developing and under_five_deaths.

install.packages("ggplot2")
library(ggplot2)

#ggplot(life_expectency_data) +  geom_jitter(aes(Economy_status_Developing, Under_five_deaths))
ggplot(life_expectency_data, aes(x = Economy_status_Developing, y = Under_five_deaths)) +
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "Economy Status (Developing) vs Under-Five Deaths",  x = "Economy Status (1 = Developing, 0 = Developed)",
       y = "Under-Five Deaths per 1,000 births")

cor.test(life_expectency_data$Economy_status_Developing, life_expectency_data$Under_five_deaths, method = "spearman", exact = FALSE)

economy_developing <- life_expectency_data[life_expectency_data$Economy_status_Developing== 1,]$Under_five_deaths
economy_developed <- life_expectency_data[life_expectency_data$Economy_status_Developing== 0,]$Under_five_deaths

# homogeneity of variance
var.test(economy_developing, economy_developed)

# running the normality test
shapiro.test(life_expectency_data$Economy_status_Developing)
shapiro.test(life_expectency_data$Under_five_deaths[life_expectency_data$Economy_status_Developing==1])
shapiro.test(life_expectency_data$Under_five_deaths[life_expectency_data$Economy_status_Developing==0])

#Question 3: How does GDP per capita influence life expectancy?
#H0: There is no correlation between GDP_per_capita and Life_expectancy.
#H1: There is a correlation between GDP_per_capita and Life_expectancy.

ggplot(life_expectency_data, aes(x = GDP_per_capita, y = Life_expectancy)) +
  geom_jitter(width = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "GDP VS Life Expectency",  x = "Gross Domestic Product (GDP) per person",
       y = "Average Life Expectency")

# Using Spearman's correlation since the data is not normal 
cor.test(life_expectency_data$GDP_per_capita, life_expectency_data$Life_expectancy, method = "spearman", exact = FALSE)

#Question 4: Do immunization rates of Measles relate to deaths of children aged <5?
#H0: There is no correlation between Measles and under_five_deaths.
#H1: There is a correlation between Measles and under_five_deaths.
correlation_model <- lm(Measles~Under_five_deaths, data=life_expectency_data)
plot(life_expectency_data$Measles, life_expectency_data$Under_five_deaths)
abline(correlation_model)
ggplot(life_expectency_data, aes(x = Measles, y = Under_five_deaths)) + geom_jitter(width = 0.1) + geom_smooth(method = "lm") 

# checking data normality
# looking at the data visually
hist(life_expectency_data$Measles)
hist(life_expectency_data$Under_five_deaths)

plot(density(life_expectency_data$Measles))
plot(density(life_expectency_data$Under_five_deaths))

# checking moments
moment_stats(life_expectency_data$Measles,"Measles Immunization")
moment_stats(life_expectency_data$Under_five_deaths,"Deaths of Children Aged Under Five")

# generating the QQ-plot (you need both lines of code)
qqnorm(life_expectency_data$Measles)
qqline(life_expectency_data$Measles)

qqnorm(life_expectency_data$Under_five_deaths)
qqline(life_expectency_data$Under_five_deaths)

# running the normality test
shapiro.test(life_expectency_data$Measles)
shapiro.test(life_expectency_data$Under_five_deaths)

#checking outliers
z_Measles <- (life_expectency_data$Measles - mean(life_expectency_data$Measles))/sd(life_expectency_data$Measles)
z_Measles[z_Measles > 3 | z_Measles < -3] #no outliers
z_Under_five_deaths <- (life_expectency_data$Under_five_deaths - mean(life_expectency_data$Under_five_deaths))/sd(life_expectency_data$Under_five_deaths)
z_Under_five_deaths[z_Under_five_deaths > 3 | z_Under_five_deaths < -3] 

cor.test(life_expectency_data$Measles, life_expectency_data$Under_five_deaths, method = "spearman", exact = FALSE)

#Question 1: Is there a significant relationship between the average BMI and adult mortality rate across countries?
#H0: There is no correlation between Adult_mortality and BMI.
#H1: There is a significant correlation Adult_mortality and BMI.
correlation_model <- lm(Adult_mortality~Schooling, data=life_expectency_data)
plot(life_expectency_data$Adult_mortality, life_expectency_data$Schooling)
abline(correlation_model)

ggplot(life_expectency_data, aes(x = Adult_mortality, y = BMI)) + geom_jitter(width = 0.1) + geom_smooth(method = "lm") 

