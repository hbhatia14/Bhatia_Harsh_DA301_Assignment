## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
getwd()
# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
head(turtle_sales)
as_tibble(turtle_sales)
tail(turtle_sales)
# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns - We are only removing Ranking and Year from the dataframe
# while keeping Genre and Publisher as these two columns will help us in grouping the 352 products
# and try and establish any impact / relationship between products and sales
df_subset <- select(turtle_sales, -Ranking, -Year)

# View the data frame.
as_tibble(df_subset)
# View the descriptive statistics.
summary(df_subset)

################################################################################

# 2. Review plots to determine insights into the data set.
# We plot some quick plots using qplot function to understand the relationship between the variables

## 2a) Scatterplots
# Create scatterplots.
qplot(Global_Sales, NA_Sales, color=factor(Product), data=df_subset, geom=c('point'))
qplot(Global_Sales, EU_Sales, color=factor(Product), data=df_subset, geom=c('point'))
qplot(EU_Sales, NA_Sales, color=factor(Product), data=df_subset, geom=c('point'))
# From the 3 plots, we can see a trend of positive correlation between the sales. Also, we can see the older products
#tend to have higher sales. This could be due to them selling for more years.

## 2b) Histograms
# Create histograms.
qplot(Global_Sales, data=df_subset, geom='histogram')
qplot(NA_Sales, data=df_subset, geom='histogram')
qplot(EU_Sales, data=df_subset, geom='histogram')
qplot(Genre, data=df_subset)

## 2c) Boxplots
# Create boxplots.
qplot(Platform, NA_Sales, data=df_subset, geom='boxplot')
qplot(Platform, EU_Sales, data=df_subset, geom='boxplot')
qplot(Platform, Global_Sales, data=df_subset, geom='boxplot')




###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
#1. Globally, the sale of most products ranges between 0-20 million pounds. We have very few countable products above 20
#   and only 1 product above 40 with max
#2. In NA, the sale of most products ranges between 0-10.We have very few products above 10 while only 30 products
#   are above 30 with max being
#3. IN EU, the sale of most products ranges between 0-5. We have very few products above 5 while only 2 products 
#   above 10 with max being 24. 
#4. Thus we can conclude to a certain extent that there is no one product or a small group that standout to 
#   contribute majorly to the sales. All products have a role to play in the sales.  
#5. From the 3 plots, we can see a trend of positive correlation between the sales. Also, we can see the older products
#   tend to have higher sales. This could be due to them selling for more years.
#6. From the Genre histogram, we can see Shooter games are most popular followed closely by Action. The other top 3 genres
#   are Sports, Role-Playing and Platform respectively. 
#7. We created 3 box plots for sales per platform. We found that there is one outlier product on thr Wii platform that is
#   the most popular globally. The median sales per platform amongst several platforms are very close however there is 
#   a high variation in sales for Wii, NES and GB have higher variation in the sales. 


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.

as_tibble(df_subset)
# Check output: Determine the min, max, and mean values.


# View the descriptive statistics.
summary(df_subset)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and Genre and determine the sum per Product.
df_aggregate_product <- aggregate(cbind(Global_Sales,NA_Sales, EU_Sales)~Product+Genre, df_subset, sum)
df_aggregate_product <- df_aggregate_product[order(df_aggregate_product$Global_Sales, decreasing=TRUE),]
total_sales_product <- sum(df_aggregate_product$Global_Sales)
df_aggregate_product$Sales_contribution<- round((df_aggregate_product$Global_Sales/total_sales_product*100),2)


#Using the aggregate function to determine the sum of sales per Platform
df_aggregate_platform <- aggregate(cbind(Global_Sales,NA_Sales, EU_Sales)~Platform+Genre, df_subset, sum)
df_aggregate_platform <- df_aggregate_platform[order(df_aggregate_platform$Global_Sales, decreasing=TRUE),]
total_sales_platform <- sum(df_aggregate_platform$Global_Sales)
df_aggregate_platform$Sales_contribution<- round((df_aggregate_platform$Global_Sales/total_sales_platform*100),2)

# View the data frame.
df_aggregate_product
df_aggregate_platform
# Explore the data frame.
dim(df_aggregate_product)
dim(df_aggregate_platform)


## 2b) Determine which plot is the best to compare game sales.
# We create more detailed plots using the ggplot function to add more elements to our charts
# Create scatterplots.
# Scatterplot between Global_Sales & NA_Sales based on Product
ggplot(data=df_aggregate_product, aes(x= NA_Sales, y =Global_Sales, color=factor(Product)))+
  geom_point()+
  labs(x = "NA_Sales(in million Pounds)", y = "Global Sales(in million Pounds)", 
       title = "Global Sales vs NA Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Scatterplot between Global_Sales & EU_Sales based on Product
ggplot(data=df_aggregate_product, aes(x= EU_Sales, y =Global_Sales, color=factor(Product)))+
  geom_point()+
  labs(x = "EU_Sales(in million Pounds)", y = "Global Sales(in million Pounds)", 
       title = "Global Sales vs EU Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Scatterplot between EU_Sales & NA_Sales based on Product
ggplot(data=df_aggregate_product, aes(x= NA_Sales, y =EU_Sales, color=factor(Product)))+
  geom_point()+
  labs(x = "NA_Sales(in million Pounds)", y = "EU Sales(in million Pounds)", 
       title = "EU Sales vs NA Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Create histograms.
ggplot(data=df_aggregate_product, aes(x=Global_Sales)) + 
  geom_histogram(binwidth = 5, color='black', fill='lightblue')+
  labs(x = "Global Sales(in million Pounds)", y = "No. of Products", 
       title = "Distribution of Global Sales / Product") +
  scale_x_continuous(breaks = seq(0, 80, 5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=df_aggregate_product, aes(x=NA_Sales)) + 
  geom_histogram(binwidth = 5, color='black', fill='lightyellow')+
  labs(x = "NA Sales(in million Pounds)", y = "No. of Products", 
       title = "Distribution of NA Sales / Product") +
  scale_x_continuous(breaks = seq(0, 80, 5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=df_aggregate_product, aes(x=EU_Sales)) + 
  geom_histogram(binwidth = 5, color='black', fill='lightpink')+
  labs(x = "EU Sales(in million Pounds)", y = "No. of Products", 
       title = "Distribution of EU Sales / Product") +
  scale_x_continuous(breaks = seq(0, 80, 5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Creating Barplot to determine the most popular platforms in terms of Global Sales / NA Sales / EU Sales

ggplot(data=df_aggregate_platform, aes(x=reorder(Platform, -Sales_contribution), y=Sales_contribution, fill=Platform)) +
  geom_bar(position = 'stack', stat='identity')+
  stat_summary(fun = sum, geom = "text", aes(label = ..y..), vjust = -0.5, size = 3.5) +
  labs(x = "Platform", y = "Global Sales Contribution %", title = "Global Sales Contribution(%) per Platform") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Observation: As we can see in the plot, Wii, X360, PS3 form the top 3 platforms in terms of sales contribution.
# Top 5 platforms contribute to 60% of the total Global Sales.

ggplot(data=df_aggregate_platform, aes(x=reorder(Platform, -NA_Sales), y=NA_Sales, fill=Platform)) +
  geom_bar(stat='identity')+
  stat_summary(fun = sum, geom = "text", aes(label = ..y..), vjust = -0.5, size = 3.5) +
  labs(x = "Platform", y = "NA Sales(in million Pounds)", title = "NA Sales per Platform") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=df_aggregate_platform, aes(x=reorder(Platform, -EU_Sales), y=EU_Sales, fill=Platform)) +
  geom_bar(stat='identity')+
  stat_summary(fun = sum, geom = "text", aes(label = ..y..), vjust = -0.5, size = 3.5) +
  labs(x = "Platform", y = "EU Sales(in million Pounds)", title = "EU Sales per Platform") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###############################################################################

head(df_aggregate_product)

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(df_aggregate_product$Global_Sales)
qqline(df_aggregate_product$Global_Sales, col='red',lwd=2)

qqnorm(df_aggregate_product$NA_Sales)
qqline(df_aggregate_product$NA_Sales, col='red',lwd=2)

qqnorm(df_aggregate_product$EU_Sales)
qqline(df_aggregate_product$EU_Sales, col='red',lwd=2)



## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)
# Perform Shapiro-Wilk test.
shapiro.test(df_aggregate_product$Global_Sales)
# p-value is less than 0.05, hence we conclude that the data is not normally distributed
shapiro.test(df_aggregate_product$NA_Sales)
# p-value is less than 0.05, hence we conclude that the data is not normally distributed
shapiro.test(df_aggregate_product$EU_Sales)
# p-value is less than 0.05, hence we conclude that the data is not normally distributed


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(df_aggregate_product$Global_Sales)
kurtosis(df_aggregate_product$Global_Sales)
#Skewness for Global Sales is 3.067 and Kurtosis is 17.80. 

skewness(df_aggregate_product$NA_Sales)
kurtosis(df_aggregate_product$NA_Sales)
#Skewness for Global Sales is 3.048 and Kurtosis is 15.60.

skewness(df_aggregate_product$EU_Sales)
kurtosis(df_aggregate_product$EU_Sales)
#Skewness for Global Sales is 2.89 and Kurtosis is 16.23.

#Since all the data is not normalized, we transform the data using logarithmic normalization:

df_aggregate_product <- mutate(df_aggregate_product, Log_Global_Sales = round(log(df_aggregate_product$Global_Sales),2), 
                               Log_NA_Sales = round(log(df_aggregate_product$NA_Sales),2),
                               Log_EU_Sales = round(log(df_aggregate_product$EU_Sales),2))

#Viewing the data set after log transformation
head(df_aggregate_product)
summary(df_aggregate_product)
#We see that min value for Log_EU_Sales is -Inf and this would create issues in our further analysis. 
#Hence we identify the product with -Inf value and remove the same from further analysis
df_aggregate_product$Log_EU_Sales
filter(df_aggregate_product, Log_EU_Sales == -Inf)
df_aggregate_product <- filter(df_aggregate_product, Product != 5510)
summary(df_aggregate_product)
dim(df_aggregate_product)

#Checking for normalization of the log of Sales across regions
#Global Sales
qqnorm(df_aggregate_product$Log_Global_Sales)
qqline(df_aggregate_product$Log_Global_Sales, col='red',lwd=2)
shapiro.test(df_aggregate_product$Log_Global_Sales)
# Though the p-value is higher than the original data set, it is still less than 0.05, hence technically the data
# is not normally distributed still
skewness(df_aggregate_product$Log_Global_Sales)
#Skewness is 0.77 which is between 1 and -1 so it fulfills the criteria of normally distributed data
kurtosis(df_aggregate_product$Log_Global_Sales)
#Kurtosis is 3.10 which is closer to 3, hence it is very close to normal data

#NA_Sales
qqnorm(df_aggregate_product$Log_NA_Sales)
qqline(df_aggregate_product$Log_NA_Sales, col='red',lwd=2)
shapiro.test(df_aggregate_product$Log_NA_Sales)
# Though the p-value is higher than the original data set, it is still less than 0.05, hence technically the data
# is not normally distributed still
skewness(df_aggregate_product$Log_NA_Sales)
#Skewness is -0.73 which between 1 and -1 so it fulfills the criteria of normally distributed data
kurtosis(df_aggregate_product$Log_NA_Sales)
#Kurtosis is 7.76, though it is less than original data, it is still on the higher side
#hence we cannot say the data is normal with high confidence

#EU_Sales
qqnorm(df_aggregate_product$Log_EU_Sales)
qqline(df_aggregate_product$Log_EU_Sales, col='red',lwd=2)
shapiro.test(df_aggregate_product$Log_EU_Sales)
# Though the p-value is higher than the original data set, it is still less than 0.05, hence technically the data
# is not normally distributed still
skewness(df_aggregate_product$Log_EU_Sales)
#Skewness is -1.19 which is outside 1 and -1 but close to -1 so it does not fulfills the criteria of
# normally distributed data with high confidence
kurtosis(df_aggregate_product$Log_EU_Sales)
#Kurtosis is 9.83, though it is less than original data, it is still on the higher side
#hence we cannot say the data is normal with high confidence

# To summarize the normality tests: even after the log transformation, though the normality has improves,
# the data is not normally distributed. Hence, we will explore the overall fit of the models when conducting
# regression analysis to determine if the the model is better with the original data or the transformed data


## 3d) Determine correlation
# Determine correlation.
cor(df_aggregate_product$NA_Sales, df_aggregate_product$Global_Sales)
# coefficient of correlation is ~ 0.92 or 92%
cor(df_aggregate_product$EU_Sales, df_aggregate_product$Global_Sales)
# coefficient of correlation is ~ 0.85 or 85%
cor(df_aggregate_product$NA_Sales, df_aggregate_product$EU_Sales)
# coefficient of correlation is ~ 0.62 or 62%


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# In addition to the plots created earlier, we also plot the Genre against Regional Sales to determine the most popular Genre
#Global Sales per Genre
ggplot(data=df_aggregate_product, aes(x=Genre, y=Global_Sales, fill=Genre)) +
  geom_bar(stat='identity')+
  stat_summary(fun = sum, geom = "text", aes(label = ..y..), vjust = -0.5, size = 3.5) +
  labs(x = "Genre", y = "Global Sales(in million Pounds)", title = "Global Sales per Genre")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#NA_Sales per Genre
ggplot(data=df_aggregate_product, aes(x=Genre, y=NA_Sales, fill=Genre)) +
  geom_bar(stat='identity')+
  stat_summary(fun = sum, geom = "text", aes(label = ..y..), vjust = -0.5, size = 3.5) +
  labs(x = "Genre", y = "NA Sales(in million Pounds)", title = "NA Sales per Genre")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=df_aggregate_product, aes(x=Genre, y=EU_Sales, fill=Genre))+
  geom_bar(stat='identity')+
  stat_summary(fun = sum, geom = "text", aes(label = ..y..), vjust = -0.5, size = 3.5) +
  labs(x = "Genre", y = "EU Sales(in million Pounds)", title = "EU Sales per Genre")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Understading correlation between the Sales columns and checking the trend by adding a trendline:
#Correlation between NA & Global Sales
ggplot(data=df_aggregate_product, aes(x= NA_Sales, y =Global_Sales))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE) +
  labs(x = "NA_Sales(in million Pounds)", y = "Global Sales(in million Pounds)", 
       title = "Global Sales vs NA Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Correlation between EU & Global Sales
ggplot(data=df_aggregate_product, aes(x= EU_Sales, y =Global_Sales))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE) +
  labs(x = "EU_Sales(in million Pounds)", y = "Global Sales(in million Pounds)", 
       title = "Global Sales vs EU Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Correlation between NA & EU Sales
ggplot(data=df_aggregate_product, aes(x= NA_Sales, y =EU_Sales))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE) +
  labs(x = "NA_Sales(in million Pounds)", y = "EU Sales(in million Pounds)", 
       title = "EU Sales vs NA Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#1. Observation: As we can see in the plot, Wii, X360, PS3 form the top 3 platforms in terms of sales contribution.
#2. Top 5 platforms contribute to 60% of the total Global Sales.
#3. There is a slight difference in the popularity of the platforms across EU - Wii, PS3, X360, DS, PS4
#   and NA - X360, Wii, PS3, DS, GB - This factor can be further explored to market products according the platform popularity
#4. There is also a slight difference in the most selling products across EU and NA - for e.g. Products 123, 254 &
#   326 have sales ranging from 26.64, 21.46, 22.08 million pounds in NA, however in EU, their sale is only 4.01, 2.42, 0.52 
#   million pounds respectively. This could be due to the popularity of the platforms in each continent, however the 
#   phenomenon can be explored in detail in a separate study. 
#5. In general, the older the products, the more contribution they have to the Global Sales. Since we do not have
#   enough information to tell us whether the data is annual or lifelong, we assume that the data is lifelong which means
#   that the older products have been around for a longer time thus contribute more to the sales. 



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.

head(df_aggregate_product)
# Determine a summary of the data frame.

summary(df_aggregate_product) 

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns

library(psych)
corPlot(select(df_aggregate_product, Global_Sales, NA_Sales, EU_Sales), cex=1)
# After plotting the corPlot, we use the below function to clear the graphics device 
# else it causes an error (margins are too large) for future plots
dev.off() 


# Create a linear regression model on the original data.
model1 <- lm(Global_Sales ~ NA_Sales, data=df_aggregate_product)
summary(model1)
qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

# Summary shows that adj.R-squared is 0.84, which shows 84% variability in Global Sales is due to NA_Sales
# p-value is less than 0.05 hence it is statistically significant

# Create a linear regression model on the logarithmic data.
model1_log <- lm(Log_Global_Sales ~ Log_NA_Sales, data=df_aggregate_product)
summary(model1_log)
qqnorm(residuals(model1_log))
qqline(residuals(model1_log), col='blue')

# Summary shows that adj.R-squared is 0.61, which shows 61% variability in Global Sales is due to NA_Sales
# p-value is less than 0.05 hence it is statistically significant

# As we observed earlier, though the normality of the data set has improved slightly with log transformation, 
# the R-squared value is lesser in the model with the transformed data. This could mean that the transformation has added
# some noise or measurement error in the dataset. Hence for further regression models, we will use the original dataset
# as it is a better overall fit. 

model2 <- lm(Global_Sales ~ EU_Sales, data=df_aggregate_product)
summary(model2)
qqnorm(residuals(model2))
qqline(residuals(model2), col='blue')

# Summary shows that adj.R-squared is 0.72, which shows 72% variability in Global Sales is due to EU_Sales
# p-value is less than 0.05 hence it is statistically significant


model3 <- lm(NA_Sales ~ EU_Sales, data=df_aggregate_product)
summary(model3)
qqnorm(residuals(model3))
qqline(residuals(model3), col='green')
# Summary shows that adj.R-squared is 0.38, which shows 38% variability in NA Sales is due to EU_Sales. Although positive,
# this is not a very number
# p-value is less than 0.05 hence it is statistically significant

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
#Model 1 - Global Sales vs NA Sales
plot(model1$residuals)
plot(df_aggregate_product$Global_Sales, df_aggregate_product$NA_Sales)
coefficients(model1)
## Intercept = 2.458 NA_Sales coeff = 1.634
abline(coefficients(model1), col='red')

#Model 2 - Global Sales vs EU Sales
plot(model2$residuals)
plot(df_aggregate_product$Global_Sales, df_aggregate_product$EU_Sales)
coefficients(model2)
# Intercept = 3.315 EU_Sales coeff = 2.24
abline(coefficients(model2), col='blue')

#Model 3 - NA Sales vs EU Sales
plot(model3$residuals)
plot(df_aggregate_product$NA_Sales, df_aggregate_product$EU_Sales)
coefficients(model3)
# Intercept = 2.033 EU_Sales coeff = 0.916
abline(coefficients(model3), col='green')

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
df_aggregate_numeric <- select(df_aggregate_product, Global_Sales, NA_Sales, EU_Sales)


# Multiple linear regression model.
modela <- lm(Global_Sales~NA_Sales+EU_Sales, data=df_aggregate_numeric)
summary(modela)
# Summary shows that adj.R-squared is 0.97, which shows 97% variability in Global Sales is due to NA & EU Sales combined
# p-value is less than 0.05 hence it is statistically significant

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

df_subset_test <- data.frame(NA_Sales = c(34.02,3.93,2.73,2.26,22.08), EU_Sales= c(23.80,1.56,0.65,0.97,0.52))
df_subset_test

predictTest <- predict(modela, newdata=df_subset_test, interval='confidence')
predictTest
#Predicted Values based on input values provided:
#NA_Sales = 34.02, EU_Sales = 23.80, Predicted_Global_Sales = 68.14 , Actual_Global_Sales = 67.85
#NA_Sales = 3.93, EU_Sales = 1.56, Predicted_Global_Sales = 7.34 , Actual_Global_Sales = 6.04 (based on our original data)
#NA_Sales = 2.73, EU_Sales = 0.65, Predicted_Global_Sales = 4.89 , Actual_Global_Sales = 4.32
##NA_Sales = 2.26, EU_Sales = 0.97, Predicted_Global_Sales = 4.74 , Actual_Global_Sales = 3.53 (based on our original data)
#NA_Sales = 22.08, EU_Sales = 0.52, Predicted_Global_Sales = 26.61 , Actual_Global_Sales = 23.21


# Testing the model for multicollinearity:
library(car)
vif(modela)
#Since VIF for NA Sales & EU Sales is 1.622, which is closer to 1, we can conclude the multicollinearity is not present

#Testing the model for heteroscedasticity:
library(lmtest)
bptest(modela)
#P-value is greater than 0.05, hence we can conclude that heteroscedasticity is not present

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The Multiple Linear Regression model has a R-squared value of 0.97 i.e. 97% of the variability in Global Sales
# is due to NA_Sales and EU_Sales. The data used for MLR is not normal so the model might not be very reliable. However, 
# there is no multicollinearity and no heteroscedasticity which makes it stable. 
# Hence, we can use this model but with some caution.

#Predicted Values based on input values provided:
#NA_Sales = 34.02, EU_Sales = 23.80, Predicted_Global_Sales = 68.06 , Actual_Global_Sales = 67.85
#NA_Sales = 3.93, EU_Sales = 1.56, Predicted_Global_Sales = 7.35 , Actual_Global_Sales = 6.04 (based on our original data)
#NA_Sales = 2.73, EU_Sales = 0.65, Predicted_Global_Sales = 4.91 , Actual_Global_Sales = 4.32
##NA_Sales = 2.26, EU_Sales = 0.97, Predicted_Global_Sales = 4.76 , Actual_Global_Sales = 3.53 (based on our original data)
#NA_Sales = 22.08, EU_Sales = 0.52, Predicted_Global_Sales = 26.63 , Actual_Global_Sales = 23.21




###############################################################################
###############################################################################


