library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("ggvis")
library(ggvis)
install.packages("corrplot")
library(corrplot)
install.packages("DT")
library(DT)

# Importing data set

HR_comma_sep <- read_csv("~/predictive_course/HR_comma_sep.csv")
hr_data <- HR_comma_sep
summary(hr_data)

# Creating Visualizations
HR_Corr <- hr_data %>% select(satisfaction_level:promotion_last_5years)
View(HR_Corr)
HR_Corr <- hr_data[1:8]
M <- cor(HR_Corr)
corrplot(M, method = 'circle')

# Creating a data frame of only people who have left company, to visualize distribution of features
hr_left1 <- subset(hr_data, hr_data$left == 1)
View(hr_left1)
par(mfrow = c(1,3))
hist(hr_left1$satisfaction_level, col = "#3090C7", main = "Satisfaction Level")
hist(hr_left1$last_evaluation, col = "#3090C7", main = "Last Evaluation")
hist(hr_left1$average_montly_hours, col = "#3090C7", main = "Average Monthly hours")

par(mfrow = c(1,2))
hist(hr_left1$Work_accident, col = "#3090C7", main = "Work Accident")
barplot(table(hr_left1$salary), col = "#3090C7", main = "Salary")

#ggplot(hr_left1, aes(x = hr_left1$salary)) +geom_bar()

# Total number of people leaving the company
nrow(hr_left1)

# Here are total number of employees that received evalution above avg, or spend at least four years in the company,
# or were working om more than 5 projects at same time and still have left the company. ** These are the people the 
# should have retained. **
# hr_good_leaving_people <- subset(hr_left1[hr_left1$last_evaluation >= 0.70
#                                           | hr_left1$time_spend_company >= 4
#                                           | hr_left1$number_project >5])
hr_good_leaving_people <- hr_left1 %>% filter(last_evaluation >= 0.70
                                          | time_spend_company >= 4
                                          | number_project >5)
nrow(hr_good_leaving_people)
View(hr_good_leaving_people)

# Reusing the data table created above that contain only the most valuable employees to see why they tend to leave
hr_good_people_select <- hr_good_leaving_people %>% select(satisfaction_level, number_project:promotion_last_5years)
N <- cor(hr_good_people_select)
corrplot(N, method = 'circle')

summary(hr_good_leaving_people)
# We can see on average valuable employees that leave are not satisfied, work on many projects, spend many more hours 
# in company and aren't promoted