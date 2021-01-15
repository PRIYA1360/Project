rm(list=ls()) # removes all variables stored perviously
library(Hmisc)# import

data <- read.csv("D:/user data/Downloads/COVID19_line_list_data (1).csv")
describe(data) # Hmisc command

#cleaned up death column
data$death_dummy <- as.integer(data$death !=0)
# death rate 
sum(data$death_dummy) / nrow(data)

#AGE
#claim: people who die are older
dead=subset(data, death_dummy ==1)
alive =subset(data, death_dummy ==0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and
# conclude that this is statistically significant

#GENDER
#claim: gender has no effect
men =subset(data, gender == "male")
women =subset(data, gender == "female") 
mean(men$death_dummy, na.rm = TRUE) #8.4%
mean(women$death_dummy, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8,8% higher chances
# of dying.
# p-value = 0.002 < 0.05, so this statstically
# significant