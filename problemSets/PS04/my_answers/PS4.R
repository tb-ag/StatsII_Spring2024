#####################
# Tolga Bag - 23371290
#####################

#####################
# load libraries
# set wd
# clear global .envir
#####################

getwd()
setwd("C:/Users/tolga/OneDrive/Documents/GitHub/StatsII_Spring2024/problemSets/PS04")
#####################
# Question 1: Model Fit
#####################

# first I need to download the relevant packages
install.packages("eha") #I need this to access the child dataset.
install.packages("survival") #this will help me compute Cox model.
library(eha)
library(survival)
data(child) #I load the data set. I found information here: https://cran.r-project.org/web/packages/eha/eha.pdf
child_surv <- with(child, Surv(enter, exit, event)) #I create a survival object
cox <- coxph(child_surv ~ sex + m.age, data = child) #I fit the cox model as gender
#and mother's age as covariates.
summary(cox)
drop1(cox, test = "Chisq")
library(stargazer)
stargazer(cox, type = "text")

########cox#####################
# Question 1: Presentation and Interpretation
#####################
# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding mother's age constant. There is a 0.008 increase in the expected log of the
#hazard for babies of mother's with older age compared to younger ones, holding gender constant.

#I exponentiate parameter estimates to obtain hazard ratios
exp(-0.007617)
# The hazard ratio of older mothers' babies is 0.007 of that of younger babies, 
cox_fit <- survfit(cox)
autoplot(cox_fit) #I plot the survival curve to visualize it.

