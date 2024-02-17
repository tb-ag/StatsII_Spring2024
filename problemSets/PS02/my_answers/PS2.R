#####################
# Tolga Bag - 23371290
#####################

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("glmnet"), pkgTest) #after a research, I use glmnet package to fit the model
#per here: https://glmnet.stanford.edu/articles/glmnet.html
lapply(c("dplyr"), pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
str(climateSupport) #I am checking the structure of the code. I can see that the format
#of the variables won't work unless I change them to fit my model.
# I convert countries to numbers and convert it to a number
climateSupport$countries <- as.numeric(gsub(" of 192", "", climateSupport$countries)) 
#then, I make 20 low, 80 medium, 160 high in terms of participation rates
climateSupport <- climateSupport %>%
   mutate(countries = case_when(
        countries == 20 ~ "low",
        countries == 80 ~ "medium",
        countries == 160 ~ "high"
   ))
#I need it to be a factor to run the model, so I change it:
climateSupport$countries <- as.factor(climateSupport$countries)
# I convert sanctions by removing '%', and converting 'None' to 0
climateSupport$sanctions <- as.numeric(sub("%", "", climateSupport$sanctions))
climateSupport$sanctions[is.na(climateSupport$sanctions)] <- 0
climateSupport$sanctions <- factor(climateSupport$sanctions, levels = c(0, 5, 15, 20))
# I convert the choice variable from supported/non supported to 1 and 0. I turn
#it into a factor for the model
climateSupport$choice <- ifelse(climateSupport$choice == "Supported", 1, 0)
climateSupport$choice <- as.factor(climateSupport$choice)
str(climateSupport) #now they are all factoral and could be fit to the model.
#I got many errors. I use this and Gemini: https://stat.ethz.ch/pipermail/r-help/2007-October/143569.html
#I decided to change to glmnet and fit a logistical regression since gam didn't work.
logit_model <- glm(choice ~ countries + sanctions, 
                   data = climateSupport, 
                   family = binomial())
summary(logit_model)
summary_output <-capture.output(summary(logit_model)) #I provide the output.
cat("My R Output:\n", summary_output, file = "output1.txt", sep = "\n")

#The global null hypothesis is none of the independent variables (the number of 
#participating countries or the sanctions the lack of compliance brings) are linked
#to the probability of an individual supporting a policy. The p value is  < 2e-16,
#meaning it is a tiny number. In other words, we can reject the null hypothesis since
#there is a statistically significant relationship between at least one of the 
#independent variables and the probability of "choice". intercept is 0.376, which 
#means that the predicted probability of "choice" when countries is "high" and 
#there are no sanctions is 37.6%.

#####################
# Problem 2
#####################
#a#. The "higher" in countries refer to the highest number of countries' participation
#into the policy. As higher is our reference point in the regression, the coefficients
# 0.19186 for 5% sanctions, -0.13325 for 10% and 15% for 20% sanctions mean the higher
#the sanctions the lesser the individual will support the policy per this model.
#In other words, increasing sanctions from 5% to 15% will decrease the odds that
#an individual will support the policy.

#b#. The model predicts that the lesser the participation of countries, the less
#likelihood of estimated probability that an individual will support a policy.
#Specifically 80 of 192 countries participating with no sanctions are referred in 
#the countriesmedium without any sanctions. the estimated probability that an individual 
#will support a policy is 31.19% less probable than the higher number of countries' 
#participation (our reference point in the model).

#c#. It depends on the data and results. The best way is to analyze it and make
#the interpretation. 

logit_model2 <- glm(choice ~ countries + sanctions + countries*sanctions, 
                    data = climateSupport, 
                    family = binomial())
summary(logit_model2)
summary_output2 <-capture.output(summary(logit_model2)) #I provide the output.
cat("My R Output:\n", summary_output2, file = "output2.txt", sep = "\n")
#Once interactions between sanctions and number of countries are included, none 
#of the results concerning these interactions are statistically significant since
#p values range from 0.0944 to 0.9966. We can"t reject our null hypothesis. 
#Even though there are bits like countriesmedium or sanctions5 with statistical
#significant results, the overall interaction terms do not reach a strong level 
#of significance.Hence, it won't change our assessment.
