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
#after a search, I check if the stats library for computing the distribution
#functions.
lapply(c("stats"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
# I prepare a function to perform Kolmogorov-Smirnoff test with normal reference 
#distribution by researching online and with the help of ChatGPT. 
ks_test <- function(data) {
  # Empirical cumulative distribution function per the code provided
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Theoretical cumulative distribution function (for normal distribution) per
  #the code provided
  theoreticalCDF <- pnorm(data)
  
  # I calculate the test statistic
  D <- max(abs(empiricalCDF - theoreticalCDF))
  
  # I calculate the p-value
  n <- length(data)
  p_value1 <- 1 - pnorm(sqrt(n) * D)
  
  # I return test statistic and p-value
  return(list(test_statistic = D, p_value1 = p_value1))
}

# settting seed for reproducibility as required
set.seed(123)

# I generate 1,000 Cauchy random variables
cauchy_data <- rcauchy(1000, location = 0, scale = 1)

# I perform the Kolmogorov-Smirnoff test
result <- ks_test(cauchy_data)

# I concatenate and print the test statistic and p-value
cat("Test Statistic:", result$test_statistic, "\n")
cat("P-value:", result$p_value, "\n")
#Test statistic is 0.1347281 and the p value is 1.019963.
#Test statistic is relatively small, indicating that the observed data and the 
#normal distribution being tested against are relatively similar.
#In Kolmogorov-Smirnoff test, the null hypothesis is that the empirical distribution 
#of the observed data matches the specified normal distribution. P is way above
# 0.05, so I fail to  reject the null hypothesis, indicating that there is no 
#significant difference between the observed datas empirical distribution and 
#the specified normal distribution.

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)
# I define the objective function for OLS
objective_function <- function(beta, x, y) {
  y_pred <- beta[1] + beta[2] * x
  residuals <- y - y_pred
  sum(residuals^2)
}

# I am using the  BFGS algorithm to estimate OLS parameters: the initial guess
#and the optimum result per https://rpubs.com/aaronsc32/newton-raphson-method
# and a help from chatGPT
initial_guess <- c(0, 0)  # Initial guess for intercept and slope
optim_result <- optim(par = initial_guess, fn = objective_function, 
                      x = data$x, y = data$y, method = "BFGS")

# I extract the coefficients from the optimization result
bfgs_intercept <- optim_result$par[1]
bfgs_slope <- optim_result$par[2]

# I print coefficients obtained using BFGS
cat("Intercept:", bfgs_intercept, "\n")
#Intercept is 0.1391778
cat("Slope:", bfgs_slope, "\n")
#slope is 2.7267

# I fit linear regression using lm() function for comparison
lm_model <- lm(y ~ x, data = data)

# I print the coefficients obtained using lm()
cat("\nCoefficients using lm():\n")
print(summary(lm_model)$coefficients)
#Intercept in the lm model is 0.1391874 and slope is 2.7266985, providing
#very identical methods in fitting the model to the data.