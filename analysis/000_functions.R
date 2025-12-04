#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Functions
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will load relevant user-written functions.

# Confidence Interval Calculator
CICalculator <- function(var,alpha) {

  sd <- sd(var, na.rm = T)
  mean <- mean(var, na.rm = T)
  perc = 1 - (alpha/2) #want to consider the two-tailed test
  z = qnorm(perc)  #used z here because N is large
  no_of_obs = sum(!is.na(var))

  lower = mean - z*sd/sqrt(no_of_obs)
  upper = mean + z*sd/sqrt(no_of_obs)

  return(c(lower,upper))
}
