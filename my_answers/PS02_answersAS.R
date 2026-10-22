rm(list=ls())

library(ggplot2)
library(tidyr)
library(dplyr)
###PROMBLEM 1
  ##Part A
  #Hypothesis

  # Observed counts
observed <- matrix(c(14, 6, 7,
                7,  7, 1),
              nrow = 2, byrow = TRUE,
              dimnames = list(
                Class = c("Upper class", "Lower class"),
                Outcome = c("Not Stopped", "Bribe requested", "Stopped/given warning")
              ))

observed

  # Row and column totals
row_total <- rowSums(observed)
row_total
column_total <- colSums(observed)
column_total
grand_total <- sum(observed) 
grand_total

#expected frequencies
expected_freq <- function(observed) {
  row_total    <- rowSums(observed)
  column_total <- colSums(observed)
  grand_total  <- sum(observed)
  
  expected <- outer(row_total, column_total) / grand_total
  dimnames(expected) <- dimnames(observed)
  expected
}

expected <- expected_freq(observed)
expected

#chi sqaure test statistic

chi_sq <- sum((observed - expected)^2 / expected)
chi_sq

(observed - expected)^2




##Part B
#degrees of freedom
deg_of_freedom <- (nrow(observed) - 1) * (ncol(observed) - 1)
deg_of_freedom 

#p-value
pvalue <- pchisq(chi_sq, deg_of_freedom, lower.tail = F)
pvalue

#checking my work
test <- chisq.test(observed)
test
 

##Part C

#Calculating residuals
row_prop    <- row_total / grand_total
column_prop <- column_total / grand_total

standard_error <- sqrt(expected * outer(1 - row_prop, 1 - column_prop))
 
residuals <- (observed - expected) / standard_error
residuals

check_residuals <- chisq.test(observed)$stdres
check_residuals


###QUESTION 2
rm(list=ls())

#load data
df<- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(df); str(df); summary(df)

#BEGIN_regression_model
model <- lm(water ~ reserved, data = df)
summary(model)
model
#END_regression_model

#START_test_statistic
estimate <- coef(summary(model))["reserved", "Estimate"]
standard_error  <- coef(summary(model))["reserved", "Std. Error"]
t_stat <- (estimate - 0) / standard_error
t_stat
summary(model)$coefficients["reserved", "t value"]
#END_test_statistic

#START_p_value
p_value<- coef(summary(model))["reserved", "Pr(>|t|)"]
p_value
#END_p_value





