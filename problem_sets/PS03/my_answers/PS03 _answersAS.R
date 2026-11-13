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

library(ggplot2)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")
inc.sub

#start_q1.1

lm1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(lm1)

#end_q1.1

#start_q1.2
scatter_dif_vote <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") +
  labs(
    title = "Vote Share vs.Log Difference in Campaign Spending",
    x = "Log Difference in Campaign Spending",
    y = "Vote Share"
  ) +
  theme_minimal()
scatter_dif_vote

ggsave("scatter_dif_vote.png", plot = scatter_dif_vote,
       width = 8, height = 6, dpi = 300)


#end_q1.2

#start_q1.3

residuals_lm1 <- residuals(lm1)
residuals_lm1
summary(residuals_lm1)

#end_q1.3



#start_q2.1

lm2 <- lm(presvote ~ difflog, data = inc.sub)
summary(lm2)

#end_q2.1

#start_q2.2
scatter_dif_pres <- ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") +
  labs(
    title = "Vote Share of the Incubent Party Presidential Candidate vs. 
    Log Difference in Campaign Spending",
    x = "Log Difference in Campaign Spending",
    y = "Vote Share of the Incumbent Party Presidential Candidate"
  ) +
  theme_minimal()

scatter_dif_pres

ggsave("scatter_dif_pres.png", plot = scatter_dif_pres,
       width = 8, height = 6, dpi = 300)


#end_q2.2

#start_q2.3

residuals_lm2 <- residuals(lm2)
residuals_lm2
summary(residuals_lm2)

#end_q2.3

#start_q3.1

lm3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(lm3)

#end_q3.1

#start_q3.2
scatter_pres_vote <- ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") +
  labs(
    title = " Incumbent Vote Share vs. Vote Share of the Incumbent Party Presidential Candidate",
    x = "Vote Share of the Incubent Party Presidential Candidate",
    y = "Incumbent Vote Share"
  ) +
  theme_minimal()

scatter_pres_vote

ggsave("scatter_pres_vote.png", plot = scatter_pres_vote,
       width = 8, height = 6, dpi = 300)

#end_q3.2

#start_q3.3 
residuals_lm3 <- residuals(lm3)
residuals_lm3
summary(residuals_lm3)
#end_q3.3


#start_q4.1
lm_res <- lm(residuals_lm1 ~ residuals_lm2)
summary(lm_res)
#end_q4.1

#start_q4.2
scatter_residuals <- ggplot(inc.sub, aes(x = residuals_lm2, y = residuals_lm1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") +
  labs(
    title = "Residuals from Question 1 vs. Question 2",
    x = "Residuals from Question 2",
    y = "Residuals from Question 1"
  ) +
  theme_minimal()

scatter_residuals

ggsave("scatter_residuals.png", plot = scatter_residuals,
       width = 8, height = 6, dpi = 300)
#end_q4.2



#start_q5.1
lm_multi <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(lm_multi)
#end_q5.1

