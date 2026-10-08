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

library(ggplot2)

library(psych)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

#part 1

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
  


#finding the confidence interval




mean_iq <- mean(y)
mean(y)
standard_dev_iq <- sd(y)
standard_dev_iq
length_iq <- length(y)
length_iq

#finding t score because n < 30
# 1-.9 = .1 (this is the alpha value) => 1./ 2= .05 ( /2 bc confidence intervals have two tails)
# 1 - .05 = .95 (cumulative probability)

t_critical <- qt(0.95, df = 24)
t_critical


ci_90_lower <- mean_iq - t_critical * (standard_dev_iq / sqrt(length_iq))
ci_90_upper <- mean_iq + t_critical * (standard_dev_iq / sqrt(length_iq))

ci_90_lower
ci_90_upper

#part 2 Using the same sample, conduct the appropriate hypothesis test with 

#Step one: assumptions about the data

#Step two: Set up hypothesis
  #null hypothesis: Students average IQ are equal to 100
  #alternate hypothesis: Students average IQ is greater than 100
  #therefore, conducting a one-sided right-tailed

#Step three: test statistic

t_stat <- (mean_iq - 100) / (standard_dev_iq / sqrt(length_iq))
t_stat

#step four: Calculate a p value

p_value <- pt(t_stat, df = length_iq - 1, lower.tail = FALSE)
p_value

#Step five: draw a conclusion

#Because the p value is .7125 with is greater than the significance value .05
#we fail to reject the null hypothesis that the students average IQ is equal to 100



#####################
# Problem 2
#####################

rm(list=ls())
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

head(expenditure)


vars <- expenditure[, c("Y", "X1", "X2", "X3")]
names(vars) <- c("Expenditure", "Income", "Insecure Residents", "People in Urban Areas")  


q2_p1 <- pairs.panels(expenditure[, c("Y", "X1", "X2", "X3")],
             method = "pearson",  
             hist.col = "hotpink",
             lm= TRUE, 
             density = TRUE,
             ellipses = FALSE,
             xaxt = "n", yaxt = "n")
q2_p1

#Please plot the relationship between Y and Region ? On average, 
#which region has the highest per capita expenditure on housing assistance?

q2_p8 <- ggplot(expenditure, aes(x = factor(Region), y = Y)) +
  geom_boxplot(
    colour = "black", fill = "lightgrey"
  ) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.75,
    colour = "hotpink"
    )+
  labs(
    x = "Region",
    y = "Per Capita Expenditure"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Northeast",
      "2" = "North Central",
      "3" = "South",
      "4" = "West"
    )
  ) +
  theme_minimal()
q2_p8

reg_means <- tapply(expenditure$Y, expenditure$Region, mean)
reg_means

ggsave("ps01_q2_P8.png", plot = q2_p8,
       width = 7, height = 5, dpi = 300)

#Please plot the relationship between Y and X1? Describe this 
#graph and the relationship. Reproduce the above graph including one more 
#variable Region and display different regions with different types of 
#symbols and colors.

q2_p9 <- ggplot(expenditure, aes(x = X1, y = Y )) +
  geom_point(size = 3) +
  labs(
    x = "Per Capita Personal Income in State",
    y = "Per Capita Housing Assistance Expenditure in State"
  ) 
q2_p9

ggsave("ps01_q2_P9.png", plot = q2_p9,
       width = 7, height = 5, dpi = 300)



q2_P10 <- ggplot(expenditure, aes(x = X1, y = Y, 
                                  shape = factor(Region), 
                                  color = factor(Region))) +
  geom_point(size = 3) +
  labs(
    x = "Per Capita Personal Income in State",
    y = "Per Capita Housing Assistance Expenditure in State",
    shape = "Region"
  ) +
  scale_shape_discrete(
    labels = c(
      "1" = "Northeast",
      "2" = "North Central",
      "3" = "South",
      "4" = "West"
    )
  ) +
  scale_color_discrete(
    labels = c(
      "1" = "Northeast",
      "2" = "North Central",
      "3" = "South",
      "4" = "West"
    )
  ) +
  theme_minimal()
q2_P10

ggsave("ps01_q2_P10.png", plot = q2_P10,
       width = 7, height = 5, dpi = 300)





