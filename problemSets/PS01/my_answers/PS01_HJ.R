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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

### Question 1: Part 1
## plot histogram to check if the data is normally distributed
hist(y, col = "lightblue", main = "Histogram Distribution", xlab = "IQ scores")

# since population standard dev is unknown and n < 30 and the data is approximately normal
# we will use t-distribution for computing our confidence interval
# we know the formula for computing the confidence interval using this method: 
# ---- Confidence Interval = sample_mean +- t_val (alpha/2, df) * (sample_std_dev/sqrt(n))


## finding input parameters for computing confidence intervals

length_y <- length(y)
sample_mean <- mean(y)
sample_std_dev <- sd(y)
alpha <- 1 - 0.90 
alpha_by_2 <- alpha/2 ## as confidence interval is a two tail test

## calculate t critical value and error bounds

t <- qt(1 - (alpha_by_2), df = length_y - 1)
error_bound <- t * (sample_std_dev/sqrt(length_y))

## upper and loewr bounds for confidence intervals

upper_error_bound <- sample_mean + error_bound
lower_error_bound <- sample_mean - error_bound

### Question 1: Part 2 

hypothesized_mean = 100
alpha_2 = 0.05 # we use 1 - alpha as this is a one-tail test
t_value = (sample_mean - hypothesized_mean) / (sample_std_dev / sqrt(length_y))
t_critical = qt(1 - alpha_2, df = length_y - 1)

if (t_value > t_critical) {
  print("Reject the null hypothesis in favor of the alternative hypothesis")
} else {
  print("Do not reject the null hypothesis.")
        }


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

##### part 1 ##### 
### plot the correlations + scatterplot matrix bw Y, X1, X2 and X3 ##### 
install.packages("corrplot")
library(corrplot)

correlation_data <- expenditure[, c("Y", "X1", "X2", "X3")]
cor_matrix <- cor(correlation_data)
corrplot(cor_matrix, method = "number")
mtext("Correlation Matrix for Y, X1, X2, X3", side = 2, line = 2, cex = 1)
pairs(correlation_data, main = "Scatterplot Matrix")


####################################################

######## plot relationship between Y and Region ########### 
plot(expenditure$Region, expenditure$Y, main = "Regional Distribution of Per Capita Expenditure",
     xlab = "Region", ylab = "Per Capita expenditure on shelters/housing",
     col = "purple")

boxplot(Y ~ Region, data = expenditure, main="Regional Distribution of Per Capita Expenditure",
        xlab="Region", 
        ylab="Per Capita expenditure on shelters/housing", 
        col = "purple")


####################################################

######## plot relationship between Y and X1 ########### 
install.packages("ggplot2")
library(ggplot2)

## basic relationship plot
p <- ggplot(expenditure, aes(x = Y, y = X1)) + 
  geom_point(alpha=0.7) + 
  ylab("per capita expenditure on shelters/housing assistance") + 
  xlab("per capita personal income in state") 


print(p)
  
## relationship plot expanded by the colors and size
p <- ggplot(expenditure, aes(x = Y, y = X1, color = Region, size = Region)) + 
  geom_point(alpha=0.7) + 
  ylab("per capita expenditure on shelters/housing assistance") + 
  xlab("per captia personal income in state") 

print(p)


