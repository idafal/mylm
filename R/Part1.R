# Part 1
install.packages("car")
library(car)
data(SLID, package = "carData")
SLID <- SLID[complete.cases(SLID), ]
install.packages("GGally")
library(GGally)
ggpairs(SLID)

# Comment briefly on the relationship between some of the variables
# Want to study how wages depends on one or more explanatory variable.
# Which assumptions do we need to make about the data if we want to
# perform a multiple linear regression analysis?



