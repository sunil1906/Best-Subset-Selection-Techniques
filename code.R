###########################
# Author: Sunil Kumar GV  #
# github: sunil1906       #
###########################

# install.packages("ISLR")
# install.packages("knitr")
# install.packages("printr")
# install.packages("leaps")
# install.packages("ggvis")

library(ISLR)
library(knitr)
library(printr)

# We are using hitters dataset
str(Hitters)

# summary about hitters dataset
print(summary(Hitters))

# Checking if NA values are present in dataset taken
sum(is.na(Hitters$Salary))

# removing the rows which contains NA values
Hitters=na.omit(Hitters)

# Checking for NA after removing rows containing NA
sum(is.na(Hitters))

library(leaps)
# contains all the best subset of size from 1 to 8 (default is 8)
regfit.full = regsubsets(Salary ~ ., data = Hitters)
summary(regfit.full)

# contains all the best subset of size from 1 to 19 (Note: This is a Exhaustive search takes a lot of time when there are lot of parameters (>40))
regfit.full = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)

# R squared error for best subset selection 
reg.summary$rsq

#plot rss
library(ggvis)
rsq <- as.data.frame(reg.summary$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

# Inference from Plot: After choosing 10 attributes, the R squared is almost a flat line. So, 10 attributes can be chosen to give the similar result as given by taking all 19 attributes.

# Forward and Backward Subset Selection
regfit.fwd = regsubsets(Salary ~. , data=Hitters,nvmax=19, method ="forward")
regfit.bwd = regsubsets(Salary ~. , data=Hitters,nvmax=19,method ="backward")

# Tells step wise what all attributes are included
summary(regfit.fwd)

# Coefficients of subset of size 7 for three differnet techniques
coef(regfit.full ,7)
coef(regfit.fwd ,7)
coef(regfit.bwd ,7)
