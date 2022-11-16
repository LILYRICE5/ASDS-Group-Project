##### Workings Tutorial 1 - Lily

library(tidyverse) #loading packages

dat <- read.table("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")
summary(data)
head(data)
list(data)

boxplot(dat$AdjSalePrice ~ dat$Bathrooms)

plot(dat$Bathrooms, dat$AdjSalePrice)
boxplot(dat$SqFtFinBasement , dat$AdjSalePrice)
boxplot(dat$SqFtTotLiving, dat$AdjSalePrice)
boxplot(dat$YrBuilt, dat$AdjSalePrice)

# We compared means between Adjusted Sales Price (AdjSalePrice) and a number of other variables, including:
# Bathrooms, Bedrooms, Property Type, Newly Constructed, Traffic Noise. We found that the most variation in 
# means was by the number of 
# bathrooms in a house.  Houses with 7 bathrooms on average had the highest Adj. Sale Price. This average 
# sale price then dropped when bathrooms reached 8. Some variation (but not as much) was found based on the
# variation in means of bedrooms vs adjusted house price. There are
# a lot of outliers in the comparison of means of number of bathrooms vs adjusted sale price, so for further
# analysis we suggest taking out some of the outliers to better assess what variation exists within these box
# plots (between these means).







