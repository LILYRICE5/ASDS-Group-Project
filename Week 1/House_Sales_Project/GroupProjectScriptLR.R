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







###########
#Week 3###
###########

# ruari found blggrade, bathrooms, sq ft adn two others had effect on predictability

# today: transform zipcode into usable categorical variable

# multivariate
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))
brblsqrlm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving)
mvlm.resid <- -resid(brblsqrlm)

summary(brblsqrlm)

summary(mvlm.resid)

install.packages("dplyr")
library(dplyr)
install.packages("stargazer")
library(stargazer)
install.packages("broom")
library(broom)

?group_by()

#trying to sort this code

zip_group <- as.data.frame(mvlm.resid) %>%
  group_by(mvlm.resid) %>%
  summarise(resids = (mvlm.resid),
            count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         mvlm.resid = ntile(cumul_count, 5))

summary(zip_group)


###Martyn's code:
zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))



dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)



# continue
zip_group_residuals <- mod4 %>%
  group_by(resids) %>%
  summarise(med_price = median(residuals),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         residuals = ntile(cumul_count, 5))

rlang::last_error()

# martyn infout - problem is resids is lm3.resid, we created object for resids as one vector,
# comes from our best model, need to bind it to dataset using cbind (to our dataset), then go 
# from there
#cbind line in there the og data set 


dat_with_residuals <- cbind(dat, residuals = mvlm.resid)
