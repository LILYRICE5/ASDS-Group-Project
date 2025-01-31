# Hallissey's Housing Price Project

## Data
dat <- read.table ("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv"
)


install.packages("dplR")
install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Landvalue and bathrooms multivariate regression
lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$LandVal)
brlv.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$LandVal)
summary(brlv.lm)

## Bathroom regression and plot
lm(dat$AdjSalePrice ~ dat$Bathrooms)
br.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary (br.lm)
plot(dat$Bathrooms, dat$AdjSalePrice)
abline(br.lm)

#Landvalue regression and plot r .65
lm(dat$AdjSalePrice ~ dat$LandVal)
lv.lm <- lm(dat$AdjSalePrice ~ dat$LandVal)
summary(lv.lm)
plot(dat$LandVal, dat$AdjSalePrice)
abline(lv.lm)

# Sqr foot to living regression and plot r .48
summary(lm(dat$AdjSalePrice ~ dat$SqFtTotLiving))
sqftol.lm <- lm(dat$AdjSalePrice ~ dat$SqFtTotLiving)
summary(sqftol.lm)
plot(dat$SqFtTotLiving, dat$AdjSalePrice)
abline(sqftol.lm)

# BldgGrade r.45
summary(lm(dat$AdjSalePrice ~ dat$BldgGrade))
bldg.lm <- lm(dat$AdjSalePrice ~ dat$BldgGrade)
summary(bldg.lm)
plot(dat$BldgGrade, dat$AdjSalePrice)
abline(bldg.lm)

# Impsval regression r. 69
summary(lm(dat$AdjSalePrice ~ dat$ImpsVal))
impval.lm <- lm(dat$AdjSalePrice ~ dat$ImpsVa)
plot(dat$AdjSalePrice ~ dat$ImpsVal)
abline(impval.lm)

# Multivariates
# r .53
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))

# r .70
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving
     + dat$ImpsVal))

# r .86 (https://medias.spotern.com/spots/w640/65/65297-1533296705.jpg)
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving
     + dat$ImpsVal + dat$LandVal))

# r .86
summary(
  lm(dat$AdjSalePrice ~ dat$ImpsVal + dat$LandVal))

### Bi and multivarte regressions for bathrooms, bldg grade, and sq ft living
## Bivariates
# Bathroom regression and plot r .28
lm(dat$AdjSalePrice ~ dat$Bathrooms)
br.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary (br.lm)
plot(dat$Bathrooms, dat$AdjSalePrice)
abline(br.lm)
# Sqr foot to living regression and plot r .48
summary(lm(dat$AdjSalePrice ~ dat$SqFtTotLiving))
sqftol.lm <- lm(dat$AdjSalePrice ~ dat$SqFtTotLiving)
summary(sqftol.lm)
plot(dat$SqFtTotLiving, dat$AdjSalePrice)
abline(sqftol.lm)
# BldgGrade r.45
summary(lm(dat$AdjSalePrice ~ dat$BldgGrade))
bldg.lm <- lm(dat$AdjSalePrice ~ dat$BldgGrade)
summary(bldg.lm)
plot(dat$BldgGrade, dat$AdjSalePrice)
abline(bldg.lm)

# Multivariate
# r .53
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))
brblsqrlm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade
                + dat$SqFtTotLiving)
mvlm.resid < -resid(brblsqrlm)


## Code for model with zip_group
# data = d

c(d, lm3.resid)

zip_group <- d %>%
  group_by(ZipCode) %>%
  summarise(resids = (lm3.resid),
            count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

zip_group <- d %>%
  group_by(ZipCode) %>%
  summarise(resids = (lm3.resid),
            count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dd <- d %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")


lm.4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup + Bathrooms, data
           = dat)

summary(lm.4)

stargazer(lm.4, type = "html")
