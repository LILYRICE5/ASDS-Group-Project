# Hallissey's Housing Price Project

## Data
dat <- read.table ("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv"
) # this is train data apparently


install.packages("dplR")
install.packages("stargazer")
library(stargazer)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Landvalue and bathrooms multivariate regression (additive)
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

# Multivariate # these are the resids from our best model
# r .53
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))
brblsqrlm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade
                + dat$SqFtTotLiving)
mvlm.resid <-residuals(brblsqrlm)


## Code for model with zip_group
# data = d

dat$resid <- mvlm.resid

summary(dat$resid)

#zip group is the categorical variable - creating this now; works
zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(mvlm.resid),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat_zip_group <- cbind(dat, zip_group = mvlm.resid) #adds category v for zcode to dat

dd <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode") #take zipcode
#above joins the two

summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))
brblsqrlm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade
                + dat$SqFtTotLiving)


# then run the best model plus the zip_group  - done on uploaded



summary(zip_group$ZipCode)

summary(dat_zip_group)

####the above is not working

unique(dat$ZipCode)


dd <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode") #take zipcode

?left_join

lm.4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup + Bathrooms, data
           = dat)

summary(lm.4)

stargazer(lm.4, type = "html")

###############################
###Week 11 Tutorial Workings - LR ###
###############################

install.packages("car")
library(car)


# square building grade - graph with and without, then see the lm output for each - is one better
#fit than the other

summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))
brblsqrlm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade
                + dat$SqFtTotLiving)

plot(brblsqrlm) #residuals vs fitted line 

Mod_Best_ZG <- lm(dd$AdjSalePrice ~ dd$Bathrooms + dd$BldgGrade + dd$SqFtTotLiving 
                  + as.factor(dd$ZipGroup))
terms <- predict(Mod_Best_ZG, type = "terms")


#retry: first creating partial resid plot for BldgGrade

dd <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode") #take zipcode

mod2 <- lm(AdjSalePrice ~ Bathrooms + BldgGrade + SqFtTotLiving + ZipGroup, data = dd)

terms <- predict(mod2, type = "terms")

partial_resid <- resid(mod2) + terms

df <- data.frame(BldGrade = dd["BldgGrade"], Terms = terms[,"BldgGrade"],
                 PartialResid = partial_resid[, "BldgGrade"])

ggplot(df, aes(BldgGrade, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(BldgGrade, Terms), colour = "red")

# adding polynomial term: squaring bldgGrade

mod4_bldgsquared <- lm(AdjSalePrice ~ Bathrooms + BldgGrade + I(BldgGrade^2)
                       + SqFtTotLiving + ZipGroup, data = dd)

terms_polybldggrade <- predict(mod4_bldgsquared, type = "terms")

partial_resid_polybldggrade <- resid(mod4_bldgsquared) + terms_polybldggrade

df_polybldggrade <- data.frame(BldgGrade = dd[, "BldgGrade"],
                               Terms = terms_polybldggrade[,"I(BldgGrade^2)"],
                               PartialResid = partial_resid_polybldggrade[, "I(BldgGrade^2)"])

ggplot(df_polybldggrade, aes(BldgGrade, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(BldgGrade, Terms), colour = "red")

summary(mod4_bldgsquared) #r squared is .5964, RSE is 244900
summary(mod2) #r squared is .5403, RSE is 261300

stargazer(mod2, type="html")

