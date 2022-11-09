##### Workings - Lily

# First, loading data into my R Studio session - doing "read.table" as this 
# pulls the data out already separated into columns. If I was to do "read.csv"
# it would pull the data out without separating into columns, as there are no
# commas between the data for it to recognise as the separation between columns

data <- read.table("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv")

# Then explore the data - below are some exploratory functions I use to do this:

summary(data)
head(data)
list(data)

str(data)

# I am going to extract just the numeric columns
# from the data dataset, because I think once
# I do that I will be able to do the pairs() function:

num_cols <- unlist(lapply(data, is.numeric))

# Now going to use this "logical vector" to take a subset 
# of our data frame:

data_num <- data[, num_cols]

# this produces a subset of the data that is only 
# columns that are numerical/integer observations

# Now I will see if I can do the pairs function
# with this subsetted numerical/integer only dataset

# first I will check that the data_num data
# is a data frame using the frame() function

is.data.frame(data_num)

#this returns "true", so the data is a data.frame, theoretically
# then, I should be able to do the pairs function on it

# just workings: dev.off()
# just workings: pairs(data_num) #this is huge/not very useful, trying below:

install.packages("psych")
library(psych)

cor.plot(Filter(is.numeric, data)) #great, produces correlation plot of the data

# Alternatively, we might take a subset of columns and use pairs() to
# visualise the correlations.

pairs(data[c("AdjSalePrice", "LandVal", "ImpsVal", "SqFtLot")],
      upper.panel = NULL) 
#this is a better production of pairs plot - use this type of 
#subset to produce more manageable pairs - also, the 
#cor.plot data is great to visualise, only thing is need
#to be able to see all of the labels on the axes.


##### Week 2 of Project:
dat <- readRDS("data/train.rds")

summary(dat)

#goal for today: to try different models
# then: produce nicely formatted regression tables to 
# compare them
# hen upload results of best models

#today we are using the dat dataset -- this is the reduceed
#dataset

install.packages("stargazer")
library("stargazer")
install.packages("broom")

mod2 <- lm(AdjSalePrice ~ Bathrooms, data = dat)
dat_add <- augment(mod2)
stargazer(mod2, type = "text", title = "Adj Sale Price and Bathrooms")

#this produces a table in the console, but would be better to get it
# in as an html

mod2BathroomsLandVal <- lm(AdjSalePrice ~ Bathrooms + LandVal, data = dat)
dat_add <- augment(mod2)
stargazer(mod2, type = "text", title = "Adj Sale Price and Bathrooms")

ggplot(dat, aes(Bathrooms, AdjSalePrice, group))