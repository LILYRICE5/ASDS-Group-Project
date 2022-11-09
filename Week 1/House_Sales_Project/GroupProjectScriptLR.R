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

install.packages("dplyr")





