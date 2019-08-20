## Prepare point data: 
## Earthquake events, company locations, CPI, industry classification

## Remember to clean temp and output everytime before running

## Setup -----

# Load packages for all scripts
require(data.table)
require(zoo)

# Set working directory to current folder
setwd("~/R project/Code replication/build/input")

## Earthquake events data -----

earthquake <- read.table("earthquake_event.txt", header = TRUE)

earthquake <- data.table(earthquake)

names(earthquake) <- c("Date","Lat","Lon","Depth","Mag","MMI6","MMI5","MMI1","Fatalities","EconLoss")

# Substract season(quarter)
earthquake$Quarter <- as.yearqtr(as.character(earthquake$Date), format = "%Y%m%d")

## Company loaction data -----
# Load the data but drop city name
company <- fread("company_location.csv", header = TRUE, drop = "Regplc")

## Quarterly CPI -----
CPI <- fread("CPI.csv")
CPI$Quarter <- as.yearqtr(CPI$Season, format = "%Y/%m/%d")

## Industry data -----
ind <- fread("Industry.csv", select = c("Stkcd","Indcd","Nindcd"))

# Drop replicated values
ind <- ind[!duplicated(ind$Stkcd),]

