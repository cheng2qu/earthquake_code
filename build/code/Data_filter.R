## Filter data before merge 

## Setup -----
# Empty workspace
rm(list = ls())

# Load packages for all scripts
require(data.table)
require(zoo)
# Load function changeDir
source("~/R project/Code replication/build/code/changeDir_function.R")

# Set working directory to current folder
setwd("~/R project/Code replication/build/input")

## 1.Earthquake data -----
earthquake <- read.table("earthquake_event.txt", header = TRUE)
earthquake <- data.table(earthquake)
names(earthquake) <- c("Date","Lat","Lon","Depth","Mag","MMI6","MMI5","MMI1","Fatalities","EconLoss")

# Substract season(quarter)
earthquake$Quarter <- as.yearqtr(as.character(earthquake$Date), format = "%Y%m%d")
earthquake$Year <- format(earthquake$Quarter, format = "%Y")

## 2.Company loaction data -----
# Load the data but drop city name
company <- fread("company_location.csv", header = TRUE)
names(company) <- c("Stkcd","City","Lon","Lat")
company <- unique(company)

## 3.CPI -----
CPI <- fread("CPI.csv")

## 4.Industry code -----
ind <- fread("Industry.csv", select = c("Stkcd","Indcd","Nindcd"))

# Drop replicated values
ind <- ind[!duplicated(Stkcd),]

## 5.Balance sheet data -----
fs <- fread("Cash_RD.csv")
names(fs) <- c("Stkcd","Accper","Reptyp","Cash","RD","TA","DebtL","TL","TE")

# Keep only consolidated report
fs <- fs[Reptyp=="A", c("Stkcd","Accper","Cash","RD","TA","DebtL","TL","TE")]
# Keep only shares listed before 2007
fs <- fs[Stkcd %in% company$Stkcd,]
# Drop year beginning record due to replication
fs <- fs[!grepl("/1/1",Accper),]
# Keep only main board stock market
fs <- fs[Stkcd<002000|Stkcd>002999,]
# Mark records where total asset is zero
fs$TA[fs$TA==0] <- NA
# # Drop empty observation
# fs <- fs[TA>0,]
# # Drop non-positive equity observation
# fs <- fs[TE>0,] # Omit 504 observations

# Substract season(quarter)
fs$Quarter <- as.yearqtr(fs$Accper, format = "%Y/%m/%d")
fs$Year <- format(fs$Quarter, format = "%Y")
fs$Season <- format(fs$Quarter, format = "%q")

## 6.Operating income -----
Income <- read.table("OpIncome.txt", header = TRUE, stringsAsFactors=FALSE)
Income <- data.table(Income)

# Keep only consolidated report and drop year beginning record
Income <- Income[Typrep == "A" & ! grepl("/01/01",Accper), c("Stkcd","Accper","OpIncome")]

## 7.Propertity insurance expenses -----
Insurance <- read.table("insurance.txt", header = TRUE, stringsAsFactors=FALSE)
Insurance <- data.table(Insurance)
Insurance <- Insurance[, c("Stkcd","Accper","Amount")]
names(Insurance) <- c("Stkcd","Accper","InsuranceExp")

## 8.Dividend -----
div <- fread("Div.csv")

# Keep only consolidated report and drop year beginning record
div <- div[Typrep=="A"& ! grepl("/01/01",Accper),]
# Correct the payoff ratio as 1-retainRatio
div$Payoff <- 1-div$Payoff
# Aggregate year end data
# Generate Year serie
div$Year <- substr(div$Accper,1,4)
# Sum semi-annual data
div[, DivT := sum(Div,na.rm = TRUE), by=.(Stkcd,Year)]
div[Payoff>0, Payoff:= DivT/Div*Payoff, by=.(Stkcd,Year)]
div <- div[grepl("/12/31",div$Accper),.(Stkcd, Accper, Year, DivT, Payoff )]

## 9.Net income -----
NI <- read.table("NI.txt", header = TRUE)
NI <- data.table(NI)

# Keep only consolidated report and year end record
NI <- NI[Typrep=="A" & grepl("/12/31",Accper),]

## Save data to temp
# Create temp direction if not exists
path_dir <- "~/R project/Code replication/build/temp"
# Switch working directory to temp folder
changeDir(path_dir)
# Save the data tables
save.image("data_pre_merge.RData")