## Merge data tables with fs
## Calculate variables from merged data

## Setup -----
# Set the working directory to access temp data
setwd("~/R project/Code replication/build/temp")

# Set link to temp data
source_data <- "data_pre_merge.RData"
# Load filter data
load(source_data)

## Merge tables -----
# Merge fs with industry code
fs <- merge(fs,ind,by = "Stkcd", all.x = TRUE)
# Merge fs with company location
fs <- merge(fs,company,by = "Stkcd", all.x = TRUE)
# Merge fs with Income
fs <- merge(fs,Income,by = c("Stkcd","Accper"), all.x = TRUE)
# Merge fs with Insurance
fs <- merge(fs,Insurance,by = c("Stkcd","Accper"), all.x = TRUE)
# Merge fs with CPI
fs <- merge(fs,CPI,by.x = "Accper", by.y ="Season", all.x = TRUE)

## Create cross index -----
fs$Ind_Season <- paste(fs$Indcd,fs$Season,sep = "_") # Industry-season index
fs$City_Season <- paste(fs$City,fs$Season,sep = "_") # City-season index

## Adjust fs with CPI -----
fs[, c("Cash","RD","TA","TL","TE","DebtL","OpIncome","InsuranceExp") :=
  lapply(.SD, "/", CPI), .SDcols = c("Cash","RD","TA","TL","TE","DebtL","OpIncome","InsuranceExp")]

## Calculate ratios -----
# Cash holding ratio
fs[, Cash_p := Cash/TA]
# Leverage ratio
fs[, Leverage := TL/TE]
# Insurance coverage ratio
fs[, Insurance_p := InsuranceExp/OpIncome]

## Mark the sample into thirds by measures
# Level for equal-size groups
n <- 3
fs[, c("quanTA","quanOI","quanLev", "quanCash_p") := 
     lapply(.SD, cut, breaks = n),
   .SDcol=c("TA","OpIncome","Leverage","Cash_p"),
   by = "Accper"]

## Save merged data
# Drop merged tables
rm(company, CPI, Income, Insurance)
# Save data to temp
save.image("data_merged.RData")