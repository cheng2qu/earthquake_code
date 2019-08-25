## Create lagged and lead term for fs

## Setup -----
# Set the working directory to access temp data
setwd("~/R project/Code replication/build/temp")

# Empty workspace
rm(list = ls())
# Set link to temp data
source_data <- "data_split.RData"
# Load filter data
load(source_data)

## Lag-lead fs data -----
# Order fs table
fs <- fs[order(Stkcd, Quarter),]

# Lag financial data
cols <- c("Cash","RD","TA","Cash_p","Leverage","DebtL","OpIncome", "Insurance_p")
addcols <- paste0(cols,"_d")
fs[order(Stkcd, Quarter), (addcols) := shift(.SD, n=1L, type = "lag"), by = Stkcd, 
   .SDcol = cols]

# Diff cash holding ratio
fs[, Cash_c := log(Cash_p)-log(Cash_p_d), by = Stkcd] 
fs$Cash_c[which(is.nan(fs$Cash_c))] <- NA
fs$Cash_c[which(is.infinite(fs$Cash_c))] <- NA

# Lead cash holding ratio and insurance
cols <- c("Cash_p","Insurance_p")
addcols <- paste0(cols,"_1f")
fs[order(Stkcd,Quarter), (addcols) := shift(.SD, n=1L, type = "lead"),
   by = Stkcd, 
   .SDcol = cols]

# Lag quake event
cols <- c("MagN","MagS")
addcols <- unlist(lapply(cols, function(x) paste0(x,"_lag",1:8)))
fs[order(Stkcd,Quarter), (addcols) := shift(.SD, n=1:8, type = "lag"),
   by = Stkcd, 
   .SDcol = cols]

# Lead quake event
cols <- c("MagN","MagS")
addcols <- paste0(cols,"_1f")
fs[order(Stkcd,Quarter), (addcols) := shift(.SD, n=1, type = "lead"),
   by = Stkcd, 
   .SDcol = cols]

## Lag-lead fs2 data -----
# Order fs table
fs2 <- fs2[order(Stkcd, Year),]

# Lag financial data
cols <- c("Cash","RD","TA","Cash_p","Leverage","DebtL","OpIncome", "Insurance_p", "Payoff")
addcols <- paste0(cols,"_d")
fs2[order(Stkcd, Year), (addcols) := shift(.SD, n=1L, type = "lag"), by = Stkcd, 
   .SDcol = cols]

# Lead cash holding ratio and insurance
cols <- c("Cash_p","Insurance_p", "Payoff")
addcols <- paste0(cols,"_1f")
fs2[order(Stkcd, Year), (addcols) := shift(.SD, n=1L, type = "lead"),
   by = Stkcd, 
   .SDcol = cols]

# Lag quake event
cols <- c("MagN","MagS")
addcols <- unlist(lapply(cols, function(x) paste0(x,"_lag",1:4)))
fs2[order(Stkcd, Year), (addcols) := shift(.SD, n=1:4, type = "lag"),
   by = Stkcd, 
   .SDcol = cols]

# Lead quake event
cols <- c("MagN","MagS")
addcols <- paste0(cols,"_1f")
fs2[order(Stkcd, Year), (addcols) := shift(.SD, n=1, type = "lead"),
   by = Stkcd, 
   .SDcol = cols]

## Save lead/lag variable
# Select data to save
lsList <- ls()
dropList <- c("addcols", "cols", "path_dir", "source_data")
saveList <- lsList[!lsList %in% dropList]
# Save data to temp
save(list=saveList, file="data_build.RData")

# Save data to output
path_dir <- "~/R project/Code replication/build/output"
changeDir(path_dir)
# Save the data tables
save(list=saveList, file="data_build.RData")

# Save data to analysis/input
path_dir <- "~/R project/Code replication/analysis/input"
changeDir(path_dir)
# Save the data tables
save(list=saveList, file="data_build.RData")
