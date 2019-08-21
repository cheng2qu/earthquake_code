## Create lagged and lead term for fs

## Setup -----
# Set the working directory to access temp data
temp_dir <- "~/R project/Code replication/build/temp"
setwd(temp_dir)

# Set link to temp data
source_data <- "data_merged.RData"
# Load filter data
load(source_data)

## Lag fs data
fs <- fs[which(!is.na(fs$Cash_p)),] # Delete records when total asset is zero
fs <- fs[which(!is.na(fs$Leverage)),] # Delete records when total equity is negative

fs <- fs[, Cash_d := shift(Cash, n=1L, type = "lag"), by = Stkcd] # Lag the cash holding
fs <- fs[, RD_d := shift(RD, n=1L, type = "lag"), by = Stkcd] # Lag the RD
fs <- fs[, TA_d := shift(TA, n=1L, type = "lag"), by = Stkcd] # Lag the RD

fs <- fs[, Cash_p_d := shift(Cash_p, n=1L, type = "lag"), by = Stkcd] # Lag the cash holding ratio
fs <- fs[, Leverage_d := shift(Leverage, n=1L, type = "lag"), by = Stkcd] # Lag the leverage ratio
fs <- fs[, DebtL_d := shift(DebtL, n=1L, type = "lag"), by = Stkcd] # Lag the leverage ratio

fs <- fs[, Cash_c := log(Cash_p)-log(Cash_p_d), by = Stkcd] # Lag the cash holding ratio
fs$Cash_c[which(is.nan(fs$Cash_c))] <- NA
fs$Cash_c[which(is.infinite(fs$Cash_c))] <- NA


lag Income
lag insurance

# Lag of quake event
fs <- fs[, n_1 := shift(Neighbor*Mag, n=1L, type = "lead"), by = Stkcd]
fs <- fs[, s_1 := shift(Struck*Mag, n=1L, type = "lead"), by = Stkcd]

fs <- fs[, n1 := shift(Neighbor*Mag, n=1L, type = "lag"), by = Stkcd]
fs <- fs[, s1 := shift(Struck*Mag, n=1L, type = "lag"), by = Stkcd]
fs <- fs[, n2 := shift(Neighbor*Mag, n=2L, type = "lag"), by = Stkcd]
fs <- fs[, s2 := shift(Struck*Mag, n=2L, type = "lag"), by = Stkcd]
fs <- fs[, n3 := shift(Neighbor*Mag, n=3L, type = "lag"), by = Stkcd]
fs <- fs[, s3 := shift(Struck*Mag, n=3L, type = "lag"), by = Stkcd]
fs <- fs[, n4 := shift(Neighbor*Mag, n=4L, type = "lag"), by = Stkcd]
fs <- fs[, s4 := shift(Struck*Mag, n=4L, type = "lag"), by = Stkcd]

fs <- fs[, n5 := shift(Neighbor*Mag, n=5L, type = "lag"), by = Stkcd]
fs <- fs[, s5 := shift(Struck*Mag, n=5L, type = "lag"), by = Stkcd]
fs <- fs[, n6 := shift(Neighbor*Mag, n=6L, type = "lag"), by = Stkcd]
fs <- fs[, s6 := shift(Struck*Mag, n=6L, type = "lag"), by = Stkcd]
fs <- fs[, n7 := shift(Neighbor*Mag, n=7L, type = "lag"), by = Stkcd]
fs <- fs[, s7 := shift(Struck*Mag, n=7L, type = "lag"), by = Stkcd]
fs <- fs[, n8 := shift(Neighbor*Mag, n=8L, type = "lag"), by = Stkcd]
fs <- fs[, s8 := shift(Struck*Mag, n=8L, type = "lag"), by = Stkcd]

## Lead fs data
# One quarter forward on cash ratio
fs <- fs[, Cash_p_1f := shift(Cash_p, n=1L, type = "lead"), by = Stkcd] # Lead the cash holding ratio

