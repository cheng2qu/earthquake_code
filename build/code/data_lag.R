## Lag the data


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

# One quarter forward
fs <- fs[, Cash_p_1f := shift(Cash_p, n=1L, type = "lead"), by = Stkcd] # Lead the cash holding ratio
