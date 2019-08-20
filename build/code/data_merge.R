## Prepare financial statement data
## Require packages: data.table, zoo
## Require data tables: company, CPI, earthquake

## Load data and filter sample -----

fs <- fread("Cash_RD.csv")

names(fs) <- c("Stkcd","Accper","Reptyp","Cash","RD","TA","DebtL","TL","TE")

# Keep only observations with consolidated statement
fs <- fs[fs$Reptyp=="A",] 

# Keep only observations in the list of company sample
fs <- fs[fs$Stkcd %in% company$Stkcd,] 

# Drop year beginning observations
fs <- fs[!grepl("/1/1",fs$Accper),] 

# Substract season(quarter)
fs$Quarter <- as.yearqtr(fs$Accper, format = "%Y/%m/%d")

# Substract subsample of MSE shares
fs_midsm <- fs[fs$Stkcd>002000&fs$Stkcd<002999,] 

# Keep only main board stock market
fs <- fs[fs$Stkcd<002000|fs$Stkcd>002999,]

## Should here starts another script?

## Merge with industry index -----

# Merge by key: Stkcd
fs <-merge(fs,ind,by = "Stkcd", all = FALSE)

# Create cross index
fs$Ind_Season <- paste(fs$Indcd,fs$Season %% 10000,sep = "_") # Industry-season index

## Merge with city location -----
fs$City <- company$Regplc[match(fs$Stkcd, company$Stkcd)] # Adding location factor into financial data
fs$City_Season <- paste(fs$City,fs$Season %% 10000,sep = "_") # City-season index

## Adjust to inflation -----

# Merge with CPI

# Divide by acc. CPI for all ab. column at once?

fs$Cash <- fs$Cash/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$RD <- fs$RD/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$TA <- fs$TA/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$TL <- fs$TL/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$TE <- fs$TE/CPI$CPI[match(fs$Accper,CPI$Season)]
fs$DebtL <- fs$DebtL/CPI$CPI[match(fs$Accper,CPI$Season)]

## Calculate the ratios -----

fs$TA[fs$TA==0] <- NA # Mark records where total asset is zero
fs$Cash_p <- fs$Cash/fs$TA # Calculate cash holding ratio
fs$Leverage <- log(fs$TL) - log(fs$TA-fs$TL) # Calculate leverage ratio
fs$Leverage[which(is.infinite(fs$Leverage)&fs$Leverage>0)] <- 9999 # Give value to infinte leverage ratio, i.e. TA=TL
fs$Leverage[which(is.infinite(fs$Leverage)&fs$Leverage<0)] <- -9999

## Mark the sample into thirds by measures
fs <- fs[, c("quanTA","quanOI","quanLev", "quanCash_p") := 
           list(findInterval(TA, quantile(TA,na.rm = TRUE, probs = c(0,1/3,2/3))),#c(0,0.1, 0.3, 0.5,0.7, 0.9))),
                findInterval(OpIncome, quantile(OpIncome,na.rm = TRUE, probs = c(0,1/3,2/3))),#c(0,0.1, 0.3, 0.5,0.7, 0.9)))
                findInterval(Leverage, quantile(Leverage,na.rm = TRUE, probs = c(0,1/3,2/3))),#c(0,0.1, 0.3, 0.5,0.7, 0.9)))
                findInterval(Cash_p, quantile(Cash_p,na.rm = TRUE, probs = c(0,1/3,2/3)))#c(0,0.1, 0.3, 0.5,0.7, 0.9)))
           ),   by = "Accper"]