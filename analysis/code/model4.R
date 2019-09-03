## Model 4 diff-in-diff on dividend ratio
## Script returns Table 5

## Setup -----
# Load R-to-table output package

# Empty workspace
rm(list = ls())
# Load prepared data
source_data <- "analysis/input/data_build.RData"
load(source_data)

# Model 4. Dividend---------------------------------

# Matched pair difference-in-difference
# Treatment post-period: neighbor firms but not ever be stricken
Neighbor <- fs[fs$Neighbor==1 & !fs$Stkcd %in% fs$Stkcd[fs$Struck==1],] 

# Match by Acceper, TA, leverage, cash holding quantile, and general industrial code
# Define function didSample()
didSample <- function(repN, output.file){
  # repN = Resample times
  # output.file = output file name
  flag <- 0 # Counting repeated times
  did <- data.frame() # Variable to save test results
  while(flag<=repN){
    matchPair <- data.frame()
    for (stock in 1:nrow(Neighbor)) {
      matchList <- which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1 | fs$Neighbor==1] &
                           fs$Accper == Neighbor$Accper[stock] &
                           fs$Indcd == Neighbor$Indcd[stock] &
                           fs$quanTA == Neighbor$quanTA[stock] &
                           fs$quanLev == Neighbor$quanLev[stock])
      if(length(matchList)==0) {
        matchPair <- rbind(matchPair, fs[NA,], fill = TRUE,
                           make.row.names = FALSE)
      } else {
        matchPair <- rbind(matchPair, fs[sample(matchList, size = 1),],
                           make.row.names = FALSE)
        # print(stock)
      }
    }
    length(which(!is.na(matchPair$Stkcd))) # 658 out of 660 matched
    
    # Treatment pre-period: Neighbor fimrs-before
    Nb <- which(div$Stkcd %in% Neighbor$Stkcd & !div$Year %in% earthquake$Year)
    # Controlled pre-period: Far-away firms-before
    Fb <- which(div$Stkcd %in% matchPair$Stkcd & !div$Year %in% earthquake$Year)
    
    # Hypotheses tests, t-test
    setkeyv(Neighbor,c("Stkcd","Year"))
    setkeyv(matchPair,c("Stkcd","Year"))
    setkeyv(div,c("Stkcd","Year"))
    
    # Treatment - Controlled, post-period
    tdiv1 <- t.test(div[Neighbor,Payoff]-div[matchPair,Payoff], alternative="less")$statistic 
    diffdiv1 <- t.test(div[Neighbor,Payoff]-div[matchPair,Payoff], alternative="less")$estimate
    
    # Post-period - pre-period, treatment
    tdiv2 <- t.test(div[Neighbor,Payoff]- div[Nb,Payoff], alternative="less")$statistic
    diffdiv2 <- t.test(div[Neighbor,Payoff]- div[Nb,Payoff], alternative="less")$estimate
    
    # Post-period - pre-period, controlled
    tdiv3 <- t.test(div[matchPair,Payoff] - div[Fb,Payoff], alternative="less")$statistic
    diffdiv3 <- t.test(div[matchPair,Payoff] - div[Fb,Payoff], alternative="less")$estimate
    
    # Treatment - Controlled, pre-period
    tdiv4 <-  t.test(div[Nb,Payoff] - div[Fb,Payoff], alternative="less")$statistic
    diffdiv4 <-  t.test(div[Nb,Payoff] - div[Fb,Payoff], alternative="less")$estimate
    
    # Difference-in-difference, (Treatment - Controlled, post-period) - (Treatment - Controlled, pre-period)
    tdiv5 <- t.test(
      (div[Neighbor,Payoff] - div[Nb,Payoff]) - (div[matchPair,Payoff] - div[Fb,Payoff]),
      alternative="less")$statistic
    diffdiv5 <-  t.test(
      (div[Neighbor,Payoff] - div[Nb,Payoff]) - (div[matchPair,Payoff] - div[Fb,Payoff]),
      alternative="less")$estimate
    
    did <- rbind(did, list("dNQ"=diffdiv1,"tNQ"=tdiv1,"dN"=diffdiv2,"tN"=tdiv2,
                           "dFQ"=diffdiv3,"tFQ"=tdiv3,"dNN"=diffdiv4,"tNN"=tdiv4,
                           "DD"=diffdiv5,"tDD"=tdiv5))
    flag <- flag + 1
  }
  write.table(did, output.file)
  
  # Output table
  didTable <- data.table(c(mean(div[Neighbor,Payoff], na.rm = TRUE), NA, mean(div[Nb,Payoff], na.rm = TRUE), NA, diffdiv2, tdiv2),
                         c(mean(div[matchPair,Payoff], na.rm = TRUE), NA, mean(div[Fb,Payoff], na.rm = TRUE), NA, diffdiv3, tdiv3),
                         c(diffdiv1, tdiv1, diffdiv4, tdiv4, diffdiv5, tdiv5))
  didTable <- cbind( c("Quake quarter","", "No-quake quarter","", "Quake - No-quake",""),didTable)
  colnames(didTable) <- c("","Neighbor firm","Faraway firm","Neighbor - Faraway")

  capture.output(stargazer(didTable,
                           summary = FALSE,
                           colnames = TRUE,
                           rownames = FALSE,
                           type = "text",
                           digits = 3),
                 file = "analysis/output/Table5.txt")
  capture.output(stargazer(didTable,
                           summary = FALSE,
                           colnames = TRUE,
                           rownames = FALSE,
                           type = "latex",
                           digits = 3),
                 file = "analysis/output/Table5.tex")
  
}

# Call function 
didSample(repN = 10, output.file = "analysis/output/Diff-in-Diff.txt")

# Narrower interval for quantiles
probs <- c(0,0.1, 0.3, 0.5,0.7, 0.9)
fs[, c("quanTA","quanOI","quanLev", "quanCash_p") := 
     lapply(.SD, function (x) 
       findInterval(x, 
                    quantile(x, probs,na.rm=TRUE), 
                    rightmost.closed = TRUE)),
   .SDcol=c("TA","OpIncome","Leverage","Cash_p"),
   by = "Accper"]

# Match by Acceper, TA, leverage, cash holding quantile, and general industrial code
didSample(repN = 10, output.file = "analysis/output/Diff-in-Diff_narrower.txt")

## Save temp data -----
save.image("analysis/temp/model4.RData")
