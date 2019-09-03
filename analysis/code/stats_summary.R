## Summary statistics
## Script returns Table 8, 9, and 10

## Setup -----
# Load R-to-table output package
require(stargazer)
require(zoo)

# Empty workspace
rm(list = ls())
# Load prepared data
source_data <- "analysis/input/data_build.RData"
load(source_data)

## Earthquake events aggregation -----
# Sum up struck and neighbor firms by earthquake events
sumByQuake <- sapply(1:nrow(earthquake),
                     function(x) {
                       colSums(StrikeTreatment(fs[,c("Quarter", "Lat", "Lon")], earthquake[x,],"Quarter"))
                     })
sumByQuake <- t(sumByQuake)
earthquake <- cbind(earthquake, sumByQuake[,1:2])

# Substract data for summary table
earthquake_tab <- earthquake[,.(Date,Lat, Lon,Mag,Depth,Struck,Neighbor)]
# Set the head names
colnames(earthquake_tab) <- c("Date","Latitude","Longtitude", 
                              "Magnitude (Richter scale)", "Depth(km)",
                              "Stricken firms","Neighbor firms")
earthquake_tab[, Date := as.Date(as.character(Date),format="%Y%m%d")]
earthquake_tab[, Date := format(Date,format="%Y/%m/%d")]

# Return table as text
capture.output(stargazer(earthquake_tab,
                         summary = FALSE,
                         type = "text",
                         digits = 2,
                         rownames=FALSE),
               file = "analysis/output/Table9.txt")

# Redo for latex script
capture.output(stargazer(earthquake_tab,
                         summary = FALSE,
                         type = "latex",
                         digits = 2,
                         rownames=FALSE),
               file = "analysis/output/Table9.tex")

## Industry distribution -----
# Sum up by industry code
industry_tab <- rbind(
  table(unique(fs[,.(Stkcd, Indcd)])$Indcd), # Ind dist. of full sample
  table(unique(fs[!fs$Stkcd %in% union(fs$Stkcd[fs$Neighbor==1],fs$Stkcd[fs$Struck==1]),.(Stkcd, Indcd)])$Indcd), # Ind dist. of unaffected group
  table(unique(fs[fs$Struck == 1,.(Stkcd, Indcd)])$Indcd)) # Ind dist. of struck groups
industry_tab <- rbind(industry_tab, industry_tab[1,]-industry_tab[2,]-industry_tab[3,]) 

# Reshape and sum up by group
industry_tab <- t(industry_tab)
industry_tab <- rbind(industry_tab, colSums(industry_tab))

# Set heading
industry_tab <- rbind(c("Full sample","Unaffected","Stricken","Neighbor"),
                      industry_tab)
industry_tab <- cbind(c("Industry section","Financials","Public utilities","Real estates",
                        "Conglomerate","Industry","Commerce","Total"),
                      industry_tab)

# Return table as text
capture.output(stargazer(industry_tab,
                         summary = FALSE,
                         type = "text",
                         rownames=FALSE),
               file = "analysis/output/Table10.txt")

# Redo for latex script
capture.output(stargazer(industry_tab,
                         summary = FALSE,
                         type = "latex",
                         rownames=FALSE),
               file = "analysis/output/Table10.tex")

## Fundamental data summary -----
# Select descriptive statitcs
statList <- c("min", "p25","median","mean","p75","max","sd","n")
# Substract data
fs[abs(Leverage)==9999, Leverage := NA]
fs <- fs[,.(Stkcd, Struck, Neighbor, 
            Cash_p*100, Leverage*100, TA/1000000, Cash/1000000,DebtL/1000000)]
colnames(fs) <- c("Stkcd", "Struck", "Neighbor", 
                  "Cash(%)", "Leverage(%)",
                  "Total assets", "Total cash", "Total Debt")

fs2 <- fs2[,.(Stkcd, Struck, Neighbor, 
              InsuranceExp/1000,Payoff*100)]
colnames(fs2) <- c("Stkcd", "Struck", "Neighbor",
                   "Insurance expenses","Payout ratio(%)")

# Save Table 8: Full sample descriptive stats

capture.output(stargazer(fs[,!c("Stkcd", "Struck", "Neighbor")],
                         fs2[, !c("Stkcd", "Struck", "Neighbor")],
                         header=FALSE,
                         summary.stat = statList,
                         flip = TRUE, 
                         type='text',
                         digits = 2,
                         title = "Panel A: Full Sample"),
               stargazer(fs[which(fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]),
                            !c("Stkcd", "Struck", "Neighbor")],
                         fs2[which(fs2$Stkcd %in% fs2$Stkcd[fs2$Neighbor==1]),
                             !c("Stkcd", "Struck", "Neighbor")],
                         header=FALSE,
                         summary.stat = statList,
                         flip = TRUE, 
                         type='text',
                         digits = 2,
                         title = "Panel B: Neighbor Firms"),
               stargazer(fs[which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1] & !fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]), 
                            !c("Stkcd", "Struck", "Neighbor")],
                         fs2[which(!fs2$Stkcd %in% fs2$Stkcd[fs2$Struck==1] & !fs2$Stkcd %in% fs2$Stkcd[fs2$Neighbor==1]),
                             !c("Stkcd", "Struck", "Neighbor")],
                         header=FALSE,
                         summary.stat = statList,
                         flip = TRUE, 
                         type='text',
                         digits = 2,
                         title = "Panel c: Unaffected Firms"),
               file = "analysis/output/Table8.txt")

# Redo for latex output
capture.output(stargazer(fs[,!c("Stkcd", "Struck", "Neighbor")],
                         fs2[, !c("Stkcd", "Struck", "Neighbor")],
                         header=FALSE,
                         summary.stat = statList,
                         flip = TRUE, 
                         type='latex',
                         digits = 2,
                         title = "Panel A: Full Sample"),
               stargazer(fs[which(fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]),
                            !c("Stkcd", "Struck", "Neighbor")],
                         fs2[which(fs2$Stkcd %in% fs2$Stkcd[fs2$Neighbor==1]),
                             !c("Stkcd", "Struck", "Neighbor")],
                         header=FALSE,
                         summary.stat = statList,
                         flip = TRUE, 
                         type='latex',
                         digits = 2,
                         title = "Panel B: Neighbor Firms"),
               stargazer(fs[which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1] & !fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]), 
                            !c("Stkcd", "Struck", "Neighbor")],
                         fs2[which(!fs2$Stkcd %in% fs2$Stkcd[fs2$Struck==1] & !fs2$Stkcd %in% fs2$Stkcd[fs2$Neighbor==1]),
                             !c("Stkcd", "Struck", "Neighbor")],
                         header=FALSE,
                         summary.stat = statList,
                         flip = TRUE, 
                         type='latex',
                         digits = 2,
                         title = "Panel c: Unaffected Firms"),
               file = "analysis/output/Table8.tex")

## Save temp data -----
save.image("analysis/temp/stats_summary.RData")
