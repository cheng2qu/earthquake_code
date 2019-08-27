## Model 2 regression on drifting effects
## Script returns Table 3

## Setup -----
# Load R-to-table output package

# Set the working directory to access input data
path_dir <- "~/R project/Code replication/analysis/input"
changeDir(path_dir)

# Empty workspace
rm(list = ls())
# Load prepared data
source_data <- "data_build.RData"
load(source_data)

# Set the working directory to output folder
path_dir <- "~/R project/Code replication/analysis/output"
changeDir(path_dir)

## Model 2, Table 3 Drifting affects -----
# Aggregate close variable
fs[,close_1 := shift(Neighbor,n=1,fill = NA,type = "lag"), by=Stkcd] # Events in last quarter
fs[,close_2 := rollsum(close_1, k = 2, fill = NaN), by= Stkcd] # Events in recent 4 quarters
fs[,close_4 := rollsum(close_1, k = 4, fill = NaN), by= Stkcd] # Events in recent 4 quarters
fs[,close_8 := rollsum(close_1, k = 8, fill = NaN), by= Stkcd] # Events in recent 8 quarters
fs[,close_12 := rollsum(close_1, k = 12, fill = NaN), by= Stkcd] # Events in recent 12 quarters

# Baseline
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City)+factor(Season) +factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# Events in last quarter
da1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close_1 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Season) +factor(Indcd)-1,
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# Events in recent 2 quarters
da2 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close_2 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# Events in recent 4 quarters
da4 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close_4 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# Events in recent 8 quarters
da8 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close_8 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# Events in recent 12 quarters
da12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
              close_12 +
              I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Season) +factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")

## Output estimation
# Output as txt table
capture.output(stargazer(fix1,da1,da2,da4,da8,da12,
                         dep.var.labels=c("Cash_t"),
                         keep = 1:7,
                         type='text',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("Controls","Yes","Yes","Yes","Yes","Yes","Yes"),
                                          c("FE","Yes","Yes","Yes","Yes","Yes","Yes"))),
               file = "Table3.txt")

# Output as Latex form
capture.output(stargazer(fix1,da1,da2,da4,da8,da12,
                         dep.var.labels=c("$Cash_{i,t}$"),
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Close1$_{i,t-1}",
                           "Close2$_{i,t-1}",
                           "Close4$_{i,t-1}",
                           "Close8$_{i,t-1}",
                           "Close12$_{i,t-1}"),
                         keep = 1:7,
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("Controls","Yes","Yes","Yes","Yes","Yes","Yes"),
                                          c("FE","Yes","Yes","Yes","Yes","Yes","Yes"))),
               file = "Table3.tex")

## Save temp data -----
path_dir <- "~/R project/Code replication/analysis/temp"
changeDir(path_dir)
save.image("model2.RData")