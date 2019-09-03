## Model 5 regression on insurance expenses
## Script returns Table 14

## Setup -----
# Load R-to-table output package

# Empty workspace
rm(list = ls())
# Load prepared data
source_data <- "analysis/input/data_build.RData"
load(source_data)

# Table 14, Regression on cash holding -----
fs2$Insurance_p[is.na(fs2$Insurance_p)] <-0

# Baseline
cashfs1<- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) +
                I(OpIncome/TA) +Leverage + log(TA) + 
                factor(City) + factor(Indcd)- 1,
              data = fs2, index = c("Accper","Stkcd"), model = "within")

# Model 1 with insurance purchase ratio
cashins1 <- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) + Insurance_p +
                  I(OpIncome/TA) +Leverage + log(TA) + 
                  factor(City) + factor(Indcd)- 1,
                data = fs2, index = c("Accper","Stkcd"), model = "within")

# Baseline, non financial sectors
cashfs2<- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) +
                I(OpIncome/TA) +Leverage + log(TA) + 
                factor(City) + factor(Indcd)- 1,
              data = fs2[fs2$Indcd %in% c(4,5,6)], index = c("Accper","Stkcd"), model = "within")

# Model 1 with insurance purchase ratio, non financial sectors
cashins2 <- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) + Insurance_p+
                  I(OpIncome/TA) +Leverage + log(TA) + 
                  factor(City) +  factor(Indcd)- 1,
                data = fs2[fs2$Indcd %in% c(4,5,6)], index = c("Accper","Stkcd"), model = "within")

## Output estimation
# Output as txt table
capture.output(stargazer(cashfs1,cashins1,cashfs2,cashins2,
                         dep.var.labels=c("Cash_t"),
                         keep = 1:6,
                         type='text',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table14.txt")

# Output as Latex form
capture.output(stargazer(cashfs1,cashins1,cashfs2,cashins2,
                         dep.var.labels=c("$Cash_{i,t}$"),
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Insurance_p$_{i,t-1}$",
                           "OpIncome$_{i,t-1}/TA$_{i,t-1}",
                           "Leverage$_{i,t-1}",
                           "log(TA$_{i,t-1})"),
                         keep = 1:6,
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table14.tex")

## Save temp data -----
save.image("analysis/temp/model5.RData")