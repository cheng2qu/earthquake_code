## Model 1 regression on dummy effects
## Script returns Table 1, 2, 12, and 13

## Setup -----
# Load package for panel regression
require(plm)

# Empty workspace
rm(list = ls())
# Load prepared data
source_data <- "analysis/input/data_build.RData"
load(source_data)

## Model 1, Table 1: Full sample -----
## Regression (1) full sample without control
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)  
            + factor(City) +factor(Season)+factor(Indcd)-1, 
            data = fs, index=c("Accper","Stkcd"), model="within")

# # Alternative model without fixed effects
# lm1 <- lm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)
#           + factor(City) +factor(Season)+factor(Indcd)+ factor(Stkcd)+ factor(Accper)-1,
#           data = fs, na.action = na.exclude)
# # Comparing models /w and w/o fixed effects 
# phtest(fix1, lm1)

# Panel regression, with plm, fixed effect vs. random effect= 
fixed <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
             +factor(City) +factor(Season)+ factor(Indcd) -1,
             data = fs, index=c("Accper","Stkcd"), model="within")

# random <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
#               +factor(City) + factor(Indcd) -1,
#               data = fs, index=c("Accper","Stkcd"), model="random")
# 
# # Hausman test selection for random or finxed effect
# phtest(fixed, random) # Selected random effect model
# plmtest(fixed, c("time"), type=("bp")) # Use fixed_time

## Regression (2) full sample with control vairables
fix2 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Season)+factor(Indcd)-1, 
            data = fs, index=c("Accper","Stkcd"), model="within")

## Regression (3) excluding event 20080512
fix3 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA) 
            +factor(City) +factor(Season)+factor(Indcd)-1, 
            data = fs[fs$Quarter!="2008 Q2"], index=c("Accper","Stkcd"), model="within")

## Regression (4) on non-financial industrial sectors
fix4 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Season)+factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")

## Regression (5) on financial industrial sectors
fix5 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Season)+factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(1,2,3)], index=c("Accper","Stkcd"), model="within")

## Output estimation
# Output as txt table
capture.output(stargazer(fix1,fix2,fix3,fix4,fix5,
                         dep.var.labels=c("Cash_t"),
                         column.labels = c("Full sample", "Sub-sample"),
                         column.separate = c(2, 3),
                         keep = c(1,2,3,4,5),
                         type='text',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table1.txt")

# Output as Latex form
capture.output(stargazer(fix1,fix2,fix3,fix4,fix5,
                         column.labels = c("Full sample", "Sub-sample"),
                         column.separate = c(2, 3),
                         dep.var.labels=c("$Cash_{i,t}$"),
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "OpIncome$_{i,t-1}/TA$_{i,t-1}",
                           "Leverage$_{i,t-1}",
                           "log(TA$_{i,t-1})"),
                         keep = c(1,2,3,4,5),
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table1.tex")

## Model 1, Table 13: Overseas earthquake -----
## Regression (1) baseline
baseline <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
                + factor(Year)+ factor(Season)+factor(City) + factor(Indcd) -1,
                data = fs, index=c("Stkcd"), model="within")

## Regression (2) include overseas events
oversea <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+other+I(OpIncome/TA) + Leverage + log(TA)
               + factor(Year)+ factor(Season)+factor(City) + factor(Indcd) -1,
               data = fs, index=c("Stkcd"), model="within")

## Output estimation
# Output as txt table
capture.output(stargazer(baseline, oversea,
                         dep.var.labels=c("Cash_t"),
                         column.labels = c("Baseline", "Oversea"),
                         column.separate = c(1, 1),
                         type='text',
                         digits = 3,
                         keep = 1:6,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes"))),
               file = "analysis/output/Table13.txt")

# Output as Latex form
capture.output(stargazer(baseline, oversea,
                         dep.var.labels=c("$Cash_{i,t}$"),
                         column.labels = c("Baseline", "Oversea"),
                         column.separate = c(1, 1),
                         keep = 1:6,
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Overseas",
                           "OpIncome$_{i,t-1}/TA$_{i,t-1}",
                           "Leverage$_{i,t-1}",
                           "log(TA$_{i,t-1})"),
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes"))),
               file = "analysis/output/Table13.tex")

## Model 1, Table 2: regression by subgroups -----
## Panel A: regression by leverage subgroups
m11 <-  plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City)+factor(Season) +factor(Indcd)-1, 
            data = fs[fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m13 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

## Panel B: regression by market cap subgroups
m21 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1], index=c("Accper","Stkcd"), model="within")

m22 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanTA==2], index=c("Accper","Stkcd"), model="within")

m23 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City)+factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanTA==3], index=c("Accper","Stkcd"), model="within")

## Output estimation
# Output as txt table
capture.output(stargazer(m11,m12,m13,m21,m22,m23,
                         dep.var.labels=c("Cash_t"),
                         column.labels = c("Panel A: By leverage", "Panel B: By scale"),
                         column.separate = c(3, 3),
                         keep = c(1,2,3,4,5),
                         type='text',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table2.txt")

# Output as Latex form
capture.output(stargazer(m11,m12,m13,m21,m22,m23,
                         column.labels = c("Panel A: By leverage", "Panel B: By scale"),
                         column.separate = c(3, 3),
                         dep.var.labels=c("$Cash_{i,t}$"),
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "OpIncome$_{i,t-1}/TA$_{i,t-1}",
                           "Leverage$_{i,t-1}",
                           "log(TA$_{i,t-1})"),
                         keep = c(1,2,3,4,5),
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table2.tex")

## Table 13: Regression by leverage x market cap subgroups
m11 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==1 & fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1 & fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m13 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1 & fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

m21 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==2 & fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m22 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==2 & fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m23 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==2 & fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

m31 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) +I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==3 & fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m32 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==3 & fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m33 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Season)+factor(Indcd)-1, 
           data = fs[fs$quanTA==3 & fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

# Output as txt table
capture.output(stargazer(m11,m12,m13,m21,m22,m23,m31,m32,m33,
                         dep.var.labels=c("Cash_t"),
                         column.labels = c("Panel A: Small scale",
                                           "Panel B: Median scale",
                                           "Panel C: Large scale"),
                         column.separate = c(3, 3, 3),
                         keep = c(1,2,3,4,5),
                         type='text',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table13.txt")

# Output as Latex form
capture.output(stargazer(m11,m12,m13,m21,m22,m23,m31,m32,m33,
                         column.labels =c("Panel A: Small scale",
                                          "Panel B: Median scale",
                                          "Panel C: Large scale"),
                         column.separate = c(3, 3, 3),
                         dep.var.labels=c("$Cash_{i,t}$"),
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "OpIncome$_{i,t-1}/TA$_{i,t-1}",
                           "Leverage$_{i,t-1}",
                           "log(TA$_{i,t-1})"),
                         keep = c(1,2,3,4,5),
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("FE","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"))),
               file = "analysis/output/Table13.tex")


## Save temp data -----
save.image("analysis/temp/model1.RData")
