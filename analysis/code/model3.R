## Model 3 regression on learning effects
## Script returns Table 4

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

## Model 3, Table 4 learning effects -----
# Previous experience
fs[,historyN := 1*(cumsum(Neighbor)-1*(Neighbor==1)>0), by=Stkcd] # Previous experience > 0
fs[,historyN0 := 1*(cumsum(Neighbor)-1*(Neighbor==1)==0), by=Stkcd] # Previous experience = 0
fs[,historyN1 := 1*(cumsum(Neighbor)-1*(Neighbor==1)==1), by=Stkcd] # Previous experience = 1
fs[,historyN2 := 1*(cumsum(Neighbor)-1*(Neighbor==1)>1), by=Stkcd] # Previous experience = 1

fs[,TotalN := cumsum(Neighbor)-1*(Neighbor==1), by=Stkcd] # Previous experience

# Aggregate close variable
fs[,close_1 := shift(Neighbor,n=1,fill = NA,type = "lag"), by=Stkcd] # Events in last quarter
fs[,close_8 := rollsum(close_1, k = 8, fill = NaN), by= Stkcd] # Events in recent 8 quarters

# Baseline
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City)+factor(Season) +factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")

# Experienced or not
ex <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag*historyN0) +I(Neighbor*Mag*historyN)+ I(Struck*Mag)+ 
            I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Indcd)+factor(Season)-1, 
          data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")

# Exp = 0, Exp>1 or Exp>2
ex2 <- plm(I(Cash_p_1f*100) ~  I(Neighbor*Mag*historyN0) + I(Neighbor*Mag*historyN1)+
             I(Neighbor*Mag*historyN2)+I(Struck*Mag)+ 
             I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Indcd)+factor(Season)-1,
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")

# Exp = 0, Exp>1 or Exp>2, controlling recent strikes in lastest 8 quarters
ex3 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag*historyN0) + I(Neighbor*Mag*historyN1)+
             I(Neighbor*Mag*historyN2)+I(Struck*Mag)+
             # I(Neighbor*Mag*historyN1*close8)+I(Neighbor*Mag*historyN2*close8)+
             close_8+ I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Indcd)+factor(Season)-1,
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")

## Output estimation
# Output as txt table
capture.output(stargazer(fix1,ex,ex2,ex3,
                         dep.var.labels=c("Cash_t"),
                         keep = 1:7,
                         type='text',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("Controls","Yes","Yes","Yes","Yes"),
                                          c("FE","Yes","Yes","Yes","Yes"))),
               file = "Table4.txt")

# Output as Latex form
capture.output(stargazer(fix1,ex,ex2,ex3,
                         dep.var.labels=c("$Cash_{i,t}$"),
                         covariate.labels=c(
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$ $\times$ Exp$=0_{i,t-1}$",
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$ $\times$ Exp$>0_{i,t-1}$",
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$ $\times$ Exp$=1_{i,t-1}$",
                           "Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$ $\times$ Exp$=2_{i,t-1}$",
                           "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
                           "Close8$_{i,t-1}"),
                         keep = 1:7,
                         type='latex',
                         digits = 3,
                         df = FALSE,
                         add.lines = list(c("Controls","Yes","Yes","Yes","Yes","Yes","Yes"),
                                          c("FE","Yes","Yes","Yes","Yes","Yes","Yes"))),
               file = "Table4.tex")

## Save temp data -----
path_dir <- "~/R project/Code replication/analysis/temp"
changeDir(path_dir)
save.image("model3.RData")