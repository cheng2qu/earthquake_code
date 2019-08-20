##         Thesis - data summary and model regression
## Require data processing before running following code
require(data.table)
require(zoo)
require(ggplot2)
require(car) # Package for panel regression
require(plm) # Package for panel regression
require(MatchIt)
require(dplyr)
require(stargazer)

setwd("Z:/Documents/Thesis/Data_and_Rcode_40944/")

# Statistic summary -------------------------------------
# 0.1. Earthquake events aggregation---------------------
earthquake[,list(N=sum(fs$Neighbor[fs$Season==Season]), S=sum(fs$Struck[fs$Season==Season])), by=Season] 
# Aggregate numbers of neighbor and stricken firms

earthquake_effect <- unique(fs$Season[fs$Neighbor==1 | fs$Struck==1]) # Aggregate earthquakes which affect listed firms
unique(earthquake[earthquake$Season %in% earthquake_effect,"Date"])
# 15 out of 16 quakes have effect

# 0.2. Industry distributions---------------------------------
distT <- table(unique(fs[,c("Stkcd","Indcd")])$Indcd) # Ind dist. of full sample
distN <- table(unique(fs[fs$Neighbor == 1,c("Stkcd","Indcd")])$Indcd) # Ind dist. of treatment group
distS <- table(unique(fs[fs$Struck == 1,c("Stkcd","Indcd")])$Indcd) # Ind dist. of stricken group
distNS <- table(unique(fs[which(fs$Stkcd %in% intersect(fs$Stkcd[fs$Neighbor==1],fs$Stkcd[fs$Struck==1])),c("Stkcd","Indcd")])$Indcd) 
# Ind dist. of firms in intersection of treatment and stricken group
distF <- table(unique(fs[!fs$Stkcd %in% union(fs$Stkcd[fs$Neighbor==1],fs$Stkcd[fs$Struck==1]),c("Stkcd","Indcd")])$Indcd) # Ind dist. of controlled group

# 0.3. Summary Pre-regression -------------------------------

# Full sample descriptive stats
summary(fs[,.(Cash_p*100,Leverage*100, TA/1000000, Cash/1000000,DebtL/1000000,OpIncome/1000000)])
summary(cbind(Insurance$Amount/1000000, div$Payoff*100))

# Neighbor firms descriptive stats
sd(fs$DebtL[which(fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1])]/1000000,na.rm = TRUE)
sd(div$Payoff[which(div$Stkcd %in% fs$Stkcd[fs$Neighbor==1])]*100,na.rm = TRUE)
sd(Insurance$Amount[which(Insurance$Stkcd %in% fs$Stkcd[fs$Neighbor==1])]/1000000,na.rm = TRUE)

# Unaffected firms descriptive stats
lapply(fs[which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1] &
                  !fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]),
       .(Cash_p*100,Leverage*100, TA/1000000, Cash/1000000,DebtL/1000000,OpIncome/1000000)],
       sd, na.rm=TRUE)
lapply(list(Insurance$Amount[which(!Insurance$Stkcd %in% fs$Stkcd[fs$Neighbor==1]&
                                     !Insurance$Stkcd %in% fs$Stkcd[fs$Struck==1])]/1000000,
       div$Payoff[which(!div$Stkcd %in% fs$Stkcd[fs$Neighbor==1] &
                          !div$Stkcd %in% fs$Stkcd[fs$Struck==1])]*100),
       sd,na.rm=TRUE)

## Model 1, preliminary model: cash holding---------------------------------

# All sample
# Rough model without control

fixp <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)  
           + factor(City) +factor(Indcd)-1, 
          data = fs, index=c("Accper","Stkcd"), model="within")
summary(fixp)

lmp <- lm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)
          + factor(City) +factor(Indcd)+ factor(Stkcd)+ factor(Accper)-1,
          data = fs, na.action = na.exclude)
summary(lmp)

phtest(fixp, lmp)

# Full sample with control vairables
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
         +factor(City) +factor(Indcd)-1, 
         data = fs, index=c("Accper","Stkcd"), model="within")
summary(fix1)

# Exclude event on 20080512
fix12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA) 
            +factor(City) +factor(Indcd)-1, 
            data = fs[fs$Season!=20080630], index=c("Accper","Stkcd"), model="within")
summary(fix12)

# lm1 <- lm(I(Cash_p_1f*100) ~ I(OpIncome/TA) + Leverage + log(TA) + I(Neighbor*Mag) + I(Struck*Mag)
#           + factor(City) +factor(Indcd)+ factor(Stkcd)+ factor(Accper)-1,
#           data = fs, na.action = na.exclude)
# summary(lm1)

# fix2 <- plm(I(Cash_p_1f*100)  ~ I(OpIncome/TA) + Leverage + log(TA) + I(Neighbor*Mag) 
#           +factor(City) +factor(Indcd)-1, 
#           data = fs[!fs$Stkcd %in% fs$Stkcd[fs$Struck==1]], index=c("Accper","Stkcd"), model="within")
# summary(fix2)

# Regression on non-financial industrial sectors
fix4 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
               +factor(City) +factor(Indcd)-1, 
         data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
summary(fix4) 

# Regression on financial industrial sectors
fix5 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
          +factor(City) +factor(Indcd)-1, 
         data = fs[fs$Indcd %in% c(1,2,3)], index=c("Accper","Stkcd"), model="within")
summary(fix5)

## Output as Latex form
stargazer(fixp,fix1,fix12,fix4, fix5, dep.var.labels=c("$Cash_t$: Full sample","$Cash_t$:Sub sample"),
          # order=c("I(Neighbor * Mag)","I(Struck * Mag)", "I(OpIncome/TA)", "Leverage", "log(TA)"), 
          covariate.labels=c("Neighbor$_{t-1}$ $\times$ Mag$_{t-1}$","Struck$_{t-1}$ $\times$ Mag$_{t-1}$", 
                             "OpIncome$_{t-1}/TA$_{t-1}","Leverage$_{t-1}","log(TA$_{t-1})"), 
          keep = c(1,2,3,4,5))

# Panel regression, with plm, fixed effect vs. random effect= 
fixed <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
             +factor(City) + factor(Indcd) -1,
             data = fs, index=c("Accper","Stkcd"), model="within")
summary(fixed)

random <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
              +factor(City) + factor(Indcd) -1,
              data = fs, index=c("Accper","Stkcd"), model="random")
summary(random)

# Hausman test selection for random or finxed effect
phtest(fixed, random) # Selected random effect model
plmtest(fixed, c("time"), type=("bp")) # Use fixed_time

# Model 1.Overseas earthquake---------------------------------
# Baseline
fixed <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
             + factor(Year)+ factor(Season4)+factor(City) + factor(Indcd) -1,
             data = fs, index=c("Stkcd"), model="within")
summary(fixed)

# Include overseas events
os <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+other+I(OpIncome/TA) + Leverage + log(TA)
          + factor(Year)+ factor(Season4)+factor(City) + factor(Indcd) -1,
             data = fs, index=c("Stkcd"), model="within")
summary(os)

## Output as Latex form
stargazer(fixed,os,dep.var.labels=c("$Cash_{i,t}$"))

# Model 1. regression by subgroups-Lev======================================
m11 <-  plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Indcd)-1, 
            data = fs[fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m13 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

# Model 1. regression by subgroups-Scale======================================

m21 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1], index=c("Accper","Stkcd"), model="within")

m22 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==2], index=c("Accper","Stkcd"), model="within")

m23 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==3], index=c("Accper","Stkcd"), model="within")

# Import regression results into Latex
stargazer(m11,m12,m13,
          dep.var.labels=c("$Cash_t$, Panel A: Subgroups by leverage"),
          column.labels=c("Low leverage","Median leverage","High leverage"),
          covariate.labels=c("Neighbor_{t-1} $\times$ Mag_{t-1}","Struck_{t-1} $\times$ Mag_{t-1}", 
                             "OpIncome/TA_{t-1}","Leverage_{t-1}","log(TA_{t-1})"), 
          keep = c(1,2,3,4,5))

stargazer(m11,m12,m13,m21,m22,m23,
          dep.var.labels=c("$Cash_t$, Panel B: Subgroups by total assets"),
          column.labels=c("Small scale","Median scale","Large scale"),
          covariate.labels=c("Neighbor$_{t-1}$ $\times$ Mag$_{t-1}$","Struck$_{t-1}$ $\times$ Mag$_{t-1}$", 
                             "OpIncome$_{t-1}/TA$_{t-1}","Leverage$_{t-1}","log(TA$_{t-1})"), 
          keep = c(1,2,3,4,5))

# Model 1. regression by subgroups-SxL======================================
m11 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1 & fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1 & fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m13 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==1 & fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

m21 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==2 & fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m22 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==2 & fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m23 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==2 & fs$quanLev==3], index=c("Accper","Stkcd"), model="within")

m31 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) +I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==3 & fs$quanLev==1], index=c("Accper","Stkcd"), model="within")

m32 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==3 & fs$quanLev==2], index=c("Accper","Stkcd"), model="within")

m33 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
           +factor(City) +factor(Indcd)-1, 
           data = fs[fs$quanTA==3 & fs$quanLev==3], index=c("Accper","Stkcd"), model="within")


stargazer(m11,m12,m13,m21,m22,m23,m31,m32,m33,
          dep.var.labels=c("Panel A: Subgroups- large assets",
                           "Panel B: Subgroups- large assets",
                           "Panel C: Subgroups- large assets"),
          covariate.labels=c("Neighbor$_{t-1}$ $\times$ Mag$_{t-1}$","Struck$_{t-1}$ $\times$ Mag$_{t-1}$", 
                             "OpIncome$_{t-1}/TA$_{t-1}","Leverage$_{t-1}","log(TA$_{t-1})"), 
          keep = c(1,2,3,4,5))

# Model 2. Drifting affects -----------------------------------

# mp <- lm(I(Cash_p_1f*100)  ~ I(OpIncome/TA) + Leverage + log(TA) + I(Neighbor*Mag) + I(Struck*Mag) 
#          +factor(City) +factor(Indcd)+factor(Year)+factor(Stkcd)-1, 
#          data = fs,na.action = na.exclude)
# summary(mp)

# Baseline
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Indcd)-1, 
            data = fs, index=c("Accper","Stkcd"), model="within")
summary(fix1)

# Quarterly regression-8 drifting quarters
d8 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)
          + n1 + s1 + n2 + s2 + n3 + s3 + n4 + s4+n5 + s5 + n6 + s6 + n7 + s7 + n8 + s8+
          +I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
                  data = fs, index=c("Accper","Stkcd"), model="within")
summary(d8)
# plot(seq(0,8,1),d8$coefficients[c(1,3,5,7,9,11,13,15,17)])
# plot(seq(0,8,1),d8$coefficients[c(2,4,6,8,10,12,14,16,18)])

# Quarterly regression-10 drifting quarters
d10 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)
           + n_1+s_1 + n1 + s1 + n2 + s2 + n3 + s3 + n4 + s4+n5 + s5 + n6 + s6 + n7 + s7 + n8 + s8+
            +I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
          data = fs, index=c("Accper","Stkcd"), model="within")
summary(d10)

# Quarterly regression-8 drifting quarters, non-financial sectors
d8b <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)
            +n1 + s1 + n2 + s2 + n3 + s3 + n4 + s4+n5 + s5 + n6 + s6 + n7 + s7 + n8 + s8+
              +I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
summary(d8b)

# Quarterly regression-10 drifting quarters, non-financial sectors
d10b <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)
          + n_1+s_1+ n1 + s1 + n2 + s2 + n3 + s3 + n4 + s4+n5 + s5 + n6 + s6 + n7 + s7 + n8 + s8+
            +I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
         data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
summary(d10b)


# Model 3. Learn from past---------------------------------

fs <- fs[order(fs$Stkcd,fs$Season),] # Sort the data by ticker and period

# Previous experience
fs <- fs[,historyN := 1*(cumsum(Neighbor)-1*(Neighbor==1)>0), by=Stkcd] # Previous experience > 0
fs <- fs[,historyN0 := 1*(cumsum(Neighbor)-1*(Neighbor==1)==0), by=Stkcd] # Previous experience = 0
fs <- fs[,historyN1 := 1*(cumsum(Neighbor)-1*(Neighbor==1)==1), by=Stkcd] # Previous experience = 1
fs <- fs[,historyN2 := 1*(cumsum(Neighbor)-1*(Neighbor==1)>1), by=Stkcd] # Previous experience > 1

fs <- fs[,historyS := 1*(cumsum(Struck)-1*(Struck==1)), by=Stkcd] # Previous experience of stricken > 0


table(fs$Stkcd[fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]],
      fs$historyN[fs$Stkcd %in% fs$Stkcd[fs$Neighbor==1]])

fs <- fs[,TotalN := cumsum(Neighbor)-1*(Neighbor==1), by=Stkcd] # Previous experience
fs <- fs[,close12 := 1*(TotalN-lag(TotalN,12)>0), by=Stkcd] # Events in recent 8 quarters
fs <- fs[,close8 := 1*(TotalN-lag(TotalN,8)>0), by=Stkcd] # Events in recent 8 quarters
fs <- fs[,close4 := 1*(TotalN-lag(TotalN,4)>0), by=Stkcd] # Events in recent 4 quarters
fs <- fs[,close2 := 1*(TotalN-lag(TotalN,2)>0), by=Stkcd] # Events in recent 2 quarters
fs <- fs[,close1 := lag(Neighbor,1), by=Stkcd] # Events in last quarter

# fs$close[is.na(fs$close)] <- 0
# table(fs$close)

fs <- fs[,TotalS := cumsum(Struck), by=Stkcd] # Cummulative previous events

# Baseline
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(fix1)

# Experienced or not
ex <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag*historyN0) +I(Neighbor*Mag*historyN)+ I(Struck*Mag)+factor(TotalN)+
            I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Indcd)-1, 
          data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(ex)

# Exp = 0, Exp>1 or Exp>2
ex2 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag*historyN0) +
             I(Neighbor*Mag*historyN1)+I(Neighbor*Mag*historyN2)+ I(Struck*Mag)+
            I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Indcd)+factor(TotalN)-1, 
          data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(ex2)

# Exp = 0, Exp>1 or Exp>2, controlling recent strikes in lastest 8 quarters
ex3 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag*historyN0) +
             I(Neighbor*Mag*historyN1)+I(Neighbor*Mag*historyN2)+
             # I(Neighbor*Mag*historyN1*close8)+I(Neighbor*Mag*historyN2*close8)+
             I(Struck*Mag)+close8+
             I(OpIncome/TA) + Leverage + log(TA)+factor(City)+factor(Indcd)+factor(TotalN)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
summary(ex3)

stargazer(fix1,ex,ex2,ex3,dep.var.labels=c("$Cash_{i,t}$"))


# Model 2. Drift cont., excluded financial sectors======================

# Baseline, excluded financial sectors
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Indcd)-1, 
            data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(fix1)

# Drift effect of quake 12 quarters before
da12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close12 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(da12)

# Drift effect of quake 8 quarters before
da8 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
              close8 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(da8)

# Drift effect of quake 4 quarters before
da4 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close4 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
summary(da4)

# Drift effect of quake 2 quarters before
da2 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close2 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(da2)

# Drift effect of quake 1 quarter before
da1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close1 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
# summary(da1)

# Controlling all drifts
da <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
            close1 + close2 +close4+close8+close12+
            I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
          data = fs[fs$Indcd %in% c(4,5,6)], index=c("Accper","Stkcd"), model="within")
summary(da)

stargazer(fix1,da1,da2,da4,da8,da12,
          dep.var.labels=c("$Cash_{i,t}$"))
          # covariate.labels=c(#"Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
          #                    #"Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$"),
          # "Quake1Q$_{i,t-1}$","Quake2Q$_{i,t-1}$",
          # "Quake4Q$_{i,t-1}$","Quake8Q$_{i,t-1}$","Quake12Q$_{i,t-1}$"),
          # keep = c(#"I(Neighbor*Mag)","I(Struck*Mag)",
          #          "close1","close2","close4","close8","close12"))
          # keep = c(1,2))

### Drift cont., included financial sectors=====================================

# Baseline
fix1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+I(OpIncome/TA) + Leverage + log(TA)
            +factor(City) +factor(Indcd)-1, 
            data = fs, index=c("Accper","Stkcd"), model="within")
# summary(fix1)

da12 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
              close12 +
              I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
            data = fs, index=c("Accper","Stkcd"), model="within")
# summary(da12)

da8 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close8 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs, index=c("Accper","Stkcd"), model="within")
# summary(da8)

da4 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close4 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs, index=c("Accper","Stkcd"), model="within")
summary(da4)

da2 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close2 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs, index=c("Accper","Stkcd"), model="within")
# summary(da2)

da1 <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
             close1 +
             I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
           data = fs, index=c("Accper","Stkcd"), model="within")
# summary(da1)

da <- plm(I(Cash_p_1f*100) ~ I(Neighbor*Mag) + I(Struck*Mag)+
            close1 + close2 +close4+close8+close12+
            I(OpIncome/TA) + Leverage + log(TA)+factor(City) +factor(Indcd)-1, 
          data = fs, index=c("Accper","Stkcd"), model="within")
summary(da)

stargazer(fix1,da1,da2,da4,da8,da12,
          dep.var.labels=c("$Cash_{i,t}$"))
#           covariate.labels=c(#"Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
#             #"Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$"),
#             "Quake1Q$_{i,t-1}$","Quake2Q$_{i,t-1}$",
#             "Quake4Q$_{i,t-1}$","Quake8Q$_{i,t-1}$","Quake12Q$_{i,t-1}$"),
#           keep = c(#"I(Neighbor*Mag)","I(Struck*Mag)",
#             "close1","close2","close4","close8","close12"))
# keep = c(1,2))


# Insurance regression-------------------------
# All sample

# Insurance as dependent variable to current earthquake events
Ins0 <- plm(Ins_p  ~  I(Neighbor*Mag) + I(Struck*Mag) +
              factor(City) +  factor(Year)+ factor(Indcd)- 1,
            data = fs2, index = c("Accper","Stkcd"), model="within")
summary(Ins0)
# No effect

# Insurance as dependent variable to previous earthquake events (without controls)
Ins1 <- plm(Ins_p_1f  ~  I(Neighbor*Mag) + I(Struck*Mag) +
             factor(City) +  factor(Year)+ factor(Indcd)- 1,
           data = fs2, index = c("Accper","Stkcd"), model = "within")
summary(Ins1)
# No effect

# Insurance as dependent variable to previous earthquake events (with controls)
Ins2 <- plm(Ins_p_1f  ~  I(Neighbor*Mag) + I(Struck*Mag)+ 
             I(OpIncome/TA) +Leverage + log(TA) + factor(Indcd) + 
             factor(City) +  factor(Year)- 1,
            data = fs2, index = c("Accper","Stkcd"), model = "within")
summary(Ins2)
# No effect

# Insurance as dependent variable to previous earthquake events (with controls, within financial sector)
Ins3 <- plm(Ins_p_1f  ~  I(Neighbor*Mag) + I(Struck*Mag)+ 
              I(OpIncome/TA) +Leverage + log(TA) + factor(Indcd) + 
              factor(City) +  factor(Year)- 1,
            data = fs2[fs2$Indcd %in% c(4,5,6)], index = c("Accper","Stkcd"), model = "within")
summary(Ins3)
# No effect


stargazer(Ins1,Ins2,Ins3,
          dep.var.labels=c("$Insurance_{i,t}$"))

# Regression for cash holding
# Baseline
cashfs1<- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) +
                I(OpIncome/TA) +Leverage + log(TA) + 
                factor(City) +  factor(Year)+ factor(Indcd)- 1,
              data = fs2, index = c("Accper","Stkcd"), model = "within")
summary(cashfs1)

# Model 1 with insurance purchase ratio
cashins1 <- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) + Ins_p+
                 I(OpIncome/TA) +Leverage + log(TA) + 
                 factor(City) +  factor(Year)+ factor(Indcd)- 1,
               data = fs2, index = c("Accper","Stkcd"), model = "within")
summary(cashins1)

# Baseline, non financial sectors
cashfs2<- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) +
                 I(OpIncome/TA) +Leverage + log(TA) + 
                 factor(City) +  factor(Year)+ factor(Indcd)- 1,
               data = fs2[fs2$Indcd %in% c(4,5,6)], index = c("Accper","Stkcd"), model = "within")
summary(cashfs2)

# Model 1 with insurance purchase ratio, non financial sectors
cashins2 <- plm(I(Cash_p_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) + Ins_p+
                 I(OpIncome/TA) +Leverage + log(TA) + 
                 factor(City) +  factor(Year)+ factor(Indcd)- 1,
               data = fs2[fs2$Indcd %in% c(4,5,6)], index = c("Accper","Stkcd"), model = "within")
summary(cashins2)

stargazer(cashfs1,cashins1,cashfs2,cashins2,
          dep.var.labels=c("$Insurance_{i,t}$"))
          # covariate.labels=c("Neighbor$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
          #                    "Struck$_{i,t-1}$ $\times$ Mag$_{i,t-1}$",
          #                    "Ins_p$_{i,t-1}$",
          #                    "OpIncome$_{t-1}$/TA$_{t-1}$",
          #                    "Leverage$_{t-1}$","log(TA$_{t-1}$)")

# Model 4. Dividend---------------------------------

# pf0 <-  plm(I(Payoff_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag)+
#               factor(City) +  factor(Year)+ factor(Indcd)- 1,
#             data = div, index = c("Accper","Stkcd"), model = "within")
# summary(pf0)
# 
# pf1 <-  plm(I(Payoff_1f*100)  ~  I(Neighbor*Mag) + I(Struck*Mag) + 
#               I(OpIncome/TA) +Leverage + log(TA) + 
#               factor(City) +  factor(Year)+ factor(Indcd)- 1,
#             data = div, index = c("Accper","Stkcd"), model = "within")
# summary(pf1)

# Matched pair difference-in-difference
# Equal interval for quantiles
fs <- fs[, c("quanTA","quanOI","quanLev", "quanCash_p") := 
           list(findInterval(TA, quantile(TA,na.rm = TRUE, probs = c(0,1/3,2/3))), 
                findInterval(OpIncome, quantile(OpIncome,na.rm = TRUE, probs = c(0,1/3,2/3))), 
                findInterval(Leverage, quantile(Leverage,na.rm = TRUE, probs = c(0,1/3,2/3))), 
                findInterval(Cash_p, quantile(Cash_p,na.rm = TRUE, probs = c(0,1/3,2/3))) 
           ),   by = "Accper"]

Neighbor <- fs[fs$Neighbor==1 & !fs$Stkcd %in% fs$Stkcd[fs$Struck==1],] # Treatment post-period

# Match by TA, leverage, cash holding quantile, and general industrial code
flag <- 0
did500 <- data.frame()
while(flag<=500){
  matchPair <- data.frame()
  for (stock in 1:nrow(Neighbor)) {
    matchList <- which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1 | fs$Neighbor==1] &
                         fs$Accper == Neighbor$Accper[stock] &
                         fs$Indcd == Neighbor$Indcd[stock] &
                         fs$quanTA == Neighbor$quanTA[stock] &
                         fs$quanLev ==Neighbor$quanLev[stock])
    if(length(matchList)==0) {
      matchPair <- rbind(matchPair, fs[NA,], fill = TRUE,
                         make.row.names = FALSE)
    } else {
      matchPair <- rbind(matchPair, fs[sample(matchList, size = 1),],
                         make.row.names = FALSE)
      # print(stock)
    }
  }
  length(which(!is.na(matchPair$Stkcd))) # 937 out of 966 matched
  
  Nb <- which(div$Stkcd %in% Neighbor$Stkcd & !div$Year %in% earthquake$Year)
  Fb <- which(div$Stkcd %in% matchPair$Stkcd & (!div$Season %in% earthquake$Season)) # Controlled group pre-period
  
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
  
  did500 <- rbind(did500, list("dNQ"=diffdiv1,"tNQ"=tdiv1,"dN"=diffdiv2,"tN"=tdiv2,
                               "dFQ"=diffdiv3,"tFQ"=tdiv3,"dNN"=diffdiv4,"tNN"=tdiv4,
                               "DD"=diffdiv5,"tDD"=tdiv5))
  flag <- flag + 1
}

write.table(did500, "did.txt")

# Narrower interval for quantiles
fs <- fs[, c("quanTA","quanOI","quanLev", "quanCash_p_d") := 
           list(findInterval(TA, quantile(TA,na.rm = TRUE, probs = c(0,0.1, 0.3, 0.5,0.7, 0.9))),
                findInterval(OpIncome, quantile(OpIncome,na.rm = TRUE, probs = c(0,0.1, 0.3, 0.5,0.7, 0.9))),
                findInterval(Leverage, quantile(Leverage,na.rm = TRUE, probs = c(0,0.1, 0.3, 0.5,0.7, 0.9)))
           ),
         by = "Accper"]

Neighbor <- fs[fs$Neighbor==1 & !fs$Stkcd %in% fs$Stkcd[fs$Struck==1],] # Treatment post-period

# Match by TA, leverage, cash holding quantile, and general industrial code
flag <- 0
did500 <- data.frame()
while(flag<=500){
  matchPair <- data.frame()
  for (stock in 1:nrow(Neighbor)) {
    matchList <- which(!fs$Stkcd %in% fs$Stkcd[fs$Struck==1 | fs$Neighbor==1] &
                         fs$Accper == Neighbor$Accper[stock] &
                         fs$Indcd == Neighbor$Indcd[stock] &
                         fs$quanTA == Neighbor$quanTA[stock] &
                         fs$quanLev ==Neighbor$quanLev[stock])
    if(length(matchList)==0) {
      matchPair <- rbind(matchPair, fs[NA,], fill = TRUE,
                         make.row.names = FALSE)
    } else {
      matchPair <- rbind(matchPair, fs[sample(matchList, size = 1),],
                         make.row.names = FALSE)
      # print(stock)
    }
  }
  length(which(!is.na(matchPair$Stkcd))) # 937 out of 966 matched
  
  Nb <- which(div$Stkcd %in% Neighbor$Stkcd & !div$Year %in% earthquake$Year)
  Fb <- which(div$Stkcd %in% matchPair$Stkcd & (!div$Season %in% earthquake$Season)) # Controlled group pre-period
  
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
  
  did500 <- rbind(did500, list("dNQ"=diffdiv1,"tNQ"=tdiv1,"dN"=diffdiv2,"tN"=tdiv2,
                               "dFQ"=diffdiv3,"tFQ"=tdiv3,"dNN"=diffdiv4,"tNN"=tdiv4,
                               "DD"=diffdiv5,"tDD"=tdiv5))
  flag <- flag + 1
}

write.table(did500, "did2.txt")
