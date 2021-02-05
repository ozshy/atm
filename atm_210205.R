# atm_210205.R replacing with new id
# atm_200304.R 3rd revision for JEB atm_xx.tex => Fixing histograms because $40 and $100 were missing Line 127
# atm_191223.R revision submitted to JEB atm_32.tex 
# atm_191214.R start revision for JEB. Adding merch=7
# atm_190909.R Line 507, improving tree plot with partykit
# atm_190908.R trying Tree package replacing rpart, (Not used, pushed to the bottom)
# atm_190904.R Adding new Section 5 looking at subgroups of respondents who used and did not use ATM during their diary period. Line 579
# atm_190827.R at a request from Claire, table of num trans by 6 merch type (not recorded on this file because it is just a 1-line code)
# atm_190817.R corresponds to atm_21.tex (revised SSRN + ATL WP)
# atm_190809.R # Revised 190808 with 2017-18 combined data + consolidating all R files into this one
# Packages used for this coding: 
library(dplyr)
#library(tidyr)# function "spread()"
library(formattable)# formats tables to allow trailing zeros (w/ numeric format)
library(plotrix)# weighted histograms
#library(lubridate) # extracts day of the week
library(ggplot2); theme_set(theme_bw())
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
library("xtable") #exporting to LaTeX
library(mfx) # logit regressions with marginal effects
#
setwd("~/Papers/Papers_accepted/atm/atm_coding") # Set working directory on your computer
# Your working directory would be different from the above!
#
dir()
#### Using the larger dataset 
trans1=readRDS("atm_merged_2017_2018_210205.rds")# Read cw data file (all transactions)
names(trans1)
nrow(trans1)
trans1_17 = subset(trans1, year == 2017)
trans1_18 = subset(trans1, year == 2018)
(trans_respondents_17 = nrow(trans1_17[unique(trans1_17$id) , ]))
(trans_respondents_18 = nrow(trans1_18[unique(trans1_18$id) , ]))

# Creating subsample restricted to cash withdrawals (all, not only ATM)
cw2 = subset(trans1, !is.na(cw_location)) # cash withdrawals only
nrow(cw2) # Num of cash withdrawals (all sources)
summary(cw2$amnt) # all sources
#
cw3 = subset(cw2, cw_location==1) # ATM cash withdrawals only (restricted dataset)
nrow(cw3)
cw3_17 = subset(cw3, year==2017)# 2017 ATM withdrawals
cw3_18 = subset(cw3, year==2018)# 2018 ATM withdrawals
nrow(cw3_17)
nrow(cw3_18)

### Section 3: Data, variables
## Restricting transactions to in-person only
trans2 = trans1
names(trans2)
dim(trans2)
table(trans2$in_person)
sum(is.na(trans2$in_person))# how many NAs under in person
sum(!is.na(trans2$in_person))# Not NAs in person
trans3 = subset(trans2,  in_person == 1)# restriction to in-person payments only
dim(trans3)# num trans
length(unique(trans3$id))# num of respondents (in-person only)
trans3 = subset(trans3,  type == "expenditure")# restriction expenditures only
dim(trans3)# num trans
length(unique(trans3$id))# num of respondents (in-person only)
# restricting to to expenditures only

## restricting to merchant type: 1:7 only (191217 JEB revision increasing from 6 merchant types)
trans4 = trans3
table(trans4$merch)
trans5 = subset(trans4, merch %in% 1:7)
# # 1 - Grocery stores, convenience stores without gas stations, pharmacies
# # 2 - Gas stations
# # 3 - Sit-down restaurants and bars
# # 4 - Fast food restaurants, coffee shops, cafeterias, food trucks
# # 5 - General merchandise stores, department stores, other stores, online shopping
#6 - General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.
#7 - Arts, entertainment, recreation
dim(trans5)# num transactions in merchants 1:6
length(unique(trans5$id))# num respondents
#
## Examining and restricting to dominant payment instruments (PI)
# 0 - Multiple payment methods
# 1 - Cash
# 2 - Check
# 3 - Credit card
# 4 - Debit card
# 5 - Prepaid/gift/EBT card
table(trans5$pi)# distribution of transactions by PI
percent(prop.table(table(trans5$pi)))
dim(trans5)
# Below, removing PI used less than 1%
trans6 = subset(trans5, pi %in% 1:5)
table(trans6$pi)
str(trans6$pi)
trans6$pi = factor(trans6$pi, levels = c("1", "2", "3", "4", "5"))
table(trans6$pi)
dim(trans6)
length(unique(trans6$id)) # num of respondents
nrow(trans6) - nrow(trans5)# num payments lost by restricting PI
length(unique(trans6$id)) - length(unique(trans5$id)) # num respondents lost by restricting PI
sum(table(trans6$pi))# Number of trans 

### Section 4: Stats on ATM withdrawals
# some statistics on ATM withdrawals by year
(cw_number_17 = nrow(cw3_17)) # Number of CW over 3 diary days
(cw_respondents_17 = nrow(cw3_17[unique(cw3_17$id) , ])) # Number of respondents who withdrew cash in 2017
(cw_number_per_respondent_17 = (cw_number_17/trans_respondents_17)) # ATM CW per diary resondent (2017, 3 days)
(cw_number_per_respondent_per_year_17 = 365*cw_number_per_respondent_17/3)
(cw_number_per_respondent_per_week_17 = cw_number_per_respondent_per_year_17/52)
# Stats about respondent who actually withdrew cash during the 3 day survey period
# Below, number of respondents who withdrew twice in 3 days
length(table(cw3_17$id)[table(cw3_17$id) == 1]) 
length(table(cw3_17$id)[table(cw3_17$id) == 2]) 
length(table(cw3_17$id)[table(cw3_17$id) == 3]) # 3 ATM withdrawals
length(table(cw3_17$id)[table(cw3_17$id) > 3]) # more than 3 ATM withdrawals
#
# the above for 2018
(cw_number_18 = nrow(cw3_18)) # Number of CW over 3 diary days
(cw_respondents_18 = nrow(cw3_18[unique(cw3_18$id) , ])) # Number of respondents who withdrew cash in 2018
(cw_number_per_respondent_18 = (cw_number_18/trans_respondents_18)) # ATM CW per diary resondent (2017, 3 days)
(cw_number_per_respondent_per_year_18 = 365*cw_number_per_respondent_18/3)
(cw_number_per_respondent_per_week_18 = cw_number_per_respondent_per_year_18/52)
# Stats about respondent who actually withdrew cash during the 3 day survey period
# Below, number of respondents who withdrew twice in 3 days
length(table(cw3_18$id)[table(cw3_18$id) == 1]) 
length(table(cw3_18$id)[table(cw3_18$id) == 2]) 
length(table(cw3_18$id)[table(cw3_18$id) == 3]) # 3 ATM withdrawals
length(table(cw3_18$id)[table(cw3_18$id) > 3]) # more than 3 ATM withdrawals

## Histograms: (Sec.4, Figure 1 in paper)
(mult20 = seq(20, 740, 20))# vector of $20 multiples
# first, data on ALL ATM withdrwals (not only <= $200)
cw3_200 = subset(cw3, amnt <= 200)
nrow(cw3_200)# num ATM withdrwals all amounts
#
table(cw3_200$amnt)
par(mar=c(4.1,4.1,1,3))
hist(cw3_200$amnt, breaks = 200, ylim = c(0,60), main = "", col = "black", xlab="ATM cash withdrawal amount ($0 to $200)", ylab = "Number of ATM withdrawals (unweighted)", xaxt='n', yaxt='n')
axis(side=1, at=seq(0,200, 10), labels=seq(0,200,10))
axis(side=2, las=2, at=seq(0,60, 10), labels=seq(0, 60, 10))
labs = seq(0, 200, by = 10)
axis(side=4, las=2, at = labs, labels = paste0(labs/2, "%"), cex.axis = 1.0)
#axis(side=4, las=2, at=seq(0,200, 10), labels=seq(0,200/2,10/2))
#grid(0,6.1)
abline(h=10, lty = 5)
abline(h=20, lty = 5)
abline(h=30, lty = 5)
abline(h=40, lty = 5)
abline(h=50, lty = 5)
abline(h=60, lty = 5)


# removed on 3rd revision 200304
# cond_200 = cw3_200$amnt %in% mult20 # is divisible by 20? T/F
# length(cond_200)
# head(cond_200)
# sum(cond_200==T)# num ATM withdrawals in multiples of $20
# sum(cond_200==F)# not multiples of $20
# sum(cond_200==F) + sum(cond_200==T)
# table(cond_200)
# percent(prop.table(table(cond_200)))
# #
# amnt_200_df = data.frame(amnt_200 = cw3_200$amnt)
# dim(amnt_200_df)
# hist(amnt_200_df$amnt_200, breaks = 200)

# The ggplot below misses $20 and $100 (hence, not in use)
#(tempgg = ggplot(cw5, aes(amnt)) + geom_histogram(bins = 100, fill = "darkblue") + xlim(0,101))
# (tempgg = ggplot(amnt_200_df, aes(x=amnt_200)) 
#   + geom_histogram(data = subset(amnt_200_df, cond_200==F), binwidth = 1, fill = "darkblue") + geom_histogram(data = subset(amnt_200_df, cond_200==T), binwidth = 1, fill = "red"))
# (tempgg2 = tempgg + scale_x_continuous(limits = c(0,201), breaks = seq(0, 200, by = 10)) + scale_y_continuous(limits = c(0,35), breaks = seq(0, 35, by = 5)))
# (tempgg3 = tempgg2+ ylab("Number of withdrawals (unweighted)") + xlab("ATM cash withdrawal amount ($0 to $200)"))

# below, hist of ATM withdrwals up to $500
cw3_500 = subset(cw3, amnt <= 500)
dim(cw3_500)
nrow(cw3_500)# num ATM withdrwals all amounts
#
table(cw3_500$amnt)
hist(cw3_500$amnt, breaks = 500, ylim = c(0,60), main = "", col = "black", xlab="ATM cash withdrawal amount ($0 to $500)", ylab = "Number of ATM withdrawals (unweighted)", xaxt='n',yaxt='n')
axis(side=1, at=seq(0,500, 20), labels=seq(0,500,20), cex.axis=0.8)
axis(side=2, las=2, at=seq(0,60, 10), labels=seq(0, 60, 10))
labs = seq(0, 500, by = 20)
axis(side=4, las=2, at = labs, labels = paste0(labs/5, "%"), cex.axis = 1.0)
#grid(0,6.1)
abline(h=10, lty = 5)
abline(h=20, lty = 5)
abline(h=30, lty = 5)
abline(h=40, lty = 5)
abline(h=50, lty = 5)
abline(h=60, lty = 5)


# cond_500 = cw3_500$amnt %in% mult20 # is divisible by 20? T/F
# length(cond_500)
# head(cond_500)
# sum(cond_500==T)# num ATM withdrawals in multiples of $20
# sum(cond_500==F)# not multiples of $20
# sum(cond_500==F) + sum(cond_500==T)
# table(cond_500)
# percent(prop.table(table(cond_500)))
# #
# amnt_500_df = data.frame(amnt_500 = cw3_500$amnt)
# #(tempgg = ggplot(cw5, aes(amnt)) + geom_histogram(bins = 100, fill = "darkblue") + xlim(0,101))
# (tempgg = ggplot(amnt_500_df, aes(x=amnt_500)) 
#   + geom_histogram(data = subset(amnt_500_df, cond_500==F), binwidth = 1, fill = "darkblue") + geom_histogram(data = subset(amnt_500_df, cond_500==T), binwidth = 1, fill = "red"))
# (tempgg2 = tempgg + scale_x_continuous(limits = c(0,501), breaks = seq(0, 500, by = 20)) + scale_y_continuous(limits = c(0,35), breaks = seq(0, 35, by = 5)))
# (tempgg3 = tempgg2+ ylab("Number of withdrawals (unweighted)") + xlab("Number of withdrawals (unweighted)") + xlab("ATM cash withdrawal amount ($0 to $500)"))

# for text in Section 4
summary(cw3$amnt)
nrow(subset(cw3, amnt > 500)) # num ATM cw exceeding $500
subset(cw3, amnt > 500)$amnt
# fraction of $20 multiples cw
percent(nrow(cw3[cw3$amnt %in% mult20, ])/nrow(cw3)) # perc $20 multiplies
percent(1 - nrow(cw3[cw3$amnt %in% mult20, ])/nrow(cw3)) # non-multiples

## Discussion at the end of Section 3 on other (non-ATM) cash withdrawals. Moved to new Section 5 !
table(cw2$cw_location) # shares by volume
percent(prop.table(table(cw2$cw_location)))
# shares by value below:
(cw3_1_amnt = sum(subset(cw2, cw_location==1)$amnt)) # sum of dollar amount ATM cw
(cw3_2_amnt = sum(subset(cw2, cw_location==2)$amnt)) # sum of dollar amount cashback
(cw3_3_amnt = sum(subset(cw2, cw_location==3)$amnt)) # sum of dollar bank teller
(cw3_4_amnt = sum(subset(cw2, cw_location==4)$amnt)) # sum of dollar amount family friends
(cw3_6_amnt = sum(subset(cw2, cw_location==6)$amnt)) # sum of dollar amount employer
(cw3_9_amnt = sum(subset(cw2, cw_location==9)$amnt)) # sum of dollar amount other
#
# shares by volume (weighted)
nrow(cw2)
sum(cw2$weight_1)
cw2$weight_cw = nrow(cw2)*cw2$weight_1/sum(cw2$weight_1) # creating new weights for cash withdrawals subset
sum(cw2$weight_cw)
#
(cw2_1_vol_w = sum(subset(cw2, cw_location==1)$weight_cw)) #ATM weighted volume
(cw2_2_vol_w = sum(subset(cw2, cw_location==2)$weight_cw)) #Cashback weighted volume
(cw2_3_vol_w = sum(subset(cw2, cw_location==3)$weight_cw)) #bank teller weighted volume
(cw2_4_vol_w = sum(subset(cw2, cw_location==4)$weight_cw)) #family friend weighted volume
(cw2_5_vol_w = sum(subset(cw2, cw_location==5)$weight_cw)) #cashing store weighted volume
(cw2_6_vol_w = sum(subset(cw2, cw_location==6)$weight_cw)) #employer weighted volume
(cw2_7_vol_w = sum(subset(cw2, cw_location==7)$weight_cw)) #returned goods weighted volume
(cw2_8_vol_w = sum(subset(cw2, cw_location==8)$weight_cw)) #Payday lender weighted volume
(cw2_9_vol_w = sum(subset(cw2, cw_location==9)$weight_cw)) #other cash weighted volume
(cw2_vol_w.vec = c(cw2_1_vol_w, cw2_2_vol_w, cw2_3_vol_w, cw2_4_vol_w, cw2_5_vol_w, cw2_6_vol_w, cw2_7_vol_w, cw2_8_vol_w, cw2_9_vol_w)) # vector of weighted vol
(cw2_vol_w_frac.vec = c(cw2_1_vol_w, cw2_2_vol_w, cw2_3_vol_w, cw2_4_vol_w, cw2_5_vol_w, cw2_6_vol_w, cw2_7_vol_w, cw2_8_vol_w, cw2_9_vol_w)/nrow(cw2)) # vector of weighted vol
sum(cw2_vol_w_frac.vec)# verify sum to 1
percent(cw2_vol_w_frac.vec)
#
# the above in value terms
(cw2_1_amnt_w = sum(subset(cw2, cw_location==1)$amnt*subset(cw2, cw_location==1)$weight_cw)) # sum of dollar amount ATM cw
(cw2_2_amnt_w = sum(subset(cw2, cw_location==2)$amnt*subset(cw2, cw_location==2)$weight_cw)) # sum of dollar amount cashback
(cw2_3_amnt_w = sum(subset(cw2, cw_location==3)$amnt*subset(cw2, cw_location==3)$weight_cw)) # sum of dollar bank teller
(cw2_4_amnt_w = sum(subset(cw2, cw_location==4)$amnt*subset(cw2, cw_location==4)$weight_cw)) # sum of dollar amount family friends
(cw2_5_amnt_w = sum(subset(cw2, cw_location==5)$amnt*subset(cw2, cw_location==5)$weight_cw)) # cashing store dollar amount family friends
(cw2_6_amnt_w = sum(subset(cw2, cw_location==6)$amnt*subset(cw2, cw_location==6)$weight_cw)) # sum of dollar amount employer
(cw2_7_amnt_w = sum(subset(cw2, cw_location==7)$amnt*subset(cw2, cw_location==7)$weight_cw)) # sum of dollar amount cash refund
(cw2_8_amnt_w = sum(subset(cw2, cw_location==8)$amnt*subset(cw2, cw_location==8)$weight_cw)) # sum of dollar amount payday lender
(cw2_9_amnt_w = sum(subset(cw2, cw_location==9)$amnt*subset(cw2, cw_location==9)$weight_cw)) # sum of dollar amount other
(cw2_amnt_w.vec = c(cw2_1_amnt_w, cw2_2_amnt_w, cw2_3_amnt_w, cw2_4_amnt_w, cw2_5_amnt_w, cw2_6_amnt_w, cw2_7_amnt_w, cw2_8_amnt_w, cw2_9_amnt_w))
(cw2_amnt_frac_w.vec = cw2_amnt_w.vec/sum(cw2$amnt))
percent(cw2_amnt_frac_w.vec)

# ## below share of cw with $20 multiples (unweighted) (not used! because it does not provide info on the exact denominations, which could also be dominated by $20 bills)
# cw3_1 = subset(cw2, cw_location==1) # ATM cash received
# cond_1 = cw3_1$amnt %in% mult20 # is divisible by 20? T/F
# percent(prop.table(table(cond_1))) # percentage obtained in $20 multiples
# #
# cw3_2 = subset(cw2, cw_location==2) # cash back
# cond_2 = cw3_2$amnt %in% mult20 # is divisible by 20? T/F
# percent(prop.table(table(cond_2))) # percentage obtained in $20 multiples
# #
# cw3_3 = subset(cw2, cw_location==3) # bank teller cash received
# cond_3 = cw3_3$amnt %in% mult20 # is divisible by 20? T/F
# percent(prop.table(table(cond_3))) # percentage obtained in $20 multiples
# #
# cw3_4 = subset(cw2, cw_location==4) # family friends cash received
# cond_4 = cw3_4$amnt %in% mult20 # is divisible by 20? T/F
# percent(prop.table(table(cond_4))) # percentage obtained in $20 multiples
# #
# cw3_6 = subset(cw2, cw_location==6) # employer cash received
# cond_6 = cw3_6$amnt %in% mult20 # is divisible by 20? T/F
# percent(prop.table(table(cond_6))) # percentage obtained in $20 multiples
# #
# cw3_9 = subset(cw2, cw_location==9) # other  cash received
# cond_9 = cw3_9$amnt %in% mult20 # is divisible by 20? T/F
# percent(prop.table(table(cond_9))) # percentage obtained in $20 multiples

### Section 5: Empirical results
## Figure 3
# Draw Methods on Amount with different colors: $0 to $45
par(mar = c(4,1,1,2))
plot(pi~amnt, data = subset(trans6, trans6$amnt<=45), col=rainbow(5), ylab = "Method of payment (Method)", xlab="Payment dollar value (Amount) $0 to $45", axes=T, breaks = seq(0, 45, 1)) # generates $5 bin sizes 
#Axis(side=1, labels = T)
#Axis(side=2, labels = T)
abline(h = 0.1, lty = 3)
abline(h = 0.2, lty = 3)
abline(h = 0.3, lty = 3)
abline(h = 0.4, lty = 3)
abline(h = 0.5, lty = 3)
abline(h = 0.6, lty = 3)
abline(h = 0.7, lty = 3)
abline(h = 0.8, lty = 3)
abline(h = 0.9, lty = 3)
legend("bottomleft", c("Cash", "check", "Credit card", "Debit card", "Prepaid card"), fill=rainbow(5))
#
# How many trans and resp in the $0-$45 range?
nrow(subset(trans6, amnt <= 45 ))# num trans
length(unique(subset(trans6, amnt <= 45)$id))# num of resp


### Figure 4 (3 panels) in the paper:
# Preparing a table and a graph of %-change in cash-use by dollar amount
atm_50 = subset(trans6, trans6$amnt <= 45) # restrict trans to less than $45
colnames(atm_50)[colnames(atm_50)=="amnt"]="Amount"
summary(atm_50$Amount)
colnames(atm_50)[colnames(atm_50)=="pi"]="Method"
table(atm_50$Method)
str(atm_50$Method)
levels(atm_50$Method) = c("cash", "check", "credit", "debit", "prepaid")
table(atm_50$Method)
#
# Below, cut Amount into 45 intervals
value = atm_50$Amount # copy amount to value
length(value)
# Below, I cut $0 to $45 amount interval to 46 intervals by rounding UP the Amount to the nearest integer ($19.23 becomes $20)
val_interval = cut(value, breaks = seq(0, 46, 1), labels = 1:46, right = T)
str(val_interval)
atm_50$Amount_interval = val_interval # add column with 45 Amount intervals
# Below, I verify rounding up ($19.23 becomes $20)
head(atm_50[, c("Method", "Amount", "Amount_interval")], 50)
tail(atm_50[, c("Method", "Amount", "Amount_interval")], 50)
table(atm_50$Amount_interval)
# Sorting according to Amount_interval
atm_50s = atm_50[order(atm_50$Amount_interval), ]
head(atm_50s[, c("Method", "Amount", "Amount_interval")], 50)
tail(atm_50s[, c("Method", "Amount", "Amount_interval")], 20)
#
# Grouping ratio of cash-use by Amount_interval
table(atm_50s$Method)
perc_mut = atm_50s %>% group_by(Amount_interval) %>% mutate(percent_cash = sum(Method=="cash")/(sum(Method=="cash")+sum(Method=="check")+sum(Method=="credit")+sum(Method=="debit") +sum(Method=="prepaid")))
# mutate above created a data frame. The newly-added column "percent_cash" is the share of cash payment in an entire Amount interval. Below, I  examine the newly-added column:
head(perc_mut$percent_cash, 60)
tail(perc_mut$percent_cash, 60)
dim(atm_50s)
length(perc_mut$percent_cash) #verify same number of lines
# Below, I add the new column to the data frame in use:
atm_50s$percent_cash = perc_mut$percent_cash # add the new column
names(atm_50s)
head(atm_50s[, c("Method", "Amount", "Amount_interval", "percent_cash")], 50)
tail(atm_50s[, c("Method", "Amount", "Amount_interval", "percent_cash")], 50)
#
# Some numbers quoted in the paper: Cash-use at $5, $10, $15, $20, etc
head(atm_50s[atm_50s$Amount_interval==1, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==5, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==10, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==15, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==20, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==25, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==30, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==40, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==45, c("Method", "Amount", "Amount_interval", "percent_cash")], 1)
#
# creating and adding a column per_cash_change (from the lower Amount_interval)
# Below, share of cash for the "lagged" observation
lag_perc_cash = lag(perc_mut$percent_cash)
head(lag_perc_cash, 60)
head(perc_mut$percent_cash, 60)
# Below, adding a coloum of lagged percentage
atm_50s$lag_perc_cash = lag_perc_cash
# Below, difference in share of cash between current and lagged observation
diff_perc_cash = perc_mut$percent_cash - lag_perc_cash
head(diff_perc_cash, 100)
tail(diff_perc_cash)
atm_50s$diff_perc_cash = diff_perc_cash #adding the difference in cash use as a new column
sample_n(atm_50s[, c("percent_cash", "diff_perc_cash")], 10)
summary(atm_50s$diff_perc_cash)
#
# Some numbers quoted in the paper: Difference in cash-use at $4-6,  
head(atm_50s[atm_50s$Amount_interval==5, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==6, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
# $9-11,
head(atm_50s[atm_50s$Amount_interval==10, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==11, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
# $14-16, 
head(atm_50s[atm_50s$Amount_interval==15, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==16, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
#$19-21, 
head(atm_50s[atm_50s$Amount_interval==20, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==21, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
#$24-26, 
head(atm_50s[atm_50s$Amount_interval==25, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==26, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
#29-31, 
head(atm_50s[atm_50s$Amount_interval==30, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==31, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)

#39-41
head(atm_50s[atm_50s$Amount_interval==40, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
head(atm_50s[atm_50s$Amount_interval==41, c("Method", "Amount", "Amount_interval", "percent_cash", "lag_perc_cash", "diff_perc_cash")], 1)
#
# Figure 4: Draw 3 graphs: percent_cash and change in percent cash
# For the 2nd plat we need to remove zero y-values (too many zeros don't look good)
dim(atm_50s)
atm_50s0 = atm_50s
atm_50s0$diff_perc_cash[diff_perc_cash == 0] = NA 
dim(atm_50s0)
#
### Start de-trending (regression first)
names(atm_50s)
str(atm_50s)
# Linear regression below
atm_lm1 = lm(percent_cash ~ as.numeric(Amount_interval), data = atm_50s)
summary(atm_lm1)
# quadratic regression below
atm_lm2 = lm(percent_cash ~ poly(as.numeric(Amount_interval),2), data = atm_50s)
summary(atm_lm2)
anova(atm_lm1, atm_lm2) # compare 2 models
#
summary(atm_50s$Amount_interval)
(fitted_unique = unique(atm_lm2$fitted.values))
#
# Figure 4 top plot: percent_cash plot
par(mar = c(4.2,4,1,0))
#par(mfrow=c(3,1)) draw 3 graphs separately
par(mfrow=c(1,1))
plot(atm_50s$percent_cash ~ atm_50s$Amount_interval, xlab="Payment amount intervals (USD)", ylab="Share of cash payments", axes = F)
axis(1, at = c(0, 1, seq(5, 45, 5)))
axis(2, at = seq(0, 1, 0.2), las = 2)
abline(v=5,  lty = 3, col="red")
abline(v=10, lty = 3, col="red")
abline(v=15, lty = 3, col="red")
abline(v=20, lty = 3, col="red")
abline(v=25, lty = 3, col="red")
abline(v=30, lty = 3, col="red")
abline(v=35, lty = 3, col="red")
abline(v=40, lty = 3, col="red")
abline(v=45, lty = 3, col="red")
abline(v=1, lty = 3, col="red")
# abline(h=0, lty = 1)
abline(h = 0.1, lty = 3)
abline(h = 0.2, lty = 3)
abline(h = 0.3, lty = 3)
abline(h = 0.4, lty = 3)
abline(h = 0.5, lty = 3)
abline(h = 0.6, lty = 3)
abline(h = 0.7, lty = 3)
abline(h = 0.8, lty = 3)
abline(h = 0.9, lty = 3)
lines(fitted_unique[4:48], lwd = 2, col = "blue")
#
# Second (middle) plot: difference in percentage cash
#
plot(atm_50s0$diff_perc_cash ~ atm_50s$Amount_interval, pch = 16, xlab="Payment amount intervals (USD)", ylab="Difference (jump) in share of cash payments", axes = F )
axis(1, at = c(0, 1, seq(5, 45, 5)))
axis(2, at = seq(-0.3, 0.3, 0.1), labels = round(seq(-0.3, 0.3, 0.1), digits = 2), las = 2)
abline(v=5,  lty = 3, col="red")
abline(v=10, lty = 3, col="red")
abline(v=15, lty = 3, col="red")
abline(v=20, lty = 3, col="red")
abline(v=25, lty = 3, col="red")
abline(v=30, lty = 3, col="red")
abline(v=35, lty = 3, col="red")
abline(v=40, lty = 3, col="red")
abline(v=45, lty = 3, col="red")
abline(v=1, lty = 3, col="red")
#abline(v=51, lty = 3, col="red")
abline(h=-0.3, lty = 3)
abline(h=-0.25, lty = 3)
abline(h=-0.2, lty = 3)
abline(h=-0.15, lty = 3)
abline(h=-0.1, lty = 3)
abline(h=-0.05, lty = 3)
abline(h = 0, lty = 1)
abline(h= 0.05, lty = 3)
abline(h = 0.1, lty = 3)
abline(h= 0.15, lty = 3)
abline(h = 0.2, lty = 3)
abline(h = 0.25, lty = 3)
abline(h = 0.3, lty = 3)
#
# Third (bottom) graph: De-trended percentage cash
atm_50s$detrend = atm_50s$percent_cash - atm_lm2$fitted.values
#
plot(atm_50s$detrend ~ atm_50s$Amount_interval, xlab="Payment amount intervals (USD)", ylab="Share of cash payments (de-trended)", axes = F )
axis(1, at = c(0, 1, seq(5, 45, 5)))
axis(2, at = seq(-0.2, 0.2, 0.1), las = 2)
abline(v=5,  lty = 3, col="red")
abline(v=10, lty = 3, col="red")
abline(v=15, lty = 3, col="red")
abline(v=20, lty = 3, col="red")
abline(v=25, lty = 3, col="red")
abline(v=30, lty = 3, col="red")
abline(v=35, lty = 3, col="red")
abline(v=40, lty = 3, col="red")
abline(v=45, lty = 3, col="red")
abline(v=1, lty = 3, col="red")
abline(h = -0.2, lty = 3)
abline(h = -0.15, lty = 3)
abline(h = -0.1, lty = 3)
abline(h = -0.05, lty = 3)
abline(h = 0, lty = 1)
abline(h = 0.05, lty = 3)
abline(h = 0.1, lty = 3)
abline(h = 0.15, lty = 3)
abline(h = 0.2, lty = 3)
#abline(h = 0.3, lty = 3)

nrow(atm_50s) # num trans in Figure 4
length(unique(atm_50s$id)) # Num resp in Figure 4
# end of Figure 3

### Subsection 5.1: Classification tree
# Renaming the variables for the tree
trans7 = trans6
names(trans7)
table(trans7$pi)
colnames(trans7)[colnames(trans7)=="pi"]="Method"
table(trans7$Method)
str(trans7$Method)
levels(trans7$Method) = c("cash", "check", "credit", "debit", "prepaid")
table(trans7$Method)
#
colnames(trans7)[colnames(trans7)=="amnt"]="Amount"
summary(trans7$Amount)
dim(trans7)
trans8 = trans7[!is.na(trans7$Amount) ,] # removing Amount=NAs
dim(trans8)
#
colnames(trans8)[colnames(trans8)=="age"]="Age"
table(trans8$Age)
#
table(trans8$hh_size)
colnames(trans8)[colnames(trans8)=="hh_size"]="HH_size"
table(trans8$HH_size)
#
table(trans8$employed)
colnames(trans8)[colnames(trans8)=="employed"]="Work"
table(trans8$Work)
#
table(trans8$married)
colnames(trans8)[colnames(trans8)=="married"]="Marital"
#
summary(trans8$income)
colnames(trans8)[colnames(trans8)=="income"]="HH_income"
summary(trans8$HH_income)
#
table(trans8$gender)
colnames(trans8)[colnames(trans8)=="gender"]="Gender"
table(trans8$Gender)
#
table(trans8$educ)
colnames(trans8)[colnames(trans8)=="educ"]="Educ"
table(trans8$Educ)

# defining regression model
method_on_demog = Method~Amount+Age+HH_size+Work+Marital+HH_income+Gender#+Education # Educ removed
#method_on_demog = Method~Amount+as.factor(merch)+Age+HH_size+Work+Marital+HH_income+Gender#+Education # Educ removed ADDED merch=1:6 (did not make any difference)
#
set.seed(1955)
method_demog_tree = rpart(method_on_demog, data = trans8, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
method_demog_tree$cptable # Select the plot that suits the paper size based on the no of splits
#
plotcp(method_demog_tree)
# The following tree was chosen for the paper (6 splits), otherwise too long to present in the paper
# Note, you may have to run it more than once to get a nice tree (first time gets distorted for some reason)
method_demog_prune_non_opt = prune.rpart(method_demog_tree, cp= 0.005) # 6 splits 
prp(method_demog_prune_non_opt, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0, Margin = 0.0, digits = -2)#faclet=0 avoids abvreviations, tweak for char size
prp(method_demog_prune_non_opt, type = 3, extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0, Margin = 0.0, digits = -2)#faclet=0 avoids abvreviations, tweak for char size
# 
## modifying the plot with partykit
method_demog_party = rpart(method_on_demog, data = trans8, method = "class", control = rpart.control(cp = 0.003))# use cp from above
# The tree below is Figure 5 in the paper
plot(as.party(method_demog_party), gp = gpar(fontsize = 7), type="simple", ip_args = list(id = TRUE, fill = "white"), tp_args = list(id = TRUE, fill = "white"))

#
nrow(trans8)# num of observations used in the tree
length(unique(trans8$id)) # num of respondents
# finding the number of trans spikes at $10 and $20
table(subset(trans8, Amount >= 9.95 & Amount <= 10.05)$Amount)
table(subset(trans8, Amount == 10)$Method)
table(subset(trans8, Amount >= 19.95 & Amount <= 20.05)$Amount)
table(subset(trans8, Amount == 20)$Method)


### Subsection 5.2: Regressions
trans9 = trans8
nrow(trans9)# num of observations used in the regression
length(unique(trans9$id)) # num of respondents

# Adding binary column: Cash == 1 and non-Cash =0
trans9$Cash = 0
trans9[trans9$Method == "cash", "Cash"] = 1
table(trans9$Cash)
#
names(trans9)
# define logit model with 25 dummies
cash_logit_model = Cash~I(Amount > 1)+I(Amount > 2)+I(Amount > 3)+I(Amount > 4)+I(Amount > 5)+I(Amount > 6)+I(Amount > 7)+I(Amount > 8)+I(Amount > 9)+I(Amount > 10)+I(Amount > 11)+I(Amount > 12)+I(Amount > 13)+I(Amount > 14)+I(Amount > 15)+I(Amount > 16)+I(Amount > 17)+I(Amount > 18)+I(Amount > 19)+I(Amount > 20)+I(Amount > 21)+I(Amount > 22)+I(Amount > 23)+I(Amount > 24)+I(Amount > 25)+Amount+Work+HH_size+HH_income+Age+Gender+Marital+Educ
# running logit regression
#cash_logit = logitmfx(cash_logit_model, data = trans9[trans9$Amount <= 50,], trans9ean = T)
cash_logit = logitmfx(cash_logit_model, data = trans9, atmean = T)
summary(cash_logit) #
round(cash_logit$mfxest, digits = 4)
# Below, fix a problem that ***, **, * do not appear => need to add column)
colnames(cash_logit$mfxest)
dim(cash_logit$mfxest)
typeof(cash_logit$mfxest)
cash_logit_m =  as.data.frame(cash_logit$mfxest)
#
# adding *, **, ***
names(cash_logit_m)
for (i in 1:nrow(cash_logit_m)){cash_logit_m[i,5]=' '}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.1){cash_logit_m[i,5]='.'}
}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.05){cash_logit_m[i,5]='*'}
}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.01){cash_logit_m[i,5]='**'}
}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.001){cash_logit_m[i,5]='***'}
}
names(cash_logit_m)
names(cash_logit_m)[5] = "Sig"
cash_logit_m
# Adding confidence intervals to marginal effects
names(cash_logit_m)
cash_logit_m$lci = cash_logit_m$`dF/dx` - cash_logit_m$`Std. Err.` * 1.96
cash_logit_m$uci = cash_logit_m$`dF/dx` + cash_logit_m$`Std. Err.` * 1.96
cash_logit_m
# Prepare for printing: Delete z and p-value columns
cash_logit_print = subset(cash_logit_m, select = c(-3, -4))
head(cash_logit_print)
# Below, Table 1 (jump.t) in the paper
print(xtable(cash_logit_print, digits = 3), include.rownames = T) # This removes row numbers

## Regression con'd: Marginal effects plot
names(cash_logit_print)
# Below, leave only 3 columns: marg. effects and ci lower and upper
cash_logit_ci = subset(cash_logit_print, select = c(-2, -3))
head(cash_logit_ci, 3)
cash_logit_ci = cash_logit_ci[1:25, ] #leave only 25 lines with dummy marginal effects
cash_logit_ci
summary(cash_logit_ci)
# 
par(mar = c(4,4,4,5)) # increasing right margin
plot(cash_logit_ci$'dF/dx' ~ seq(1:25), type = "l", lwd = 2, axes = F, xlab = "Payment dollar amount (USD)", ylab = "Marginal effect for each $1 amount increment")
lines(cash_logit_ci$'lci', lty = 3)
lines(cash_logit_ci$'uci', lty = 3)
axis(side = 1, at = 1:25)
axis(side = 2, at = c(-0.4, -0.35, -0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4), las =1)
axis(side = 4, at = c(-0.4, -0.35, -0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4), las =1)
abline(h = 0)


### THE END OF atm_191223.R ###

