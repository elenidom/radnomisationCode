################################################################################################################
# search which was the last date of exposure before the index date:
# the variable 'new' defines a new column in the AB dataset that will keep the indexdate from the 'myPERSON' dataset
# The idea is to subtract from this value, the last day that the patient was exposed to AB, in order to define
# the exposure based on the interval that a patient was exposed before the indexdate.

# put the distinct ids in my_ids 
library(dplyr)
lastabexp <- myAB %>% group_by(patid2) %>% summarise(max(datepres))

#change column names
names(lastabexp) <- c("patid2", "lastday")

# do some checks
summary(lastabexp)
sum(lastabexp$lastday < 0)

# move the variable lastday into myPERSON
myPERSON$lastabexp <- lastabexp$lastday

# subtract the index date from the above column to calculate the days from the last exposure before the ID
myPERSON$abdifflastid <- myPERSON$dtindex - myPERSON$lastabexp

# convert days into months
myPERSON$m_exp <- round(myPERSON$abdifflastid/30, 3)

# check
summary(myPERSON$m_exp)
boxplot(myPERSON$m_exp)
sum(myPERSON$m_exp < 0) # 0


# we define 550 cutoffs to decide the intervals of exposure (550 = max of exposure)
exp_cutoffs <- seq(from=0, to=550, by=1)
exp_cutoffs <- as.numeric(exp_cutoffs)
tempvar11 <- cut(myPERSON$m_exp, exp_cutoffs)

# count how many times, thus number of patients, have been exposed in every interval  
n_occur_exp <- data.frame(table(tempvar11))

# define exposure
myPERSON$exposure_stage <- 0
myPERSON$exposure_stage <- ifelse(myPERSON$m_exp <= 3 & myPERSON$m_exp >= 0, 2, myPERSON$exposure_stage)
myPERSON$exposure_stage <- ifelse(myPERSON$m_exp <= 24 & myPERSON$m_exp > 3, 1, myPERSON$exposure_stage)
myPERSON$exposure_stage <- ifelse(myPERSON$m_exp > 24, 0, myPERSON$exposure_stage)
myPERSON$exposure_stage <- as.factor(myPERSON$exposure_stage)

summary(myPERSON$exposure_stage)


# calculate the exposure categories for those who have cancer (64,991 excluded)
table(myPERSON$exposure_stage)
