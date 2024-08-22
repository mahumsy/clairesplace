library(tidyverse)
library(moderndive)
library(infer)
library(dplyr)
library(ggplot2)

claires_patients<- read.csv("/Users/mahumsyed/Downloads/Claires_Patients_FA23.csv")

#########
# All the code in this R script is produced by MAHUM SYED.

###########

# claires_patients_FA23 data:
# - each patient and amount granted, w other info
# ●	ID	Submission ID
# ●	Date	Date the patient applied for the grant
# ●	Amt_granted	Total amount granted for this application
# ●	State	State where the patient locates
# ●	New	TRUE for new patients, otherwise returned patients
# ●	Minor	Yes for minor, No for adult

###########

mean(claires_patients$Amt_granted)

# Convert date column to date format
claires_patients$Date <- as.Date(claires_patients$Date, "%m/%d/%Y")

summary(claires_patients)
#mean amount granted is 1599.533 (about 1600)
# the min (earliest) date is 2/3/2020

sum(claires_patients$Amt_granted == 0)
# there are 18 entries with an amount granted of 0

#filter out the zeroes
nonzero_claires <- claires_patients %>% 
  filter(Amt_granted!=0)

summary(nonzero_claires)
#new (data without zeros) overall mean granted amount is $1697.1

#filter by period - before or after policy
before_policy <- nonzero_claires[nonzero_claires$Date < as.Date("2023-06-01"), ]

after_policy <- nonzero_claires[nonzero_claires$Date >= as.Date("2023-06-01"), ]

summary(before_policy)
nrow(before_policy)
sum(before_policy$Amt_granted == 0)
#before june policy:
# - 262 rows (people who got grants)
# - min date is 2/3/2020
# - max date is 5/26/23
# - 12 people granted 0 (filtered out)
# - mean of amt_granted = 1706.4
# - max granted was 4748.8


summary(after_policy)
nrow(after_policy)
sum(after_policy$Amt_granted == 0)
#after june policy:
# - 33 rows (people who got grants)
# - min date is 6/22/2023
# - max date is 11/14/2023
# - 6 people granted 0 (filtered out)
# - mean of amt_granted = 1623 (slighty less than before)
# - max granted was 5300.0 (higher max than before policy)

###########
# number of grants given each month:
# look at it per month since they give grants for one month of rent or mortgage after the policy.

# Before policy change
monthly_grants_before <- table(format(before_policy$Date, "%Y-%m"))
monthly_grants_before
# this table shows the number of grants given each month, before the policy
mean(monthly_grants_before)
# average monthly grants given before the policy is 6.717949

# After policy change
monthly_grants_after <- table(format(after_policy$Date, "%Y-%m"))
monthly_grants_after
# this table shows the number of grants given each month, after the policy
mean(monthly_grants_after)
# average monthly grants given after the policy is 5.5 

# with an initial look at the data, they are giving out LESS monthly grants after the policy, on average.

#########
# hypothesis testing for amount granted.

# H0: there is no significant difference in the average amount granted before vs after june 1, 2023
#H1: the average amount granted after the policy is less than the average amount granted before the policy
nonzero_claires <- nonzero_claires %>%
  mutate(period = ifelse(Date < as.Date("2023-06-01"), "Before", "After"))

obs_cf <-nonzero_claires %>% 
  specify(Amt_granted~period) %>% 
  calculate(stat="diff in means",
            order = c("Before", "After"))

null_cf <- nonzero_claires %>% 
  specify(Amt_granted~period) %>% 
  hypothesise(null="independence") %>% 
  generate(reps = 1000, type="permute") %>% 
  calculate(stat="diff in means",
            order=c("Before", "After"))

null_cf %>% 
  get_p_value(obs_cf, direction="left")

# the p value : 0.679
# therefore we fail to reject the null hypothesis. there is no significant difference in the dollar amounts granted before vs after the policy.

#######
# hypothesis test with CI (to check same claim as above^)
# H0: there is no significant difference in the average amount granted before vs after june 1, 2023
#H1: the average amount granted after the policy is less than the average amount granted before the policy

# Generate bootstrap distribution
diff_dist <- nonzero_claires %>% 
  specify(Amt_granted ~ period) %>%  
  generate(reps = 1000) %>%
  calculate(stat = "diff in means", order = c("Before", "After"))

# Calculate point estimate
new_diff <- nonzero_claires %>%
  specify(Amt_granted ~ period) %>% 
  calculate(stat = "diff in means", order = c("Before", "After"))

# Standard error CI
diff_seci <- diff_dist %>%
  get_confidence_interval(type = "se", 
                          point_estimate = new_diff)

# lower ci is -276.696 
# upper ci is 442.8632
# 0 is in the confidence interval
#There is a 95% chance the true population mean lies between this range, which does not conclusively tell us that there is high variability between Amt_granted before and after the policy. 
#Specifically, since 0 is in the range, we cannot conclusively say if there was an increase or decrease in the averages.

# Visualize 
diff_dist %>%
  visualize() +
  shade_confidence_interval(endpoints = diff_seci) + 
  geom_vline(xintercept = 0, color="red")


############
#hypothesis test: for per-month grants

# H0: there is no significant difference in the average number of grants per month before vs after June 1, 2023
# H1: the average number of grants per month after the policy is more than the average number of grants per month before the policy.

# Create a separate data frame
# shows year-month (such as 2020-01, 2020-02, and so on), period (before or after), and grants given that month.
grant_data <- data.frame(
  period = rep(c("Before", "After"), c(length(monthly_grants_before), length(monthly_grants_after))),
  grants = c(monthly_grants_before, monthly_grants_after)
)

grant_test <- grant_data %>%
  specify(grants ~ period) %>%
  calculate(stat = "diff in means", order = c("Before", "After"))

null_grant_test <- grant_data %>%
  specify(grants ~ period) %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Before", "After"))

null_grant_test %>%
  get_p_value(grant_test, direction = "right")

#p value = 0.184
# therefore we fail to reject the null.
# there is no significant difference in the average number of grants given per month, before vs after the policy.

############
# visualize the grants per month:
colors()
# there are mean lines for the average num of grants before, and avg num of grants after.
grant_data %>% ggplot(aes(x=grants, fill=period)) +
  geom_bar(position = "dodge") +
  facet_wrap(~period)+
  scale_fill_manual(values = c("#ffa461", "#a4b2fe"))+
  geom_vline(xintercept = mean(monthly_grants_after), color="orange", linetype="dashed", linewidth=0.7) +
  geom_vline(xintercept = mean(monthly_grants_before), color="purple", linetype="dashed", linewidth=0.7)+
  labs(x="Amount of Grants Per Month")


#############
# diff in props hypothesis test to test if the period is "after", the patient is more likely to be New.
# if yes, then their policy is helping them reach more new patients

# I am using the filtered data set that does NOT have 0s, since those people were not granted any money.

#H0 : The probability that patients are New in the period After the policy is less than or equal to the probability that patients are New in the period Before the policy.
# P(New, After) <= P(New, Before)

#H1 :  The probability that patients are New in the period After the policy is more than the probability that patients are New in the period Before the policy
# P(New, After) > P(New, Before)

#obs
obs_rev <- nonzero_claires %>% 
  specify(New~period,
          success="TRUE") %>% 
  calculate(stat="diff in props",
            order=c("Before", "After"))

#null
null_rev <- nonzero_claires %>% 
  specify(New~period,
          success="TRUE") %>% 
  hypothesise(null="independence") %>% 
  generate(reps = 1000, type="permute") %>% 
  calculate(stat="diff in props",
            order=c("Before", "After"))
#p value
null_rev %>% 
  get_p_value(obs_rev, direction="right")

#the p value is 0.024, which is < 0.05 (significant)
#therefore we reject the null hypothesis.
# The probability that patients are New in the period After the policy is greater than the probability that patients are New in the period Before the policy.


############
# confirm the above claim with a bootstrap analysis. 
# again, use the data that is filtered without 0s. 

# Take bootstrap samples from each time period
boot_dist <- nonzero_claires %>% 
  specify(New ~ period, success = "TRUE") %>% 
  generate(reps = 1000, 
           type = "bootstrap", 
           strata = "period") 

# Calculate diff in proportions in each bootstrap sample
boot_results <- boot_dist %>% 
  calculate(stat = "diff in props", 
            order = c("Before", "After"))

# Get CI and visualize
boot_ci <- boot_results %>%
  get_confidence_interval()

# lower_ci = 0.01979141
# upper_ci = 0.3708849

boot_results %>%
  visualize() +
  shade_confidence_interval(boot_ci) +
  geom_vline(xintercept = 0)


# since 0 is not in the range, we can say that there is an increase in proportions.

#Specifically, based on this test, we can say with 95% confidence that the difference in proportions in within the range of about 2 % to 37 % of an increase.

# This supports the claim that a higher proportion of New people are likely to be granted money after the introduction of the policy.


