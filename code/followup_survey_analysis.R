# this script includes exploratory analysis of the follow-up survey
# the analysis of the Bayesian model is in a different script
# written by Rotem Botvinik-Nezer

################
# PREPERATIONS #
################

## Use checkpoint for reproducibility
library(checkpoint)
checkpoint("2021-06-30")
## Required R package
library(readxl)
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyverse)
library(ordinal)
library(ggpubr)
library(gtsummary)
library(RColorBrewer)

## Clear workspace
rm(list=ls())

## Define local paths
input_path="data/"

## load data
filename = paste(input_path, "combined_survey_data_for_analysis_after_exclusion.csv",sep="")
combined_data = read.csv(filename)

## organize ordered factor level
combined_data$FraudProbCatBiden = factor(combined_data$FraudProbCatBiden, levels = c("Extremely unlikely", "Unlikely", "Likely", "Extremely likely"), order = TRUE)
combined_data$FraudProbCatTrump = factor(combined_data$FraudProbCatTrump, levels = c("Extremely unlikely", "Unlikely", "Likely", "Extremely likely"), order = TRUE)

## keep only participants with data from both surveys
combined_data = filter(combined_data, !is.na(combined_data$PrefCand_present))

## df with only follow-up data
followup_survey_data = select(combined_data, -c(T_start:state.x))

#################################
#### FOLLOW-UP DATA ANALYSES ####
#################################
#### Fraud beliefs across surveys
# correlate posterior fraud beliefs from the original survey (when map=Biden) with fraud beliefs in favor of Biden in the follow-up survey
# include only participants with consistent preferences across the surveys who did not prefer "Other"
combined_data = filter(combined_data,!PrefCand=="Other" & !PrefCand_present=="Other" & PrefCand==PrefCand_present)  

cor.test(as.numeric(combined_data$FraudProbCatBiden[combined_data$Map=="Dem"]), combined_data$FraudProbMap[combined_data$Map=="Dem"], method = "spearman")

#### True vote belief: who would have won without fraud? (v)
### Chi Squared test
win_fraudless_chi_data = select(followup_survey_data[followup_survey_data$PrefCand_present!="Other",], c("WinFraudless", "PrefCand_present"))
win_fraudless_chi_data = droplevels(win_fraudless_chi_data)
summary(table(win_fraudless_chi_data$WinFraudless, win_fraudless_chi_data$PrefCand_present))

### bar plot for frequency of winner in fraudless elections
win_fraudless_election_by_prefcand = count(followup_survey_data, PrefCand_present, WinFraudless)
win_fraudless_election_by_prefcand$PrefCand_present = factor(win_fraudless_election_by_prefcand$PrefCand_present, levels = c("Dem","Other","Rep"))
win_fraudless_election_by_prefcand$WinFraudless = factor(win_fraudless_election_by_prefcand$WinFraudless, levels = c("Dem","Don't know","Rep"))
win_fraudless_by_prefcand_bar = ggplot(win_fraudless_election_by_prefcand, aes(y = n, x = WinFraudless, fill = PrefCand_present)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("blue3","purple3", "red3")) +
  labs(x = "Who would have won in a fraudless election?", fill = "Preferred candidate") +
  theme(legend.position="bottom") +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(0,600,50))
save(win_fraudless_by_prefcand_bar, win_fraudless_election_by_prefcand, file="code/plots/win_fraudless_by_prefcand_bar.rdata")

### how many believe their candidate is the true winner
# dems
n_dems=sum(win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$PrefCand_present=="Dem"])
perc_dem_fraudless_win_dem = win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$WinFraudless=="Dem" & win_fraudless_election_by_prefcand$PrefCand_present=="Dem"]/n_dems*100
perc_dem_fraudless_win_unknown = win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$WinFraudless=="Don't know" & win_fraudless_election_by_prefcand$PrefCand_present=="Dem"]/n_dems*100
perc_dem_fraudless_win_rep = win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$WinFraudless=="Rep" & win_fraudless_election_by_prefcand$PrefCand_present=="Dem"]/n_dems*100
print(paste(round(perc_dem_fraudless_win_dem,2), "% of ", n_dems, " Democrats believed Biden would have won in a fraudless election", sep=""))
print(paste(round(perc_dem_fraudless_win_unknown,2), "% of ", n_dems, " Democrats didn't know who would have won in a fraudless election", sep=""))
print(paste(round(perc_dem_fraudless_win_rep,2), "% of ", n_dems, " Democrats believed Trump would have won in a fraudless election", sep=""))
# reps
n_reps=sum(win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$PrefCand_present=="Rep"])
perc_rep_fraudless_win_rep = win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$WinFraudless=="Rep" & win_fraudless_election_by_prefcand$PrefCand_present=="Rep"]/n_reps*100
perc_rep_fraudless_win_unknown = win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$WinFraudless=="Don't know" & win_fraudless_election_by_prefcand$PrefCand_present=="Rep"]/n_reps*100
perc_rep_fraudless_win_dem = win_fraudless_election_by_prefcand$n[win_fraudless_election_by_prefcand$WinFraudless=="Dem" & win_fraudless_election_by_prefcand$PrefCand_present=="Rep"]/n_reps*100
print(paste(round(perc_rep_fraudless_win_rep,2), "% of ", n_reps, " Republicans believed Trump would have won in a fraudless election", sep=""))
print(paste(round(perc_rep_fraudless_win_unknown,2), "% of ", n_reps, " Republicans didn't know who would have won in a fraudless election", sep=""))
print(paste(round(perc_rep_fraudless_win_dem,2), "% of ", n_reps, " Republicans believed Biden would have won in a fraudless election", sep=""))

#### candidate benefiting from fraud (c)
### ordinal regression fraud likelihood in favor of each candidate, as a function of preferred candidate
## long version of the data
fraud_candidate_likelihood_data = gather(followup_survey_data, benefitingCandidate, likelihood, FraudProbCatBiden:FraudProbCatTrump, factor_key = TRUE)
levels(fraud_candidate_likelihood_data$benefitingCandidate) = c("Joe Biden", "Donald Trump")
fraud_candidate_likelihood_data = rename(fraud_candidate_likelihood_data, PrefCand = PrefCand_present)
fraud_candidate_likelihood_data$likelihood = factor(fraud_candidate_likelihood_data$likelihood, levels = c("Extremely unlikely", "Unlikely", "Likely", "Extremely likely"), order = TRUE)
## likelihood distributions
likelihood_dist = aggregate(x = fraud_candidate_likelihood_data$likelihood, by = list(benefitingCandidate = fraud_candidate_likelihood_data$benefitingCandidate, PrefCand = fraud_candidate_likelihood_data$PrefCand), function(x) frequency = summary(x))
## ordinal logistic regression
fraud_candidate_likelihood_data_for_ordinal = fraud_candidate_likelihood_data[fraud_candidate_likelihood_data$PrefCand!="Other",]
fraud_candidate_likelihood_data_for_ordinal = droplevels(fraud_candidate_likelihood_data_for_ordinal)
contrasts(fraud_candidate_likelihood_data_for_ordinal$PrefCand) = contr.sum(2)
contrasts(fraud_candidate_likelihood_data_for_ordinal$benefitingCandidate) = contr.sum(2)
fraud_likelihood_ord_reg = clm(likelihood ~ benefitingCandidate * PrefCand, data = fraud_candidate_likelihood_data_for_ordinal, link = "logit")
summary(fraud_likelihood_ord_reg)
# regression table
fraud_likelihood_ord_reg_tbl = tbl_regression(fraud_likelihood_ord_reg,
                                              label = list(benefitingCandidate = "Fraud beneficiary", PrefCand = "Preferred candidate")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
exp(coef(fraud_likelihood_ord_reg))
save(fraud_likelihood_ord_reg_tbl, file="code/plots/fraud_likelihood_ord_reg_tbl.rdata")

## Frequencies
# organize data
fraud_biden_by_prefcand = count(followup_survey_data, FraudProbCatBiden, PrefCand_present)
fraud_trump_by_prefcand = count(followup_survey_data, FraudProbCatTrump, PrefCand_present)
fraud_biden_by_prefcand$PrefCand_present = factor(fraud_biden_by_prefcand$PrefCand_present, levels = c("Dem","Other","Rep"))
fraud_trump_by_prefcand$PrefCand_present = factor(fraud_trump_by_prefcand$PrefCand_present, levels = c("Dem","Other","Rep"))

# % of participants claiming fraud was unlikely or extremey unlikely to be in favor of their preferred candidate
perc_reps_fraud_rep_unlikely = sum(fraud_trump_by_prefcand$n[fraud_trump_by_prefcand$PrefCand_present=="Rep" & fraud_trump_by_prefcand$FraudProbCatTrump%in%c("Extremely unlikely","Unlikely")])/sum(fraud_trump_by_prefcand$n[fraud_trump_by_prefcand$PrefCand_present=="Rep"])*100
perc_dems_fraud_dem_unlikely = sum(fraud_biden_by_prefcand$n[fraud_biden_by_prefcand$PrefCand_present=="Dem" & fraud_biden_by_prefcand$FraudProbCatBiden%in%c("Extremely unlikely","Unlikely")])/sum(fraud_biden_by_prefcand$n[fraud_biden_by_prefcand$PrefCand_present=="Dem"])*100
print(paste(round(perc_reps_fraud_rep_unlikely,2), "% of republicans believed it was extremely unlikely or unlikely that fraud in favor of Trump significantly affected the election outcome",sep=""))
print(paste(round(perc_dems_fraud_dem_unlikely,2), "% of democrats believed it was extremely unlikely or unlikely that fraud in favor of Biden significantly affected the election outcome",sep=""))

## visualize
# bar plot for frequency of fraud in favor of Biden, by the preferred candidate
plot_fraud_biden = ggplot(fraud_biden_by_prefcand, aes(y = n, x = FraudProbCatBiden, fill = PrefCand_present)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("blue3","purple3", "red3")) +
  labs(x = "Likelihood of significant fraud in favor of Biden", fill = "Preferred candidate during follow-up survey") +
  theme(text = element_text(size = 12)) +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(0, 500, 50), limits = c(0,500))
save(plot_fraud_biden, fraud_biden_by_prefcand, file="code/plots/plot_fraud_biden.rdata")

# bar plot for frequency of fraud in favor of Trump, by the preferred candidate
plot_fraud_trump = ggplot(fraud_trump_by_prefcand, aes(y = n, x = FraudProbCatTrump, fill = PrefCand_present)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("blue3","purple3", "red3")) +
  labs(x = "Likelihood of significant fraud in favor of Trump", fill = "Preferred candidate during follow-up survey") +
  theme(text = element_text(size = 12)) +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(0, 500, 50), limits = c(0,500))
save(plot_fraud_trump, fraud_trump_by_prefcand, file="code/plots/plot_fraud_trump.rdata")

## beliefs about the candidate benefiting most from fraud
# rep
n_reps = sum(followup_survey_data$PrefCand_present=="Rep")
n_reps_fraud_favoring_rep = sum(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Rep"]>50)
perc_reps_fraud_favoring_trump = n_reps_fraud_favoring_rep/n_reps*100
n_reps_no_fraud_or_equal = sum(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Rep"]==50)
perc_reps_no_fraud_or_equal = n_reps_no_fraud_or_equal/n_reps*100
n_reps_fraud_favoring_dem = sum(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Rep"]<50)
perc_reps_fraud_favoring_dem = n_reps_fraud_favoring_dem/n_reps*100
print(paste("Out of ", n_reps, " Republicans in the follow-up survey", paste=""))
print(paste(round(perc_reps_fraud_favoring_trump,2), "% believed fraudulent activity favored Trump", paste=""))
print(paste(round(perc_reps_fraud_favoring_dem,2), "% believed fraudulent activity favored Biden", paste=""))
print(paste("and ", round(perc_reps_no_fraud_or_equal,2), "% believed there was no fraud or it benefited both candidates equally", paste=""))
# dem
n_dems = sum(followup_survey_data$PrefCand_present=="Dem")
n_dems_fraud_favoring_rep = sum(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Dem"]>50)
perc_dems_fraud_favoring_trump = n_dems_fraud_favoring_rep/n_dems*100
n_dems_no_fraud_or_equal = sum(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Dem"]==50)
perc_dems_no_fraud_or_equal = n_dems_no_fraud_or_equal/n_dems*100
n_dems_fraud_favoring_dem = sum(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Dem"]<50)
perc_dems_fraud_favoring_dem = n_dems_fraud_favoring_dem/n_dems*100
print(paste("Out of ", n_dems, " Democrats in the follow-up survey", paste=""))
print(paste(round(perc_dems_fraud_favoring_trump,2), "% believed fraudulent activity favored Trump", paste=""))
print(paste(round(perc_dems_fraud_favoring_dem,2), "% believed fraudulent activity favored Biden", paste=""))
print(paste("and ", round(perc_dems_no_fraud_or_equal,2), "% believed there was no fraud or it benefited both candidates equally", paste=""))
# t-test
dem_mean = mean(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Dem"])
dem_sd = sd(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Dem"])
print(paste("Dem: M = ", round(dem_mean,2), ", SD = ", round(dem_sd, 2),sep =""))
t_dem = t.test(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Dem"],mu = 50)
print(paste("t(", t_dem$parameter , ") = ", round(t_dem$statistic, 2), ", p = ", round(t_dem$p.value,3),sep =""))
rep_mean = mean(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Rep"])
rep_sd = sd(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Rep"])
print(paste("Rep: M = ", round(rep_mean,2), ", SD = ", round(rep_sd, 2),sep =""))
t_rep = t.test(followup_survey_data$Fraudbenefitrump[followup_survey_data$PrefCand_present=="Rep"],mu = 50)
print(paste("t(", t_rep$parameter , ") = ", round(t_rep$statistic, 2), ", p = ", round(t_rep$p.value,3),sep =""))

# density plot for fraud benefit trump, colored by current preferred candidate
breaks.major <- seq(0,100,10)
breaks.minor <- c(0,50,100)
labels.minor <- c("100%\nBiden", "Even\nsplit", "100%\nTrump")
benefited_from_fraud_plot = ggplot(followup_survey_data, aes(x=Fraudbenefitrump, color=PrefCand_present, fill=PrefCand_present, alpha = 0.5)) +
  geom_density() +
  scale_color_manual(values = c("blue3", "purple3", "red3")) +
  scale_fill_manual(values = c("blue3", "purple3", "red3")) +
  guides(alpha = "none", color = "none") +
  labs(x = "Who benefited most from fraudulent voting activity?", fill = "Preferred candidate") +
  scale_x_continuous(breaks = c(0,50,100),labels = labels.minor)
save(benefited_from_fraud_plot, followup_survey_data, file="code/plots/benefited_from_fraud_plot.rdata")


#### fraud types analysis
# calculate Ns for labels
x_labels_PrefCand_present = data.frame("PrefCand_present" = levels(followup_survey_data$PrefCand_present))
x_labels_PrefCand_present$N = data.frame(table(followup_survey_data$PrefCand_present, dnn="PrefCand_present"))$Freq
x_labels_PrefCand_present$label = paste(x_labels_PrefCand_present$PrefCand_present, "\n(N=",x_labels_PrefCand_present$N , ")", sep="")

# plot all the different "fraudulent" activities and their believed impact as a function of the preferred candidate
fraud_types = c("ElectoralManipulationPreElection", "NewsAggregFalseNews", "RightMediaFalseNews", "LeftMediaFalseNews", "SocialMediaFalseNews", "VoteFraud", "MailFraud", "BallotFraud", "ElectronicSystems", "Misrecording", "Misreporting")
fraud_types_impact = paste("Impact", fraud_types, sep="")
fraud_types_benefit = paste("Benefit", fraud_types, sep="")

# plot all the different "fraudulent" activities and their believed benefiting candidate as a function of the preferred candidate
curr_data = select(followup_survey_data, "participant_num", "PrefCand_present", all_of(fraud_types_benefit))
curr_data_long = gather(curr_data, activity_type, benefit, all_of(fraud_types_benefit),factor_key = FALSE)
curr_data_long$activity_type <- sub("Benefit", "", curr_data_long$activity_type)
curr_data_long$activity_type = factor(curr_data_long$activity_type, levels = fraud_types)
activity_type_labels = c("Electoral manipulation pre-election",
                         "False news: news aggregator services",
                         "False news: \"right-wing\" media",
                         "False news: \"left-wing\" media",
                         "False information in social media",
                         "Vote buying / impersonation / misuse",
                         "Mail-in ballot fraud",
                         "Ballot manipulation / stuffing",
                         "Electronic voting systems tempering",
                         "Misrecording by counters / officials",
                         "Misreporting by officials")
names(activity_type_labels) = fraud_types

fraud_benefit_density = ggplot(curr_data_long, aes(x=benefit, color=PrefCand_present, fill=PrefCand_present, alpha = 0.5)) +
  geom_density() +
  scale_color_manual(values = c("purple3", "blue3", "red3")) +
  scale_fill_manual(values = c("purple3", "blue3", "red3")) +
  guides(alpha = "none", color = "none") +
  labs(x = "", fill = "Preferred candidate") +
  scale_x_continuous(breaks = c(0,50, 100),labels = c("100%\nBiden", "even\nsplit", "100%\nTrump")) +
  theme(text = element_text(size = 9)) +
  theme(legend.position="bottom") +
  facet_wrap(~ activity_type, labeller = labeller(activity_type = activity_type_labels), ncol = 3, scales = "free")
save(fraud_benefit_density, curr_data_long, file = "code/plots/fraud_benefit_density.rdata")

### "Fraudulent" activities considered
patterns = c(
  "Electorate manipulation prior to election day",
  "news aggregator services",
  "right-wing",
  "left-wing",
  "Social media campaigns spreading false information",
  "Vote buying / voter impersonation / misuse of proxy votes",
  "Mail-in ballot fraud",
  "Ballot manipulation or ballot-box stuffing",
  "Tampering with electronic voting systems",
  "Mis-recording of votes by state vote-counters and state electoral officials",
  "Mis-reporting of final vote counts by state electoral officials"
)

full_patterns = c(
  "Electorate manipulation prior to election day",
  "False news stories by news aggregator services (e.g., Reuters, Allied Press)",
  "False news stories by \"right-wing\" media",
  "False news stories by \"left-wing\" media",
  "Social media campaigns spreading false information",
  "Vote buying / voter impersonation / misuse of proxy votes",
  "Mail-in ballot fraud",
  "Ballot manipulation or ballot-box stuffing",
  "Tampering with electronic voting systems",
  "Mis-recording of votes by state vote-counters and state electoral officials",
  "Mis-reporting of final vote counts by state electoral officials"
)
followup_survey_data_with_activities = as.data.frame(followup_survey_data)
for (activity_type in fraud_types) {
  followup_survey_data_with_activities[,activity_type] = NA
}
for (row_ind in 1:nrow(followup_survey_data_with_activities)) {
  for (pattern_ind in 1:length(patterns)) {
    activity_pattern = patterns[pattern_ind]
    activity_chosen = !is_empty(grep(activity_pattern,followup_survey_data_with_activities$FraudActivities[row_ind]))
    followup_survey_data_with_activities[row_ind, fraud_types[pattern_ind]] = activity_chosen
  }
}
# create long data version to plot
fraud_activities_plot_data = select(followup_survey_data_with_activities, "participant_num", "PrefCand_present", all_of(fraud_types))
fraud_activities_plot_data_long = gather(fraud_activities_plot_data, activity_type, chosen, all_of(fraud_types),factor_key = FALSE)
fraud_activities_plot_data_long_bar = aggregate(fraud_activities_plot_data_long$chosen, list(PrefCand=fraud_activities_plot_data_long$PrefCand_present, activity=fraud_activities_plot_data_long$activity_type), sum)
fraud_activities_plot_data_long_bar = rename(fraud_activities_plot_data_long_bar, num_chosen = x)
fraud_activities_plot_data_long_bar$n = aggregate(fraud_activities_plot_data_long$chosen, list(PrefCand=fraud_activities_plot_data_long$PrefCand_present, activity=fraud_activities_plot_data_long$activity_type), length)$x
fraud_activities_plot_data_long_bar$percent_chosen = round(fraud_activities_plot_data_long_bar$num_chosen / fraud_activities_plot_data_long_bar$n * 100,1)
fraud_activities_plot_data_long_bar$PrefCand = factor(fraud_activities_plot_data_long_bar$PrefCand, levels = c("Other", "Dem", "Rep"))
fraud_activities_plot_data_long_bar$activity = factor(fraud_activities_plot_data_long_bar$activity, levels = fraud_types)

# bar plot
fraud_activities_bar = ggplot(fraud_activities_plot_data_long_bar, aes(x = activity, y = percent_chosen))+
  geom_bar(
    aes(fill = PrefCand), stat = "identity",
    position = position_dodge()) +
  scale_fill_manual(values = c("purple3", "blue3", "red3")) +
  theme(legend.position="bottom") +
  scale_y_continuous(breaks=seq(0,100,10), limits = c(0,100)) +
  scale_x_discrete(limits = rev(levels(fraud_activities_plot_data_long_bar$activity)), labels = rev(full_patterns)) +
  labs(x = "Activity", y = "% participants", fill = "Preferred candidate") +
  theme(text = element_text(size = 12),axis.text.y = element_text(hjust = 0)) +
  coord_flip()
save(fraud_activities_bar, fraud_activities_plot_data_long_bar, file = "code/plots/fraud_activities_bar.rdata")
