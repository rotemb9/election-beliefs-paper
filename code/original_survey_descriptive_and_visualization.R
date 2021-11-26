# this script includes descriptive stats and visualizations for the data of the original survey
# written by Rotem Botvinik-Nezer

################
# PREPERATIONS #
################

## Use checkpoint for reproducibility
library(checkpoint)
checkpoint("2021-06-30")
## Required R package
library(ggplot2)
library(tidyverse)

## Clear workspace
rm(list=ls())

## Define local paths
input_path="data/"

## load data
filename = paste(input_path, "original_survey_data_for_analysis.csv",sep="")
data2fit = read.csv(filename)

## remove participants with preferred candidate = Other
warning(paste("Removing ", sum(data2fit$PrefCand=="Other"), " participants with PrefCand = Other", sep=""))
data2fit = data2fit[data2fit$PrefCand!="Other", ]
data2fit$PrefCand = droplevels(data2fit$PrefCand)



#######################################
# DESCRIPTIVE STAT AND VISUALIZATIONS #
#######################################

### barplot- affiliations and preferred candidate

## calculate Ns for labels
x_labels_PrefCand = data.frame("PrefCand" = levels(data2fit$PrefCand))
x_labels_PrefCand$N = data.frame(table(data2fit$PrefCand, dnn="PrefCand"))$Freq
x_labels_PrefCand$label = paste(x_labels_PrefCand$PrefCand, "\n(N=",x_labels_PrefCand$N , ")", sep="")

## prepare data and plot the stacked barplot
data2fit$Affiliation=factor(data2fit$Affiliation, levels = c("Dem", "Rep", "Ind", "Other"))
affiliation_and_preferred_counts = count(data2fit, Affiliation, PrefCand)
preferred_by_affiliation_bar = ggplot(affiliation_and_preferred_counts, aes(x = PrefCand, y = n))+
  geom_bar(aes(fill = Affiliation), width= 0.5, stat = "identity", color = "white",
           position = "stack") +
  scale_fill_manual(values = c("blue2", "red2", "purple2", "green2")) +
  theme(legend.position="bottom") +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(0,1200,100)) +
  labs(x = "Preferred candidate", y = "N") +
  scale_x_discrete(labels= x_labels_PrefCand$label)
save(preferred_by_affiliation_bar, file="code/plots/preferred_by_affiliation_bar.rdata")

### Descriptives

## Preferred candidate
N = nrow(data2fit)
n_dems = sum(data2fit$PrefCand=="Dem")
n_reps = sum(data2fit$PrefCand=="Rep")
print(paste("N = ", N, " of which ", n_dems, " preferred Joe Biden and ", n_reps, " preferred Donald Trump", sep = ""))

## states
n_states = length(unique(data2fit$state[!is.na(data2fit$state)]))
states_freq = data.frame(table(data2fit$state[!is.na(data2fit$state)], dnn="state"))$Freq
states_freq_range = range(states_freq)
print(paste("Participants came from ", n_states, " states. Number of participants in each state ranged between ", states_freq_range[1], " to ",states_freq_range[2] ,sep=""))

## preferred candidate by state
state_dist = count(data2fit, state, PrefCand)
state_by_preferred_bar = ggplot(state_dist, aes(fill=PrefCand, y=n, x=state)) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()+
  theme(legend.position="bottom") +
  labs(fill = "Preferred candidate", y = "N", x = "State") +
  scale_fill_manual(values = c("blue3","red3")) +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(breaks=seq(0,150,10))
save(state_by_preferred_bar, file="code/plots/state_by_preferred_bar.rdata")

## age
age_mean = round(mean(data2fit$age, na.rm=T), 1)
age_sd = round(sd(data2fit$age, na.rm=T),1)
dem_age_mean = round(mean(data2fit$age[data2fit$PrefCand=="Dem"], na.rm=T),1)
dem_age_sd = round(sd(data2fit$age[data2fit$PrefCand=="Dem"], na.rm=T),1)
rep_age_mean = round(mean(data2fit$age[data2fit$PrefCand=="Rep"], na.rm=T),1)
rep_age_sd = round(sd(data2fit$age[data2fit$PrefCand=="Rep"], na.rm=T),1)
print(paste("Age: mean ", age_mean, " SD: ", age_sd, sep=""))
print(paste("Age Dems: mean ", dem_age_mean, " SD: ", dem_age_sd, sep=""))
print(paste("Age Reps: mean ", rep_age_mean, " SD: ", rep_age_sd, sep=""))

# Density plot: Age as a function of the preferred candidate
age_by_preferred_density = ggplot(data2fit, aes(x=age, color=PrefCand, fill=PrefCand, alpha = 0.5)) +
  geom_density() +
  scale_color_manual(values = c("blue3","red3")) +
  scale_fill_manual(values = c("blue3","red3")) +
  guides(color = "none", alpha = "none") +
  labs(fill = "Preferred candidate", x = "Age", y = "Density") + 
  theme(text = element_text(size = 12)) +
  theme(legend.position="bottom")
save(age_by_preferred_density, data2fit, file="code/plots/age_by_preferred_density.rdata")

## preferences
PrefStrength_mean = round(mean(data2fit$PrefStrength, na.rm=T), 1)
PrefStrength_sd = round(sd(data2fit$PrefStrength, na.rm=T),1)
dem_PrefStrength_mean = round(mean(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], na.rm=T),1)
dem_PrefStrength_sd = round(sd(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], na.rm=T),1)
rep_PrefStrength_mean = round(mean(data2fit$PrefStrength[data2fit$PrefCand=="Rep"], na.rm=T),1)
rep_PrefStrength_sd = round(sd(data2fit$PrefStrength[data2fit$PrefCand=="Rep"], na.rm=T),1)
print(paste("PrefStrength: mean ", PrefStrength_mean, " SD: ", PrefStrength_sd, sep=""))
print(paste("PrefStrength Dems: mean ", dem_PrefStrength_mean, " SD: ", dem_PrefStrength_sd, sep=""))
print(paste("PrefStrength Reps: mean ", rep_PrefStrength_mean, " SD: ", rep_PrefStrength_sd, sep=""))
PrefStrength_var_test = var.test(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], data2fit$PrefStrength[data2fit$PrefCand=="Rep"])
PrefStrength_t_test = t.test(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], data2fit$PrefStrength[data2fit$PrefCand=="Rep"], paired = F, var.equal = PrefStrength_var_test$p.value>0.05)
print(paste("indepentdent samples t-test, p-value = ", round(PrefStrength_t_test$p.value,4), sep=""))
PrefStrength_perc_100 = round(sum(data2fit$PrefStrength==100, na.rm=T)/nrow(data2fit)*100, 1)
PrefStrength_perc_less_50 = round(sum(data2fit$PrefStrength<50, na.rm=T)/nrow(data2fit)*100, 1)
print(paste("For ", PrefStrength_perc_100, "% of participants preferences strength was maximal",sep=""))
print(paste("For ", PrefStrength_perc_less_50, "% of participants preferences strength was at the lower half (<50)",sep=""))

# Density plot: Preference strength as a function of the preferred candidate
prefStrength_by_preferred_density = ggplot(data2fit, aes(x=PrefStrength, color=PrefCand, fill=PrefCand, alpha = 0.5)) +
  geom_density() +
  scale_color_manual(values = c("blue3","red3")) +
  scale_fill_manual(values = c("blue3","red3")) +
  guides(color = "none", alpha = "none") +
  labs(fill = "Preferred candidate", x = "Preference strength", y = "Density") + 
  theme(text = element_text(size = 12)) +
  theme(legend.position="bottom")
save(prefStrength_by_preferred_density, data2fit, file="code/plots/prefStrength_by_preferred_density.rdata")

## prior beliefs - winner
WinProb_mean = round(mean(data2fit$WinProb, na.rm=T), 1)
WinProb_sd = round(sd(data2fit$WinProb, na.rm=T),1)
dem_WinProb_mean = round(mean(data2fit$WinProb[data2fit$PrefCand=="Dem"], na.rm=T),1)
dem_WinProb_sd = round(sd(data2fit$WinProb[data2fit$PrefCand=="Dem"], na.rm=T),1)
rep_WinProb_mean = round(mean(data2fit$WinProb[data2fit$PrefCand=="Rep"], na.rm=T),1)
rep_WinProb_sd = round(sd(data2fit$WinProb[data2fit$PrefCand=="Rep"], na.rm=T),1)
print(paste("WinProb: mean ", WinProb_mean, " SD: ", WinProb_sd, sep=""))
print(paste("WinProb Dems: mean ", dem_WinProb_mean, " SD: ", dem_WinProb_sd, sep=""))
print(paste("WinProb Reps: mean ", rep_WinProb_mean, " SD: ", rep_WinProb_sd, sep=""))
WinProb_var_test = var.test(data2fit$WinProb[data2fit$PrefCand=="Dem"], data2fit$WinProb[data2fit$PrefCand=="Rep"])
WinProb_t_test = t.test(data2fit$WinProb[data2fit$PrefCand=="Dem"], data2fit$WinProb[data2fit$PrefCand=="Rep"], paired = F, var.equal = WinProb_var_test$p.value>0.05)
print(paste("indepentdent samples t-test, p-value = ", round(WinProb_t_test$p.value,4), sep=""))
WinProb_perc_above_50 = round(sum(data2fit$WinProb>50, na.rm=T)/nrow(data2fit)*100,1)
print(paste(WinProb_perc_above_50, "% of participants believed their candidate will win (>50% chance)",sep=""))

# density plot for prior win belief
winProb_by_preferred_density = ggplot(data2fit, aes(x=WinProb, color=PrefCand, fill=PrefCand, alpha = 0.5)) +
  geom_density() +
  scale_color_manual(values = c("blue3","red3")) +
  scale_fill_manual(values = c("blue3","red3")) +
  guides(color = "none", alpha = "none") +
  theme(text = element_text(size = 12)) +
  theme(legend.position="bottom") +
  labs(fill = "Preferred candidate", x = "Prior win belief", y = "Density")
save(winProb_by_preferred_density, data2fit, file="code/plots/winProb_by_preferred_density.rdata")

## prior beliefs - fraud
FraudProb_mean = round(mean(data2fit$FraudProb, na.rm=T), 1)
FraudProb_sd = round(sd(data2fit$FraudProb, na.rm=T),1)
dem_FraudProb_mean = round(mean(data2fit$FraudProb[data2fit$PrefCand=="Dem"], na.rm=T),1)
dem_FraudProb_sd = round(sd(data2fit$FraudProb[data2fit$PrefCand=="Dem"], na.rm=T),1)
rep_FraudProb_mean = round(mean(data2fit$FraudProb[data2fit$PrefCand=="Rep"], na.rm=T),1)
rep_FraudProb_sd = round(sd(data2fit$FraudProb[data2fit$PrefCand=="Rep"], na.rm=T),1)
print(paste("FraudProb: mean ", FraudProb_mean, " SD: ", FraudProb_sd, sep=""))
print(paste("FraudProb Dems: mean ", dem_FraudProb_mean, " SD: ", dem_FraudProb_sd, sep=""))
print(paste("FraudProb Reps: mean ", rep_FraudProb_mean, " SD: ", rep_FraudProb_sd, sep=""))
FraudProb_var_test = var.test(data2fit$FraudProb[data2fit$PrefCand=="Rep"], data2fit$FraudProb[data2fit$PrefCand=="Dem"])
FraudProb_t_test = t.test(data2fit$FraudProb[data2fit$PrefCand=="Rep"], data2fit$FraudProb[data2fit$PrefCand=="Dem"], paired = F, var.equal = FraudProb_var_test$p.value>0.05)
print(paste("indepentdent samples t-test, p-value = ", round(FraudProb_t_test$p.value,4), sep=""))

# density plot for prior fraud belief
fraudProb_by_preferred_density = ggplot(data2fit, aes(x=FraudProb, color=PrefCand, fill=PrefCand, alpha = 0.5)) +
  geom_density() +
  scale_color_manual(values = c("blue3","red3")) +
  scale_fill_manual(values = c("blue3","red3")) +
  guides(color = "none", alpha = "none") +
  theme(text = element_text(size = 12)) +
  theme(legend.position="bottom") +
  labs(fill = "Preferred candidate", x = "Prior fraud belief", y = "Density")
save(fraudProb_by_preferred_density, file="code/plots/fraudProb_by_preferred_density.rdata")

## correlation between prior fraud belief and preference strength
cor_fraudprob_prefstrength_rep = cor.test(data2fit$FraudProb[data2fit$PrefCand=="Rep"], data2fit$PrefStrength[data2fit$PrefCand=="Rep"])
cor_fraudprob_prefstrength_dem = cor.test(data2fit$FraudProb[data2fit$PrefCand=="Dem"], data2fit$PrefStrength[data2fit$PrefCand=="Dem"])