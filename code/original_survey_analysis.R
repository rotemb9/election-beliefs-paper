# this script includes descriptive stats, tests and visualizations for the data of the original survey
# written by Rotem Botvinik-Nezer

################
# PREPERATIONS #
################

## Use checkpoint for reproducibility
library(checkpoint)
#checkpoint("2021-06-30", scanForPackages = FALSE)
checkpoint("2021-06-30")
## Required R package
library(grid)
library(caret)
library(car)
library(ggpubr)
library(gtsummary)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(gridExtra)
library(cowplot)
library(GauPro)
library(ggplot2)
library(tidyverse)
library(Matrix)

## Clear workspace
rm(list=ls())

## to save time, the gp_reg vars can be loaded from a pre-saved copy
# do not use the loaded one if you haven't run this before or if you changed something in the data!
load_gp_reg = FALSE
if (load_gp_reg) {
  warning("loading instead of calculating GP regression")
}

## Define local paths
input_path="data/"

## load data
filename = paste(input_path, "original_survey_data_for_analysis.csv",sep="")
data2fit = read.csv(filename)

## remove participants with preferred candidate = Other
warning(paste("Removing ", sum(data2fit$PrefCand=="Other"), " participants with PrefCand = Other", sep=""))
data2fit = data2fit[data2fit$PrefCand!="Other", ]
data2fit$PrefCand = droplevels(data2fit$PrefCand)


############################
# STATS AND VISUALIZATIONS #
############################
# orgnize and z -score variables
data2fit$state = relevel(data2fit$state, ref = "Florida") # the state with most residents
data2fit$numericEndTime = as.numeric(data2fit$T_end)
data2fit$PrefStrength_z = scale(data2fit$PrefStrength, center = TRUE, scale=TRUE)
data2fit$WinProb_z = scale(data2fit$WinProb, center = TRUE, scale=TRUE)
data2fit$FraudProb_z = scale(data2fit$FraudProb, center = TRUE, scale=TRUE)
data2fit$DeltaFraudProb_z = scale(data2fit$DeltaFraudProb, center = TRUE, scale=TRUE)
data2fit$age_z = scale(data2fit$age, center = TRUE, scale=TRUE)
data2fit$numericEndTime_z = scale(data2fit$numericEndTime, center = TRUE, scale=TRUE)
contrasts(data2fit$PrefCand) = contr.sum(2)

#### test fraud belief updating
print("Fraud belief update")
### loss
print(paste("LOSS scenario: N = ", sum(!data2fit$MapMatchPrefer), "; mean: ",round(mean(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer]),2)," SD: ", round(sd(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer]),2), sep=""))
loss_t = t.test(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer])
print(paste("t-test fraud belief update loss scenario: t(df =", loss_t$parameter,  ") =", round(loss_t$statistic,2), ", p =", round(loss_t$p.value,4),  sep = " ")) 
model_belief_update_loss = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[!data2fit$MapMatchPrefer,], na.action=na.omit)
summary(model_belief_update_loss)

## regression table
model_belief_update_loss_table = tbl_regression(model_belief_update_loss, intercept = TRUE,
                                                label = list(FraudProb_z ~ "Prior fraud belief", PrefCand ~ "Preferred candidate", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(model_belief_update_loss_table, file="code/plots/model_belief_update_loss_table.rdata")

## check if state should be included
model_belief_update_loss_without_state = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[!data2fit$MapMatchPrefer & !is.na(data2fit$state),], na.action=na.omit)
model_belief_update_loss_with_state = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z + state, data=data2fit[!data2fit$MapMatchPrefer & !is.na(data2fit$state),], na.action=na.omit)
anova(model_belief_update_loss_without_state,model_belief_update_loss_with_state)

## validate robustness
# same model without age (because some participants did not report their age and are therefore excluded above)
model_belief_update_loss_no_age = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z + numericEndTime_z, data=data2fit[!data2fit$MapMatchPrefer,], na.action=na.omit)
# same model without all covariates
model_belief_update_loss_no_covar = lm(DeltaFraudProb_z ~ 1 + PrefStrength_z + WinProb_z, data=data2fit[!data2fit$MapMatchPrefer,], na.action=na.omit)

### win
print(paste("WIN scenario: N = ", sum(data2fit$MapMatchPrefer), "; mean: ",round(mean(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer]),2)," SD: ", round(sd(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer]),2), sep=""))
win_t = t.test(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer])
print(paste("t-test fraud belief update win scenario: t(df =", win_t$parameter,  ") =", round(win_t$statistic,2), ", p =", round(win_t$p.value,4),  sep = " ")) 
model_belief_update_win = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[data2fit$MapMatchPrefer,], na.action=na.omit)
summary(model_belief_update_win)

## regression table
model_belief_update_win_table = tbl_regression(model_belief_update_win, intercept = TRUE,
                                               label = list(FraudProb_z ~ "Prior fraud belief", PrefCand ~ "Preferred candidate", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(model_belief_update_win_table, file="code/plots/model_belief_update_win_table.rdata")

## check if state should be included
model_belief_update_win_without_state = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[data2fit$MapMatchPrefer & !is.na(data2fit$state),], na.action=na.omit)
model_belief_update_win_with_state = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z + state, data=data2fit[data2fit$MapMatchPrefer & !is.na(data2fit$state),], na.action=na.omit)
anova(model_belief_update_win_without_state,model_belief_update_win_with_state)

## validate robustness
# same model without age (because some participants did not report their age and are therefore excluded above)
model_belief_update_win_no_age = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefCand + PrefStrength_z + WinProb_z + numericEndTime_z, data=data2fit[data2fit$MapMatchPrefer,], na.action=na.omit)
# same model without all covariates
model_belief_update_win_no_covar = lm(DeltaFraudProb_z ~ 1 + PrefStrength_z + WinProb_z, data=data2fit[data2fit$MapMatchPrefer,], na.action=na.omit)

### visualizations
## preferences (by map type)
fraudUpdateByPref = ggplot(data2fit, aes(x = PrefStrength, y = DeltaFraudProb, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.8) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Preference strength", y = "Fraud belief update\n(after - before)", color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))
save(fraudUpdateByPref, data2fit, file="code/plots/fraudUpdateByPref.rdata")

# verify that the crossover in the fraudUpdateByPref plot above is due to the linearity constraints, by fitting a GP regression smoother
x_loss_fraudupdate_by_pref = data2fit$PrefStrength[!data2fit$MapMatchPrefer]
y_loss_fraudupdate_by_pref = data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer]
if (load_gp_reg) {
  load("code/plots/GP_reg_loss_fraudupdate_by_pref.rdata")
} else {
  GP_reg_loss_fraudupdate_by_pref = GauPro(x_loss_fraudupdate_by_pref, y_loss_fraudupdate_by_pref, parallel=FALSE)
  save(GP_reg_loss_fraudupdate_by_pref, file = "code/plots/GP_reg_loss_fraudupdate_by_pref.rdata")
}
data2fit$gp_reg_fraudupdate_by_pref[!data2fit$MapMatchPrefer] = GP_reg_loss_fraudupdate_by_pref$predict(x_loss_fraudupdate_by_pref)
x_win_fraudupdate_by_pref = data2fit$PrefStrength[data2fit$MapMatchPrefer]
y_win_fraudupdate_by_pref = data2fit$DeltaFraudProb[data2fit$MapMatchPrefer]
if (load_gp_reg) {
  load("code/plots/GP_reg_win_fraudupdate_by_pref.rdata")
} else {
  GP_reg_win_fraudupdate_by_pref = GauPro(x_win_fraudupdate_by_pref, y_win_fraudupdate_by_pref, parallel=FALSE)
  save(GP_reg_win_fraudupdate_by_pref, file = "code/plots/GP_reg_win_fraudupdate_by_pref.rdata")
}
data2fit$gp_reg_fraudupdate_by_pref[data2fit$MapMatchPrefer] = GP_reg_win_fraudupdate_by_pref$predict(x_win_fraudupdate_by_pref)

ggplot(data2fit, aes(x = PrefStrength, y = DeltaFraudProb, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_point(alpha = 0.1) +
  #geom_smooth(method = "lm", alpha = 0.2, linetype="dashed") +
  geom_line(aes(y=gp_reg_fraudupdate_by_pref, color = MapMatchPrefer)) +
  scale_y_continuous(breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Preference strength", y = "Fraud belief update\n(after - before)", color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))

## prior beliefs (by map type)
fraudUpdateByPred = ggplot(data2fit, aes(x = WinProb, y = DeltaFraudProb, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.8) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Prior win belief", y = "Fraud belief update\n(after - before)", color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))
save(fraudUpdateByPred, data2fit, file="code/plots/fraudUpdateByPred.rdata")

## color by MapMatchPredict instead?
data2fit$PredMap[(data2fit$PrefCand=="Dem" & data2fit$WinProb > 50) | (data2fit$PrefCand=="Rep" & data2fit$WinProb <= 50)] = "Dem"
data2fit$PredMap[(data2fit$PrefCand=="Rep" & data2fit$WinProb > 50) | (data2fit$PrefCand=="Dem" & data2fit$WinProb <= 50)] = "Rep"
data2fit$MapMatchPredict = data2fit$Map == data2fit$PredMap
fraudUpdateByPred2 = ggplot(data2fit, aes(x = WinProb, y = DeltaFraudProb, color = MapMatchPredict)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.8) +
  geom_point(alpha = 0.1) +
  scale_y_continuous(breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Disconfirmatory", "Confirmatory")) +
  labs(x = "Prior win belief", y = "Fraud belief update\n(after - before)", color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))

# fit a GP regression smoother to the fraudUpdateByPred data as well
x_loss_fraudupdate_by_pred = data2fit$WinProb[!data2fit$MapMatchPrefer]
y_loss_fraudupdate_by_pred = data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer]
if (load_gp_reg) {
  load("code/plots/GP_reg_loss_fraudupdate_by_pred.rdata")
} else {
  GP_reg_loss_fraudupdate_by_pred = GauPro(x_loss_fraudupdate_by_pred, y_loss_fraudupdate_by_pred, parallel=FALSE)
  save(GP_reg_loss_fraudupdate_by_pred, file = "code/plots/GP_reg_loss_fraudupdate_by_pred.rdata")
}
data2fit$gp_reg_fraudupdate_by_pred[!data2fit$MapMatchPrefer] = GP_reg_loss_fraudupdate_by_pred$predict(x_loss_fraudupdate_by_pred)
x_win_fraudupdate_by_pred = data2fit$WinProb[data2fit$MapMatchPrefer]
y_win_fraudupdate_by_pred = data2fit$DeltaFraudProb[data2fit$MapMatchPrefer]
if (load_gp_reg) {
  load("code/plots/GP_reg_win_fraudupdate_by_pred.rdata")
} else {
  GP_reg_win_fraudupdate_by_pred = GauPro(x_win_fraudupdate_by_pred, y_win_fraudupdate_by_pred, parallel=FALSE)
  save(GP_reg_win_fraudupdate_by_pred, file = "code/plots/GP_reg_win_fraudupdate_by_pred.rdata")
}
data2fit$gp_reg_fraudupdate_by_pred[data2fit$MapMatchPrefer] = GP_reg_win_fraudupdate_by_pred$predict(x_win_fraudupdate_by_pred)

ggplot(data2fit, aes(x = WinProb, y = DeltaFraudProb, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.2, linetype="dashed") +
  geom_line(aes(y=gp_reg_fraudupdate_by_pred, color = MapMatchPrefer)) +
  scale_y_continuous(breaks=seq(-100,100,20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Prior win belief", y = "Fraud belief update\n(after - before)", color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))

### fraud belief update for each partisan group
## losing + dem
print(paste("DEM LOSS scenario: N = ", sum(!data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"), "; mean: ",round(mean(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"]),2)," SD: ", round(sd(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"]),2), sep=""))
loss_dem_t = t.test(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"])
print(paste("t-test: t(df =", loss_dem_t$parameter,  ") =", round(loss_dem_t$statistic,2), ", p =", round(loss_dem_t$p.value,4),  sep = " ")) 
model_belief_update_loss_dem = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[!data2fit$MapMatchPrefer & data2fit$PrefCand == "Dem",], na.action=na.omit)
summary(model_belief_update_loss_dem)
loss_dem_table = tbl_regression(model_belief_update_loss_dem, intercept = TRUE,
                                label = list(FraudProb_z ~ "Prior fraud belief", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(loss_dem_table, file="code/plots/loss_dem_table.rdata")

## winning + dem
print(paste("DEM WIN scenario: N = ", sum(data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"), "; mean: ",round(mean(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"]),2)," SD: ", round(sd(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"]),2), sep=""))
win_dem_t = t.test(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer & data2fit$PrefCand=="Dem"])
print(paste("t-test: t(df =", win_dem_t$parameter,  ") =", round(win_dem_t$statistic,2), ", p =", round(win_dem_t$p.value,4),  sep = " ")) 
model_belief_update_win_dem = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[data2fit$MapMatchPrefer & data2fit$PrefCand == "Dem",], na.action=na.omit)
summary(model_belief_update_win_dem)
win_dem_table = tbl_regression(model_belief_update_win_dem, intercept = TRUE,
                               label = list(FraudProb_z ~ "Prior fraud belief", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(win_dem_table, file="code/plots/win_dem_table.rdata")

## losing + rep
print(paste("REP LOSS scenario: N = ", sum(!data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"), "; mean: ",round(mean(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"]),2)," SD: ", round(sd(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"]),2), sep=""))
loss_rep_t = t.test(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"])
print(paste("t-test: t(df =", loss_rep_t$parameter,  ") =", round(loss_rep_t$statistic,2), ", p =", round(loss_rep_t$p.value,4),  sep = " ")) 
model_belief_update_loss_rep = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[!data2fit$MapMatchPrefer & data2fit$PrefCand == "Rep",], na.action=na.omit)
summary(model_belief_update_loss_rep)
loss_rep_table = tbl_regression(model_belief_update_loss_rep, intercept = TRUE,
                                label = list(FraudProb_z ~ "Prior fraud belief", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(loss_rep_table, file="code/plots/loss_rep_table.rdata")

## winning + rep
print(paste("REP WIN scenario: N = ", sum(data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"), "; mean: ",round(mean(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"]),2)," SD: ", round(sd(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"]),2), sep=""))
win_rep_t = t.test(data2fit$DeltaFraudProb[data2fit$MapMatchPrefer & data2fit$PrefCand=="Rep"])
print(paste("t-test: t(df =", win_rep_t$parameter,  ") =", round(win_rep_t$statistic,2), ", p =", round(win_rep_t$p.value,4),  sep = " ")) 
model_belief_update_win_rep = lm(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefStrength_z + WinProb_z +  age_z + numericEndTime_z, data=data2fit[data2fit$MapMatchPrefer & data2fit$PrefCand == "Rep",], na.action=na.omit)
summary(model_belief_update_win_rep)
win_rep_table = tbl_regression(model_belief_update_win_rep, intercept = TRUE,
                               label = list(FraudProb_z ~ "Prior fraud belief", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(win_rep_table, file="code/plots/win_rep_table.rdata")

### fraud belief update with categorized preferences
# aim: to test for the same effects with categorized data, as the distribution of preferences is skewed
pref_strength_levels = c("Low-moderate (0-80)", "High (81-99)","Maximal (100)")
data2fit$PrefStrength_cat = cut(data2fit$PrefStrength, breaks = c(0, 80, 99, 100), labels = pref_strength_levels, include.lowest = TRUE)
data2fit$PrefStrength_cat = ordered(data2fit$PrefStrength_cat)
summary(data2fit$PrefStrength_cat)

## ANCOVA
# loss
ancova_model_loss = aov(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefStrength_cat + WinProb + PrefCand +  age_z + numericEndTime_z, data=data2fit[!data2fit$MapMatchPrefer,])
Anova(ancova_model_loss, type="III")
# win
ancova_model_win = aov(DeltaFraudProb_z ~ 1 + FraudProb_z + PrefStrength_cat + WinProb + PrefCand +  age_z + numericEndTime_z, data=data2fit[data2fit$MapMatchPrefer,])
Anova(ancova_model_win, type="III")

## simple effects
TukeyHSD(ancova_model_loss, which = "PrefStrength_cat")
TukeyHSD(ancova_model_win, which = "PrefStrength_cat")

## visualize
x_labels_PrefStrength_cat = data.frame("PrefStrength_cat" = levels(data2fit$PrefStrength_cat))
x_labels_PrefStrength_cat$N = data.frame(table(data2fit$PrefStrength_cat, dnn="PrefStrength_cat"))$Freq
x_labels_PrefStrength_cat$label = paste(x_labels_PrefStrength_cat$PrefStrength_cat, "\n(N=",x_labels_PrefStrength_cat$N , ")", sep="")

data_bar_plot_pref_cat = aggregate(data2fit$DeltaFraudProb,list(MapMatchPrefer = data2fit$MapMatchPrefer, PrefStrength_cat = data2fit$PrefStrength_cat), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
data_bar_plot_pref_cat = cbind(data_bar_plot_pref_cat[,1:2], as.data.frame(data_bar_plot_pref_cat$x))
data_bar_plot_pref_cat$SEM = data_bar_plot_pref_cat$sd/sqrt(data_bar_plot_pref_cat$n)
bar_plot_pref_cat = ggplot(data=data_bar_plot_pref_cat, aes(x=PrefStrength_cat, y=mean, fill=MapMatchPrefer)) +
  geom_bar(width=.8,colour="black",position=position_dodge(0.8), stat="identity", alpha = 0.5) + # Bar plot
  geom_errorbar(position=position_dodge(0.8), width=1/4, aes(ymin=mean-SEM, ymax=mean+SEM))  + # add error bar of SEM
  geom_point(data=data2fit, aes(y = DeltaFraudProb, x = PrefStrength_cat, color=MapMatchPrefer), alpha = 0.2, position=position_jitterdodge(jitter.width = .25, dodge.width = 0.8))+
  xlab("Preference Strength") +
  scale_y_continuous("Fraud belief update\n(after - before)",breaks=seq(-100, 100, 20)) +
  scale_fill_manual("Outcome", values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_color_manual("Outcome", values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  theme(text = element_text(size = 10), legend.position="bottom", panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels= x_labels_PrefStrength_cat$label)
save(bar_plot_pref_cat, data_bar_plot_pref_cat, file = "code/plots/bar_plot_pref_cat.rdata")


### fraud belief update - preregistered model
## model
data2fit$WinProbRep_z = scale(data2fit$WinProbRep, center = TRUE, scale=TRUE)
data2fit$PrefStrengthRep_z = scale(data2fit$PrefStrengthRep, center = TRUE, scale=TRUE)

contrasts(data2fit$Map) = contr.sum(2)
model2_prereg = lm(DeltaFraudProb_z ~ 1 + Map + WinProbRep_z + PrefStrengthRep_z + Map:WinProbRep_z + Map:PrefStrengthRep_z + age_z + numericEndTime_z, data=data2fit, na.action=na.omit)
summary(model2_prereg)

## model 2 - simple (and other) effects (and other effects) as pre-registered
fraud_update_loss_t = t.test(data2fit$DeltaFraudProb[!data2fit$MapMatchPrefer])
map_dem_prefRep = lm(DeltaFraudProb_z ~ 1 + PrefStrengthRep_z, data=data2fit[data2fit$Map=="Dem",], na.action=na.omit)
summary(map_dem_prefRep)
map_dem_predRep = lm(DeltaFraudProb_z ~ 1 + WinProbRep_z, data=data2fit[data2fit$Map=="Dem",], na.action=na.omit)
summary(map_dem_predRep)
map_rep_prefRep = lm(DeltaFraudProb_z ~ 1 + PrefStrengthRep_z, data=data2fit[data2fit$Map=="Rep",], na.action=na.omit)
summary(map_rep_prefRep)
map_rep_predRep = lm(DeltaFraudProb_z ~ 1 + WinProbRep_z, data=data2fit[data2fit$Map=="Rep",], na.action=na.omit)
summary(map_rep_predRep)


### pre-registered model for prior beliefs
## model1 pre-registered - prior beliefs in fraud and win likelihood
# all
model1_all_prereg = lm(FraudProb_z ~ 1 + WinProb_z + PrefCand + PrefStrength_z + WinProb_z:PrefStrength_z + PrefCand:PrefStrength_z + age_z + numericEndTime_z, data=data2fit, na.action=na.omit)
summary(model1_all_prereg)
# regression table
model1_all_prereg_table = tbl_regression(model1_all_prereg, intercept = TRUE,
                                         label = list(PrefCand ~ "Preferred candidate", PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(model1_all_prereg_table, file="code/plots/model1_all_prereg_table.rdata")

# dem
model1_dem_prereg = lm(FraudProb_z ~ 1 + WinProb_z * PrefStrength_z + age_z + numericEndTime_z, data=data2fit[data2fit$PrefCand=="Dem",], na.action=na.omit)
summary(model1_dem_prereg)
# regression table
model1_dem_prereg_table = tbl_regression(model1_dem_prereg, intercept = TRUE,
                                         label = list(PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(model1_dem_prereg_table, file="code/plots/model1_dem_prereg_table.rdata")

# rep
model1_rep_prereg = lm(FraudProb_z ~ 1 + WinProb_z * PrefStrength_z + age_z + numericEndTime_z, data=data2fit[data2fit$PrefCand=="Rep",], na.action=na.omit)
summary(model1_rep_prereg)
# regression table
model1_rep_prereg_table = tbl_regression(model1_rep_prereg, intercept = TRUE,
                                         label = list(PrefStrength_z ~ "Preference strength", WinProb_z ~ "Prior win belief", age_z ~ "Age", numericEndTime_z ~ "Survey submission order")) %>%
  bold_p(t = 0.05) %>%
  italicize_levels()
save(model1_rep_prereg_table, file="code/plots/model1_rep_prereg_table.rdata")

## full model with all interaction terms for completeness
model1_full = lm(FraudProb_z ~ 1 + WinProb_z * PrefCand * PrefStrength_z + age_z + numericEndTime_z, data=data2fit, na.action=na.omit)
summary(model1_full)

## visualize
priorFraud_by_priorWin_preferred_scatter = ggplot(data2fit, aes(x=WinProb, y=FraudProb, color = PrefCand)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method=lm, se=TRUE, fullrange=FALSE) +
  scale_color_manual(values = c("blue3","red3")) +
  scale_alpha(guide='none') +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(x = "Prior win belief", y = "Prior belief in fraud ", color = "Preferred candidate") +
  theme(text = element_text(size = 9), legend.position="bottom")
save(priorFraud_by_priorWin_preferred_scatter, data2fit, file="code/plots/priorFraud_by_priorWin_preferred_scatter.rdata")


### visualize fraud udpdate as a function of prior fraud belief (to compare to the Bayesian model predictions)
#  color by pref cand and grid by win/loss
data_for_plot = select(data2fit, "participant_num", "MapMatchPrefer", "PrefCand", "FraudProb", "DeltaFraudProb")
# loss
data_for_plot_loss = data_for_plot[!data_for_plot$MapMatchPrefer, ]
y_loss = data_for_plot_loss$DeltaFraudProb
x_loss = data_for_plot_loss$FraudProb
if (load_gp_reg) {
  load("code/plots/GP_reg_loss.rdata")
} else {
  GP_reg_loss = GauPro(x_loss, y_loss, parallel=FALSE)
  save(GP_reg_loss, file = "code/plots/GP_reg_loss.rdata")
}
fraudUpdate_by_priorFraud_loss = ggplot() +
  geom_point(aes(x=x_loss, y=y_loss, color = data_for_plot_loss$PrefCand), alpha = 0.4) +
  geom_line(aes(x=x_loss, y=GP_reg_loss$predict(x_loss))) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_alpha(guide='none') +
  scale_y_continuous("Fraud belief update\n(posterior - prior)",breaks=seq(-100, 100, 20), limits = c(-100,100)) +
  scale_x_continuous("Prior fraud belief",breaks=seq(0, 100, 10)) +
  labs(color = "Preferred\ncandidate") +
  scale_fill_manual(values=c("blue3","red3")) + # color of bars
  scale_color_manual(values=c("blue3","red3")) + # color of dots
  theme(text = element_text(size = 10)) +
  ggtitle("Loss: Empirical update")
save(fraudUpdate_by_priorFraud_loss,x_loss, y_loss, GP_reg_loss, data_for_plot_loss, file="code/plots/fraudUpdate_by_priorFraud_loss.rdata")

# win
data_for_plot_win = data_for_plot[data_for_plot$MapMatchPrefer, ]
y_win = data_for_plot_win$DeltaFraudProb
x_win = data_for_plot_win$FraudProb
if (load_gp_reg) {
  load("code/plots/GP_reg_win.rdata")
} else {
  GP_reg_win = GauPro(x_win, y_win, parallel=FALSE)
  save(GP_reg_win, file = "code/plots/GP_reg_win.rdata")
}
fraudUpdate_by_priorFraud_win = ggplot() +
  geom_point(aes(x=x_win, y=y_win, color = data_for_plot_win$PrefCand), alpha = 0.4) +
  geom_line(aes(x=x_win, y=GP_reg_win$predict(x_win))) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_alpha(guide='none') +
  scale_y_continuous("Fraud belief update\n(posterior - prior)",breaks=seq(-100, 100, 20), limits = c(-100,100)) +
  scale_x_continuous("Prior fraud belief",breaks=seq(0, 100, 10)) +
  labs(color = "Preferred\ncandidate") +
  scale_fill_manual(values=c("blue3","red3")) + # color of bars
  scale_color_manual(values=c("blue3","red3")) + # color of dots
  theme(text = element_text(size = 10)) +
  ggtitle("Win: Empirical update")
save(fraudUpdate_by_priorFraud_win, x_win, y_win, GP_reg_win, data_for_plot_win,  file="code/plots/fraudUpdate_by_priorFraud_win.rdata")

##################################
# additional plots for the paper #
##################################

### bar plot - partisan direction desirability effect
## prepare data
pref_group_bar_plot = aggregate(data2fit$DeltaFraudProb,list(PrefCand = data2fit$PrefCand, Map = data2fit$Map), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
pref_group_bar_plot = cbind(pref_group_bar_plot[,1:2], as.data.frame(pref_group_bar_plot$x))
pref_group_bar_plot$SEM = pref_group_bar_plot$sd/sqrt(pref_group_bar_plot$n)
levels(pref_group_bar_plot$PrefCand) <- list(Democrats  = "Dem", Republicans = "Rep")
levels(pref_group_bar_plot$Map) <- list(Biden  = "Dem", Trump = "Rep")
data_for_points = select(data2fit, c(DeltaFraudProb, Map, PrefCand))
levels(data_for_points$PrefCand) <- list(Democrats  = "Dem", Republicans = "Rep")
levels(data_for_points$Map) <- list(Biden  = "Dem", Trump = "Rep")
## plot
partisan_direction_desirability_effect = ggplot(data=pref_group_bar_plot, aes(x=Map, y=mean, fill = PrefCand)) +
  geom_bar(width=1,colour="black",position=position_dodge(1), stat="identity", alpha=0.15) + # Bar plot
  geom_errorbar(position=position_dodge(0.8), width=1/8, aes(ymin=mean-SEM, ymax=mean+SEM))  + # add error bar of SEM
  geom_point(data=data_for_points, aes(y = DeltaFraudProb, x = Map, color = PrefCand), position=position_jitterdodge(jitter.width = 1.2, dodge.width = 0.8), alpha=0.1, size = 0.8)+
  scale_x_discrete(name = "", label = c("Biden\nwins", "Trump\nwins")) +
  scale_y_continuous("Fraud belief update\n(after - before)",breaks=seq(-100, 100, 20), limits = c(-100, 100)) +
  scale_fill_manual("Preferred candidate", values=c("blue3","red3")) + # color of bars
  scale_color_manual("Preferred candidate", values=c("blue3","red3")) + # color of dots
  theme(text = element_text(size = 9), legend.position="none", panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.placement = "outside", strip.background = element_rect(fill = "white")) +
  facet_grid(~PrefCand, switch = "x")
save(partisan_direction_desirability_effect, pref_group_bar_plot, data_for_points, file="code/plots/partisan_direction_desirability_effect.rdata")

### same as above but with before and after values
## prepare data
prepost_data = select(data2fit,"participant_num","PrefCand", "Map", "FraudProb","FraudProbMap")
prepost_data = gather(prepost_data, FraudTimePoint, FraudJudgement, FraudProb:FraudProbMap,factor_key = FALSE)
prepost_data$FraudTimePoint[prepost_data$FraudTimePoint == "FraudProb"] = "Prior"
prepost_data$FraudTimePoint[prepost_data$FraudTimePoint == "FraudProbMap"] = "Posterior"
prepost_data$FraudTimePoint = factor(prepost_data$FraudTimePoint, levels = c("Prior", "Posterior"))
prepost_data$participant_num = as.factor(prepost_data$participant_num)
prepost_data_sum = aggregate(prepost_data$FraudJudgement,list(TimePoint = prepost_data$FraudTimePoint, PrefCand = prepost_data$PrefCand, Map = prepost_data$Map), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
prepost_data_sum = cbind(prepost_data_sum[,1:3], as.data.frame(prepost_data_sum$x))
levels(prepost_data_sum$PrefCand) <- list(Democrats  = "Dem", Republicans = "Rep")
levels(prepost_data_sum$Map) <- list(Biden  = "Dem", Trump = "Rep")
levels(prepost_data_sum$TimePoint) <- list(Prior  = "Before", Posterior = "After")
prepost_data_sum$SEM = prepost_data_sum$sd/sqrt(prepost_data_sum$n)
data_for_points = select(prepost_data, c(FraudTimePoint, FraudJudgement, Map, PrefCand, participant_num))
levels(data_for_points$PrefCand) <- list(Democrats  = "Dem", Republicans = "Rep")
levels(data_for_points$Map) <- list(Biden  = "Dem", Trump = "Rep")
levels(data_for_points$FraudTimePoint) <- list(Prior  = "Before", Posterior = "After")
map_labels = c(Biden = "Biden wins", Trump = "Trump wins")

partisan_direction_desirability_effect_prepost = ggplot(data=prepost_data_sum, aes(x=TimePoint, y=mean, fill = PrefCand)) +
  geom_bar(width=1,colour="black",position=position_dodge(1), stat="identity", alpha=0.15) + # Bar plot
  geom_errorbar(position=position_dodge(0.8), width=1/8, aes(ymin=mean-SEM, ymax=mean+SEM))  + # add error bar of SEM
  geom_point(data=data_for_points, aes(y = FraudJudgement, x = FraudTimePoint, color = PrefCand), position=position_jitterdodge(jitter.width = 1.2, dodge.width = 0.8), alpha=0.2, size = 0.8)+
  scale_x_discrete(name = "Time point and scenario") +
  scale_y_continuous("Fraud belief",breaks=seq(0, 100, 10), limits = c(0, 100)) +
  scale_fill_manual("Preferred candidate", values=c("blue4","red4")) + # color of bars
  scale_color_manual("Preferred candidate", values=c("blue4","red4")) + # color of dots
  theme(text = element_text(size = 10), legend.position="bottom", panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.placement = "outside", strip.background = element_rect(fill = "white")) +
  facet_grid(PrefCand~Map, switch = "x", labeller = labeller(Map = map_labels, PrefCand = label_value))
save(partisan_direction_desirability_effect_prepost, prepost_data_sum, data_for_points, file="code/plots/partisan_direction_desirability_effect_prepost.rdata")
