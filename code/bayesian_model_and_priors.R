# this script includes the analysis of the Bayesian model and the empirically obtained priors
# written by Rotem Botvinik-Nezer

################
# PREPERATIONS #
################

# load libraries
library(checkpoint)
checkpoint("2021-06-30")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(GauPro)
library(RColorBrewer)
library(Metrics)

## Clear workspace
rm(list=ls())

## to save time, the gp_reg vars can be loaded
# do not use the loaded one if you haven't run this before or if you changed something in the data
load_gp_reg = TRUE
if (load_gp_reg) {
  warning("loading instead of calculating GP regression")
}

##################
# BAYESIAN MODEL #
##################
### predictions across all priors
# A will be considered the preferred candidate here
# probability of sunstantial fraud, given a win by A:
# Pr[F=1 | W=A] = f*c/(v+f*(c-v))
# Pr[F=1 | W=B] = f*(1-c)/(1-v+f*(v-c))

## priors
v = seq(from = 0,to = 1, by = 0.01) # the probability of A winning based on the true (fraudless) votes
f = seq(from = 0,to = 1, by = 0.01) # the probability of substantial fraud
c = seq(from = 0,to = 1, by = 0.01) # the probability that fraud favors A (if there's fraud)
scenario = c("win", "loss")

# df for model
model_df = expand.grid(f = f, v = v, c = c, scenario = scenario)

## posterior f
# A wins
model_df$f_posterior[model_df$scenario=="win"] = model_df$f[model_df$scenario=="win"]*model_df$c[model_df$scenario=="win"]/(model_df$v[model_df$scenario=="win"]+model_df$f[model_df$scenario=="win"]*(model_df$c[model_df$scenario=="win"]-model_df$v[model_df$scenario=="win"]))
# A loses
model_df$f_posterior[model_df$scenario=="loss"] = (model_df$f[model_df$scenario=="loss"]*(1-model_df$c[model_df$scenario=="loss"]))/(1-model_df$v[model_df$scenario=="loss"]+model_df$f[model_df$scenario=="loss"]*(model_df$v[model_df$scenario=="loss"]-model_df$c[model_df$scenario=="loss"]))
# belief update
model_df$f_update = model_df$f_posterior - model_df$f

# when f = 0.5, show heat map for the "desirability effect" (posterior f for loss - posterior f for win) across c and v values
model_df_posterior = spread(select(model_df, -f_update), scenario, f_posterior)
model_df_posterior$desirability_effect = model_df_posterior$loss - model_df_posterior$win
seq_by = 0.1
# f = 0.5
f_value = 0.5
heat_map_desirability_effect = ggplot(data=model_df_posterior[model_df_posterior$f == f_value,], aes(x=c, y=v, fill = desirability_effect)) +
  geom_tile() +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray")) +
  scale_fill_gradient2(paste("Desirability\neffect\n(f=", f_value, ")", sep=""), high = "darkorange", mid = "#f7f7f7", low = "purple")
save(heat_map_desirability_effect, file="code/plots/heat_map_desirability_effect.rdata")

# visualize - fraud update, both scenarios
model_fraud_update = ggplot(model_df[model_df$c %in% c(0,0.2,0.4,0.6,0.8,1) & model_df$v %in% c(0,0.2,0.4,0.6,0.8,1),]) +
  geom_line(aes(x = f, y = f_update, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  scale_x_continuous("Prior fraud belief (f)", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  labs(y = "Fraud belief update") +
  theme(text = element_text(size = 10), legend.position = "none") +
  facet_grid(v ~ c, labeller = labeller(v = label_both, c = label_both))
save(model_fraud_update, model_df, file="code/plots/model_fraud_update.rdata")

## posterior v
# A wins
model_df$v_posterior[model_df$scenario=="win"] = (model_df$v[model_df$scenario=="win"] * (1 - model_df$f[model_df$scenario=="win"]) + model_df$v[model_df$scenario=="win"] * model_df$f[model_df$scenario=="win"]* model_df$c[model_df$scenario=="win"]) / (model_df$v[model_df$scenario=="win"] + model_df$f[model_df$scenario=="win"] * (model_df$c[model_df$scenario=="win"] -model_df$v[model_df$scenario=="win"]))
# A loses
model_df$v_posterior[model_df$scenario=="loss"] = model_df$v[model_df$scenario=="loss"] * model_df$f[model_df$scenario=="loss"] * (1 - model_df$c[model_df$scenario=="loss"]) / (1 - model_df$v[model_df$scenario=="loss"] + model_df$f[model_df$scenario=="loss"] * (model_df$v[model_df$scenario=="loss"] - model_df$c[model_df$scenario=="loss"]))
# belief update
model_df$v_update = model_df$v_posterior - model_df$v

# visualize - v update, BOTH scenario
model_v_update = ggplot(model_df[model_df$c %in% c(0,0.2,0.4,0.6,0.8,1) & model_df$f %in% c(0,0.2,0.4,0.6,0.8,1),]) +
  geom_line(aes(x = v, y = v_update, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  scale_x_continuous("v", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  labs(y = "True winner belief update (posterior - prior)") +
  theme(text = element_text(size = 10), legend.position = "bottom") +
  facet_grid(f ~ c, labeller = labeller(f = label_both, c = label_both))
save(model_v_update, file="code/plots/model_v_update.rdata")


### predictions for the empirically obtained priors (follow-up survey)
## read the data
input_path="data/"
filename = paste(input_path, "combined_survey_data_for_analysis_after_exclusion.csv",sep="")
data2fit = read.csv(filename)

## exclude participants
warning(paste("Out of", nrow(data2fit), "observations:"))
warning(paste("Removing", sum(is.na(data2fit$PrefCand_present)) ,"participants without follow-up survey data"))
data2fit = filter(data2fit,!is.na(PrefCand_present))  
warning(paste("Removing", sum(data2fit$PrefCand=="Other") ,"participants with PrefCand == Other in the original survey"))
data2fit = filter(data2fit,!PrefCand=="Other")
warning(paste("Removing", sum(data2fit$PrefCand_present=="Other", na.rm = TRUE) ,"participants with PrefCand == Other in the follow-up survey"))
data2fit = filter(data2fit,!PrefCand_present=="Other" | is.na(PrefCand_present))
warning(paste("Removing", sum(data2fit$PrefCand!=data2fit$PrefCand_present) ,"participants for whom the preferred candidate changed from the original to the follow-up survey"))
data2fit = filter(data2fit,PrefCand==PrefCand_present)  

## v
data2fit$pred_v[data2fit$PrefCand_present!="Other" & data2fit$WinFraudless!="Don't know" & as.character(data2fit$PrefCand_present)==as.character(data2fit$WinFraudless)] =
  data2fit$WinFraudless_confidence[data2fit$PrefCand_present!="Other" & data2fit$WinFraudless!="Don't know" & as.character(data2fit$PrefCand_present)==as.character(data2fit$WinFraudless)]
data2fit$pred_v[!data2fit$PrefCand_present=="Other" & !data2fit$WinFraudless=="Don't know" & as.character(data2fit$PrefCand_present)!=as.character(data2fit$WinFraudless)] =
  -1*data2fit$WinFraudless_confidence[!data2fit$PrefCand_present=="Other" & !data2fit$WinFraudless=="Don't know" & as.character(data2fit$PrefCand_present)!=as.character(data2fit$WinFraudless)]
data2fit$pred_v[data2fit$WinFraudless=="Don't know"] = 0
data2fit$pred_v = (data2fit$pred_v+100)/200

## c
data2fit$fraud_benefiting_dispreferred[data2fit$PrefCand_present=="Dem"] = data2fit$Fraudbenefitrump[data2fit$PrefCand_present=="Dem"]
data2fit$fraud_benefiting_dispreferred[data2fit$PrefCand_present=="Rep"] = 100-data2fit$Fraudbenefitrump[data2fit$PrefCand_present=="Rep"]
data2fit$pred_c = (100-data2fit$fraud_benefiting_dispreferred)/100

## calculate number of participants included in the analyses
warning(paste("analyzing", nrow(data2fit), "participants"))

## scale the empirical fraud values to 0-1
data2fit$FraudProb_scaled = data2fit$FraudProb/100
data2fit$DeltaFraudProb_scaled = data2fit$DeltaFraudProb/100
data2fit$FraudProbMap_scaled = data2fit$FraudProbMap/100

## predict posterior f based on model
# loss scenario
data2fit$pred_posterior_f[!data2fit$MapMatchPrefer] = data2fit$FraudProb_scaled[!data2fit$MapMatchPrefer]*(1-data2fit$pred_c[!data2fit$MapMatchPrefer])/(1-data2fit$pred_v[!data2fit$MapMatchPrefer]+data2fit$FraudProb_scaled[!data2fit$MapMatchPrefer]*(data2fit$pred_v[!data2fit$MapMatchPrefer]-data2fit$pred_c[!data2fit$MapMatchPrefer]))
# win scenario
data2fit$pred_posterior_f[data2fit$MapMatchPrefer] = data2fit$FraudProb_scaled[data2fit$MapMatchPrefer]*data2fit$pred_c[data2fit$MapMatchPrefer]/(data2fit$pred_v[data2fit$MapMatchPrefer]+data2fit$FraudProb_scaled[data2fit$MapMatchPrefer]*(data2fit$pred_c[data2fit$MapMatchPrefer]-data2fit$pred_v[data2fit$MapMatchPrefer]))

## compute fraud update (posterior - prior)
data2fit$pred_fraud_update = data2fit$pred_posterior_f - data2fit$FraudProb_scaled
# number of participants with predictions
warning(paste("Was able to predict fraud belief for", sum(!is.na(data2fit$pred_posterior_f)), "participants"))

## plot predicted vs. actual values
scatter_predicted_actual_fraud_update = ggplot(data = data2fit, aes(x = DeltaFraudProb_scaled, y = pred_fraud_update, color = MapMatchPrefer)) +
  geom_abline(slope = 1,intercept=0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.6) +
  geom_point(alpha = 0.2) +
  scale_color_manual(name = "Scenario", values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_y_continuous("Fraud belief update\n(after - before)", breaks = seq(-1,1,0.2), limits = c(-1,1)) +
  scale_x_continuous("Predicted fraud update", breaks = seq(-1,1,0.2), limits = c(-1,1)) +
  theme(legend.position = "none", text = element_text(size = 9), panel.background = element_rect(fill = "white"))
save(scatter_predicted_actual_fraud_update, data2fit,  file="code/plots/scatter_predicted_actual_fraud_update.rdata")

## reproduce the empirical findings with the predicted values based on the model
pred_fraudUpdateByPref = ggplot(data2fit, aes(x = PrefStrength, y = pred_fraud_update*100, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.5, linetype = 0) +
  geom_line(stat="smooth",method = "lm", alpha=0.6, size = 1) +
  geom_point(alpha = 0.3, shape = 5) +
  geom_line(aes(x = PrefStrength, y = DeltaFraudProb), method = "lm", linetype = "dashed", stat="smooth", size=1) +
  scale_y_continuous(breaks=seq(-100,100,20), limits = c(-100,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Preference strength", y = "Predicted\nfraud belief update", color = "Map type") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))
save(pred_fraudUpdateByPref, data2fit,  file="code/plots/pred_fraudUpdateByPref.rdata")

pred_fraudUpdateByPred = ggplot(data2fit, aes(x = WinProb, y = pred_fraud_update*100, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.5, linetype = 0) +
  geom_line(stat="smooth",method = "lm", alpha=0.6, size = 1) +
  geom_point(alpha = 0.3, shape = 5) +
  geom_line(aes(x = WinProb, y = DeltaFraudProb), method = "lm", linetype = "dashed", stat="smooth", size=1) +
  scale_y_continuous(breaks=seq(-100,100,20), limits = c(-100,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Prior win belief", y = "Predicted\nfraud belief update", color = "Map type") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))
save(pred_fraudUpdateByPred, data2fit,  file="code/plots/pred_fraudUpdateByPred.rdata")

# the "win" scenario lines are very similar- make sure there's no error- plot the empirical values without the predictions for comparison
qa_lines_and_points = ggplot(data2fit, aes(x = WinProb, y = DeltaFraudProb, color = MapMatchPrefer)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.5, linetype = 0) +
  geom_line(stat="smooth",method = "lm", alpha=0.6, size = 1) +
  geom_point(alpha = 0.3) +
  scale_y_continuous(breaks=seq(-100,100,20), limits = c(-100,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  labs(x = "Prior win belief", y = "Fraud belief update", color = "Map type") +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))


## compare plots for fraud belief update as a function of prior fraud belief
load("code/plots/fraudUpdate_by_priorFraud_loss.rdata")
load("code/plots/fraudUpdate_by_priorFraud_win.rdata")
if (load_gp_reg) {
  load("code/plots/GP_reg_loss_pred.rdata")
} else {
  GP_reg_loss_pred = GauPro(data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer], data2fit$pred_fraud_update[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer], parallel=FALSE)
  save(GP_reg_loss_pred, file = "code/plots/GP_reg_loss_pred.rdata")
}
pred_fraudUpdate_by_priorFraud_loss = ggplot() +
  geom_point(aes(x=data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer], y=data2fit$pred_fraud_update[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer], color = data2fit$PrefCand[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer]), alpha = 0.4, shape = 8) +
  geom_line(aes(x=data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer], y=GP_reg_loss_pred$predict(data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & !data2fit$MapMatchPrefer]))) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_alpha(guide='none') +
  # NOTE THAT I SET THE AXIS LABELS HERE AS -100 - 100 ALTHOUGH THEY WERE ORIGINALLY BETWEEN -1 TO 1, BECAUSE I WANTED IT TO BE SIMILAR TO THE EMPIRICAL PLOTS
  scale_y_continuous("Predicted fraud\nbelief update", breaks=seq(-1, 1, 0.2), limits = c(-1,1), labels = seq(-100, 100, 20)) +
  scale_x_continuous("Prior fraud belief",breaks=seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  labs(color = "Preferred candidate") +
  scale_fill_manual(values=c("blue3","red3")) + # color of bars
  scale_color_manual(values=c("blue3","red3")) + # color of dots
  theme(text = element_text(size = 10)) +
  ggtitle("Loss: Predicted update")

if (load_gp_reg) {
  load("code/plots/GP_reg_win_pred")
} else {
  GP_reg_win_pred = GauPro(data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer], data2fit$pred_fraud_update[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer], parallel=FALSE)
  save(GP_reg_win_pred, file = "code/plots/GP_reg_win_pred")
}
pred_fraudUpdate_by_priorFraud_win = ggplot() +
  geom_point(aes(x=data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer], y=data2fit$pred_fraud_update[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer], color = data2fit$PrefCand[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer]), alpha = 0.4, shape = 8) +
  geom_line(aes(x=data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer], y=GP_reg_win_pred$predict(data2fit$FraudProb_scaled[!is.na(data2fit$pred_fraud_update) & data2fit$MapMatchPrefer]))) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_alpha(guide='none') +
  # NOTE THAT I SET THE AXIS LABELS HERE AS -100 - 100 ALTHOUGH THEY WERE ORIGINALLY BETWEEN -1 TO 1, BECAUSE I WANTED IT TO BE SIMILAR TO THE EMPIRICAL PLOTS
  scale_y_continuous("Predicted fraud\nbelief update",breaks=seq(-1, 1, 0.2), limits = c(-1,1), labels = seq(-100, 100, 20)) +
  scale_x_continuous("Prior fraud belief",breaks=seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  labs(color = "Preferred candidate") +
  scale_fill_manual(values=c("blue3","red3")) + # color of bars
  scale_color_manual(values=c("blue3","red3")) + # color of dots
  theme(text = element_text(size = 10)) +
  ggtitle("Win: Predicted update")

save(pred_fraudUpdate_by_priorFraud_loss, GP_reg_loss_pred, data2fit, file="code/plots/pred_fraudUpdate_by_priorFraud_loss.rdata")
save(pred_fraudUpdate_by_priorFraud_win, GP_reg_win_pred, data2fit, file="code/plots/pred_fraudUpdate_by_priorFraud_win.rdata")


fraudUpdate_by_priorFraud_empirical_and_pred = ggarrange(fraudUpdate_by_priorFraud_loss, fraudUpdate_by_priorFraud_win, 
                                                         pred_fraudUpdate_by_priorFraud_loss, pred_fraudUpdate_by_priorFraud_win,
                                                         labels = c("A", "B", "C", "D"),
                                                         ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

save(fraudUpdate_by_priorFraud_empirical_and_pred, file="code/plots/fraudUpdate_by_priorFraud_empirical_and_pred.rdata")

### model fit
corr_pred_empirical_all = cor(data2fit$pred_fraud_update, data2fit$DeltaFraudProb_scaled, use="complete.obs")
corr_pred_empirical_loss = cor(data2fit$pred_fraud_update[!data2fit$MapMatchPrefer], data2fit$DeltaFraudProb_scaled[!data2fit$MapMatchPrefer], use="complete.obs")
corr_pred_empirical_win = cor(data2fit$pred_fraud_update[data2fit$MapMatchPrefer], data2fit$DeltaFraudProb_scaled[data2fit$MapMatchPrefer], use="complete.obs")
corr_pred_empirical_dem = cor(data2fit$pred_fraud_update[data2fit$PrefCand=="Dem"], data2fit$DeltaFraudProb_scaled[data2fit$PrefCand=="Dem"], use="complete.obs")
corr_pred_empirical_rep = cor(data2fit$pred_fraud_update[data2fit$PrefCand=="Rep"], data2fit$DeltaFraudProb_scaled[data2fit$PrefCand=="Rep"], use="complete.obs")
corr_pred_empirical_dem_map = cor(data2fit$pred_fraud_update[data2fit$Map=="Dem"], data2fit$DeltaFraudProb_scaled[data2fit$Map=="Dem"], use="complete.obs")
corr_pred_empirical_dem_map = cor(data2fit$pred_fraud_update[data2fit$Map=="Rep"], data2fit$DeltaFraudProb_scaled[data2fit$Map=="Rep"], use="complete.obs")

# all
cor.test(data2fit$pred_fraud_update, data2fit$DeltaFraudProb_scaled, use="complete.obs")
#data2fit$model_error = data2fit$pred_posterior_f - data2fit$FraudProbMap_scaled
RMSE_all = rmse(data2fit$FraudProbMap_scaled[!is.nan(data2fit$pred_posterior_f)], data2fit$pred_posterior_f[!is.nan(data2fit$pred_posterior_f)])
n_pred_vals = sum(!is.nan(data2fit$pred_posterior_f))
# loss
cor.test(data2fit$pred_fraud_update[!data2fit$MapMatchPrefer], data2fit$DeltaFraudProb_scaled[!data2fit$MapMatchPrefer], use="complete.obs")
RMSE_loss = rmse(data2fit$FraudProbMap_scaled[!is.nan(data2fit$pred_posterior_f) & !data2fit$MapMatchPrefer], data2fit$pred_posterior_f[!is.nan(data2fit$pred_posterior_f) & !data2fit$MapMatchPrefer])
n_vals_loss = sum(!is.nan(data2fit$pred_posterior_f) & !data2fit$MapMatchPrefer)
# win
cor.test(data2fit$pred_fraud_update[data2fit$MapMatchPrefer], data2fit$DeltaFraudProb_scaled[data2fit$MapMatchPrefer], use="complete.obs")
RMSE_win = rmse(data2fit$FraudProbMap_scaled[!is.nan(data2fit$pred_posterior_f) & data2fit$MapMatchPrefer], data2fit$pred_posterior_f[!is.nan(data2fit$pred_posterior_f) & data2fit$MapMatchPrefer])
n_vals_win = sum(!is.nan(data2fit$pred_posterior_f) & data2fit$MapMatchPrefer)


### desirability effect heat map with participants' points
seq_by = 0.1 # defines the break size for c and v values between 0 and 1
data2fit$pref_strength_rep[data2fit$PrefCand_present=="Rep"] = data2fit$PrefStrength_present[data2fit$PrefCand_present=="Rep"]
data2fit$pref_strength_rep[data2fit$PrefCand_present=="Dem"] = 100-data2fit$PrefStrength_present[data2fit$PrefCand_present=="Dem"]

f_value = 0.5
heat_map_desirability_effect_dots = ggplot() +
  geom_tile(data=model_df_posterior[model_df_posterior$f == f_value,], aes(x=c, y=v, fill = desirability_effect)) +
  geom_point(data = data2fit, aes(x=pred_c, y=pred_v, color = PrefCand), alpha = 0.15, size = 0.3, position = position_jitter(width = 0.01, height = 0.01)) +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray")) +
  scale_fill_gradient2(paste("Desirability\neffect\n(f=", f_value, ")", sep=""), high = "darkorange", mid = "#f7f7f7", low = "purple") +
  scale_color_manual("Preferred\ncandidate", values = c("#0000FF", "#FF0000")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5)))
save(heat_map_desirability_effect_dots,model_df_posterior,data2fit, file="code/plots/heat_map_desirability_effect_dots.rdata")

### participants from each party on the c,v prior space
# heat map- participants' proportions in each combination
seq_by = 0.1 # defines the break size for c and v values between 0 and 1
data2fit$c_for_visualization = cut(data2fit$pred_c, breaks = c(0,seq(seq_by/2,1-seq_by/2,seq_by),1), labels = paste(seq(0,1,seq_by)), include.lowest = TRUE)
data2fit$v_for_visualization = cut(data2fit$pred_v, breaks = c(0,seq(seq_by/2,1-seq_by/2,seq_by),1), labels = paste(seq(0,1,seq_by)), include.lowest = TRUE)
data2fit$c_for_visualization_factor = factor(data2fit$c_for_visualization, levels = seq(0,1,seq_by))
data2fit$v_for_visualization_factor = factor(data2fit$v_for_visualization, levels = seq(0,1,seq_by))

# seperate Dems and Reps- heat map for each
per_v_c_party = count(data2fit[!data2fit$PrefCand_present=="Other",], c_for_visualization_factor, v_for_visualization_factor, PrefCand_present,.drop = FALSE)
per_v_c_party = per_v_c_party[per_v_c_party$PrefCand_present!="Other",]
per_v_c_party$Proportion[per_v_c_party$PrefCand_present=="Dem"] = per_v_c_party$n[per_v_c_party$PrefCand_present=="Dem"] / sum(per_v_c_party$n[per_v_c_party$PrefCand_present=="Dem"])
per_v_c_party$Proportion[per_v_c_party$PrefCand_present=="Rep"] = per_v_c_party$n[per_v_c_party$PrefCand_present=="Rep"] / sum(per_v_c_party$n[per_v_c_party$PrefCand_present=="Rep"])
per_v_c_party$Proportion[is.nan(per_v_c_party$Proportion)] = 0
per_v_c_party = rename(per_v_c_party, c = c_for_visualization_factor, v = v_for_visualization_factor, prefCand = PrefCand_present)
per_v_c_party$v = as.numeric(levels(per_v_c_party$v))[per_v_c_party$v]
per_v_c_party$c = as.numeric(levels(per_v_c_party$c))[per_v_c_party$c]

# Dems
heat_map_dems = ggplot(data=per_v_c_party[per_v_c_party$prefCand=="Dem",],aes(x=c, y=v, fill = Proportion)) +
  geom_tile(alpha = 0.8) +
  geom_text(aes(label = round(Proportion, 2)), size = 3) +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray"), axis.ticks = element_blank()) +
  scale_fill_gradient("Proportion\n", low="white", high="blue", limits = c(min(per_v_c_party$Proportion), max(per_v_c_party$Proportion))) +
  geom_vline(xintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey") +
  geom_hline(yintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey")
save(heat_map_dems, per_v_c_party, file="code/plots/heat_map_dems.rdata")

# Reps
heat_map_reps = ggplot(data=per_v_c_party[per_v_c_party$prefCand=="Rep",],aes(x=c, y=v, fill = Proportion)) +
  geom_tile(alpha = 0.8) +
  geom_text(aes(label = round(Proportion, 2)), size = 3) +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray"), axis.ticks = element_blank()) +
  scale_fill_gradient("Proportion\n", low="white", high="red", limits = c(min(per_v_c_party$Proportion), max(per_v_c_party$Proportion))) +
  geom_vline(xintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey") +
  geom_hline(yintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey")
save(heat_map_reps, per_v_c_party, file="code/plots/heat_map_reps.rdata")

# create heat map of averaged preferences- only where there are at least 1% of participants
pref_v_c_party = aggregate(data2fit$PrefStrength_present[data2fit$PrefCand_present!="Other"],
                           list(c = data2fit$c_for_visualization_factor[data2fit$PrefCand_present!="Other"], v = data2fit$v_for_visualization_factor[data2fit$PrefCand_present!="Other"], prefCand = data2fit$PrefCand_present[data2fit$PrefCand_present!="Other"]),
                           mean, drop = FALSE)
pref_v_c_party = pref_v_c_party[pref_v_c_party$prefCand!="Other",]
pref_v_c_party <- pref_v_c_party[order(pref_v_c_party$c, pref_v_c_party$v, pref_v_c_party$prefCand),]
pref_v_c_party$Proportion = per_v_c_party$Proportion
pref_v_c_party$x[pref_v_c_party$Proportion<0.01] = NA
pref_v_c_party$v = as.numeric(levels(pref_v_c_party$v))[pref_v_c_party$v]
pref_v_c_party$c = as.numeric(levels(pref_v_c_party$c))[pref_v_c_party$c]

# dems
heat_map_pref_dems = ggplot(data=pref_v_c_party[pref_v_c_party$prefCand=="Dem",],aes(x=c, y=v, fill=x)) +
  geom_tile(alpha = 0.8) +
  geom_text(aes(label = round(x, 1)), size = 3) +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray"), axis.ticks = element_blank()) +
  scale_fill_gradient2("Mean\npreference\n", low="white", mid = "white", midpoint = min(pref_v_c_party$x, na.rm = TRUE)-5, high="blue", na.value = "white", limits = c(min(pref_v_c_party$x, na.rm = TRUE), 100)) +
  geom_vline(xintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey") +
  geom_hline(yintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey")
save(heat_map_pref_dems, pref_v_c_party, file="code/plots/heat_map_pref_dems.rdata")

# reps
heat_map_pref_reps = ggplot(data=pref_v_c_party[pref_v_c_party$prefCand=="Rep",],aes(x=c, y=v, fill = x)) +
  geom_tile(alpha = 0.8) +
  geom_text(aes(label = round(x, 1)), size = 3) +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray"), axis.ticks = element_blank()) +
  scale_fill_gradient2("Mean\npreference\n", low="white", mid = "white", midpoint = min(pref_v_c_party$x, na.rm = TRUE)-5,high="red", na.value = "white", limits = c(min(pref_v_c_party$x, na.rm = TRUE), 100)) +
  geom_vline(xintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey") +
  geom_hline(yintercept=seq(seq_by/2, 1-seq_by/2, by=seq_by), color = "grey")
save(heat_map_pref_reps, pref_v_c_party, file="code/plots/heat_map_pref_reps.rdata")

### predict posterior v and v update for our sample
## win
data2fit$pred_v_posterior[data2fit$MapMatchPrefer] = (data2fit$pred_v[data2fit$MapMatchPrefer] * (1 - data2fit$FraudProb_scaled[data2fit$MapMatchPrefer]) + data2fit$pred_v[data2fit$MapMatchPrefer] * data2fit$FraudProb_scaled[data2fit$MapMatchPrefer]* data2fit$pred_c[data2fit$MapMatchPrefer]) / (data2fit$pred_v[data2fit$MapMatchPrefer] + data2fit$FraudProb_scaled[data2fit$MapMatchPrefer] * (data2fit$pred_c[data2fit$MapMatchPrefer] -data2fit$pred_v[data2fit$MapMatchPrefer]))
## loss
data2fit$pred_v_posterior[!data2fit$MapMatchPrefer] = data2fit$pred_v[!data2fit$MapMatchPrefer] * data2fit$FraudProb_scaled[!data2fit$MapMatchPrefer] * (1 - data2fit$pred_c[!data2fit$MapMatchPrefer]) / (1 - data2fit$pred_v[!data2fit$MapMatchPrefer] + data2fit$FraudProb_scaled[!data2fit$MapMatchPrefer] * (data2fit$pred_v[!data2fit$MapMatchPrefer] - data2fit$pred_c[!data2fit$MapMatchPrefer]))
## belief update
data2fit$pred_v_update = data2fit$pred_v_posterior - data2fit$pred_v

### "election-proof" participants
data_for_election_proof = data2fit
# predict posterior v for loss
data_for_election_proof$pred_v_posterior_for_loss = data_for_election_proof$pred_v * data_for_election_proof$FraudProb_scaled * (1 - data_for_election_proof$pred_c) / (1 - data_for_election_proof$pred_v + data_for_election_proof$FraudProb_scaled * (data_for_election_proof$pred_v - data_for_election_proof$pred_c))
data_for_election_proof$pred_v_update_for_loss = data_for_election_proof$pred_v_posterior_for_loss - data_for_election_proof$pred_v
# predict posterior f for loss
data_for_election_proof$pred_f_posterior_for_loss = data_for_election_proof$FraudProb_scaled * (1 - data_for_election_proof$pred_c) / (1 - data_for_election_proof$pred_v + data_for_election_proof$FraudProb_scaled * (data_for_election_proof$pred_v - data_for_election_proof$pred_c))
data_for_election_proof$pred_f_update_for_loss = data_for_election_proof$pred_f_posterior_for_loss - data_for_election_proof$FraudProb_scaled

# how many participants with predictions?
n_predicted_values = sum(!is.na(data_for_election_proof$pred_v_update_for_loss) & !is.na(data_for_election_proof$pred_f_update_for_loss))
print(paste("Participants with predicted posterior v and f for loss:", n_predicted_values))

## restrict priors range to prevent misleading certain priors
new_max = 0.95
new_min = 0.05
warning(paste("capping v, f and c to be between", new_min, "and",new_max))
data_for_election_proof_capped = data2fit
data_for_election_proof_capped$pred_v[data_for_election_proof_capped$pred_v < new_min] = new_min
data_for_election_proof_capped$pred_v[data_for_election_proof_capped$pred_v > new_max] = new_max
data_for_election_proof_capped$pred_c[data_for_election_proof_capped$pred_c < new_min] = new_min
data_for_election_proof_capped$pred_c[data_for_election_proof_capped$pred_c > new_max] = new_max
data_for_election_proof_capped$FraudProb_scaled[data_for_election_proof_capped$FraudProb_scaled < new_min] = new_min
data_for_election_proof_capped$FraudProb_scaled[data_for_election_proof_capped$FraudProb_scaled > new_max] = new_max

# predict posteriors based on the capped values
data_for_election_proof_capped$pred_v_posterior_for_loss = data_for_election_proof_capped$pred_v * data_for_election_proof_capped$FraudProb_scaled * (1 - data_for_election_proof_capped$pred_c) / (1 - data_for_election_proof_capped$pred_v + data_for_election_proof_capped$FraudProb_scaled * (data_for_election_proof_capped$pred_v - data_for_election_proof_capped$pred_c))
data_for_election_proof_capped$pred_v_update_for_loss = data_for_election_proof_capped$pred_v_posterior_for_loss - data_for_election_proof_capped$pred_v
# predict posterior f for loss
data_for_election_proof_capped$pred_f_posterior_for_loss = data_for_election_proof_capped$FraudProb_scaled * (1 - data_for_election_proof_capped$pred_c) / (1 - data_for_election_proof_capped$pred_v + data_for_election_proof_capped$FraudProb_scaled * (data_for_election_proof_capped$pred_v - data_for_election_proof_capped$pred_c))
data_for_election_proof_capped$pred_f_update_for_loss = data_for_election_proof_capped$pred_f_posterior_for_loss - data_for_election_proof_capped$FraudProb_scaled

## calculate the proportion of update that goes to fraud vs. true vote belief
# without capping
data_for_election_proof$prop_update_to_fraud = abs(data_for_election_proof$pred_f_update_for_loss)/(abs(data_for_election_proof$pred_f_update_for_loss) + abs(data_for_election_proof$pred_v_update_for_loss))
print("participants with undefined posterior f or v:")
sum(is.na(data_for_election_proof$pred_f_update_for_loss) | is.na(data_for_election_proof$pred_v_update_for_loss))
print("participants with 0 update for both f and v:")
sum(data_for_election_proof$pred_f_update_for_loss == 0 & data_for_election_proof$pred_v_update_for_loss == 0, na.rm = T)
print("participants with values:")
sum(!is.na(data_for_election_proof$prop_update_to_fraud))
print("participants who increase f more than they decrease v")
sum(data_for_election_proof$prop_update_to_fraud > 0.5 & data_for_election_proof$pred_f_update_for_loss > 0 & data_for_election_proof$pred_v_update_for_loss<=0, na.rm = TRUE)/sum(!is.na(data_for_election_proof$prop_update_to_fraud)) * 100
print("participant with more than 90% of the update going to fraud rather than true vote belief")
sum(data_for_election_proof$prop_update_to_fraud > 0.9  & data_for_election_proof$pred_f_update_for_loss > 0 & data_for_election_proof$pred_v_update_for_loss<=0, na.rm = TRUE)/sum(!is.na(data_for_election_proof$prop_update_to_fraud)) * 100

# with capping
data_for_election_proof_capped$prop_update_to_fraud = abs(data_for_election_proof_capped$pred_f_update_for_loss)/(abs(data_for_election_proof_capped$pred_f_update_for_loss) + abs(data_for_election_proof_capped$pred_v_update_for_loss))
print("participants with undefined posterior f or v:")
sum(is.na(data_for_election_proof_capped$pred_f_update_for_loss) | is.na(data_for_election_proof_capped$pred_v_update_for_loss))
print("participants with 0 update for both f and v:")
sum(data_for_election_proof_capped$pred_f_update_for_loss == 0 & data_for_election_proof_capped$pred_v_update_for_loss == 0, na.rm = T)
print("participants with values:")
sum(!is.na(data_for_election_proof_capped$prop_update_to_fraud))
print("participants who increase f more than they decrease v")
sum(data_for_election_proof_capped$prop_update_to_fraud > 0.5 & data_for_election_proof_capped$pred_f_update_for_loss > 0 & data_for_election_proof_capped$pred_v_update_for_loss<=0, na.rm = TRUE)/sum(!is.na(data_for_election_proof_capped$prop_update_to_fraud)) * 100
print("participant with more than 90% of the update going to fraud rather than true vote belief")
sum(data_for_election_proof_capped$prop_update_to_fraud > 0.9  & data_for_election_proof_capped$pred_f_update_for_loss > 0 & data_for_election_proof_capped$pred_v_update_for_loss<=0, na.rm = TRUE)/sum(!is.na(data_for_election_proof_capped$prop_update_to_fraud)) * 100

## how many Republicans in our sample were indeed still sure that Trump was the true winner, although Biden won?
perc_rep_v_1 = round(sum(data2fit$pred_v==1 & data2fit$PrefCand_present=="Rep")/sum(data2fit$PrefCand_present=="Rep")*100,1)
print(paste("Based on our follow-up data,", perc_rep_v_1, "% of Republicans in our sample were still certain that Trump was the true winner although Biden officially won"))

### correlations between preference strength from the first survey, and c and v from the follow-up survey
## with v
cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], data2fit$pred_v[data2fit$PrefCand =="Dem"], use="complete.obs")
sum(data2fit$PrefCand=="Dem")
cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Rep"], data2fit$pred_v[data2fit$PrefCand =="Rep"], use="complete.obs")
sum(data2fit$PrefCand=="Rep")
## with c
cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], data2fit$pred_c[data2fit$PrefCand=="Dem"], use="complete.obs")
sum(data2fit$PrefCand=="Dem")
cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Rep"], data2fit$pred_c[data2fit$PrefCand=="Rep"], use="complete.obs")
sum(data2fit$PrefCand=="Rep")

### plot c and v curves on a scale from Biden to Trump, to illustrate the biased priors
data_biased_priors_plot = select(data2fit, PrefCand, Fraudbenefitrump)
data_biased_priors_plot$PrefCand = droplevels(data_biased_priors_plot$PrefCand)
data_biased_priors_plot$true_winner_trump = data2fit$pred_v
data_biased_priors_plot$true_winner_trump[data_biased_priors_plot$PrefCand=="Dem"] = 1 - data_biased_priors_plot$true_winner_trump[data_biased_priors_plot$PrefCand=="Dem"] 

breaks.major <- seq(0,100,10)
breaks.minor <- c(0,50,100)
labels.minor <- c("100%\nBiden", "Even\nsplit", "100%\nTrump")

## plot true winner curves- 0 is Biden and 1 is Trump
true_winner_plot = ggplot(data_biased_priors_plot, aes(x=true_winner_trump*100, fill=PrefCand, color=PrefCand)) +
  geom_histogram(bins = 25, position="identity", alpha = 0.5) +
  scale_color_manual(values = c("blue3", "red3")) +
  scale_fill_manual(values = c("blue3", "red3")) +
  guides(alpha = "none", color = "none", fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "True winner", fill = "Preferred\ncandidate") +
  scale_x_continuous(breaks = c(0,50,100),labels = labels.minor)
save(true_winner_plot, data_biased_priors_plot, file="code/plots/true_winner_plot.rdata")

# plot fraud beneficiary curves
breaks.minor <- c(0,100)
labels.minor <- c("100%\nBiden", "Don't know", "100%\nTrump")
fraud_beneficiary_plot = ggplot(data_biased_priors_plot, aes(x=Fraudbenefitrump, fill=PrefCand, color=PrefCand)) +
  geom_histogram(bins = 25, position="identity", alpha = 0.5) +
  scale_color_manual(values = c("blue3", "red3")) +
  scale_fill_manual(values = c("blue3", "red3")) +
  guides(alpha = "none", color = "none", fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "Fraud beneficiary", fill = "Preferred\ncandidate") +
  scale_x_continuous(breaks = c(0,50,100),labels = labels.minor)
save(fraud_beneficiary_plot, data_biased_priors_plot, file="code/plots/fraud_beneficiary_plot.rdata")

biased_priors_plot = ggarrange(true_winner_plot, fraud_beneficiary_plot,
                               labels = c("C", "D"),
                               ncol = 1, nrow = 2,
                               common.legend = TRUE, legend = "right")
save(biased_priors_plot, data_biased_priors_plot, file="code/plots/biased_priors_plot.rdata")