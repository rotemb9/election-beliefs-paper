# this script includes the analysis of the Bayesian model and the empirically obtained priors
# in addition to 4 variations of the model: Hypothesis Desirability, Outcome Desirability, Fraud-Only and Random Beneficiary.
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
# DO NOT use the load_gp_reg option if you haven't run this before or if you changed something in the data
load_gp_reg = FALSE
if (load_gp_reg) {
  warning("loading instead of calculating GP regression")
}

######################
# MODELS SIMULATIONS #
######################
### predictions across all priors
# A = the preferred candidate

## priors
v = round(seq(from = 0,to = 1, by = 0.01),2) # the probability of A winning based on the true (fraudless) votes
f = round(seq(from = 0,to = 1, by = 0.01),2) # the probability of substantial fraud
c = round(seq(from = 0,to = 1, by = 0.1),2) # the probability that fraud favors A (if there's fraud)
u = round(seq(from = 0.5,to = 1, by = 0.05),2) # preference for the model variations
scenario = c("win", "loss")

# df for model
model_df = expand.grid(f = f, v = v, c = c, u = u, scenario = scenario)

## posterior f
# probability of substantial fraud

# BAYESIAN MODEL
bayes_model = function(w,f,v,c) {
  if (w == 1) {
    posterior_f = (f * c) / (v + f * (c - v))
  } else if (w == 0) {
    posterior_f = f * (1 - c) / (1 - v + f * (v - c))
  } else {
    print("w should be 1 for win or 0 for loss of preferred candidate A")
    posterior_f = NaN
  }
  return(posterior_f)
}

# A wins
model_df$f_posterior[model_df$scenario=="win"] = bayes_model(1,model_df$f[model_df$scenario=="win"],model_df$v[model_df$scenario=="win"],model_df$c[model_df$scenario=="win"])
# A loses
model_df$f_posterior[model_df$scenario=="loss"] = bayes_model(0,model_df$f[model_df$scenario=="loss"],model_df$v[model_df$scenario=="loss"],model_df$c[model_df$scenario=="loss"])

# belief update
model_df$f_update = model_df$f_posterior - model_df$f

# when f = 0.5, show heat map for the "desirability effect" (posterior f for loss - posterior f for win) across c and v values
seq_by = 0.1
f_value = 0.5
model_df_posterior = spread(select(model_df[model_df$f==f_value & model_df$v %in% round(seq(0,1,seq_by),5) & model_df$c %in% round(seq(0,1,seq_by),5),], -f_update), scenario, f_posterior)
model_df_posterior$desirability_effect = model_df_posterior$loss - model_df_posterior$win

heat_map_desirability_effect = ggplot(data=model_df_posterior[model_df_posterior$f == f_value,], aes(x=c, y=v, fill = desirability_effect)) +
  geom_tile() +
  scale_y_reverse(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 12), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray")) +
  scale_fill_gradient2(paste("Posterior fraud belief\nloss minus win\n(f=", f_value, ")", sep=""), high = "darkorange", mid = "#f7f7f7", low = "purple")
save(heat_map_desirability_effect, model_df_posterior, file="code/plots/heat_map_desirability_effect.rdata")

# visualize - fraud update, both scenarios
data_for_model_fraud_update = model_df[model_df$c %in% round(seq(0,1,0.2),1) & model_df$v %in% round(seq(0,1,0.2),1),]
model_fraud_update = ggplot(data_for_model_fraud_update) +
  geom_line(aes(x = f, y = f_update, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  scale_x_continuous("Prior fraud belief (f)", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("Fraud belief update", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom") +
  facet_grid(v ~ c, labeller = labeller(v = label_both, c = label_both)) +
  ggtitle("Original Bayesian")
save(model_fraud_update, data_for_model_fraud_update, file="code/plots/model_fraud_update.rdata")

## posterior v
bayes_model_v = function(w,f,v,c) {
  if (w == 1) {
    posterior_v = (v * (1 - f) + v * f * c) / (v + f * (c - v))
  } else if (w == 0) {
    posterior_v = v * f * (1 - c) / (1 - v + f * (v - c))
  } else {
    print("w should be 1 for win or 0 for loss of preferred candidate A")
    posterior_v = NaN
  }
  return(posterior_v)
}
# A wins
model_df$v_posterior[model_df$scenario=="win"] = bayes_model_v(1,model_df$f[model_df$scenario=="win"],model_df$v[model_df$scenario=="win"],model_df$c[model_df$scenario=="win"])
# A loses
model_df$v_posterior[model_df$scenario=="loss"] = bayes_model_v(0,model_df$f[model_df$scenario=="loss"],model_df$v[model_df$scenario=="loss"],model_df$c[model_df$scenario=="loss"])
# belief update
model_df$v_update = model_df$v_posterior - model_df$v

# visualize - v update, both scenario
data_for_model_v_update = model_df[model_df$c %in% round(seq(0,1,0.2),1) & model_df$f %in% round(seq(0,1,0.2),1),]
model_v_update = ggplot(data_for_model_v_update) +
  geom_line(aes(x = v, y = v_update, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  scale_x_continuous("v", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("True vote belief update (posterior - prior)", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 10), legend.position = "bottom") +
  facet_grid(f ~ c, labeller = labeller(f = label_both, c = label_both)) +
  ggtitle("Original Bayesian")
save(model_v_update, data_for_model_v_update, file="code/plots/model_v_update.rdata")

#=====================

### predictions for the empirically obtained priors (follow-up survey)
## read the data
input_path="data/"
filename = paste(input_path, "combined_survey_data_for_analysis_after_exclusion.csv",sep="")
data2fit = read.csv(filename)
data2fit$PrefCand = as.factor(data2fit$PrefCand)
data2fit$PrefCand_present = as.factor(data2fit$PrefCand_present)
data2fit$PrefCand_election_day = as.factor(data2fit$PrefCand_election_day)

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

## regularization
regularization_value = 0.01 # 0 (no regularization, [0,1]), 0.01 [0.01,0.99], 0.05 [0.05,0.95], etc
if (regularization_value > 0) {
  warning(paste("For the model fit, regularizing f,v,c values to ", regularization_value, "-", 1-regularization_value))
}
fvc = select(data2fit, c("FraudProb_scaled","pred_v","pred_c"))
fvc[fvc<regularization_value] = regularization_value
fvc[fvc>(1-regularization_value)] = 1-regularization_value
data2fit$FraudProb_scaled_for_model = fvc$FraudProb_scaled
data2fit$pred_v_for_model = fvc$pred_v
data2fit$pred_c_for_model = fvc$pred_c

## shrink prior beliefs to test expressive responding implications?
shrink_priors = FALSE # TRUE or FALSE
## also shrink fraud beliefs?
shrink_f = 0 # 0 = no; 1 = yes; 2 = only for Republicans (assuming expressive responding would result higher fraud beliefs for Republicans, but not necessarily for Democrats)
plot_dir = "code/plots/"
if (shrink_priors) {
  # shrink priors (assuming expressive responding)
  shrinkage_factor = 0.1
  # save plots and results to a "shrinked_priors" folder to clearly separate results
  plot_dir = "code/plots/shrinked_priors/"
  warning(paste("shrinking prior v and c by ", shrinkage_factor, sep = ""))
  warning(paste("saving plots in "), plot_dir, sep = "")
  data2fit$regularized_v = fvc$pred_v
  data2fit$regularized_c = fvc$pred_c
  data2fit$pred_v_for_model = data2fit$pred_v_for_model * (1 - shrinkage_factor)
  data2fit$pred_c_for_model = data2fit$pred_c_for_model * (1 - shrinkage_factor) + shrinkage_factor
  data2fit$pred_c_for_model[data2fit$pred_c_for_model > (1-regularization_value)] =1-regularization_value
  data2fit$shrinked_v = data2fit$pred_v_for_model
  data2fit$shrinked_c = data2fit$pred_c_for_model
  # plot prior shrinkage
  shrinkage_v = ggplot(data = data2fit, aes(x = shrinked_v, y = regularized_v)) +
    geom_point(alpha = 0.1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  
  shrinkgae_c = ggplot(data = data2fit, aes(x = shrinked_c, y = regularized_c)) +
    geom_point(alpha = 0.1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  
  if (shrink_f) {
    plot_dir = "code/plots/shrinked_beliefs/"
    # prior
    data2fit$regularized_f = fvc$FraudProb_scaled
    if (shrink_f == 1) {
      warning(paste("also shrinking prior and posterior f by ", shrinkage_factor, sep = ""))
      data2fit$FraudProb_scaled_for_model = data2fit$FraudProb_scaled_for_model * (1 - shrinkage_factor)
    } else {
      warning(paste("also shrinking prior and posterior f by ", shrinkage_factor, " only for Republican participants", sep = ""))
      data2fit$FraudProb_scaled_for_model[data2fit$PrefCand=="Rep"] = data2fit$FraudProb_scaled_for_model[data2fit$PrefCand=="Rep"] * (1 - shrinkage_factor)
    }
    data2fit$shrinked_f = data2fit$FraudProb_scaled_for_model
    
    # plot prior shrinkage
    shrinkage_f = ggplot(data = data2fit, aes(x = shrinked_f, y = regularized_f)) +
      geom_point(aes(color = PrefCand), alpha = 0.1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      scale_color_manual("Preferred\ncandidate", values = c("blue3", "red3")) +
      guides(color = guide_legend(override.aes = list(alpha = 1)))
    
    # posterior
    data2fit$reported_posterior = data2fit$FraudProbMap_scaled
    if (shrink_f == 1) {
      data2fit$FraudProbMap_scaled = data2fit$FraudProbMap_scaled * (1 - shrinkage_factor)
    } else {
      data2fit$FraudProbMap_scaled[data2fit$PrefCand=="Rep"] = data2fit$FraudProbMap_scaled[data2fit$PrefCand=="Rep"] * (1 - shrinkage_factor)
    }
    data2fit$shrinked_posterior = data2fit$FraudProbMap_scaled
    
    # plot posterior shrinkage
    shrinkage_posterior = ggplot(data = data2fit, aes(x = shrinked_posterior, y = reported_posterior)) +
      geom_point(aes(color = PrefCand), alpha = 0.1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      scale_color_manual("Preferred\ncandidate", values = c("blue3", "red3")) +
      guides(color = guide_legend(override.aes = list(alpha = 1)))
    
  }
}

## Main Bayesian Model
# loss scenario
data2fit$pred_posterior_f[!data2fit$MapMatchPrefer] = bayes_model(0,data2fit$FraudProb_scaled_for_model[!data2fit$MapMatchPrefer],data2fit$pred_v_for_model[!data2fit$MapMatchPrefer],data2fit$pred_c_for_model[!data2fit$MapMatchPrefer])
# win scenario
data2fit$pred_posterior_f[data2fit$MapMatchPrefer] = bayes_model(1,data2fit$FraudProb_scaled_for_model[data2fit$MapMatchPrefer],data2fit$pred_v_for_model[data2fit$MapMatchPrefer],data2fit$pred_c_for_model[data2fit$MapMatchPrefer])

## compute fraud update (posterior - prior)
data2fit$pred_fraud_update = data2fit$pred_posterior_f - data2fit$FraudProb_scaled_for_model
# number of participants with predictions
warning(paste("Was able to predict fraud belief for", sum(!is.na(data2fit$pred_posterior_f)), "participants"))

### desirability effect heat map with participants' points
seq_by = 0.1 # defines the break size for c and v values between 0 and 1 (e.g., seq_by=0.1 --> 0,0.1,0.2,...,0.9,1)
data2fit$c_for_visualization = round(data2fit$pred_c ,1)
data2fit$v_for_visualization = round(data2fit$pred_v ,1)

data2fit$pref_strength_rep[data2fit$PrefCand_present=="Rep"] = data2fit$PrefStrength_present[data2fit$PrefCand_present=="Rep"]
data2fit$pref_strength_rep[data2fit$PrefCand_present=="Dem"] = 100-data2fit$PrefStrength_present[data2fit$PrefCand_present=="Dem"]

f_value = 0.5
data_for_heat_map_desirability_effect_dots = model_df_posterior[model_df_posterior$f == f_value & model_df_posterior$v %in% round(seq(0,1,0.1),1) & model_df_posterior$c %in% round(seq(0,1,0.1),1),]
heat_map_desirability_effect_dots = ggplot() +
  geom_tile(data = data_for_heat_map_desirability_effect_dots, aes(x=c, y=v, fill = desirability_effect)) +
  geom_point(data = data2fit, aes(x=c_for_visualization, y=v_for_visualization, color = PrefCand), alpha = 0.4, size = 1, position = position_jitter(width = 0.025, height = 0.025)) +
  scale_y_reverse("True vote (v)", breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  scale_x_continuous("Fraud beneficiary (c)", breaks = seq(0, 1, by = seq_by), expand = c(0, 0)) +
  theme(text = element_text(size = 10), panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "gray"), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  scale_fill_gradient2(paste("Posterior fraud belief\nloss minus win\n(f=", f_value, ")", sep=""), high = "#7fbf7b", mid = "#f7f7f7", low = "#fec44f") +
  scale_color_manual("Preferred\ncandidate", values = c("blue3", "red3")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5)))
save(heat_map_desirability_effect_dots,data_for_heat_map_desirability_effect_dots,data2fit, file=paste(plot_dir, "heat_map_desirability_effect_dots.rdata", sep=""))

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
save(heat_map_dems, per_v_c_party, file=paste(plot_dir, "heat_map_dems.rdata", sep=""))

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
save(heat_map_reps, per_v_c_party, file=paste(plot_dir, "heat_map_reps.rdata", sep=""))

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
save(heat_map_pref_dems, pref_v_c_party, file=paste(plot_dir, "heat_map_pref_dems.rdata", sep=""))

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
save(heat_map_pref_reps, pref_v_c_party, file=paste(plot_dir, "heat_map_pref_reps.rdata", sep=""))

### correlations between preference strength from the original survey, and c and v from the follow-up survey
## with v
cor_pref_v_dem = cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], data2fit$pred_v[data2fit$PrefCand =="Dem"], use="complete.obs")
cor_pref_v_rep = cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Rep"], data2fit$pred_v[data2fit$PrefCand =="Rep"], use="complete.obs")
## with c
cor_pref_c_dem = cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Dem"], data2fit$pred_c[data2fit$PrefCand=="Dem"], use="complete.obs")
cor_pref_c_rep = cor.test(data2fit$PrefStrength[data2fit$PrefCand=="Rep"], data2fit$pred_c[data2fit$PrefCand=="Rep"], use="complete.obs")
## Ns
N_dem = sum(data2fit$PrefCand=="Dem")
N_rep = sum(data2fit$PrefCand=="Rep")
# compare Dems vs. Reps
library(psych)
r_comparison_pref_v = paired.r(cor_pref_v_dem$estimate, cor_pref_v_rep$estimate, NULL, N_dem, N_rep)
r_comparison_pref_c = paired.r(cor_pref_c_dem$estimate, cor_pref_c_rep$estimate, NULL, N_dem, N_rep)
detach("package:psych") 

### plot c and v curves on a scale from Biden to Trump, to illustrate the biased priors
data_biased_priors_plot = select(data2fit, PrefCand, Fraudbenefitrump)
data_biased_priors_plot$PrefCand = droplevels(data_biased_priors_plot$PrefCand)
data_biased_priors_plot$true_winner_trump = data2fit$pred_v
data_biased_priors_plot$true_winner_trump[data_biased_priors_plot$PrefCand=="Dem"] = 1 - data_biased_priors_plot$true_winner_trump[data_biased_priors_plot$PrefCand=="Dem"] 

breaks.major <- seq(0,100,10)
breaks.minor <- c(0,50,100)
labels.minor <- c("100%\nBiden", "Don't\nknow", "100%\nTrump")

## plot true vote curves- 0 is Biden and 1 is Trump
true_winner_plot = ggplot(data_biased_priors_plot, aes(x=true_winner_trump*100, fill=PrefCand, color=PrefCand)) +
  geom_histogram(bins = 25, position="identity", alpha = 0.5) +
  scale_color_manual(values = c("blue3", "red3")) +
  scale_fill_manual(values = c("blue3", "red3")) +
  guides(alpha = "none", color = "none", fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "True votes", fill = "Preferred\ncandidate") +
  scale_x_continuous(breaks = c(0,50,100),labels = labels.minor) +
  theme(text = element_text(size = 9))
save(true_winner_plot, data_biased_priors_plot, file=paste(plot_dir, "true_winner_plot.rdata", sep=""))

# plot fraud beneficiary curves
breaks.minor <- c(0,100)
labels.minor <- c("100%\nBiden", "Even\nsplit", "100%\nTrump")
fraud_beneficiary_plot = ggplot(data_biased_priors_plot, aes(x=Fraudbenefitrump, fill=PrefCand, color=PrefCand)) +
  geom_histogram(bins = 25, position="identity", alpha = 0.5) +
  scale_color_manual(values = c("blue3", "red3")) +
  scale_fill_manual(values = c("blue3", "red3")) +
  guides(alpha = "none", color = "none", fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "Fraud beneficiary", fill = "Preferred\ncandidate") +
  scale_x_continuous(breaks = c(0,50,100),labels = labels.minor) +
  theme(text = element_text(size = 9))
save(fraud_beneficiary_plot, data_biased_priors_plot, file=paste(plot_dir, "fraud_beneficiary_plot.rdata", sep=""))

biased_priors_plot = ggarrange(true_winner_plot, fraud_beneficiary_plot,
                               labels = c("B", "C"),
                               ncol = 1, nrow = 2,
                               common.legend = TRUE, legend = "right")
save(biased_priors_plot, data_biased_priors_plot, file=paste(plot_dir, "biased_priors_plot.rdata", sep=""))

### Fraud-only model
fraudonly_model = function(w,f,c) {
  posterior_f = bayes_model(w,f,0.5,c)
  return(posterior_f)
}

## visualize model simulations
model_df$f_posterior_fraudonly[model_df$scenario=="win"] = fraudonly_model(1,model_df$f[model_df$scenario=="win"],model_df$c[model_df$scenario=="win"])
model_df$f_posterior_fraudonly[model_df$scenario=="loss"] = fraudonly_model(0,model_df$f[model_df$scenario=="loss"],model_df$c[model_df$scenario=="loss"])
# belief update
model_df$f_update_fraudonly = model_df$f_posterior_fraudonly - model_df$f

data_for_fraudonly_fraud_update = model_df[model_df$c %in% round(seq(0,1,0.2),1) ,]
data_for_fraudonly_fraud_update = select(data_for_fraudonly_fraud_update, f, c, scenario, f_posterior_fraudonly, f_update_fraudonly)
data_for_fraudonly_fraud_update = distinct(data_for_fraudonly_fraud_update)
fraudonly_fraud_update = ggplot(data_for_fraudonly_fraud_update) +
  geom_line(aes(x = f, y = f_update_fraudonly, color = scenario)) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_x_continuous("Prior fraud belief (f)", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("Fraud belief update", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom") +
  facet_wrap(~c, labeller = labeller(c = label_both)) +
  ggtitle("Fraud-Only")
save(fraudonly_fraud_update,data_for_fraudonly_fraud_update,file=paste(plot_dir, "fraudonly_fraud_update.rdata", sep=""))

### Random beneficiary model
randombeneficiary_model = function(w,f,v) {
  posterior_f = bayes_model(w,f,v,0.5)
  return(posterior_f)
}

## visualize model simulations
model_df$f_posterior_randombeneficiary[model_df$scenario=="win"] = randombeneficiary_model(1,model_df$f[model_df$scenario=="win"],model_df$v[model_df$scenario=="win"])
model_df$f_posterior_randombeneficiary[model_df$scenario=="loss"] = randombeneficiary_model(0,model_df$f[model_df$scenario=="loss"],model_df$v[model_df$scenario=="loss"])
# belief update
model_df$f_update_randombeneficiary = model_df$f_posterior_randombeneficiary - model_df$f

data_for_randombeneficiary_fraud_update = model_df[model_df$v %in% round(seq(0,1,0.2),1) ,]
data_for_randombeneficiary_fraud_update = select(data_for_randombeneficiary_fraud_update, f, v, scenario, f_posterior_randombeneficiary, f_update_randombeneficiary)
data_for_randombeneficiary_fraud_update = distinct(data_for_randombeneficiary_fraud_update)
randombeneficiary_fraud_update = ggplot(data_for_randombeneficiary_fraud_update) +
  geom_line(aes(x = f, y = f_update_randombeneficiary, color = scenario)) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_x_continuous("Prior fraud belief (f)", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("Fraud belief update", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom") +
  facet_wrap(~v, labeller = labeller(v = label_both)) +
  ggtitle("Random Beneficiary")
save(randombeneficiary_fraud_update,data_for_randombeneficiary_fraud_update,file=paste(plot_dir, "randombeneficiary_fraud_update.rdata", sep=""))

### Non-Bayesian model deviations: Hypothesis Desirability and Outcome Desirability
## Hypothesis Desirability (a is the free parameter alpha)
hypothesis_desire_model = function(w,f,v,c,u,a) {
  if (w == 1) {
    f_numerator = f*c + a*(f*c - u*f*c - v*f*c + 2*u*v*f*c)
    f_denominator = v - v*f + f*c + a*(u*v + f*c - u*v*f - u*f*c - v*f*c + 2*u*v*f*c)
  } else if (w == 0) {
    f_numerator = f*(1 - c) + a*(f - u*f - v*f - f*c + 2*u*v*f + u*f*c + v*f*c - 2*u*v*f*c)
    f_denominator = 1 - v + v*f - f*c + a*(1 - u - v + u*v - f*c + u*v*f + u*f*c + v*f*c - 2*u*v*f*c)
  } else {
    print("w should be 1 for win or 0 for loss of preferred candidate A")
    f_numerator = NaN
    f_denominator = NaN
  }
  posterior_f = f_numerator / f_denominator
  posterior_f[is.infinite(posterior_f)] = NaN
  return(posterior_f)
}


## Outcome Desirability (a is the free parameter alpha)
outcome_desire_model = function(w,f,v,c,u,a) {
  if (w == 1) {
    f_numerator = 2*f*c + a*(1 - u)*f*(1 - 2*c)
    f_denominator = 2*(v - v*f + f*c) + a*(1 - u)*(1 - 2*v + 2*v*f - 2*f*c)
  } else if (w == 0) {
    f_numerator = 2*f*(1 - c) - a*u*f*(1 - 2*c)
    f_denominator = 2*(1 - v + v*f - f*c) - a*u*(1 - 2*v + 2*v*f - 2*f*c)
  } else {
    print("w should be 1 for win or 0 for loss of preferred candidate A")
    f_numerator = NaN
    f_denominator = NaN
  }
  posterior_f = f_numerator / f_denominator
  return(posterior_f)
}


## parameter fitting for the OD and HD models
library(Metrics)
data2fit$u = 0.5 + data2fit$PrefStrength / 200
data_for_fitting = select(data2fit, w="MapMatchPrefer", f="FraudProb_scaled_for_model", v="pred_v_for_model", c="pred_c_for_model", "u", empirical_update="DeltaFraudProb_scaled", "PrefStrength")
all_a = seq(0,1,0.001)

## function for a optimization
alpha_optimization = function(all_a,data,indices,models_to_optimize) {
  data_for_fitting = data[indices,]
  data_for_fitting = select(data_for_fitting, w="MapMatchPrefer", f="FraudProb_scaled_for_model", v="pred_v_for_model", c="pred_c_for_model", "u", empirical_update="DeltaFraudProb_scaled", "PrefStrength")
  model_fitting_df = data.frame(a = all_a)
  for (ind in 1:length(all_a)) {
    if ("OD" %in% models_to_optimize) {
    # OD
    data_for_fitting$OD_pred_posterior_f[data_for_fitting$w] = outcome_desire_model(1,data_for_fitting$f[data_for_fitting$w],data_for_fitting$v[data_for_fitting$w],data_for_fitting$c[data_for_fitting$w],data_for_fitting$u[data_for_fitting$w], all_a[ind])
    data_for_fitting$OD_pred_posterior_f[!data_for_fitting$w] = outcome_desire_model(0,data_for_fitting$f[!data_for_fitting$w],data_for_fitting$v[!data_for_fitting$w],data_for_fitting$c[!data_for_fitting$w],data_for_fitting$u[!data_for_fitting$w], all_a[ind])
    range_OD_posteriors = range(data_for_fitting$OD_pred_posterior_f[!is.nan(data_for_fitting$OD_pred_posterior_f)])
    if (range_OD_posteriors[1] < 0 | range_OD_posteriors[2] > 1) {
      print("a:")
      print(all_a[ind])
      print("range:")
      print(range(data_for_fitting$OD_pred_posterior_f[!is.nan(data_for_fitting$OD_pred_posterior_f)]))
    }

    data_for_fitting$OD_pred_update = data_for_fitting$OD_pred_posterior_f - data_for_fitting$f
    model_fitting_df$OD_n = sum(!is.na(data_for_fitting$OD_pred_posterior_f))
    model_fitting_df$OD_sse[ind] = sse(data_for_fitting$OD_pred_update[!is.nan(data_for_fitting$OD_pred_update)], data_for_fitting$empirical_update[!is.nan(data_for_fitting$OD_pred_update)])
    }
    if ("HD" %in% models_to_optimize) {
    # HD
    data_for_fitting$HD_pred_posterior_f[data_for_fitting$w] = hypothesis_desire_model(1,data_for_fitting$f[data_for_fitting$w],data_for_fitting$v[data_for_fitting$w],data_for_fitting$c[data_for_fitting$w],data_for_fitting$u[data_for_fitting$w], all_a[ind])
    data_for_fitting$HD_pred_posterior_f[!data_for_fitting$w] = hypothesis_desire_model(0,data_for_fitting$f[!data_for_fitting$w],data_for_fitting$v[!data_for_fitting$w],data_for_fitting$c[!data_for_fitting$w],data_for_fitting$u[!data_for_fitting$w], all_a[ind])
    data_for_fitting$HD_pred_update = data_for_fitting$HD_pred_posterior_f - data_for_fitting$f
    model_fitting_df$HD_n = sum(!is.na(data_for_fitting$HD_pred_posterior_f))
    model_fitting_df$HD_sse[ind] = sse(data_for_fitting$HD_pred_update[!is.nan(data_for_fitting$HD_pred_update)], data_for_fitting$empirical_update[!is.nan(data_for_fitting$HD_pred_update)])
    }
  }
  if ("OD" %in% models_to_optimize) {
    ## OD
      # get the a with the minimal SSE across fraud update values
      OD_optimized_alpha = model_fitting_df$a[model_fitting_df$OD_sse==min(model_fitting_df$OD_sse)]
    # make sure the model was able to predict for all participants
    if (length(unique(model_fitting_df$OD_n)) > 1 | unique(model_fitting_df$OD_n)!=nrow(data_for_fitting)) {
      warning("not all values of alpha resulted valid OD predictions for all participants- please check!")
    }
  }
  if ("HD" %in% models_to_optimize) {
    ## HD
    # get the a with the minimal SSE across fraud update values
    HD_optimized_alpha = model_fitting_df$a[model_fitting_df$HD_sse==min(model_fitting_df$HD_sse)]  
    # make sure the model was able to predict for all participants
    if (length(unique(model_fitting_df$HD_n)) > 1 | unique(model_fitting_df$HD_n)!=nrow(data_for_fitting)) {
      warning("not all values of alpha resulted valid HD predictions for all participants- please check!")
    }
  }
  
  ## create output variable as a list
  if ("OD" %in% models_to_optimize & "HD" %in% models_to_optimize) {
  optimized_alpha = list(OD = OD_optimized_alpha, HD = HD_optimized_alpha, df = model_fitting_df, fitted_a_vals = all_a)
  }
  else if ("OD" %in% models_to_optimize) {
    optimized_alpha = list(OD = OD_optimized_alpha, df = model_fitting_df, fitted_a_vals = all_a)
  }
  else if ("HD" %in% models_to_optimize) {
    optimized_alpha = list(HD = HD_optimized_alpha, df = model_fitting_df, fitted_a_vals = all_a)
  }
  return (optimized_alpha)
}

## optimize a for empirical data
indices = 1:nrow(data2fit)
optimized_alpha = alpha_optimization(all_a,data2fit,indices, c("HD","OD"))
model_fitting_df = optimized_alpha$df
# plot SSE as a function of a
ss_across_a_vals_OD = ggplot() + geom_line(aes(model_fitting_df$a, model_fitting_df$OD_sse)) + ggtitle("OD model") + labs(x = "a", y = "SSE")
ss_across_a_vals_HD = ggplot() + geom_line(aes(model_fitting_df$a, model_fitting_df$HD_sse)) + ggtitle("HD model") + labs(x = "a", y = "SSE")

a_optimize_OD = function(all_a,data,indices) {
  optimized_alpha = alpha_optimization(all_a,data2fit,indices,"OD")
  optimized_alpha_OD = optimized_alpha$OD
  return(optimized_alpha_OD)
}

## bootstrapping for a distribution
library(boot)
bootstrap_results = boot(data = data2fit, statistic = a_optimize_OD, R = 10000, all_a = seq(0,1,0.001))
save(bootstrap_results, file="bootstrap_results")
# get bootstrap CIs
bootstrap_CI = boot.ci(bootstrap_results)
detach("package:boot") 

## visualize model simulations
model_df$f_posterior_OD[model_df$scenario=="win"] = outcome_desire_model(1,model_df$f[model_df$scenario=="win"],model_df$v[model_df$scenario=="win"],model_df$c[model_df$scenario=="win"],model_df$u[model_df$scenario=="win"],optimized_alpha$OD)
model_df$f_posterior_OD[model_df$scenario=="loss"] = outcome_desire_model(0,model_df$f[model_df$scenario=="loss"],model_df$v[model_df$scenario=="loss"],model_df$c[model_df$scenario=="loss"],model_df$u[model_df$scenario=="loss"],optimized_alpha$OD)
# belief update
model_df$f_update_OD = model_df$f_posterior_OD - model_df$f

data_for_OD_fraud_update_u055 = model_df[model_df$c %in% round(seq(0,1,0.2),1) & model_df$v %in% round(seq(0,1,0.2),1) & model_df$u == 0.55,]
OD_fraud_update_u055 = ggplot(data_for_OD_fraud_update_u055) +
  geom_line(aes(x = f, y = f_update_OD, color = scenario)) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_x_continuous("Prior fraud belief (f)", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("Fraud belief update", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 10), legend.position = "bottom") +
  facet_grid(v ~ c, labeller = labeller(v = label_both, c = label_both)) +
  ggtitle("Outcome Desirability model, u = 0.55")
save(OD_fraud_update_u055,data_for_OD_fraud_update_u055,file=paste(plot_dir, "OD_fraud_update_u055.rdata", sep=""))

data_for_OD_fraud_update_u095 = model_df[model_df$c %in% round(seq(0,1,0.2),1) & model_df$v %in% round(seq(0,1,0.2),1) & model_df$u == 0.95,]
OD_fraud_update_u095 = ggplot(data_for_OD_fraud_update_u095) +
  geom_line(aes(x = f, y = f_update_OD, color = scenario)) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_x_continuous("Prior fraud belief (f)", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("Fraud belief update", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 9), legend.position = "bottom") +
  facet_grid(v ~ c, labeller = labeller(v = label_both, c = label_both)) +
  ggtitle("Outcome Desirability, u = 0.95")
save(OD_fraud_update_u095,data_for_OD_fraud_update_u095,file=paste(plot_dir, "OD_fraud_update_u095.rdata", sep=""))

### model comparison
## organize priors and model predictions across models
priors = select(data2fit, w="MapMatchPrefer", f_original = "FraudProb_scaled",f="FraudProb_scaled_for_model", v="pred_v_for_model", c="pred_c_for_model", "u", "WinProb", "PrefCand", "PrefStrength",empirical_fraud_update = "DeltaFraudProb_scaled" ,empirical="FraudProbMap_scaled", bayes="pred_posterior_f")

#priors$WinProb = priors$WinProb/100

models_comparison = priors

# hypothesis desirability
models_comparison$hypodesire[models_comparison$w] = hypothesis_desire_model(1,models_comparison$f[models_comparison$w],models_comparison$v[models_comparison$w],models_comparison$c[models_comparison$w], models_comparison$u[models_comparison$w], optimized_alpha$HD)
models_comparison$hypodesire[!models_comparison$w] = hypothesis_desire_model(0,models_comparison$f[!models_comparison$w],models_comparison$v[!models_comparison$w],models_comparison$c[!models_comparison$w], models_comparison$u[!models_comparison$w], optimized_alpha$HD)
#hist(models_comparison$hypodesire)
# outcome desirability
models_comparison$outcomedesire[models_comparison$w] = outcome_desire_model(1,models_comparison$f[models_comparison$w],models_comparison$v[models_comparison$w],models_comparison$c[models_comparison$w], models_comparison$u[models_comparison$w], optimized_alpha$OD)
models_comparison$outcomedesire[!models_comparison$w] = outcome_desire_model(0,models_comparison$f[!models_comparison$w],models_comparison$v[!models_comparison$w],models_comparison$c[!models_comparison$w], models_comparison$u[!models_comparison$w], optimized_alpha$OD)
#hist(models_comparison$outcomedesire)
# fraud only
models_comparison$fraudonly[models_comparison$w] = fraudonly_model(1,models_comparison$f[models_comparison$w],models_comparison$c[models_comparison$w])
models_comparison$fraudonly[!models_comparison$w] = fraudonly_model(0,models_comparison$f[!models_comparison$w],models_comparison$c[!models_comparison$w])
#hist(models_comparison$fraudonly)
# random fraud beneficiary
models_comparison$randombeneficiary[models_comparison$w] = randombeneficiary_model(1,models_comparison$f[models_comparison$w],models_comparison$v[models_comparison$w])
models_comparison$randombeneficiary[!models_comparison$w] = randombeneficiary_model(0,models_comparison$f[!models_comparison$w],models_comparison$v[!models_comparison$w])
#hist(models_comparison$randombeneficiary)

# gather model prediction to compute cor and rmse
model_names = c("empirical","bayes","hypodesire","outcomedesire","fraudonly","randombeneficiary")
models_comparison_long = gather(models_comparison, model, posterior_f, all_of(model_names),factor_key=TRUE)
models_comparison_long$fraud_update = models_comparison_long$posterior_f - models_comparison_long$f
models_comparison_long$fraud_update[models_comparison_long$model == "empirical"] = data2fit$DeltaFraudProb_scaled
empirical_fraud_update = data2fit$DeltaFraudProb_scaled
# get residuals for each model
models_comparison_long$model_residuals = models_comparison_long$empirical_fraud_update - models_comparison_long$fraud_update 

model_comparison_table = data.frame(model=model_names)
for (ind in 1:length(model_names)) {
  cur_model = model_names[ind]
  cur_data = models_comparison_long[models_comparison_long$model == cur_model,]

  # across the sample
  cortest_all = cor.test(cur_data$fraud_update, empirical_fraud_update, )
  model_comparison_table$cor_all[model_comparison_table$model == cur_model] = round(cortest_all$estimate,3)
  model_comparison_table$RMSE_all[model_comparison_table$model == cur_model] = round(rmse(cur_data$fraud_update[!is.nan(cur_data$fraud_update)], empirical_fraud_update[!is.nan(cur_data$fraud_update)]),3)
  model_comparison_table$SSE_all[model_comparison_table$model == cur_model] = round(sse(cur_data$fraud_update[!is.nan(cur_data$fraud_update)], empirical_fraud_update[!is.nan(cur_data$fraud_update)]),3)
  model_comparison_table$N_all[model_comparison_table$model == cur_model] = sum(!is.nan(cur_data$fraud_update))
  # win
  cortest_win = cor.test(cur_data$fraud_update[cur_data$w], empirical_fraud_update[cur_data$w])
  model_comparison_table$cor_win[model_comparison_table$model == cur_model] = round(cortest_win$estimate,3)
  model_comparison_table$RMSE_win[model_comparison_table$model == cur_model] = round(rmse(cur_data$fraud_update[cur_data$w & !is.nan(cur_data$fraud_update)], empirical_fraud_update[cur_data$w & !is.nan(cur_data$fraud_update)]),3)
  model_comparison_table$N_win[model_comparison_table$model == cur_model] = sum(!is.nan(cur_data$fraud_update) & cur_data$w)  
  # loss
  cortest_loss = cor.test(cur_data$fraud_update[!cur_data$w], empirical_fraud_update[!cur_data$w])
  model_comparison_table$cor_loss[model_comparison_table$model == cur_model] = round(cortest_loss$estimate,3)
  model_comparison_table$RMSE_loss[model_comparison_table$model == cur_model] = round(rmse(cur_data$fraud_update[!cur_data$w & !is.nan(cur_data$fraud_update)], empirical_fraud_update[!cur_data$w & !is.nan(cur_data$fraud_update)]),3)
  model_comparison_table$N_loss[model_comparison_table$model == cur_model] = sum(!is.nan(cur_data$fraud_update) & !cur_data$w)
  # dem
  cortest_dem = cor.test(cur_data$fraud_update[cur_data$PrefCand == "Dem"], empirical_fraud_update[cur_data$PrefCand == "Dem"])
  model_comparison_table$cor_dem[model_comparison_table$model == cur_model] = round(cortest_dem$estimate,3)
  model_comparison_table$RMSE_dem[model_comparison_table$model == cur_model] = round(rmse(cur_data$fraud_update[cur_data$PrefCand == "Dem" & !is.nan(cur_data$fraud_update)], empirical_fraud_update[cur_data$PrefCand == "Dem" & !is.nan(cur_data$fraud_update)]),3)
  model_comparison_table$N_dem[model_comparison_table$model == cur_model] = sum(!is.nan(cur_data$fraud_update) & cur_data$PrefCand == "Dem")   
  # rep
  cortest_rep = cor.test(cur_data$fraud_update[cur_data$PrefCand == "Rep"], empirical_fraud_update[cur_data$PrefCand == "Rep"])
  model_comparison_table$cor_rep[model_comparison_table$model == cur_model] = round(cortest_rep$estimate,3)
  model_comparison_table$RMSE_rep[model_comparison_table$model == cur_model] = round(rmse(cur_data$fraud_update[cur_data$PrefCand == "Rep" & !is.nan(cur_data$fraud_update)], empirical_fraud_update[cur_data$PrefCand == "Rep" & !is.nan(cur_data$fraud_update)]),3)
  model_comparison_table$N_rep[model_comparison_table$model == cur_model] = sum(!is.nan(cur_data$fraud_update) & cur_data$PrefCand == "Rep")   
  
  # desirability effects
  # win - mean
  model_comparison_table$mean_update_win[model_comparison_table$model == cur_model] = round(mean(cur_data$fraud_update[cur_data$w], na.rm=T),3)
  # loss - mean
  model_comparison_table$mean_update_loss[model_comparison_table$model == cur_model] = round(mean(cur_data$fraud_update[!cur_data$w], na.rm=T),3)
   # win - slope
  model_comparison_table$cor_preference_update_win[model_comparison_table$model == cur_model] = round(cor(cur_data$fraud_update[cur_data$w], cur_data$PrefStrength[cur_data$w]),3)
  # loss - slope
  model_comparison_table$cor_preference_update_loss[model_comparison_table$model == cur_model] = round(cor(cur_data$fraud_update[!cur_data$w], cur_data$PrefStrength[!cur_data$w]),3)
  
  # regress age on the model residuals
  cor_raw_res_age = cor.test(cur_data$model_residuals, data2fit$age.x)
  model_comparison_table$cor_raw_res_age[model_comparison_table$model == cur_model] = round(cor_raw_res_age$estimate,3)
  model_comparison_table$cor_raw_res_age_p[model_comparison_table$model == cur_model] = round(cor_raw_res_age$p.value,5)
  cor_squared_res_age = cor.test(cur_data$model_residuals^2, data2fit$age.x)
  model_comparison_table$cor_squared_res_age[model_comparison_table$model == cur_model] = round(cor_squared_res_age$estimate,3)
  model_comparison_table$cor_squared_res_age_p[model_comparison_table$model == cur_model] = round(cor_squared_res_age$p.value,5)  
  # compare the residuals between Dems and Reps
  t_raw_res_prefcand = t.test(cur_data$model_residuals[data2fit$PrefCand=="Dem"], cur_data$model_residuals[data2fit$PrefCand=="Rep"])
  model_comparison_table$t_raw_res_prefcand[model_comparison_table$model == cur_model] = round(t_raw_res_prefcand$statistic,3)
  model_comparison_table$t_raw_res_prefcand_p[model_comparison_table$model == cur_model] = round(t_raw_res_prefcand$p.value,5)
  t_squared_res_prefcand = t.test(cur_data$model_residuals[data2fit$PrefCand=="Dem"]^2, cur_data$model_residuals[data2fit$PrefCand=="Rep"]^2)
  model_comparison_table$t_squared_res_prefcand[model_comparison_table$model == cur_model] = round(t_squared_res_prefcand$statistic,3)
  model_comparison_table$t_squared_res_prefcand_p[model_comparison_table$model == cur_model] = round(t_squared_res_prefcand$p.value,5)
  # regress pref strength on the model residuals
  cor_raw_res_prefstrength = cor.test(cur_data$model_residuals, data2fit$PrefStrength)
  model_comparison_table$cor_raw_res_prefstrength[model_comparison_table$model == cur_model] = round(cor_raw_res_prefstrength$estimate,3)
  model_comparison_table$cor_raw_res_prefstrength_p[model_comparison_table$model == cur_model] = round(cor_raw_res_prefstrength$p.value,5)
  cor_squared_res_prefstrength = cor.test(cur_data$model_residuals^2, data2fit$PrefStrength)
  model_comparison_table$cor_squared_res_prefstrength[model_comparison_table$model == cur_model] = round(cor_squared_res_prefstrength$estimate,3)
  model_comparison_table$cor_squared_res_prefstrength_p[model_comparison_table$model == cur_model] = round(cor_squared_res_prefstrength$p.value,5)  
  
}

## visualize model fit comparison
model_comparison_table$model = factor(model_comparison_table$model, levels = model_names)
#model_comparison_table_for_visualization_rmse = select(model_comparison_table, "model", contains("RMSE"))
#model_comparison_table_long = gather(model_comparison_table_for_visualization_rmse, sample, RMSE, RMSE_all:RMSE_rep,factor_key=TRUE)

# F test comparing the Base model and the OD model
OD_ss = model_comparison_table$SSE_all[model_comparison_table$model == "outcomedesire"]
OD_df = 1
OD_res_df = model_comparison_table$N_all[model_comparison_table$model == "outcomedesire"] - OD_df
ms_res = OD_ss / OD_res_df
Base_ss = model_comparison_table$SSE_all[model_comparison_table$model == "bayes"]
ss_diff = Base_ss - OD_ss
df_diff = 1 # number of additional free parameters for the bigger (OD) model
ms_diff = ss_diff / df_diff

F_OD = ms_diff / ms_res
ftest_pval_OD = pf(F_OD, df_diff, OD_res_df, lower.tail = FALSE)

# F test comparing the Base model and the HD model
HD_ss = model_comparison_table$SSE_all[model_comparison_table$model == "hypodesire"]
HD_df = 1
HD_res_df = model_comparison_table$N_all[model_comparison_table$model == "hypodesire"] - HD_df
ms_res = HD_ss / HD_res_df
Base_ss = model_comparison_table$SSE_all[model_comparison_table$model == "bayes"]
ss_diff = Base_ss - HD_ss
df_diff = 1 # number of additional free parameters for the bigger (HD) model
ms_diff = ss_diff / df_diff

F_HD = ms_diff / ms_res
ftest_pval_HD = pf(F_HD, df_diff, HD_res_df, lower.tail = FALSE)


## compare the fit of the Bayesian model between the win and loss scenarios
library(psych)
cor_empirical_predicted_loss = model_comparison_table$cor_loss[model_comparison_table$model=="bayes"]
cor_empirical_predicted_win = model_comparison_table$cor_win[model_comparison_table$model=="bayes"]
N_loss = model_comparison_table$N_loss[model_comparison_table$model=="bayes"]
N_win = model_comparison_table$N_win[model_comparison_table$model=="bayes"]
r_comparison_bayes_win_loss = paired.r(cor_empirical_predicted_loss, cor_empirical_predicted_win, NULL, N_loss, N_win)
detach("package:psych") 

## statistically test desirability effects in the predictions of the Bayesian model
# orgnize and z -score variables
data2fit$numericEndTime = as.numeric(data2fit$T_end)
data2fit$PrefStrength_z = scale(data2fit$PrefStrength, center = TRUE, scale=TRUE)
data2fit$WinProb_z = scale(data2fit$WinProb, center = TRUE, scale=TRUE)
data2fit$FraudProb_z = scale(data2fit$FraudProb, center = TRUE, scale=TRUE)
data2fit$pred_fraud_update_z = scale(data2fit$pred_fraud_update, center = TRUE, scale=TRUE)
data2fit$age_z = scale(data2fit$age, center = TRUE, scale=TRUE)
data2fit$numericEndTime_z = scale(data2fit$numericEndTime, center = TRUE, scale=TRUE)
data2fit$PrefCand = as.factor(data2fit$PrefCand)
contrasts(data2fit$PrefCand) = contr.sum(2)

# test fraud belief updating
data2fit$pred_fraud_update_unscaled = data2fit$pred_fraud_update*100
#print("Predicted fraud belief update")
# loss
#print(paste("LOSS scenario: N = ", sum(!data2fit$MapMatchPrefer), "; mean: ",round(mean(data2fit$pred_fraud_update_unscaled[!data2fit$MapMatchPrefer]),2)," SD: ", round(sd(data2fit$pred_fraud_update_unscaled[!data2fit$MapMatchPrefer]),2), sep=""))
loss_t = t.test(data2fit$pred_fraud_update_unscaled[!data2fit$MapMatchPrefer])
#print(paste("t-test fraud belief update loss scenario: t(df =", loss_t$parameter,  ") =", round(loss_t$statistic,2), ", p =", round(loss_t$p.value,4),  sep = " ")) 
model_belief_update_loss = lm(pred_fraud_update_unscaled ~ 1 + FraudProb + PrefCand + PrefStrength + WinProb +  age.x, data=data2fit[!data2fit$MapMatchPrefer,], na.action=na.omit)
#summary(model_belief_update_loss)
# win
#print(paste("WIN scenario: N = ", sum(data2fit$MapMatchPrefer), "; mean: ",round(mean(data2fit$pred_fraud_update_unscaled[data2fit$MapMatchPrefer]),2)," SD: ", round(sd(data2fit$pred_fraud_update_unscaled[data2fit$MapMatchPrefer]),2), sep=""))
win_t = t.test(data2fit$pred_fraud_update_unscaled[data2fit$MapMatchPrefer])
#print(paste("t-test fraud belief update win scenario: t(df =", win_t$parameter,  ") =", round(win_t$statistic,2), ", p =", round(win_t$p.value,4),  sep = " ")) 
model_belief_update_win = lm(pred_fraud_update_unscaled ~ 1 + FraudProb + PrefCand + PrefStrength + WinProb +  age.x, data=data2fit[data2fit$MapMatchPrefer,], na.action=na.omit)
#summary(model_belief_update_win)
  
## Compare the patterns of predictions across the models
# choose models to plot
models_for_plots = c("bayes","outcomedesire","fraudonly","randombeneficiary")
models_labels_for_plots = c("Original Bayesian", "Outcome Desirability", "Fraud-Only", "Random Beneficiary")
models_comparison_long_for_plot = models_comparison_long[models_comparison_long$model %in% models_for_plots,]
models_comparison_long_for_plot$model = droplevels(models_comparison_long_for_plot$model)
levels(models_comparison_long_for_plot$model) = models_labels_for_plots 

# plot predicted vs. actual values
# posterior fraud belief
models_comparison_long_for_plot$empirical_posterior_f = data2fit$FraudProbMap_scaled
scatter_predicted_actual_f_posterior_models = ggplot(data = models_comparison_long_for_plot, aes(x = posterior_f, y = empirical_posterior_f, color = w)) +
  geom_abline(slope = 1,intercept=0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.6) +
  geom_point(alpha = 0.1) +
  scale_color_manual(name = "Outcome", values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_y_continuous("Empirical posterior fraud belief", breaks = seq(0,1,0.2), limits = c(0,1)) +
  scale_x_continuous("Predicted posterior fraud belief", breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(legend.position = "bottom", text = element_text(size = 9), panel.background = element_rect(fill = "white")) +
  facet_wrap(~ model, nrow = 2)

# fraud update
scatter_predicted_actual_f_update_models = ggplot(data = models_comparison_long_for_plot, aes(x = fraud_update, y = empirical_fraud_update, color = w)) +
  geom_abline(slope = 1,intercept=0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.6, fullrange=TRUE) +
  geom_point(alpha = 0.1) +
  scale_color_manual(name = "Outcome", values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_y_continuous("Empirical fraud belief update", breaks = seq(-1,1,0.2), limits = c(-1,1)) +
  scale_x_continuous("Predicted fraud belief update", breaks = seq(-1,1,0.2), limits = c(-1,1)) +
  theme(legend.position = "bottom", text = element_text(size = 9), panel.background = element_rect(fill = "white")) +
  facet_wrap(~ model, nrow = 2)
save(models_comparison_long_for_plot, scatter_predicted_actual_f_update_models, file=paste(plot_dir, "scatter_predicted_actual_f_update_models.rdata", sep=""))

## reproduce the empirical findings with the predicted values based on the model
pred_fraudUpdateByPref_models = ggplot(models_comparison_long_for_plot, aes(x = PrefStrength, y = fraud_update, color = w)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.6, linetype = 0) +
  geom_line(stat="smooth",method = "lm", size = 1, aes(linetype = "longdash")) +
  geom_point(alpha = 0.1, shape = 20) +
  geom_line(aes(x = PrefStrength, y = empirical_fraud_update, linetype = "solid"), method = "lm", alpha=0.6, stat="smooth", size=1) +
  scale_y_continuous(breaks=seq(-1,1,0.2), limits = c(-1,1), labels = seq(-100, 100, 20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_linetype_manual(values = c(2, 1), labels = c("Model", "Data")) +
  labs(x = "Preference strength", y = "Predicted fraud belief update\n(after - before)", color = "Outcome", linetype = "") +
  guides(linetype = guide_legend(override.aes = list(size = 0.5))) +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white")) +
  facet_wrap(~ model, nrow = 2)
save(models_comparison_long_for_plot, pred_fraudUpdateByPref_models, file=paste(plot_dir, "pred_fraudUpdateByPref_models.rdata", sep=""))

# the above for the Bayesian model only
if (shrink_priors) {
  title_for_plot = "Transformed priors"
} else {
  title_for_plot = "Reported priors"
}
pred_fraudUpdateByPref_bayesian = ggplot(models_comparison_long_for_plot[models_comparison_long_for_plot$model == "Original Bayesian",], aes(x = PrefStrength, y = fraud_update, color = w)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.6, linetype = 0) +
  geom_line(stat="smooth",method = "lm", size = 1, aes(linetype = "longdash")) +
  geom_point(alpha = 0.1, shape = 20) +
  geom_line(aes(x = PrefStrength, y = empirical_fraud_update, linetype = "solid"), method = "lm", alpha=0.6, stat="smooth", size=1) +
  scale_y_continuous(breaks=seq(-1,1,0.2), limits = c(-1,1), labels = seq(-100, 100, 20)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_linetype_manual(values = c(2, 1), labels = c("Model", "Data")) +
  labs(x = "Preference strength", y = "Predicted fraud belief update\n(after - before)", color = "Outcome", linetype = "") +
  guides(linetype = guide_legend(override.aes = list(size = 0.5))) +
  ggtitle(title_for_plot) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white"))
save(models_comparison_long_for_plot, pred_fraudUpdateByPref_bayesian, file=paste(plot_dir, "pred_fraudUpdateByPref_bayesian.rdata", sep=""))

pred_fraudUpdateByPred_models = ggplot(models_comparison_long_for_plot, aes(x = WinProb, y = fraud_update, color = w)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_smooth(method = "lm", alpha = 0.6, linetype = 0) +
  geom_line(stat="smooth",method = "lm", size = 1, aes(linetype = "longdash")) +
  geom_point(alpha = 0.1, shape = 20) +
  geom_line(aes(x = WinProb, y = empirical_fraud_update, linetype = "solid"), method = "lm",alpha=0.6, stat="smooth", size=1) +
  scale_y_continuous(breaks=seq(-1,1,0.2), limits = c(-1,1), labels = seq(-100, 100, 20)) +
  scale_x_continuous(breaks=seq(0, 100, 10)) +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  scale_linetype_manual(values = c(2, 1), labels = c("Model", "Data")) +
  labs(x = "Prior win belief", y = "Predicted fraud belief update\n(after - before)", color = "Outcome", linetype = "") +
  guides(linetype = guide_legend(override.aes = list(size = 0.5))) +
  theme(text = element_text(size = 9), legend.position = "bottom", panel.background = element_rect(fill = "white")) +
  facet_wrap(~ model, nrow = 2)
save(models_comparison_long_for_plot, pred_fraudUpdateByPred_models, file=paste(plot_dir, "pred_fraudUpdateByPred_models.rdata", sep=""))

## prior and posterior fraud belief by map
models_comparison_long_for_plot$Map = "Dem"
models_comparison_long_for_plot$Map[models_comparison_long_for_plot$w==1 & models_comparison_long_for_plot$PrefCand=="Rep"] = "Rep"
models_comparison_long_for_plot$Map[models_comparison_long_for_plot$w==0 & models_comparison_long_for_plot$PrefCand=="Dem"] = "Rep"
prepost_data = select(models_comparison_long_for_plot,"PrefCand", "Map", "f_original","posterior_f", "model")
# rescale to 0 - 100 to reproduce figures
prepost_data$f_original = prepost_data$f_original * 100
prepost_data$posterior_f = prepost_data$posterior_f * 100
prepost_data = gather(prepost_data, FraudTimePoint, FraudJudgement, f_original:posterior_f,factor_key = FALSE)
prepost_data$FraudTimePoint[prepost_data$FraudTimePoint == "f_original"] = "Prior"
prepost_data$FraudTimePoint[prepost_data$FraudTimePoint == "posterior_f"] = "Posterior"
prepost_data$FraudTimePoint = factor(prepost_data$FraudTimePoint, levels = c("Prior", "Posterior"))
#prepost_data$participant_num = as.factor(prepost_data$participant_num)
prepost_data_sum = aggregate(prepost_data$FraudJudgement,list(TimePoint = prepost_data$FraudTimePoint, PrefCand = prepost_data$PrefCand, Map = prepost_data$Map, model = prepost_data$model), function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
prepost_data_sum = cbind(prepost_data_sum[,1:4], as.data.frame(prepost_data_sum$x))
levels(prepost_data_sum$PrefCand) <- list(Democrats  = "Dem", Republicans = "Rep")
levels(prepost_data_sum$Map) <- list(Biden  = "Dem", Trump = "Rep")
levels(prepost_data_sum$TimePoint) <- list(Prior  = "Before", Posterior = "After")
prepost_data_sum$SEM = prepost_data_sum$sd/sqrt(prepost_data_sum$n)
#data_for_points = select(prepost_data, c(FraudTimePoint, FraudJudgement, Map, PrefCand, participant_num))
data_for_points = select(prepost_data, c(FraudTimePoint, FraudJudgement, Map, PrefCand, model))
levels(data_for_points$PrefCand) <- list(Democrats  = "Dem", Republicans = "Rep")
levels(data_for_points$Map) <- list(Biden  = "Dem", Trump = "Rep")
levels(data_for_points$FraudTimePoint) <- list(Prior  = "Before", Posterior = "After")
map_labels = c(Dem = "Biden wins", Rep = "Trump wins")
prepost_data_sum$PrefCand[prepost_data_sum$PrefCand == "Dem"] = "Democrats"
prepost_data_sum$PrefCand[prepost_data_sum$PrefCand == "Rep"] = "Republicans"
data_for_points$PrefCand[data_for_points$PrefCand == "Dem"] = "Democrats"
data_for_points$PrefCand[data_for_points$PrefCand == "Rep"] = "Republicans"

#cur_model = "Original Bayesian" # "Original Bayesian" / "Outcome Desirability" / "Fraud-Only" / "Random Beneficiary"
partisan_direction_desirability_effect_prepost_models = vector('list', length(models_for_plots))
for (model_ind in 1:length(models_for_plots)) {
  cur_model = models_for_plots[model_ind]
  cur_model_label = models_labels_for_plots[model_ind]
  partisan_direction_desirability_effect_prepost_models[[model_ind]] = ggplot(data=prepost_data_sum[prepost_data_sum$model == cur_model_label,], aes(x=TimePoint, y=mean, fill = PrefCand)) +
    geom_bar(width=1,colour="black",position=position_dodge(1), stat="identity", alpha=0.15) + # Bar plot
    geom_errorbar(position=position_dodge(0.8), width=1/8, aes(ymin=mean-SEM, ymax=mean+SEM))  + # add error bar of SEM
    geom_point(data=data_for_points[data_for_points$model == cur_model_label,], aes(y = FraudJudgement, x = FraudTimePoint, color = PrefCand), position=position_jitterdodge(jitter.width = 0.7, dodge.width = 0.8), alpha=0.2, size = 0.8)+
    scale_x_discrete(name = "Time point and scenario") +
    scale_y_continuous("Fraud belief",breaks=seq(0, 100, 10), limits = c(0, 100)) +
    scale_fill_manual("Preferred candidate", values=c("blue4","red4")) + # color of bars
    scale_color_manual("Preferred candidate", values=c("blue4","red4")) + # color of dots
    theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5), legend.position="bottom", panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), strip.placement = "outside", strip.background = element_rect(fill = "white")) +
    ggtitle(cur_model_label) +
    facet_grid(PrefCand~Map, switch = "x", labeller = labeller(Map = map_labels))
}
save(partisan_direction_desirability_effect_prepost_models, prepost_data_sum, data_for_points, file=paste(plot_dir, "partisan_direction_desirability_effect_prepost_models.rdata", sep=""))

## compare plots for fraud belief update as a function of prior fraud belief
if (load_gp_reg) {
  load(paste(plot_dir, "GP_reg_pred_models.rdata", sep = ""))
} else {
  GP_reg_pred_models = data.frame(w = data2fit$MapMatchPrefer, f = data2fit$FraudProb_scaled_for_model, f_original = data2fit$FraudProb_scaled)
  for (model_ind in 1:length(model_names)) {
    cur_model = model_names[model_ind]
    print(cur_model)
    cur_model_pred_fraud_update = models_comparison_long$fraud_update[models_comparison_long$model == cur_model]
    print("loss")
    model_gaupro_loss = GauPro(GP_reg_pred_models$f[!GP_reg_pred_models$w], cur_model_pred_fraud_update[!GP_reg_pred_models$w], parallel=TRUE)
    GP_reg_pred_models[!GP_reg_pred_models$w,cur_model] = model_gaupro_loss$predict(GP_reg_pred_models$f[!GP_reg_pred_models$w])
    print("win")
    model_gaupro_win = GauPro(GP_reg_pred_models$f[GP_reg_pred_models$w], cur_model_pred_fraud_update[GP_reg_pred_models$w], parallel=TRUE)
    GP_reg_pred_models[GP_reg_pred_models$w,cur_model] = model_gaupro_win$predict(GP_reg_pred_models$f[GP_reg_pred_models$w])
  }
  save(GP_reg_pred_models, file=paste(plot_dir, "GP_reg_pred_models.rdata", sep=""))
}
GP_reg_pred_models_long = gather(GP_reg_pred_models, model, GP_pred_update, model_names[1]:model_names[length(model_names)],factor_key=TRUE)
GP_reg_pred_models_long$pred_update = models_comparison_long$fraud_update
GP_reg_pred_models_long$GP_empirical_update = GP_reg_pred_models_long$GP_pred_update[GP_reg_pred_models_long$model=="empirical"]
pred_fraudUpdate_by_priorFraud = ggplot(data = GP_reg_pred_models_long[GP_reg_pred_models_long$model != "empirical",]) +
  geom_point(aes(x=f_original, y=pred_update, color = w), alpha = 0.1, shape = 8) +
  geom_line(aes(x=f_original, y=GP_pred_update, color = w)) +
  geom_line(aes(x=f_original, y=GP_empirical_update, color = w), linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_alpha(guide='none') +
  # scale axes labels to [-100, 100] to match the empirical plots
  scale_y_continuous("Predicted fraud\nbelief update", breaks=seq(-1, 1, 0.2), limits = c(-1,1), labels = seq(-100, 100, 20)) +
  scale_x_continuous("Prior fraud belief",breaks=seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  labs(color = "Outcome") +
  scale_color_manual(values = c("#4dac26", "#d01c8b"), labels = c("Undesired (loss)", "Desired (win)")) +
  theme(text = element_text(size = 9)) +
  ggtitle("Predicted fraud update with GP smoother") +
  facet_wrap(~ model, nrow = 3)


### predict posterior v and v update for our sample
## win
data2fit$pred_v_posterior_bayes[data2fit$MapMatchPrefer] = bayes_model_v(1,data2fit$FraudProb_scaled_for_model[data2fit$MapMatchPrefer],data2fit$pred_v_for_model[data2fit$MapMatchPrefer],data2fit$pred_c_for_model[data2fit$MapMatchPrefer])
## loss
data2fit$pred_v_posterior_bayes[!data2fit$MapMatchPrefer] = bayes_model_v(0,data2fit$FraudProb_scaled_for_model[!data2fit$MapMatchPrefer],data2fit$pred_v_for_model[!data2fit$MapMatchPrefer],data2fit$pred_c_for_model[!data2fit$MapMatchPrefer])

## belief update
data2fit$pred_v_update_bayes = data2fit$pred_v_posterior_bayes - data2fit$pred_v

## Outcome Desirability - posterior v
outcome_desire_model_v = function(w,f,v,c,u,a) {
  if (w == 1) {
    v_numerator = v*(2*a*c*f*u-2*a*c*f-2*a*f*u+2*a*f+a*u-a+2*c*f-2*f+2)
    v_denominator = 2*a*c*f*u-2*a*c*f-2*a*f*u*v+2*a*f*v+2*a*u*v-a*u-2*a*v+a+2*c*f-2*f*v+2*v
  } else if (w == 0) {
    v_numerator = v*(2*a*c*f*u-2*a*f*u+a*u-2*c*f+2*f)
    v_denominator = 2*a*c*f*u-2*a*f*u*v+2*a*u*v-a*u-2*c*f+2*f*v-2*v+2
  } else {
    print("w should be 1 for win or 0 for loss of preferred candidate A")
    v_numerator = NaN
    v_denominator = NaN
  }
  posterior_v = v_numerator / v_denominator
  return(posterior_v)
}
# win
data2fit$pred_v_posterior_OD[data2fit$MapMatchPrefer] = outcome_desire_model_v(1,data2fit$FraudProb_scaled_for_model[data2fit$MapMatchPrefer],data2fit$pred_v_for_model[data2fit$MapMatchPrefer],data2fit$pred_c_for_model[data2fit$MapMatchPrefer], data2fit$u[data2fit$MapMatchPrefer], optimized_alpha$OD)
# loss
data2fit$pred_v_posterior_OD[!data2fit$MapMatchPrefer] = outcome_desire_model_v(0,data2fit$FraudProb_scaled_for_model[!data2fit$MapMatchPrefer],data2fit$pred_v_for_model[!data2fit$MapMatchPrefer],data2fit$pred_c_for_model[!data2fit$MapMatchPrefer], data2fit$u[!data2fit$MapMatchPrefer], optimized_alpha$OD)

# belief update
data2fit$pred_v_update_OD = data2fit$pred_v_posterior_OD - data2fit$pred_v

# visualize - v update, both scenario
# A wins
model_df$OD_v_posterior[model_df$scenario=="win"] = outcome_desire_model_v(1,model_df$f[model_df$scenario=="win"],model_df$v[model_df$scenario=="win"],model_df$c[model_df$scenario=="win"],model_df$u[model_df$scenario=="win"],optimized_alpha$OD)
# A loses
model_df$OD_v_posterior[model_df$scenario=="loss"] = outcome_desire_model_v(0,model_df$f[model_df$scenario=="loss"],model_df$v[model_df$scenario=="loss"],model_df$c[model_df$scenario=="loss"],model_df$u[model_df$scenario=="loss"],optimized_alpha$OD)
# belief update
model_df$OD_v_update = model_df$OD_v_posterior - model_df$v

data_for_OD_v_update_u055 = model_df[model_df$c %in% round(seq(0,1,0.2),1) & model_df$f %in% round(seq(0,1,0.2),1) & model_df$u == 0.55,]
OD_v_update_u055 = ggplot(data_for_OD_v_update_u055) +
  geom_line(aes(x = v, y = OD_v_update, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  scale_x_continuous("v", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("True vote belief update (posterior - prior)", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 10), legend.position = "none") +
  facet_grid(f ~ c, labeller = labeller(f = label_both, c = label_both)) +
  ggtitle("Outcome Desirability, u = 0.55")
save(OD_v_update_u055, data_for_OD_v_update_u055, file=paste(plot_dir, "OD_v_update_u055.rdata", sep=""))

data_for_OD_v_update_u095 = model_df[model_df$c %in% round(seq(0,1,0.2),1) & model_df$f %in% round(seq(0,1,0.2),1) & model_df$u == 0.95,]
OD_v_update_u095 = ggplot(data_for_OD_v_update_u095) +
  geom_line(aes(x = v, y = OD_v_update, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  scale_color_manual(values = c("#d01c8b", "#4dac26"), labels = c("Desired (win)", "Undesired (loss)")) +
  scale_x_continuous("v", breaks = c(0,0.5,1), labels = c("0","0.5","1")) +
  scale_y_continuous("True vote belief update (posterior - prior)", breaks = c(-1,0,1), limits = c(-1,1)) +
  labs(color = "Outcome") +
  theme(text = element_text(size = 10), legend.position = "none") +
  facet_grid(f ~ c, labeller = labeller(f = label_both, c = label_both)) +
  ggtitle("Outcome Desirability, u = 0.95")
save(OD_v_update_u095, data_for_OD_v_update_u095, file=paste(plot_dir, "OD_v_update_u095.rdata", sep=""))


### "election-proof" participants
data_for_election_proof = data2fit
model_for_election_proof = "bayes"
print(paste("predicting posterior f and v for loss across the sample, based on the", model_for_election_proof, "model", sep=" "))

# predict posteriors for loss scenarios
if (model_for_election_proof == "bayes") {
  data_for_election_proof$pred_v_posterior_for_loss = bayes_model_v(0, data2fit$FraudProb_scaled_for_model,data2fit$pred_v_for_model,data2fit$pred_c_for_model)
  data_for_election_proof$pred_f_posterior_for_loss = bayes_model(0, data2fit$FraudProb_scaled_for_model,data2fit$pred_v_for_model,data2fit$pred_c_for_model)
} else if (model_for_election_proof == "OD") {
  data_for_election_proof$pred_v_posterior_for_loss = outcome_desire_model_v(0, data2fit$FraudProb_scaled_for_model,data2fit$pred_v_for_model,data2fit$pred_c_for_model,data2fit$u,optimized_alpha$OD)
  data_for_election_proof$pred_f_posterior_for_loss = outcome_desire_model(0, data2fit$FraudProb_scaled_for_model,data2fit$pred_v_for_model,data2fit$pred_c_for_model,data2fit$u,optimized_alpha$OD)
}
data_for_election_proof$pred_v_update_for_loss = data_for_election_proof$pred_v_posterior_for_loss - data_for_election_proof$pred_v_for_model
data_for_election_proof$pred_f_update_for_loss = data_for_election_proof$pred_f_posterior_for_loss - data_for_election_proof$FraudProb_scaled_for_model

## calculate the proportion of update that goes to fraud vs. true vote belief
overall_update = (abs(data_for_election_proof$pred_f_update_for_loss) + abs(data_for_election_proof$pred_v_update_for_loss))
data_for_election_proof$prop_update_to_fraud = abs(data_for_election_proof$pred_f_update_for_loss)/overall_update
data_for_election_proof$election_proof = data_for_election_proof$prop_update_to_fraud > 0.9  & data_for_election_proof$pred_f_update_for_loss > 0 & data_for_election_proof$pred_v_update_for_loss <= 0
data_for_election_proof$defined_posteriors = !is.na(data_for_election_proof$pred_f_update_for_loss) & !is.na(data_for_election_proof$pred_v_update_for_loss)
print("participants with values:")
sum(data_for_election_proof$defined_posteriors)
print("Number of participant with more than 90% of the update going to fraud rather than true vote belief")
sum(data_for_election_proof$election_proof)
print("% election-proof:")
sum(data_for_election_proof$election_proof, na.rm = TRUE)/sum(data_for_election_proof$defined_posteriors) * 100
print("% Democrats:")
sum(data_for_election_proof$election_proof & data_for_election_proof$PrefCand == "Dem", na.rm = TRUE)/sum(data_for_election_proof$defined_posteriors & data_for_election_proof$PrefCand == "Dem") * 100
print("% Republicans:")
sum(data_for_election_proof$election_proof & data_for_election_proof$PrefCand == "Rep", na.rm = TRUE)/sum(data_for_election_proof$defined_posteriors & data_for_election_proof$PrefCand == "Rep") * 100


## how many Republicans in our sample were indeed still sure that Trump was the true winner, although Biden won?
perc_rep_v_1 = round(sum(data2fit$pred_v==1 & data2fit$PrefCand_present=="Rep")/sum(data2fit$PrefCand_present=="Rep")*100,1)
print(paste("Based on our follow-up data,", perc_rep_v_1, "% of Republicans in our sample were still certain that Trump was the true winner although Biden officially won"))

###### COMPARE INTERVENTIONS
mean(data2fit$FraudProbMap_scaled[!data2fit$MapMatchPrefer])
mean(models_comparison_long$posterior_f[!models_comparison_long$w & models_comparison_long$model=="outcomedesire"])
intervention_priors = priors[!priors$w,]
intervention_efficacies = seq(0, 1, 0.005)
intervention_df = as.data.frame(intervention_efficacies)
for (ind in 1:length(intervention_efficacies)) {
  intervention_efficacy = intervention_efficacies[ind]
  new_f = intervention_priors$f * (1 - intervention_efficacy)
  new_v = intervention_priors$v * (1 - intervention_efficacy)
  new_c = intervention_priors$c + intervention_efficacy  * (1 - intervention_priors$c)

  new_f_multiple_beliefs = intervention_priors$f * (1 - intervention_efficacy/2)
  new_v_multiple_beliefs = intervention_priors$v * (1 - intervention_efficacy/2)

  intervention_f = outcome_desire_model(0,new_f,intervention_priors$v,intervention_priors$c, intervention_priors$u, optimized_alpha$OD)
  intervention_df$f[ind] = mean(intervention_f)
  intervention_v = outcome_desire_model(0,intervention_priors$f,new_v,intervention_priors$c, intervention_priors$u, optimized_alpha$OD)
  intervention_df$v[ind] = mean(intervention_v)
  intervention_c = outcome_desire_model(0,intervention_priors$f,intervention_priors$v, new_c, intervention_priors$u, optimized_alpha$OD)
  intervention_df$c[ind] = mean(intervention_c)
  intervention_fv = outcome_desire_model(0,new_f_multiple_beliefs,new_v_multiple_beliefs,intervention_priors$c, intervention_priors$u, optimized_alpha$OD)
  intervention_df$fv[ind] = mean(intervention_fv)
}
intervention_df_long = gather(intervention_df, intervention, mean_posterior_f, f:fv, factor_key=TRUE)

levels(intervention_df_long$intervention) = c("f toward 0", "v toward 0", "c toward 1", "f and v toward 0")
intervention_comparison_plot = ggplot(data = intervention_df_long, aes(x = intervention_efficacies, y = mean_posterior_f, color = intervention)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "inetervention efficacy", y = "mean posterior fraud belief", color = "intervention")
save(intervention_comparison_plot, intervention_df_long, file=paste(plot_dir, "intervention_comparison_plot.rdata", sep=""))