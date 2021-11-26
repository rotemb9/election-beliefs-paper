# this script praganizes and saves the figures
# written by Rotem Botvinik-Nezer

# load libraries
library(checkpoint)
checkpoint("2021-06-30")
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(png, include.only = 'readPNG')

## Clear workspace
rm(list=ls())

fig_output_path = "figures"

### MAIN FIGURES
# Fig. 1: Fraud belief update given hypothetical election outcome maps
# should be combined with the maps
load("code/plots/fraudUpdateByPref.rdata")
load("code/plots/fraudUpdateByPred.rdata")
load("code/plots/partisan_direction_desirability_effect.rdata")

fig1_no_maps = ggarrange(ggarrange(partisan_direction_desirability_effect),
                         ggarrange(fraudUpdateByPref, fraudUpdateByPred, 
                                   ncol = 2, nrow = 1,
                                   common.legend = TRUE, legend = "bottom"),
                         ncol = 2, nrow = 1, widths = c(1,1.8))
size_factor=0.75 
ggsave('fig1_no_maps.pdf',
       plot = fig1_no_maps,
       path = fig_output_path,
       width = 270*size_factor,
       height = 80*size_factor,
       units = "mm",
       dpi = 300)


# Fig. 2: Model diagram, desirability effect heat maps, model predictions
model_diagram = readPNG("code/plots/model_diagram.png")
gg_model_diagram <- ggplot() + 
  background_image(model_diagram) +
  # This ensures that the image leaves some space at the edges
  theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"), panel.background = element_rect(fill = "white"))
load("code/plots/model_fraud_update.rdata")
load("code/plots/biased_priors_plot.rdata")
load("code/plots/heat_map_desirability_effect_dots.rdata")
load("code/plots/scatter_predicted_actual_fraud_update.rdata")
load("code/plots/pred_fraudUpdateByPref.rdata")
load("code/plots/pred_fraudUpdateByPred.rdata")


fig_2 = ggarrange(
  ggarrange(ggarrange(gg_model_diagram, model_fraud_update, biased_priors_plot,
                      labels = c("A", "B", ""),
                      ncol = 3, nrow = 1,
                      widths = c(1, 1.5, 1.2)),
            ggarrange(heat_map_desirability_effect_dots, scatter_predicted_actual_fraud_update,
                      labels = c("E", "F"),
                      ncol = 2, nrow = 1),
            ncol = 1, nrow = 2),
  ggarrange(pred_fraudUpdateByPref, pred_fraudUpdateByPred,
            labels = c("G", "H"),
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "bottom"),
  ncol = 1, nrow = 2,
  heights = c(2, 1)
)

ggsave('fig_2.pdf',
       plot = fig_2,
       path = fig_output_path,
       width = 268,
       height = 282,
       units = "mm",
       dpi = 300)

### SUPP FIGURES AND TABLES
# Demographic and partisan affiliation of participants.
load("code/plots/preferred_by_affiliation_bar.rdata")
load("code/plots/state_by_preferred_bar.rdata")
load("code/plots/age_by_preferred_density.rdata")
supp_fig_1 = as_ggplot(grid.arrange(preferred_by_affiliation_bar,state_by_preferred_bar, age_by_preferred_density,
                                    ncol = 2, nrow = 2,
                                    layout_matrix = cbind(c(1,3), c(2,2)))) +
  draw_plot_label(label = c("A", "B", "C"), size = 12,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))
ggsave('supp_fig_1.pdf',
       plot = supp_fig_1,
       path = fig_output_path,
       width = 364,
       height = 247,
       units = "mm",
       dpi = 300)

# Preference and prior belief data
load("code/plots/prefStrength_by_preferred_density.rdata")
load("code/plots/winProb_by_preferred_density.rdata")
load("code/plots/fraudProb_by_preferred_density.rdata")
load("code/plots/priorFraud_by_priorWin_preferred_scatter.rdata")
supp_fig_2 = ggarrange(prefStrength_by_preferred_density, winProb_by_preferred_density, fraudProb_by_preferred_density, priorFraud_by_priorWin_preferred_scatter,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave('supp_fig_2.pdf',
       plot = supp_fig_2,
       path = fig_output_path,
       width = 224,
       height = 210,
       units = "mm",
       dpi = 300)

# bar plot directional desirability effect - pre + post
load("code/plots/partisan_direction_desirability_effect_prepost.rdata")
load("code/plots/bar_plot_pref_cat.rdata")
supp_fig_3 = ggarrange(partisan_direction_desirability_effect_prepost, bar_plot_pref_cat,
                       labels = c("A", "B"),
                       ncol = 2, nrow = 1)

ggsave('supp_fig_3.pdf',
       plot = supp_fig_3,
       path = fig_output_path,
       width = 200,
       height = 150,
       units = "mm",
       dpi = 300)


# heat maps for c and v and for pref by c and v
load("code/plots/heat_map_dems.rdata")
load("code/plots/heat_map_reps.rdata")
load("code/plots/heat_map_pref_dems.rdata")
load("code/plots/heat_map_pref_reps.rdata")

supp_fig_4 = ggarrange(heat_map_dems, heat_map_reps,
                       heat_map_pref_dems, heat_map_pref_reps,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2)

ggsave('supp_fig_4.pdf',
       plot = supp_fig_4,
       path = fig_output_path,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

# empirical vs. predicted fraud belief update as a function of prior fraud belief
load("code/plots/fraudUpdate_by_priorFraud_empirical_and_pred.rdata")

supp_fig_5 = fraudUpdate_by_priorFraud_empirical_and_pred
ggsave('supp_fig_5.pdf',
       plot = supp_fig_5,
       path = fig_output_path,
       width = 10,
       height = 10,
       units = "in",
       dpi = 300)

# v update
load("code/plots/model_v_update.rdata")
supp_fig_6 = model_v_update
ggsave('supp_fig_6.pdf',
       plot = supp_fig_6,
       path = fig_output_path,
       width = 6,
       height = 6,
       units = "in",
       dpi = 300)

# Follow-up survey: fraud likelihood and benefitting candidate
load("code/plots/win_fraudless_by_prefcand_bar.rdata")
load("code/plots/benefited_from_fraud_plot.rdata")
load("code/plots/plot_fraud_trump.rdata")
load("code/plots/plot_fraud_biden.rdata")

supp_fig_7 = ggarrange(win_fraudless_by_prefcand_bar,benefited_from_fraud_plot,
                       plot_fraud_trump, plot_fraud_biden, 
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave('supp_fig_7.pdf',
       plot = supp_fig_7,
       path = fig_output_path,
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)


# Follow-up survey: activities participants considered as fraud and beneficier of fraud activities
load("code/plots/fraud_activities_bar.rdata")
load("code/plots/fraud_benefit_density.rdata")
supp_fig_8 = ggarrange(fraud_activities_bar, fraud_benefit_density,
                       common.legend = TRUE, legend = "bottom",
                       labels = c("A", "B"),
                       nrow = 2, ncol = 1,
                       heights = c(1,1.8))

ggsave('supp_fig_8.pdf',
       plot = supp_fig_8,
       path = fig_output_path,
       width = 220,
       height = 247,
       units = "mm",
       dpi = 300)

### SUPP TABLES
load("code/plots/model_belief_update_loss_table.rdata")
load("code/plots/model_belief_update_win_table.rdata")
load("code/plots/model1_all_prereg_table.rdata")
load("code/plots/model1_dem_prereg_table.rdata")
load("code/plots/model1_rep_prereg_table.rdata")
load("code/plots/loss_dem_table.rdata")
load("code/plots/win_dem_table.rdata")
load("code/plots/loss_rep_table.rdata")
load("code/plots/win_rep_table.rdata")
load("code/plots/fraud_likelihood_ord_reg_tbl.rdata")

supp_table_2 = model_belief_update_loss_table
supp_table_3 = model_belief_update_win_table
supp_table_5 = model1_all_prereg_table
supp_table_6 = model1_dem_prereg_table
supp_table_7 = model1_rep_prereg_table
supp_table_8 = loss_dem_table
supp_table_9 = win_dem_table
supp_table_10 = loss_rep_table
supp_table_11 = win_rep_table
supp_table_12 = fraud_likelihood_ord_reg_tbl