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
load("code/plots/partisan_direction_desirability_effect_prepost.rdata")
load("code/plots/fraudUpdateByPref.rdata")
load("code/plots/fraudUpdateByPred.rdata")

fig1 = ggarrange(ggarrange(partisan_direction_desirability_effect_prepost),
                 ggarrange(fraudUpdateByPref, fraudUpdateByPred,
                           labels = c("B", "C"),
                           ncol = 1, nrow = 2,
                           common.legend = TRUE, legend = "bottom"),
                 labels = c("A", ""),
                 ncol = 2, nrow = 1)
                 #ncol = 2, nrow = 1, widths = c(1,1.8))
size_factor=0.5
ggsave('fig1.pdf',
       plot = fig1,
       path = fig_output_path,
       width = 350*size_factor,
       height = 300*size_factor,
       units = "mm",
       dpi = 300)


# Fig. 2: Model diagram, desirability effect heat maps, model predictions
model_diagram = readPNG("code/plots/model_diagram.png")
gg_model_diagram <- ggplot() + 
  background_image(model_diagram) +
  # This ensures that the image leaves some space at the edges
  theme(plot.margin = margin(t=0.3, l=0.1, r=0.1, b=0.3, unit = "cm"), panel.background = element_rect(fill = "white"))
load("code/plots/biased_priors_plot.rdata")
load("code/plots/heat_map_desirability_effect_dots.rdata")

fig2 = ggarrange(ggarrange(gg_model_diagram, biased_priors_plot,
                            labels = c("A", ""),
                            ncol = 2, nrow = 1,
                            widths = c(1.5, 1)),
                  heat_map_desirability_effect_dots,
                  labels = c("","D"),
                  ncol = 1, nrow = 2,
                  heights = c(1, 1.4)
)

size_factor=0.6
ggsave('fig2.pdf',
       plot = fig2,
       path = fig_output_path,
       width = 230*size_factor,
       height = 290*size_factor,
       units = "mm",
       dpi = 300)

# Fig. 3: predictions for the fraud update as a function of preferences across models
load("code/plots/pred_fraudUpdateByPref_models.rdata")

fig3 = pred_fraudUpdateByPref_models
size_factor=0.75
ggsave('fig3.pdf',
       plot = fig3,
       path = fig_output_path,
       width = 200*size_factor,
       height = 200*size_factor,
       units = "mm",
       dpi = 300)   

### EXTENDED DATA FIGURES

# Extended Data Fig. 1 - maps

# Extended Data Fig. 2 - preference and prior belief data + bar plot categorical prefstrength
load("code/plots/prefStrength_by_preferred_density.rdata")
load("code/plots/winProb_by_preferred_density.rdata")
load("code/plots/fraudProb_by_preferred_density.rdata")
load("code/plots/priorFraud_by_priorWin_preferred_scatter.rdata")
load("code/plots/bar_plot_pref_cat.rdata")

supp_fig2 = ggarrange(ggarrange(prefStrength_by_preferred_density, winProb_by_preferred_density, fraudProb_by_preferred_density, priorFraud_by_priorWin_preferred_scatter,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom"),
                      bar_plot_pref_cat,
                      labels= c("", "E"),
                      ncol = 2, nrow = 1,
                      widths = c(1.5,1)
                      
)

size_factor = 0.75
ggsave('supp_fig2.pdf',
       plot = supp_fig2,
       path = fig_output_path,
       width = 350*size_factor,
       height = 150*size_factor,
       units = "mm",
       dpi = 300)


# Extended Data Fig. 3 - simulations of predicted fraud update across models
load("code/plots/model_fraud_update.rdata")
load("code/plots/OD_fraud_update_u095.rdata")
load("code/plots/fraudonly_fraud_update.rdata")
load("code/plots/randombeneficiary_fraud_update.rdata")
supp_fig3 = ggarrange(model_fraud_update,  OD_fraud_update_u095,
                      fraudonly_fraud_update, randombeneficiary_fraud_update,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2,
                      common.legend = TRUE, legend = "bottom")
size_factor = 0.75
ggsave('supp_fig3.pdf',
       plot = supp_fig3,
       path = fig_output_path,
       width = 250*size_factor,
       height = 260*size_factor,
       units = "mm",
       dpi = 300)

# Extended Data Fig. 4 - heat maps for c and v and for pref by c and v
load("code/plots/heat_map_dems.rdata")
load("code/plots/heat_map_reps.rdata")
load("code/plots/heat_map_pref_dems.rdata")
load("code/plots/heat_map_pref_reps.rdata")

supp_fig4 = ggarrange(heat_map_dems, heat_map_reps,
                       heat_map_pref_dems, heat_map_pref_reps,
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2)

ggsave('supp_fig4.pdf',
       plot = supp_fig4,
       path = fig_output_path,
       width = 10,
       height = 8,
       units = "in",
       dpi = 300)

# Extended Data Fig. 5 - model fit
load("code/plots/scatter_predicted_actual_f_update_models.rdata")

supp_fig5 = scatter_predicted_actual_f_update_models

size_factor=0.75
ggsave('supp_fig5.pdf',
       plot = supp_fig5,
       path = fig_output_path,
       width = 180*size_factor,
       height = 150*size_factor,
       units = "mm",
       dpi = 300)   


# Extended Data Fig. 6 - predictions for fraud update as a function of prior win belief across models
load("code/plots/pred_fraudUpdateByPred_models.rdata")

supp_fig6 = pred_fraudUpdateByPred_models

size_factor=0.75
ggsave('supp_fig6.pdf',
       plot = supp_fig6,
       path = fig_output_path,
       width = 200*size_factor,
       height = 200*size_factor,
       units = "mm",
       dpi = 300)  

# Extended Data Fig. 7 - simulations of predicted v update - Bayesian and OD
load("code/plots/model_v_update.rdata")
load("code/plots/OD_v_update_u055.rdata")
load("code/plots/OD_v_update_u095.rdata")
supp_fig7 = as_ggplot(grid.arrange(model_v_update, OD_v_update_u055, OD_v_update_u095, ncol = 2, layout_matrix=rbind(c(NA,1,1,NA), c(2,2,3,3))))


size_factor = 0.75
ggsave('supp_fig7.pdf',
       plot = supp_fig7,
       path = fig_output_path,
       width = 250*size_factor,
       height = 260*size_factor,
       units = "mm",
       dpi = 300)



# Extended Data Fig. 8 - follow-up survey: fraud likelihood and benefitting candidate
load("code/plots/win_fraudless_by_prefcand_bar.rdata")
load("code/plots/benefited_from_fraud_plot.rdata")
load("code/plots/plot_fraud_trump.rdata")
load("code/plots/plot_fraud_biden.rdata")

supp_fig8 = ggarrange(win_fraudless_by_prefcand_bar,benefited_from_fraud_plot,
                       plot_fraud_trump, plot_fraud_biden, 
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave('supp_fig8.pdf',
       plot = supp_fig8,
       path = fig_output_path,
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)


# Extended Data Fig. 9 - follow-up survey: activities participants considered as fraud and beneficier of fraud activities
load("code/plots/fraud_activities_bar.rdata")
load("code/plots/fraud_benefit_density.rdata")
supp_fig9 = ggarrange(fraud_activities_bar, fraud_benefit_density,
                       common.legend = TRUE, legend = "bottom",
                       labels = c("A", "B"),
                       nrow = 2, ncol = 1,
                       heights = c(1,1.8))


ggsave('supp_fig9.pdf',
       plot = supp_fig9,
       path = fig_output_path,
       width = 190,
       height = 207,
       units = "mm",
       dpi = 300)

# Extended Data Fig. 10 - intervention comparison
load("code/plots/intervention_comparison_plot")
supp_fig10 = intervention_comparison_plot

size_factor = 0.75
ggsave('supp_fig10.pdf',
       plot = supp_fig10,
       path = fig_output_path,
       width = 220 * size_factor,
       height = 180 * size_factor,
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
supp_table_13 = fraud_likelihood_ord_reg_tbl

