library(plyr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(sensitivity)

source("consolidateSimulationOutput.R")
source("getGroups.R")
source("utilityFuncs.R")
source("writeParams.R") # split_param

source("plotOutput.R")

#-----------------------------------------------------------------------------------
# output files
#-----------------------------------------------------------------------------------

SA1_biomass_file = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.90.100_41_traj.csv"
SA1_stability_file = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/stability_biomass_slope_in_range.csv"
SA1_nums_file = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/nums.90.100_41_traj.csv"

SA2_biomass_file = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/biomass.90.100.csv"
SA2_stability_file = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/stability_biomass_slope_in_range.csv"
SA2_nums_file = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/nums.90.100.csv"

#-----------------------------------------------------------------------------------
# SA_first try
#-----------------------------------------------------------------------------------

consolid_results_biomass = read.csv(SA1_biomass_file)
stability_slope_in_range = read.csv(SA1_stability_file)
consolid_results_nums = read.csv(SA1_nums_file)

design_matrix = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)
output_folder = "Y:/SA_first_try/output_done"

plotfolder = "plots/SA_first_try/paper"
analysis = "SA1"

#-----------------------------------------------------------------------------------
# SA_two_noPPP
#-----------------------------------------------------------------------------------

consolid_results_biomass = read.csv(SA2_biomass_file)
stability_slope_in_range = read.csv(SA2_stability_file)

consolid_results_nums = read.csv(SA2_nums_file)
consolid_results_nums = select(consolid_results_nums, -which(apply(consolid_results_nums, 2, function(x) all(is.na(x)))))

#---

design_matrix = read.csv(DESIGN_MATRIX_FILE_TWO, stringsAsFactors = FALSE)
output_folder = "Y:/SA_two_noPPP/output_done"

plotfolder = "plots/SA_two_noPPP/paper"
analysis = "SA2"
#-----------------------------------------------------------------------------------
#   for all cases
# ----------------------------------------------

group_info = read.csv(GROUP_INFO_FILE, stringsAsFactors = FALSE)
param_info = read.csv(PARAM_INFO_FILE, stringsAsFactors = FALSE)


if (!dir.exists(plotfolder)) dir.create(plotfolder, recursive = TRUE)



# for now subselect design matrix for just some sims
morris_out_biomass = get_morris_output(design_matrix, consolid_results_biomass)
morris_out_nums = get_morris_output(design_matrix, consolid_results_nums)

mu_biomass = calculate_mu(morris_out_biomass$ee)
mu.star_biomass = calculate_mu.star(morris_out_biomass$ee)
sigma_biomass = calculate_sigma(morris_out_biomass$ee)

mu_nums = calculate_mu(morris_out_nums$ee)
mu.star_nums = calculate_mu.star(morris_out_nums$ee)
sigma_nums = calculate_sigma(morris_out_nums$ee)

# remove mL_CLU_adult since all it's values are 0, was there since juvenile was
p_remove = "mL_CLU_adult"
mu_biomass = mu_biomass[-which(rownames(mu_biomass) == p_remove),]
mu.star_biomass = mu.star_biomass[-which(rownames(mu.star_biomass) == p_remove),]
sigma_biomass = sigma_biomass[-which(rownames(sigma_biomass) == p_remove),]

mu_nums = mu_nums[-which(rownames(mu_nums) == p_remove),]
mu.star_nums = mu.star_nums[-which(rownames(mu.star_nums) == p_remove),]
sigma_nums = sigma_nums[-which(rownames(sigma_nums) == p_remove),]


# total biomass / numbers

total_biomass = consolid_results_biomass %>% dplyr::select(-c("sim", "BB", "DET", "DL", "DR", "PB")) %>% rowSums()
total_nums = consolid_results_nums %>% dplyr::select(-"sim") %>% rowSums()

morris_out_total_biomass = get_morris_output(design_matrix, data.frame(sim = consolid_results_biomass$sim, 
                                                                       bio = total_biomass))
morris_out_total_nums = get_morris_output(design_matrix, data.frame(sim = consolid_results_nums$sim, 
                                                                    nums = total_nums))

mu_total_biomass = calculate_mu(morris_out_total_biomass$ee)
mu.star_total_biomass = calculate_mu.star(morris_out_total_biomass$ee)
sigma_total_biomass = calculate_sigma(morris_out_total_biomass$ee)

mu_total_biomass = mu_total_biomass[-which(rownames(mu_total_biomass) == p_remove),]
mu.star_total_biomass = mu.star_total_biomass[-which(rownames(mu.star_total_biomass) == p_remove),]
sigma_total_biomass = sigma_total_biomass[-which(rownames(sigma_total_biomass) == p_remove),]

mu_total_nums = calculate_mu(morris_out_total_nums$ee)
mu.star_total_nums = calculate_mu.star(morris_out_total_nums$ee)
sigma_total_nums = calculate_sigma(morris_out_total_nums$ee)

mu_total_nums = mu_total_nums[-which(rownames(mu_total_nums) == p_remove),]
mu.star_total_nums = mu.star_total_nums[-which(rownames(mu.star_total_nums) == p_remove),]
sigma_total_nums = sigma_total_nums[-which(rownames(sigma_total_nums) == p_remove),]

#-------------------------------------------------------------------------------
# stability
#-------------------------------------------------------------------------------


# stability using slope-in-range metric

per_stable_per_sim = apply(stability_slope_in_range[,-1], 1, function(x) sum(x, na.rm = TRUE) / length(x)) # percent stable per sim
hist(per_stable_per_sim)

morris_out_stability = get_morris_output(design_matrix, data.frame(sim = stability_slope_in_range$sim, 
                                                                   per = per_stable_per_sim))
mu_stability = calculate_mu(morris_out_stability$ee)
mu.star_stability = calculate_mu.star(morris_out_stability$ee)
sigma_stability = calculate_sigma(morris_out_stability$ee)

mu_stability = mu_stability[-which(rownames(mu_stability) == p_remove),,drop = FALSE]
mu.star_stability = mu.star_stability[-which(rownames(mu.star_stability) == p_remove),,drop = FALSE]
sigma_stability = sigma_stability[-which(rownames(sigma_stability) == p_remove),,drop = FALSE]

# percent none or all stable
sum(per_stable_per_sim == 0) / length(per_stable_per_sim)
sum(per_stable_per_sim == 1) / length(per_stable_per_sim)

summary(per_stable_per_sim)


mu_stability[order(mu_stability),] # values much smaller!

# create these variables when running for each SA, be careful!!

pdf(file.path(plotfolder, "boxplot_stability_SA1_SA2.pdf"), height = 4, width = 4)
par(mar = c(2, 4, 0, 0) + 0.1, mgp = c(2, 0.5, 0))
boxplot(per_stable_per_sim_SA1, per_stable_per_sim_SA2, 
        ylab = "Proportion groups stable", names = ,
        col = "gray85", frame = FALSE, xaxt='n')
axis(side = 1, at = 1:2,labels = c("SA1", "SA2"), lwd.ticks = FALSE, lwd = FALSE)
dev.off()

### tables of ranks
#------------------------------------------------------

# list params from high to low for each metric
tot_effect_ranks = data.frame( mu.star_biomass = tot_effect_norm_biomass$param[order(tot_effect_norm_biomass$totalMuStar, decreasing = TRUE)],
                               sigma_biomass = tot_effect_norm_biomass$param[order(tot_effect_norm_biomass$totalSigma, decreasing = TRUE)],
                               mu.star_nums = tot_effect_norm_nums$param[order(tot_effect_norm_nums$totalMuStar, decreasing = TRUE)],
                               sigma_nums = tot_effect_norm_nums$param[order(tot_effect_norm_nums$totalSigma, decreasing = TRUE)],
                               mu.star_stability = rownames(mu.star_stability)[order(mu.star_stability, decreasing = TRUE)],
                               sigma_stability = rownames(sigma_stability)[order(sigma_stability, decreasing = TRUE)]
)

write.csv(tot_effect_ranks, file = file.path(plotfolder, "ranked_params.csv"), row.names = FALSE, quote = FALSE)

#### mu* vs sigma for biomass, nums, stability
#----------------------------------------------------

# NOTE!!! the cut offs for labeling points ans the positions are hard-coded and change for the first and secon SA
# PIA and be careful when running the code

# also, some optional lines commented out which are used to make the presentation version of the plot

pdf(file.path(plotfolder, "mu.star_vs_sigma_biom_nums_stab.pdf"), height = 4, width = 12)
#png(file.path(plotfolder, "mu.star_vs_sigma_biom_nums_stab.png"), height = 4, width = 12, units = 'in', res = 300)
par(mfrow = c(1,3), mar = c(3, 3, 2.5, 0.5), mgp = c(2, 0.5, 0))
param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
param_info_plt = param_info[match(rownames(mu.star_stability), param_info$param_code),]
#param_info_plt$invertIdx = param_info_plt$invertIdx + 15 # to make pts solid


xs = switch(analysis, SA1 = log(mu.star_total_biomass), SA2 = mu.star_total_biomass ) 
ys = switch(analysis, SA1 = log(sigma_total_biomass), SA2 = sigma_total_biomass ) 
plot_lim = c(switch(analysis, SA1 = min(xs, ys), SA2 = 0), 1.05 * max(xs, ys)) # to have asp = 1, using that makes x-axis neg in some cases
plot(xs, ys, 
     pch = param_info_plt$invertIdx, col = param_palatte[param_info_plt$colIdx], 
     xlim = plot_lim, ylim = plot_lim, 
     xlab = switch(analysis, SA1 = "log(mu.star)", SA2 = ""), ylab = switch(analysis, SA1 = "log(sigma)", SA2 = "sigma"), bty = "l", cex.lab = 1.2)
label_pts = switch(analysis,
                   SA1 = which(xs > 17), # 5 for first_try
                   SA2 = which(xs > 1e6) ) # for two_noPPP
pos_pts = switch(analysis,
                 SA1 = c(4, 2, 4, 2, 2, 2, 4), # first_try
                 SA2 = c(4, 1, 4, 4, 4, 4, 4, 4, 4, 2) ) # two_noPPP
if (analysis == "SA1")
{
  abline(a = log(0.1), b = 1, lty = 2, col = alpha("black", 0.5))
  abline(a = log(0.5), b = 1, lty = 3, col = alpha("black", 0.6))
  abline(a = log(1), b = 1, lty = 4, col = alpha("black", 0.5))
} else if (analysis == "SA2")
{
  abline(a = 0, b = 0.1, lty = 2, col = alpha("black", 0.5))
  abline(a = 0, b = 0.5, lty = 3, col = alpha("black", 0.6))
  abline(a = 0, b = 1, lty = 4, col = alpha("black", 0.5))
}
text(xs[label_pts], ys[label_pts], 
     labels = param_info_plt$code[label_pts], 
     pos = pos_pts, cex = 0.9, xpd = TRUE)
mtext("Total biomass", side = 3, line = 1)

legend("topleft", pch = c(19, 19, 19, 1, 2), 
       col = c(param_palatte, "black", "black"), 
       legend = c("growth", "mortality", "recruitment", "invertebrate", "vertebrate"), 
       bty = "n")

xs = switch(analysis, SA1 = log(mu.star_total_nums), SA2 = mu.star_total_nums ) 
ys = switch(analysis, SA1 = log(sigma_total_nums), SA2 = sigma_total_nums ) 
plot_lim = c(switch(analysis, SA1 = min(xs, ys), SA2 = 0), 1.05 * max(xs, ys)) # to have asp = 1, using that makes x-axis neg in some cases
plot(xs, ys, 
     pch = param_info_plt$invertIdx, col = param_palatte[param_info_plt$colIdx], 
     xlim = plot_lim, ylim = plot_lim, 
     xlab = switch(analysis, SA1 = "log(mu.star)", SA2 = "mu.star"), ylab = "", bty = "l", cex.lab = 1.2)
label_pts = switch(analysis,
                   SA1 = which(xs > 21.5), # 2 for first_try
                   SA2 = which(xs > 7e8) ) #4 for two_noPPP
pos_pts = switch(analysis,
                 SA1 = c(2, 4, 4, 2, 4, 2, 2, 3, 3, 2, 1, 4), # first_try
                 SA2 = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4) ) #two_noPPP

if (analysis == "SA1")
{
  abline(a = log(0.1), b = 1, lty = 2, col = alpha("black", 0.5))
  abline(a = log(0.5), b = 1, lty = 3, col = alpha("black", 0.6))
  abline(a = log(1), b = 1, lty = 4, col = alpha("black", 0.5))
} else if (analysis == "SA2")
{
  abline(a = 0, b = 0.1, lty = 2, col = alpha("black", 0.5))
  abline(a = 0, b = 0.5, lty = 3, col = alpha("black", 0.6))
  abline(a = 0, b = 1, lty = 4, col = alpha("black", 0.5))
}
text(xs[label_pts], ys[label_pts], 
     labels = param_info_plt$code[label_pts], 
     pos = pos_pts, cex = 0.9, xpd = TRUE)
mtext("Total numbers", side = 3, line = 1)


plot_lim = c(0, 1.05 * max(mu.star_stability, sigma_stability) )
plot(mu.star_stability, sigma_stability, 
     pch = param_info_plt$invertIdx, col = param_palatte[param_info_plt$colIdx], 
     xlim = plot_lim, ylim = plot_lim, 
     xlab = switch(analysis, SA1 = "log(mu.star)", SA2 = ""), ylab = switch(analysis, SA1 = "log(sigma)", SA2 = ""), bty = "l", cex.lab = 1.2)
label_pts = switch(analysis,
                   SA1 = which(mu.star_stability > 0.1), # 0.1 for first_try
                   SA2 = which(mu.star_stability > 0.05) ) # 0.0.05 for two_noPPP
pos_pts = switch(analysis,
                 SA1 = c(4, 4, 4, 4, 2, 4, 4, 4), # first_try
                 SA2 = c(2, 4, 4, 4, 4, 4, 4, 4, 4, 3) ) # two_noPPP

abline(a = 0, b = 0.1, lty = 2, col = alpha("black", 0.5))
abline(a = 0, b = 0.5, lty = 3, col = alpha("black", 0.6))
abline(a = 0, b = 1, lty = 4, col = alpha("black", 0.5))
text(mu.star_stability[label_pts], sigma_stability[label_pts], labels = param_info_plt$code[label_pts], 
     pos = pos_pts, cex = 0.9, xpd = TRUE)
mtext("Proportion groups stable", side = 3, line = 1)

dev.off()



#-----------heatmaps--------------------------------------------------------------------



# organize by guild--------------------------------------
mu.star_plot = plot_ee_heatmap_by_guild(mu.star_norm_biomass, "mu.star", param_info, group_info, title = "mu.star")
mu_plot = plot_ee_heatmap_by_guild(mu_norm_biomass, "mu", param_info, group_info, col = diverging_palette(), title = "mu")
sigma_plot = plot_ee_heatmap_by_guild(sigma_norm_biomass, "sigma", param_info, group_info, title = "sigma", annotate_type = TRUE)

pdf(file = file.path(plotfolder, "guild_all_norm_unit_biomass.pdf"), width = 24, height = 12)
mu.star_plot + mu_plot + sigma_plot
dev.off()


mu.star_plot = plot_ee_heatmap_by_guild(mu.star_norm_nums, "mu.star", param_info, group_info, title = "mu.star")
mu_plot = plot_ee_heatmap_by_guild(mu_norm_nums, "mu", param_info, group_info, col = diverging_palette(), title = "mu")
sigma_plot = plot_ee_heatmap_by_guild(sigma_norm_nums, "sigma", param_info, group_info, title = "sigma", annotate_type = TRUE)

pdf(file = file.path(plotfolder, "guild_all_norm_unit_nums.pdf"), width = 24, height = 12)
mu.star_plot + mu_plot + sigma_plot
dev.off()

# -------------individual group plots --------------------------------------

pdf(file = file.path(plotfolder, "individual_biomass_mu.star_vs_sigma.pdf"), width = 20, height = 16*2)
par(mfrow = c(8, 5), mar = c(3, 3, 2, 0) + 0.1, mgp = c(1, 0.5, 0), oma = c(2, 2, 0, 0.1))

plot_mu_vs_sigma_by_group(mu.star_biomass, sigma_biomass, param_info, group_info, 1:20)
plot_mu_vs_sigma_by_group(mu.star_biomass, sigma_biomass, param_info, group_info, 21:40)

dev.off()


pdf(file = file.path(plotfolder, "individual_nums_mu.star_vs_sigma.pdf"), width = 28, height = 12)
par(mfrow = c(3, 7), mar = c(3, 3, 2, 0) + 0.1, mgp = c(1, 0.5, 0), oma = c(2, 2, 0, 0.1))

plot_mu_vs_sigma_by_group(mu.star_nums, sigma_nums, param_info, group_info, 1:21)

dev.off()


######--------------non-infuential parameters------------------------------------------

library(VennDiagram)

# initial rough calculation, take as non-infuential those that have an agg mu.star < 1

# from tot_efffect_X in each case
ni_biom_SA1 = c("C_SXX_age", "mL_POL_juv", "C_SB_age", "C_ZOG_single", "mL_LBT_adult", "mQ_CLU_juv", "mQ_ZOG_single", "mL_RAY_juv",
  "mL_LBT_juv", "mL_RAY_adult", "mL_PP_single", "C_CET_age", "mL_ZOG_single", "C_CEP_adult", "mL_SB_adult", "mL_SHK_juv", "mL_SHK_adult", "mL_CLU_juv", "mQ_CLU_adult", "mL_CET_adult")
ni_nums_SA1 = c("mL_SXX_adult", "mQ_ZOO_single", "C_SMD_age", "C_CLU_age", "C_CRA_single", "C_BIV_single", "mL_ZOO_single", "mQ_DEP_single", "mQ_ECH_single", "C_WHE_single", "mL_POL_adult", "C_RAY_age",
  "C_WHG_age", "C_SCE_single", "mQ_CRA_single", "C_SXX_age", "mQ_BIV_single", "C_GAD_age", "mL_POL_juv", "C_SHK_age", "C_MUL_age", "mL_RAY_juv", "mQ_CLU_juv", "C_ZOC_single",
  "C_SUS_single", "mL_LBT_juv", "mQ_ZOC_single", "C_SB_age", "C_CET_age", "mL_RAY_adult", "mL_LBT_adult", "mL_SB_adult", "C_DAB_age", "mL_SHK_juv", "C_GUX_age", "mL_ZOC_single", "mQ_WHE_single",
  "C_LBT_age", "C_MAC_age", "mQ_ZOG_single", "C_SHP_single", "mQ_SCE_single", "C_OFF_age", "C_COD_age", "C_SOL_age", "C_POL_age", "mQ_SHP_single", "mQ_CLU_adult", "mQ_SUS_single", "mL_PP_single",
  "C_SPA_age", "mL_SHK_adult", "C_BSS_age", "C_PLE_age", "mQ_LBE_single", "C_ZOG_single", "mL_ZOG_single", "mL_CLU_juv", "C_LBE_single", "C_CEP_adult", "mL_CET_adult")
ni_biom_SA2 = c()
ni_nums_SA2 = c("mL_LBT_juv", "C_BSS_age", "mQ_SHP_single", "mL_SHK_juv", "C_POL_age", "C_MUL_age", "C_GAD_age", "mQ_LBE_single",
  "mQ_CLU_juv", "mL_POL_juv", "mQ_CLU_adult", "mQ_SUS_single", "C_CEP_adult", "C_SPA_age", "C_GUX_age", "C_LBT_age", "C_SUS_single")

venn.diagram(x = list(ni_biom_SA1, ni_nums_SA1, ni_nums_SA2),
             category.names = c("biom SA1", "nums SA1", "nums SA2"),
             filename = file.path(plotfolder, "venn-diagram_not_infl.png"))

sort(setdiff(ni_nums_SA1, ni_nums_SA2))
sort(setdiff(ni_nums_SA2, ni_nums_SA1)) # no params are ni in SA2 but not SA1

# those that are almost always ni (except biom_SA2 and stability)
intersect(intersect(ni_biom_SA1, ni_nums_SA1), ni_nums_SA2)

# those that are ni to both biom and nums in SA1
intersect(ni_biom_SA1, ni_nums_SA1)


### correlation of outputs ####
##---------------------------------------------------------------------------------------------
library(corrplot)

pdf(file = file.path(plotfolder, "correlation_biomass_nums.pdf"), width = 12, height = 12)
par(mfrow = c(2,2), mar = c(0, 5, 4, 1) + 0.1)

plot_corr_plot(read.csv(SA1_biomass_file), group_info)
mtext("SA1 biomass", side = 3, line = 2.5, cex = 1.2)
plot_corr_plot(read.csv(SA1_nums_file), group_info, add_missing_groups = TRUE)
mtext("SA1 numbers", side = 3, line = 2.5, cex = 1.2)

plot_corr_plot(read.csv(SA2_biomass_file), group_info)
mtext("SA2 biomass", side = 3, line = 2.5, cex = 1.2)
plot_corr_plot(read.csv(SA2_nums_file), group_info)
mtext("SA2 numbers", side = 3, line = 2.5, cex = 1.2)

dev.off()

########### compare diff numner of trajectories #################################
reps = 100
set.seed(31)

samp_10 = design_sample(design_matrix, n_sample = 10, n_rep = reps)
samp_20 = design_sample(design_matrix, n_sample = 20, n_rep = reps)
samp_30 = design_sample(design_matrix, n_sample = 30, n_rep = reps)
samp_40 = design_sample(design_matrix, n_sample = 40, n_rep = reps)

mu.star_10 = sample_morris_output(consolid_results_biomass, design_matrix, samp_10, effect = "mu.star")
mu.star_20 = sample_morris_output(consolid_results_biomass, design_matrix, samp_20, effect = "mu.star")
mu.star_30 = sample_morris_output(consolid_results_biomass, design_matrix, samp_30, effect = "mu.star")
mu.star_40 = sample_morris_output(consolid_results_biomass, design_matrix, samp_40, effect = "mu.star")

sigma_10 = sample_morris_output(consolid_results_biomass, design_matrix, samp_10, effect = "sigma")
sigma_20 = sample_morris_output(consolid_results_biomass, design_matrix, samp_20, effect = "sigma")
sigma_30 = sample_morris_output(consolid_results_biomass, design_matrix, samp_30, effect = "sigma")
sigma_40 = sample_morris_output(consolid_results_biomass, design_matrix, samp_40, effect = "sigma")

# annoying, but we need a different ylim for SA1 and SA2
ylim = c(-1, 4) # SA_first_try
ylim = c(-0.7, 1) # SA_two_noPPP

pdf(file.path(plotfolder, paste0("traj_mu.star_biomass.pdf")), width = 10, height = 14)
par(mfrow = c(8, 5), mar = c(3, 2, 2, 0) + 0.1, mgp = c(1, 0.5, 0), oma = c(0, 2, 0, 0), las = 1)
plot_subset_traj(mu.star_biomass, mu.star_10, mu.star_20, mu.star_30, mu.star_40, ylim = ylim, param_info)
dev.off()

ylim = c(-1, 2) # SA_first_try
ylim = c(-1, 1.2) # SA_two_noPPP

pdf(file.path(plotfolder, paste0("traj_sigma_biomass.pdf")), width = 10, height = 14)
par(mfrow = c(8, 5), mar = c(3, 2, 2, 0) + 0.1, mgp = c(1, 0.5, 0), oma = c(0, 2, 0, 0), las = 1)
plot_subset_traj(sigma_biomass, sigma_10, sigma_20, sigma_30, sigma_40, ylim = ylim, param_info)
dev.off()

order_mu.star_10 = calculate_output_order_agreement(mu.star_biomass, mu.star_10, 10)
order_mu.star_20 = calculate_output_order_agreement(mu.star_biomass, mu.star_20, 10)
order_mu.star_30 = calculate_output_order_agreement(mu.star_biomass, mu.star_30, 10)
order_mu.star_40 = calculate_output_order_agreement(mu.star_biomass, mu.star_40, 10)

order_sigma_10 = calculate_output_order_agreement(sigma_biomass, sigma_10, 10)
order_sigma_20 = calculate_output_order_agreement(sigma_biomass, sigma_20, 10)
order_sigma_30 = calculate_output_order_agreement(sigma_biomass, sigma_30, 10)
order_sigma_40 = calculate_output_order_agreement(sigma_biomass, sigma_40, 10)

pdf(file.path(plotfolder, "traj_rank_mu.star_sigma_biomass.pdf"), width = 10, height = 8)
par(mfrow = c(2, 1), mar = c(3, 3, 2, 0) + 0.1, mgp = c(1.5, 0.5, 0))
plot_subset_order(order_mu.star_10, order_mu.star_20, order_mu.star_30, order_mu.star_40, legend = TRUE)
title("mu.star")
plot_subset_order(order_sigma_10, order_sigma_20, order_sigma_30, order_sigma_40, legend = FALSE)
title("sigma")
dev.off()

summary(apply(order_mu.star_10, 2, mean))
summary(apply(order_sigma_10, 2, mean))
summary(apply(order_mu.star_40, 2, mean))
summary(apply(order_sigma_40, 2, mean))


#### --------------- simulations gone amok analysis ----------------------------------------------------
range(consolid_results_biomass$CET)
hist(consolid_results_biomass$CET)
table(cut(consolid_results_biomass$CET, breaks = c(0, 0.01, 0.1, 0.5, 1, 10, 100, 1000, 1e8, 1e9, 1e10, 2e10)))

design_matrix_sub = design_matrix %>% filter(simID %in% consolid_results_biomass$sim) 
cet_idx = grep("CET", colnames(design_matrix_sub))

apply(design_matrix_sub[consolid_results_biomass$CET < 0.5, cet_idx], 2, table)
apply(design_matrix_sub[consolid_results_biomass$CET > 1000, cet_idx], 2, table)
apply(design_matrix_sub[consolid_results_biomass$CET > 1e8, cet_idx], 2, table)

# look at range for important params of top pred
range_output_for_param = function(sim_output, param_col)
{
  return (ldply(tapply(sim_output, 
                       design_matrix_sub[,grep(param_col, colnames(design_matrix_sub))], 
                       range)) )
}

range_output_for_param(consolid_results_biomass$CET, "KDENR_CET_single")
range_output_for_param(consolid_results_biomass$CET, "mL_CET_juv")
range_output_for_param(consolid_results_biomass$CET, "mL_CET_adult")

range_output_for_param(consolid_results_biomass$SB, "KDENR_SB_single")
range_output_for_param(consolid_results_biomass$SB, "mL_SB_juv")
range_output_for_param(consolid_results_biomass$SB, "mL_SB_adult")

range_output_for_param(consolid_results_biomass$SXX, "KDENR_SXX_single")
range_output_for_param(consolid_results_biomass$SXX, "mL_SXX_juv")
range_output_for_param(consolid_results_biomass$SXX, "mL_SXX_adult")

# look at all sims with the 'bad' param combos mL_juv = 1 and KDENR >= 5
# how many crashed and how many didn't

crash_sims = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)
crash_sims$y = rep(1, length(crash_sims$simID))
crash_sims$y[crash_sims$simID %in% sims_too_short] = 0

crash_sims_cet = crash_sims[crash_sims$mL_CET_juv == 1 & crash_sims$KDENR_CET_single >= 5,]
table(paste(crash_sims_cet$mL_CET_juv, crash_sims_cet$KDENR_CET_single), crash_sims_cet$y)
summary(consolid_results_biomass$CET[consolid_results_biomass$sim %in% crash_sims_cet$simID[crash_sims_cet$y == 1]])
nrow(crash_sims_cet)


# summary crash happens when mL_CET_juv == 1 and KDENR_CET_single >= 5, this happened for 9 trajectories
# however, two trajectories when KDENR_CET_single == 5 and didn't crash and one that did, so a bit more marginal
# and somewhat dependant on other parameters for state of ecosystem

############################################################################################

# to create param_info and group_info files, need to have created a morris output summary, i.e. mu.star

#get init biomass, nums, for now use first sim output
load(file.path(output_folder, "AEEC_SA_sim10001.rdata")) # this loads metrics object into env

create_group_and_param_info(mu.star_biomass, metrics, FUNCTIONAL_GROUPS_FILE, PARAM_INFO_FILE, GROUP_INFO_FILE)
