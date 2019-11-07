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
# SA_first try
#-----------------------------------------------------------------------------------

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.90.100_41_traj.csv")
stability_pval = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/stability_biomass_pvals.csv")
stability_slope = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/stability_biomass_slope.csv")
stability_slope_norm = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/stability_biomass_slope_norm.csv")
stability_range_norm = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/stability_biomass_range_norm.csv")
stability_slope_in_range = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/stability_biomass_slope_in_range.csv")
plotfolder = "plots/SA_first_try/biomass.90.100_traj_41"
consolid_results_60 = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.60.70_41_traj.csv")

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.60.70_50_traj.csv")
plotfolder = "plots/SA_first_try/biomass.60.70_traj_50"

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/nums.90.100_41_traj.csv")
plotfolder = "plots/SA_first_try/nums.90.100_traj_41"

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/nums.60.70_50_traj.csv")
plotfolder = "plots/SA_first_try/nums.60.70_traj_50"

#---

design_matrix = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)
output_folder = "Y:/SA_first_try/output_done"

#-----------------------------------------------------------------------------------
# SA_two_noPPP
#-----------------------------------------------------------------------------------

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/biomass.90.100.csv")
stability_pval = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/stability_biomass_pvals.csv")
stability_slope = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/stability_biomass_slope.csv")
stability_slope_norm = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/stability_biomass_slope_norm.csv")
stability_range_norm = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/stability_biomass_range_norm.csv")
stability_slope_in_range = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/stability_biomass_slope_in_range.csv")
plotfolder = "plots/SA_two_noPPP/biomass.90.100"

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/nums.90.100.csv")
consolid_results = select(consolid_results, -which(apply(consolid_results, 2, function(x) all(is.na(x)))))
plotfolder = "plots/SA_two_noPPP/nums.90.100"


design_matrix = read.csv(DESIGN_MATRIX_FILE_TWO, stringsAsFactors = FALSE)
output_folder = "Y:/SA_two_noPPP/output_done"

#-----------------------------------------------------------------------------------
#   for all cases
# ----------------------------------------------

group_info = read.csv(GROUP_INFO_FILE, stringsAsFactors = FALSE)
param_info = read.csv(PARAM_INFO_FILE, stringsAsFactors = FALSE)


if (!dir.exists(plotfolder)) dir.create(plotfolder, recursive = TRUE)

#----------------special comparing which decades of sims---------------------
png(filename = "plots/years90.100vs60.70.png", width = 1200, height = 1200, res = 250)
par(mar = c(3, 3, 1, 1), mgp = c(2, 0.5, 0))
plot(0, 0, asp = 1, type = "n", xlim = range(consolid_results_60 + 1e-10, na.rm = TRUE), 
     ylim = range(consolid_results + 1e-10, na.rm = TRUE),
     xlab = "years 60-70", ylab = "years 90-100", log = "xy")
for(i in 1:nrow(consolid_results))
  points(consolid_results_60[i,-1] + 1e-10, consolid_results[i,-1] + 1e-10, pch = 16, 
         col = rainbow(40, alpha = 0.5), cex = 0.5) #
abline(a = 0, b = 1, col = "black")
segments(x0 = c(1e-10, 1), y0 = c(1, 1), x1 = c(1, 1), y1 = c(1, 1e-10))
legend("topleft", pch = 16, col = rainbow(40), legend = colnames(consolid_results)[-1], 
       bty = "n", ncol = 2, cex = 0.7)
dev.off()

# correlation between results at years 60-70 and years 90-100
# correlation by group to account for different scales
cbind(names(consolid_results), sapply(1:ncol(consolid_results), function(x) cor(consolid_results[,x], consolid_results_60[,x])) )

#--------------------------------------------------------------------------------

# TODO there are some NAs that should be 0, but revisit this with new processOutput
#consolid_results = consolid_results %>% replace(., is.na(.), 0) #(list(name = ~ replace(., which(is.na(.)), 0))) 
# TODO new problem is that entire rows are NA !!!
# final update: all bad rows fixed :)
sum(is.na(consolid_results))
bad_rows = unique(which(is.na(consolid_results), arr.ind = TRUE)[,1])
consolid_results$sim[bad_rows]


# for now subselect design matrix for just some sims
morris_out = get_morris_output(design_matrix, consolid_results)

#plot(morris_out)
#plot3d.morris(morris_out)

mu = calculate_mu(morris_out$ee)
mu.star = calculate_mu.star(morris_out$ee)
sigma = calculate_sigma(morris_out$ee)
per_pos = calculate_percent_positive(morris_out$ee)


# normalized versions

mu_norm = normalize_effects_by_max(mu)
mu.star_norm = normalize_effects_by_max(mu.star)
sigma_norm = normalize_effects_by_max(sigma)


# for biomass, restart with results, reuse same morris_full
consolid_results_norm = normalize_results_by_init(consolid_results, 
                                                  group_info %>% dplyr::select(code, biomass.init) %>% rename(val = biomass.init))
# or for nums
consolid_results_norm = normalize_results_by_init(consolid_results, 
                                                  group_info %>% dplyr::select(code, nums.init) %>% rename(val = nums.init))


morris_out_norm = get_morris_output(design_matrix, consolid_results_norm)

mu_norm_init = calculate_mu(morris_out_norm$ee)
mu.star_norm_init = calculate_mu.star(morris_out_norm$ee)
sigma_norm_init = calculate_sigma(morris_out_norm$ee)


# loook at important params by normalized effect size
tot_effect_norm = data.frame(param = row.names(mu.star_norm), 
                             totalMuStar = rowSums(mu.star_norm),
                             totalSigma = rowSums(sigma_norm))
#or by percent change (normalized by initial biomass)
tot_effect_norm_init = data.frame(param = row.names(mu.star_norm_init), 
                                  totalMuStar = rowSums(mu.star_norm_init),
                                  totalSigma = rowSums(sigma_norm_init))

tot_effect_norm[order(tot_effect_norm$totalMuStar, decreasing = TRUE),]
tot_effect_norm_init[order(tot_effect_norm_init$totalMuStar, decreasing = TRUE),]

# check params in same order
all(order(tot_effect_norm$param), order(tot_effect_norm_init$param))
# use nnegative to get rank from high to low
tot_effect_ranks = data.frame( param = tot_effect_norm$param,
                               norm_mu.star = rank(-tot_effect_norm$totalMuStar),
                               norm_sigma = rank(-tot_effect_norm$totalSigma),
                               init_mu.star = rank(-tot_effect_norm_init$totalMuStar),
                               init_sigma = rank(-tot_effect_norm_init$totalSigma) )
tot_effect_ranks[order(tot_effect_ranks$norm_mu.star),]



pdf(file.path(plotfolder, "total_normalized_effect_biomass.pdf"), height = 5, width = 10)
#pdf("plots/total_per_change_init_effect_biomass.pdf", height = 5, width = 10)
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1), mgp = c(2, 0.5, 0))

plot(tot_effect_norm$totalMuStar, tot_effect_norm$totalSigma, xlab = "mu.star", ylab = "sigma")
label_pts = which(tot_effect_norm$totalSigma > 3)
text(tot_effect_norm$totalMuStar[label_pts], tot_effect_norm$totalSigma[label_pts], labels = tot_effect_norm$param[label_pts], 
     pos = 2, cex = 0.8)
# same but zoom in 
plot(tot_effect_norm$totalMuStar, tot_effect_norm$totalSigma, xlab = "mu.star", ylab = "sigma", 
     xlim = c(0, 10), ylim = c(0, 10))
label_pts = which(tot_effect_norm$totalSigma > 2)
text(tot_effect_norm$totalMuStar[label_pts], tot_effect_norm$totalSigma[label_pts], labels = tot_effect_norm$param[label_pts], 
     pos = 2)
legend("bottomright", title = "(ZOOM)", legend = "", bty = "n")

dev.off()

# the numbers match for the biomass/stability plot below
pdf(file.path(plotfolder, "agg_mu.star_vs_sigma.pdf"), height = 5, width = 5)
par(mar = c(3, 3, 2, 1), mgp = c(2, 0.5, 0))
param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
param_info_plt = param_info[match(rownames(mu.star_stability), param_info$param_code),]

plot(tot_effect_norm$totalMuStar, tot_effect_norm$totalSigma, 
     pch = param_info_plt$invertIdx, col = param_palatte[param_info_plt$colIdx], 
     xlab = "mu.star", ylab = "sigma")
label_pts = which(tot_effect_norm$totalMuStar > 1) # 2 for first_try, 4 for two_noPPP
text(tot_effect_norm$totalMuStar[label_pts], tot_effect_norm$totalSigma[label_pts], labels = param_info_plt$code[label_pts], 
     pos = c(2, 4), cex = 0.8)
mtext("Aggregated normalized effects)", side = 3, line = 1)

dev.off()

#-----------heatmaps--------------------------------------------------------------------

pdf(file = file.path(plotfolder, "mu.star_norm_unit.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu.star_norm, "mu.star", param_info, group_info)
dev.off()

pdf(file = file.path(plotfolder, "mu_norm_unit.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu_norm, "mu", param_info, group_info, col = diverging_palette())
dev.off()

pdf(file = file.path(plotfolder, "sigma_norm_unit.pdf"), width = 12, height = 12)
plot_ee_heatmap(sigma_norm, "sigma", param_info, group_info)
dev.off()

pdf(file = file.path(plotfolder, "all_norm_unit.pdf"), width = 24, height = 12)
plot_mustar_mu_sigma(mu.star_norm, mu_norm, sigma_norm, param_info, group_info)
dev.off()

# normalized by init biomass--------------------------------------

pdf(file = file.path(plotfolder, "mu.star_norm_init.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu.star_norm_init, "mu.star", param_info, group_info, col = gray_palette(range = range(mu.star_norm_init)))
dev.off()

pdf(file = file.path(plotfolder, "mu_norm_init.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu_norm_init, "mu", param_info, group_info, col = diverging_palette(range = range(mu_norm_init)))
dev.off()

pdf(file = file.path(plotfolder, "sigma_norm_init.pdf"), width = 12, height = 12)
plot_ee_heatmap(sigma_norm_init, "sigma", param_info, group_info, col = gray_palette(range = range(sigma_norm_init)))
dev.off()

pdf(file = file.path(plotfolder, "all_norm_init.pdf"), width = 24, height = 12)
plot_mustar_mu_sigma(mu.star_norm_init, mu_norm_init, sigma_norm_init, param_info, group_info)
dev.off()

# organize by guild--------------------------------------
mu.star_plot = plot_ee_heatmap_by_guild(mu.star_norm, "mu.star", param_info, group_info)
mu_plot = plot_ee_heatmap_by_guild(mu_norm, "mu", param_info, group_info, col = diverging_palette())
sigma_plot = plot_ee_heatmap_by_guild(sigma_norm, "sigma", param_info, group_info)

pdf(file = file.path(plotfolder, "guild_all_norm_unit.pdf"), width = 24, height = 12)
mu.star_plot + mu_plot + sigma_plot
dev.off()

#█ norm init biomass
mu.star_plot = plot_ee_heatmap_by_guild(mu.star_norm_init, "mu.star", param_info, group_info, col = gray_palette(range = range(mu.star_norm_init)))
mu_plot = plot_ee_heatmap_by_guild(mu_norm_init, "mu", param_info, group_info, col = diverging_palette(range = range(mu_norm_init)))
sigma_plot = plot_ee_heatmap_by_guild(sigma_norm_init, "sigma", param_info, group_info, col = gray_palette(range = range(sigma_norm_init)))

pdf(file = file.path(plotfolder, "guild_all_norm_init.pdf"), width = 24, height = 12)
mu.star_plot + mu_plot + sigma_plot
dev.off()

#☻ look at difference between mu and mu.star (i.e. non-linear effects?)
# raw biomass
mu_diff = (mu.star - abs(mu))
pdf(file = file.path(plotfolder, "diff_mu_mu.star.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu_diff, "diff", param_info, group_info, col = gray_palette(range = range(mu_diff)))
dev.off()

mu_diff = log(mu_diff + 1)
pdf(file = file.path(plotfolder, "diff_log_mu_mu.star.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu_diff, "log(diff+1)", param_info, group_info, col = gray_palette(range = range(mu_diff)))
dev.off()

# percent positive, note that we select those that have more than a neglible effect
per_pos_imp = per_pos
per_pos_imp[which(mu.star_norm < 0.05, arr.ind = TRUE)] = -1
pdf(file = file.path(plotfolder, "percent_eff_positive.pdf"), width = 12, height = 12)
plot_ee_heatmap(per_pos_imp, "%pos", param_info, group_info, col = diverging_center_palette())
dev.off()

# for curiosity, plot relationship
plot(mu.star_norm[which(mu.star_norm > 0.05)], per_pos[which(mu.star_norm > 0.05, arr.ind = TRUE)], 
     xlab = "mu.star (normalized)", ylab = "percent positive",
     pch = 16, col = alpha("black", alpha = 0.5))

library(MASS)
z <- kde2d(mu.star_norm[which(mu.star_norm > 0.05)], per_pos[which(mu.star_norm > 0.05, arr.ind = TRUE)], n=50)
contour(z,  nlevels = 9, col= rev(brewer.pal(9, "RdGy")), add=TRUE)

# or try with ggplot
# https://stats.stackexchange.com/questions/31726/scatterplot-with-contour-heat-overlay

# ----------------- start PCA -----------------------------------------------------------
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

#start with mu.star

# transpose the data, this way we are analysing data on 40 types of groups, for each of which
# we have a measurement of the effect of a set of parameters
# need to remove mL_CLU_adult which has an effect of 0 on everything
mu.star_t = t(mu.star)
mu.star_t = mu.star_t[,-which(colnames(mu.star_t) == "mL_CLU_adult")]

# look at how parameters are organized by their similar effects on groups
mu.star_pca = prcomp(mu.star, center = TRUE, scale = TRUE)
summary(mu.star_pca)

# look at how groups are organized by how they are similar in their reaction to parameters
mu.star_t_pca = prcomp(mu.star_t, center = TRUE, scale = TRUE)
summary(mu.star_t_pca)

# PCA of parameters
pdf(file = file.path(plotfolder, "pca_mu.star_params_PC1-2.pdf"), width = 10, height = 10)
ggbiplot(mu.star_pca, labels = rownames(mu.star), ellipse = TRUE, groups = param_info$guild)
dev.off()

pdf(file = file.path(plotfolder, "pca_mu.star_params_PC3-4.pdf"), width = 10, height = 10)
ggbiplot(mu.star_pca, choices = c(3, 4), labels = rownames(mu.star), ellipse = TRUE, groups = param_info$guild)
dev.off()

pdf(file = file.path(plotfolder, "pca_mu.star_params_PC5-6.pdf"), width = 10, height = 10)
ggbiplot(mu.star_pca, choices = c(5, 6), labels = rownames(mu.star), ellipse = TRUE, groups = param_info$guild)
dev.off()

# PCA of groups
pdf(file = file.path(plotfolder, "pca_mu.star_groups_PC1-2.pdf"), width = 10, height = 10)
ggbiplot(mu.star_t_pca, labels = rownames(mu.star_t), ellipse = TRUE, groups = group_info$guild)
dev.off()

pdf(file = file.path(plotfolder, "pca_mu.star_groups_PC3-4.pdf"), width = 10, height = 10)
ggbiplot(mu.star_t_pca, choices = c(3, 4), labels = rownames(mu.star_t), ellipse = TRUE, groups = group_info$guild)
dev.off()

pdf(file = file.path(plotfolder, "pca_mu.star_groups_PC5-6.pdf"), width = 10, height = 10)
ggbiplot(mu.star_t_pca, choices = c(5, 6), labels = rownames(mu.star_t), ellipse = TRUE, groups = group_info$guild)
dev.off()


# some other plots
ggbiplot(mu.star_pca, labels = row_labels)
ggbiplot(mu.star_pca, obs.scale = 1, var.scale = 1, labels = row_labels, ellipse = TRUE, groups = pt_grouping)


# now try PCA on all groups' biomasses, so each sim is a measurement, then try to run morris on PC1 of that
# i.e. don't run a PCA on the morris output but the raw output

pca_biomass = prcomp(consolid_results_biomass[,-1], center = TRUE, scale = TRUE)
summary(pca_biomass)
pca_biomass$rotation[,1:5]

pca_nums = prcomp(consolid_results_nums[,-1], center = TRUE, scale = TRUE)
summary(pca_nums)
pca_nums$rotation[,1:5]

library(corrplot)
corrplot(cor(consolid_results_biomass[,-1]), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

library("PerformanceAnalytics")
chart.Correlation(consolid_results_biomass[,-1], histogram=TRUE, pch=19)


# -------------individual group plots --------------------------------------
param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
i = 10
most_influ = NULL
target_out = NULL
age_codes = get_age_group_codes(FUNCTIONAL_GROUPS_FILE)

pdf(file = file.path(plotfolder, "individual_mu.star_vs_sigma.pdf"), width = 20, height = 16)
par(mfrow = c(4, 5), mar = c(3, 3, 2, 0) + 0.1, mgp = c(1, 0.5, 0))

for (i in 1:ncol(mu.star))
{
  code = colnames(mu.star)[i]
  pchs = param_info$invertIdx
  pchs[param_info$code == code] = ifelse(code %in% age_codes, 17, 19)
  
  mu_thresh = mean(mu.star[,i]) + sd(mu.star[,i])
  sigma_thresh = mean(sigma[,i]) + sd(sigma[,i])
  label_pts = which(sigma[,i] > sigma_thresh | mu.star[,i] > mu_thresh)
  most_influ = c(most_influ, label_pts)
  target_out = c(target_out, rep(code, length(label_pts)))
  
  for (t in 2) # c(2, 4)
  {
#    png(filename = file.path(plotfolder, paste0(code, "_morris_ee_text_", t, ".png")), width = 600, height = 600)
    plot(mu.star[,i], sigma[,i], 
         pch = pchs, col = param_palatte[param_info$colIdx], 
         cex = 2.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
         xlab = "mu.star", ylab = "sigma", main = paste(get_group_long_names(code, FUNCTIONAL_GROUPS_FILE), code))
    text(mu.star[label_pts,i], sigma[label_pts,i], labels = param_info$code[label_pts], 
         pos = t, cex = 1.5)
    
    # dev.off()
    # 
    # png(filename = file.path(plotfolder, paste0(code, "_morris_mu_text_", t, ".png")), width = 600, height = 600)
    # plot(mu.star[,i], mu[,i], 
    #      pch = pchs, col = param_palatte[param_info$colIdx], 
    #      cex = 2.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
    #      xlab = "mu.star", ylab = "mu", main = get_group_long_names(code, FUNCTIONAL_GROUPS_FILE))
    # text(mu.star[label_pts,i], mu[label_pts,i], labels = param_info$longName[label_pts], 
    #      pos = t, cex = 1.5)
    
#    dev.off()
    
  }
  # print(paste(code, "mu thresh", round(mu_thresh), "pts >", sum(mu.star[,i] > mu_thresh),
  #             "sigma thresh", round(sigma_thresh), "pts >", sum(sigma[,i] > sigma_thresh)))
}
dev.off()






png(filename = file.path("plots", "legend_morris_ee.png"), width = 600, height = 350)

plot(0, 0, type = "n", xlim = c(0,1), ylim = c(0,0.5))
legend("topleft", pch = c(19, 19, 19, 1, 2), 
       col = c(param_palatte, "black", "black"), 
       legend = c("growth", "mortality", "recruitment", "invertebrate", "vertebrate"), 
       bty = "n", cex = 2)

dev.off()

table(row.names(mu)[most_influ])
table(param_info$param)[most_influ]
table(param_info$longName[most_influ])

most_most_influ = data.frame(table(most_influ))
most_most_influ$most_influ = as.numeric(as.character(most_most_influ$most_influ))
most_most_influ = most_most_influ[most_most_influ$Freq > 1,]
most_most_influ = cbind(group = param_info$longName[most_most_influ$most_influ], 
                        param = param_info$param[most_most_influ$most_influ], 
                        most_most_influ)
most_most_influ[order(most_most_influ$Freq, decreasing = TRUE),]

##############-------------------------------------------------------------------

# analyze how sample size changes effects -----------------------------------------

reps = 10
mu.star_10 = sample_morris_output(consolid_results, design_matrix, n_sample = 10, n_rep = reps)
mu.star_15 = sample_morris_output(consolid_results, design_matrix, n_sample = 15, n_rep = reps)
mu.star_20 = sample_morris_output(consolid_results, design_matrix, n_sample = 20, n_rep = reps)
mu.star_25 = sample_morris_output(consolid_results, design_matrix, n_sample = 25, n_rep = reps)
mu.star_30 = sample_morris_output(consolid_results, design_matrix, n_sample = 30, n_rep = reps)
mu.star_35 = sample_morris_output(consolid_results, design_matrix, n_sample = 35, n_rep = reps)

j = 15 # group
pdf(file.path(plotfolder, paste0("test_traj-size_", colnames(mu.star_10)[j], ".pdf")), width = 18, height = 12)
par(mfrow = c(9, 10), mar = c(2, 2, 2, 0), mgp = c(1, 0.5, 0))

for (i in 1:nrow(mu.star_10)) # for each parameter
{
  plot(c(rep(c(10, 15, 20, 25, 30, 35), each = reps)), 
       c(mu.star_10[i, j,], mu.star_15[i, j,], mu.star_20[i, j,], mu.star_25[i, j,], mu.star_30[i, j,], mu.star_35[i, j,]), 
       xlab = "", ylab = "",
       pch = 16, xlim = c(5, 40), ylim = range(mu.star_10[, j,]))
  abline(h = mu.star[i,j])
  mtext(rownames(mu.star_10)[i], side = 3, line = 0.5)
}
dev.off()

#-------------------------------------------------------------------------------
# stability
#-------------------------------------------------------------------------------


# for now change NA (sim crashed or group extict) to 0 so can count as unstable
stability_pval[which(is.na(stability_pval), arr.ind = TRUE)] = 0
stability_pval$per_stable = apply(stability_pval[,-1], 1, function(x) sum(x > 0.05) / length(x))
hist(stability_pval$per_stable)

hist(unlist(stability_range_norm[,-1]), xlim = c(0,1), breaks = 200)
stability_range_norm$per_stable = apply(stability_range_norm[,-1], 1, function(x) sum(x < 0.05, na.rm = TRUE) / length(x))

pdf(file = file.path(plotfolder, "stability_pval_vs_range.pdf"), width = 5, height = 5)
plot(stability_pval$per_stable, stability_range_norm$per_stable, col = alpha("black", alpha = 0.05), 
     pch = 16, xlab = "% stable (slope insig.)", ylab = "% stable (range < 5% * mean)")
abline(a = 0, b = 1, col = "red")
dev.off()

pdf(file = file.path(plotfolder, "stability_bloxplot_slopes.pdf"), width = 15, height = 5)
boxplot(as.matrix(stability_slope[,-1]), main = "Slope from last 10 yrs", las = 2 )#, ylim = c(-5e6, 8e7))
dev.off()
pdf(file = file.path(plotfolder, "stability_bloxplot_slopes_div_mean.pdf"), width = 15, height = 5)
boxplot(as.matrix(stability_slope_norm[,-1]), main = "Slope/mean from last 10 years", las = 2)
dev.off()

# per group, how often stable
stab_pval = apply(stability_pval[,-c(1,42)], 2, function(x) sum(x > 0.05) / length(x))

apply(stability_range_norm, 2, function(x) sum(!is.na(x)) / length(x)) # percent not NA
apply(stability_range_norm, 2, function(x) mean(x, na.rm = TRUE)) # of those, avg range
stab_range = apply(stability_range_norm[,-c(1,42)], 2, function(x) sum(x < 0.05, na.rm = TRUE) / length(x)) # how often stable

stab_cor = sapply(1:40, function(x) cor(stability_pval[,x+1] > 0.05, 
                                        !is.na(stability_range_norm[,x+1]) & stability_range_norm[,x+1] < 0.05, 
                                        use = "complete.obs"))

round(cbind(stab_pval, stab_range, stab_cor), digits = 2)

stability_pval[stability_pval$per_stable > 0.7,]

# stability using slope-in-range metric

pdf(file = file.path(plotfolder, "stability_slope_in_range.pdf"), width = 10, height = 5)
per_slope_in_range = apply(stability_slope_in_range[,-1], 2, function(x) sum(x, na.rm = TRUE) / length(x)) # percent stable per group
barplot(per_slope_in_range, las = 2, ylim = c(0, 1), ylab = "Proportion stable")
dev.off()

per_stable_per_sim = apply(stability_slope_in_range[,-1], 1, function(x) sum(x, na.rm = TRUE) / length(x)) # percent stable per sim
hist(per_stable_per_sim)
morris_out_stability = get_morris_output(design_matrix, data.frame(sim = stability_slope_in_range$sim, per = per_stable_per_sim))
mu.star_stability = calculate_mu.star(morris_out_stability$ee)
sigma_stability = calculate_sigma(morris_out_stability$ee)

# percent none or all stable
sum(per_stable_per_sim == 0) / length(per_stable_per_sim)
sum(per_stable_per_sim == 1) / length(per_stable_per_sim)

summary(per_stable_per_sim)

pdf(file.path(plotfolder, "stability_mu.star_vs_sigma.pdf"), height = 5, width = 10)
par(mfrow = c(1,2), mar = c(3, 3, 2, 1), mgp = c(2, 0.5, 0))
param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
param_info_plt = param_info[match(rownames(mu.star_stability), param_info$param_code),]

plot(tot_effect_norm$totalMuStar, tot_effect_norm$totalSigma, 
     pch = param_info_plt$invertIdx, col = param_palatte[param_info_plt$colIdx], 
     xlab = "mu.star", ylab = "sigma")
label_pts = which(tot_effect_norm$totalMuStar > 5) # 5 for first_try, 10 for two_noPPP
text(tot_effect_norm$totalMuStar[label_pts], tot_effect_norm$totalSigma[label_pts], labels = param_info_plt$code[label_pts], 
     pos = c(2, 4), cex = 0.8)
mtext("Biomass (aggregated normalized effects)", side = 3, line = 1)

legend("topleft", pch = c(19, 19, 19, 1, 2), 
       col = c(param_palatte, "black", "black"), 
       legend = c("growth", "mortality", "recruitment", "invertebrate", "vertebrate"), 
       bty = "n", cex = 0.8)

plot(mu.star_stability, sigma_stability, 
     pch = param_info_plt$invertIdx, col = param_palatte[param_info_plt$colIdx], 
     xlab = "mu.star", ylab = "")
label_pts = which(mu.star_stability > 0.1) # 0.1 for first_try, 0.05 for two_noPPP
text(mu.star_stability[label_pts], sigma_stability[label_pts], labels = param_info_plt$code[label_pts], 
     pos = c(2,4), cex = 0.8)
mtext("Number groups stable", side = 3, line = 1)

dev.off()


# most stable from SA_first_try
plot_stability_last_ten_years(output_folder, plotfolder, 11012)
plot_stability_last_ten_years(output_folder, plotfolder, 11042)
plot_stability_last_ten_years(output_folder, plotfolder, 12185)
plot_stability_last_ten_years(output_folder, plotfolder, 13827)
# some examples of just a couple of groups stable from SA_first_try
plot_stability_last_ten_years(output_folder, plotfolder, 10073)
plot_stability_last_ten_years(output_folder, plotfolder, 10517)
plot_stability_last_ten_years(output_folder, plotfolder, 10820)
plot_stability_last_ten_years(output_folder, plotfolder, 11339)

# most stable from SA_two_noPPP
plot_stability_last_ten_years(output_folder, plotfolder, 10671)
plot_stability_last_ten_years(output_folder, plotfolder, 12070)
plot_stability_last_ten_years(output_folder, plotfolder, 12682)
plot_stability_last_ten_years(output_folder, plotfolder, 13181)
# some examples of just a couple of groups stable from SA_two_noPPP
plot_stability_last_ten_years(output_folder, plotfolder, 10232)
plot_stability_last_ten_years(output_folder, plotfolder, 12539)
plot_stability_last_ten_years(output_folder, plotfolder, 10945)
plot_stability_last_ten_years(output_folder, plotfolder, 11588)
plot_stability_last_ten_years(output_folder, plotfolder, 10217)


# random forest to learn about stability ?
library(randomForest)
ind = sample(2, nrow(design_matrix), replace=TRUE, prob=c(0.7,0.3))
trainData = cbind(design_matrix[ind == 1, -1], per_stable = stability_pval$per_stable[ind == 1] * 40)
testData = cbind(design_matrix[ind == 2, -1], per_stable = stability_pval$per_stable[ind == 2] * 40)

per_stable_rf = randomForest(per_stable ~ ., data=trainData, ntree=200, proximity=TRUE)
plot(predict(per_stable_rf), trainData$per_stable)
plot(per_stable_rf)
importance(per_stable_rf)


####-------------------------------------------------------------------------------
# old plain heatmap

# make heat map
graysc = gray(seq(from = 1, to = 0, length = 256))
divsc = colorRampPalette(brewer.pal(11, "PiYG"))(256)
param_cols = brewer.pal(3, "Dark2")[param_info$colIdx]
group_cols = brewer.pal(3, "Set1")[group_info$invertIdx]
heatmap(mu.star, scale = "column", col = graysc) # this appears to be different and not what we want

# basic heatmap
heatmap(mu.star_norm, scale = "none", col = graysc,
        RowSideColors = param_cols, ColSideColors = group_cols)

heatmap(sigma_norm, scale = "none", col = graysc,
        RowSideColors = param_cols, ColSideColors = group_cols)

heatmap(mu_norm, scale = "none", col = divsc, 
        breaks = seq(from = -1, to = 1, length = length(divsc) + 1),
        RowSideColors = param_cols, ColSideColors = group_cols)

#### --------------- simulations gone amok analysis ----------------------------------------------------
range(consolid_results$CET)
hist(consolid_results$CET)
table(cut(consolid_results$CET, breaks = c(0, 0.01, 0.1, 0.5, 1, 10, 100, 1000, 1e8, 1e9, 1e10, 2e10)))

design_matrix_sub = design_matrix %>% filter(simID %in% consolid_results$sim) 
cet_idx = grep("CET", colnames(design_matrix_sub))

apply(design_matrix_sub[consolid_results$CET < 0.5, cet_idx], 2, table)
apply(design_matrix_sub[consolid_results$CET > 1000, cet_idx], 2, table)
apply(design_matrix_sub[consolid_results$CET > 1e8, cet_idx], 2, table)

# look at range for important params of top pred
range_output_for_param = function(sim_output, param_col)
{
  return (ldply(tapply(sim_output, 
                       design_matrix_sub[,grep(param_col, colnames(design_matrix_sub))], 
                       range)) )
}

range_output_for_param(consolid_results$CET, "KDENR_CET_single")
range_output_for_param(consolid_results$CET, "mL_CET_juv")
range_output_for_param(consolid_results$CET, "mL_CET_adult")

range_output_for_param(consolid_results$SB, "KDENR_SB_single")
range_output_for_param(consolid_results$SB, "mL_SB_juv")
range_output_for_param(consolid_results$SB, "mL_SB_adult")

range_output_for_param(consolid_results$SXX, "KDENR_SXX_single")
range_output_for_param(consolid_results$SXX, "mL_SXX_juv")
range_output_for_param(consolid_results$SXX, "mL_SXX_adult")

# look at all sims with the 'bad' param combos mL_juv = 1 and KDENR >= 5
# how many crashed and how many didn't

crash_sims = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)
crash_sims$y = rep(1, length(crash_sims$simID))
crash_sims$y[crash_sims$simID %in% sims_too_short] = 0

crash_sims_cet = crash_sims[crash_sims$mL_CET_juv == 1 & crash_sims$KDENR_CET_single >= 5,]
table(paste(crash_sims_cet$mL_CET_juv, crash_sims_cet$KDENR_CET_single), crash_sims_cet$y)
summary(consolid_results$CET[consolid_results$sim %in% crash_sims_cet$simID[crash_sims_cet$y == 1]])
nrow(crash_sims_cet)

# the rest is analysis not used ###############################

# now try to predict param combos that crash
# response is binary 0,1 if simulation crashed, predictors are param values, could be categorical or continuous
# uses sims_too_short caluculated in runConsolidateSimuationOutput.R
# this doesn't work because there are too many parameter values randomly but not enough data

crash_sims = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)
crash_sims$y = rep(1, length(crash_sims$simID))
crash_sims$y[crash_sims$simID %in% sims_too_short] = 0

crash_model = glm(y ~ . -simID, data = crash_sims)
summary(crash_model)

crash_interaction_model = glm(y ~ . + .^2, data = crash_sims[,-1])
summ_interaction_model = summary(crash_interaction_model)
var_signif = data.frame(summ_interaction_model$coef[summ_interaction_model$coef[,4] <= .05, ])

library(caret)
x <- crash_sims %>% select(-c(simID, y))
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))

pdf("plots/crashing_sims_density.pdf", width = 30, height = 30)
featurePlot(x = x[,], y = as.factor(crash_sims$y),
  plot = "density", scales = scales)
dev.off()

pdf("plots/crashing_sims_pairs.pdf", width = 50, height = 50)
pairs(crash_sims %>% filter(y == 0) %>% select(-c(simID, y)))
dev.off()


# table param values per traj
traj = rep(1:50, each = length(crash_sims$simID) / 50)
apply(crash_sims[crash_sims$simID %in% sims_too_short,-1], 2, function(i) table(i, traj[crash_sims$simID %in% sims_too_short]))

# sims where CET juv mort = 1 and KDENR high, others that didn't crash
bad_comb = crash_sims$simID[crash_sims$mL_CET_juv == 1 & crash_sims$KDENR_CET_single >= 5]
outcome = crash_sims$y
outcome[crash_sims$simID %in% setdiff(bad_comb, sims_too_short)] = 2


pdf("plots/crashing_sims_noncrash_density.pdf", width = 30, height = 30)
featurePlot(x = x[,], y = as.factor(outcome),
            plot = "density", scales = scales)
dev.off()

non_crashing = setdiff(bad_comb, sims_too_short)
design_matrix_ponly = crash_sims %>% select(-c(simID, y))
param_changed = rep(NA, length(non_crashing))
param_new_value = rep(NA, length(non_crashing))
param_changed_after = rep(NA, length(non_crashing))
param_new_value_after = rep(NA, length(non_crashing))

for( i in 1:length(non_crashing))
{
  rowIdx = non_crashing[i] - 10000
  
  colIdx = which(apply(design_matrix_ponly[(rowIdx-1):rowIdx,], 2, diff) != 0)
  param_changed[i] = ifelse(length(colIdx) > 1, NA, names(design_matrix_ponly)[colIdx])
  param_new_value[i] = ifelse(length(colIdx) > 1, NA, design_matrix_ponly[rowIdx, colIdx])
  
  colIdx = which(apply(design_matrix_ponly[rowIdx:(rowIdx+1),], 2, diff) != 0)
  param_changed_after[i] = ifelse(length(colIdx) > 1, NA, names(design_matrix_ponly)[colIdx])
  param_new_value_after[i] = ifelse(length(colIdx) > 1, NA, design_matrix_ponly[rowIdx, colIdx])
}

sims_nocrash  = cbind(non_crashing, param_changed, param_new_value, param_changed_after, param_new_value_after)

# all traj with this param combo
crash_sims$simID[crash_sims$mL_CET_juv == 1 & crash_sims$KDENR_CET_single == 5]

# those that didn't crash
setdiff(bad_comb, sims_too_short)

# summary crash happens when mL_CET_juv == 1 and KDENR_CET_single >= 5, this happened for 9 trajectories
# however, two trajectories when KDENR_CET_single == 5 and didn't crash and one that did, so a bit more marginal
# and somewhat dependant on other parameters for state of ecosystem

############################################################################################

# to create param_info and group_info files, need to have created a morris output summary, i.e. mu.star

#get init biomass, nums, for now use first sim output
load(file.path(output_folder, "AEEC_SA_sim10001.rdata")) # this loads metrics object into env

create_group_and_param_info(mu.star, metrics, FUNCTIONAL_GROUPS_FILE, PARAM_INFO_FILE, GROUP_INFO_FILE)
