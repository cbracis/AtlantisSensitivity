library(plyr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(sensitivity)

source("consolidateSimulationOutput.R")
source("getGroups.R")
source("utilityFuncs.R")

source("plotOutput.R")

#-----------------------------------------------------------------------------------
# SA_first try
#-----------------------------------------------------------------------------------

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.90.100_41_traj.csv")
plotfolder = "plots/SA_first_try/biomass.90.100_traj_41"
consolid_results_60 = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.60.70_41_traj.csv")

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/biomass.60.70_50_traj.csv")
plotfolder = "plots/SA_first_try/biomass.60.70_traj_50"

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/nums.90.100_41_traj.csv")
plotfolder = "plots/SA_first_try/nums.90.100_traj_41"

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try/nums.60.70_50_traj.csv")
plotfolder = "plots/SA_first_try/nums.60.70_traj_50"

design_matrix = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------------
# SA_two_noPPP
#-----------------------------------------------------------------------------------

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP/biomass.90.100.csv")
plotfolder = "plots/SA_two_noPPP/biomass.90.100_temp"

design_matrix = read.csv(DESIGN_MATRIX_FILE_TWO, stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------------

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

plot(morris_out)
plot3d.morris(morris_out)

mu = calculate_mu(morris_out$ee)
mu.star = calculate_mu.star(morris_out$ee)
sigma = calculate_sigma(morris_out$ee)

# turn into data.frame, add column for split_param
# make colors for C, mL, etc
# make diff symbol for self params
# and category?
param_info = data.frame( t( sapply(row.names(mu.star), split_param, simplify = TRUE) ) )

age_codes = get_age_group_codes(FUNCTIONAL_GROUPS_FILE)

param_info$colIdx = 0
param_info$colIdx[param_info$name %in% c("C", "mum")] = 1
param_info$colIdx[param_info$name %in% c("mL", "mQ")] = 2
param_info$colIdx[param_info$name %in% c("BHalpha", "KDENR")] = 3
param_info$invertIdx = 1
param_info$invertIdx[param_info$group %in% age_codes] = 2
param_info$longName = get_group_long_names(param_info$group, FUNCTIONAL_GROUPS_FILE)
param_info$param_code = rownames(param_info)

group_info = data.frame(code = colnames(mu.star), stringsAsFactors = FALSE)
group_info$longName = get_group_long_names(group_info$code, FUNCTIONAL_GROUPS_FILE)
group_info$invertIdx = 1
group_info$invertIdx[group_info$code %in% age_codes] = 2


#get init biomass, for now use first sim output
load(file.path(output_folder, "AEEC_SA_sim10001.rdata"))
# now have object metrics
# NOTE!!!! changing Carrion DET from 0 to 1 to avoid divide by 0, so it won't be normalized
biomass0 = metrics$biomass %>% filter(time == 0) %>% 
  rename(biomass.init = atoutput) %>% 
  select(-time) %>% 
  mutate(biomass.init = ifelse(biomass.init == 0, 1, biomass.init))
group_info = right_join(group_info, biomass0, by = c("longName" = "species"))

# alternatively, add nums
nums0 = metrics$nums %>% filter(time == 0) %>% 
  rename(nums.init = atoutput) %>% 
  select(-time)
group_info = right_join(group_info, nums0, by = c("longName" = "species"))

# note that the merge can re-order the rows! need to be careful with the heat map plotting
group_info = group_info[match(colnames(mu.star), group_info$code),]



# normalized versions

mu_norm = normalize_effects_by_max(mu)
mu.star_norm = normalize_effects_by_max(mu.star)
sigma_norm = normalize_effects_by_max(sigma)


# for biomass, restart with results, reuse same morris_full
consolid_results_norm = normalize_results_by_init(consolid_results, 
                                                  group_info %>% select(code, biomass.init) %>% rename(val = biomass.init))
# or for nums
consolid_results_norm = normalize_results_by_init(consolid_results, 
                                                  group_info %>% select(code, nums.init) %>% rename(val = nums.init))


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
     pos = 2, cex = 0.8)
legend("bottomright", title = "(ZOOM)", legend = "", bty = "n")

dev.off()


#-----------heatmaps--------------------------------------------------------------------

pdf(file = file.path(plotfolder, "biomass_mu.star_norm_unit.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu.star_norm, "mu.star", param_info, group_info)
dev.off()

pdf(file = file.path(plotfolder, "biomass_mu_norm_unit.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu_norm, "mu", param_info, group_info, col = diverging_palette())
dev.off()

pdf(file = file.path(plotfolder, "biomass_sigma_norm_unit.pdf"), width = 12, height = 12)
plot_ee_heatmap(sigma_norm, "sigma", param_info, group_info)
dev.off()

pdf(file = file.path(plotfolder, "biomass_all_norm_unit.pdf"), width = 24, height = 12)
plot_mustar_mu_sigma(mu.star_norm, mu_norm, sigma_norm, param_info, group_info)
dev.off()

# normalized by init biomass--------------------------------------

pdf(file = file.path(plotfolder, "biomas_mu.star_norm_init.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu.star_norm_init, "mu.star", param_info, group_info, col = gray_palette(range = range(mu.star_norm_init)))
dev.off()

pdf(file = file.path(plotfolder, "biomass_mu_norm_init.pdf"), width = 12, height = 12)
plot_ee_heatmap(mu_norm_init, "mu", param_info, group_info, col = diverging_palette(range = range(mu_norm_init)))
dev.off()

pdf(file = file.path(plotfolder, "biomass_sigma_norm_init.pdf"), width = 12, height = 12)
plot_ee_heatmap(sigma_norm_init, "sigma", param_info, group_info, col = gray_palette(range = range(sigma_norm_init)))
dev.off()

pdf(file = file.path(plotfolder, "biomass_all_norm_init.pdf"), width = 24, height = 12)
plot_mustar_mu_sigma(mu.star_norm_init, mu_norm_init, sigma_norm_init, param_info, group_info)
dev.off()


#☻ look at difference between mu and mu.star (i.e. non-linear effects?)
# would be better with normalized biomass
mu_diff = mu.star - abs(mu)
Heatmap(mu_diff, name = "diff", column_title = "Group", row_title = "Parameter",
        row_names_gp = gpar(fontsize = 7), column_names_gp = gpar(fontsize = 9),
        left_annotation = row_annot(param_info), top_annotation = col_annot(group_info),
        col = colorRamp2(breaks = range(mu_diff), colors = c("white", "black")))



param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
i = 10
most_influ = NULL
target_out = NULL
for (i in 1:ncol(mu.star))
{
  code = colnames(mu.star)[i]
  pchs = param_info$invertIdx
  pchs[param_info$group == code] = ifelse(code %in% age_codes, 17, 19)
  
  mu_thresh = mean(mu.star[,i]) + sd(mu.star[,i])
  sigma_thresh = mean(sigma[,i]) + sd(sigma[,i])
  label_pts = which(sigma[,i] > sigma_thresh | mu.star[,i] > mu_thresh)
  most_influ = c(most_influ, label_pts)
  target_out = c(target_out, rep(code, length(label_pts)))
  
  for (t in 2) # c(2, 4)
  {
    png(filename = file.path(plotfolder, paste0(code, "_morris_ee_text_", t, ".png")), width = 600, height = 600)
    plot(mu.star[,i], sigma[,i], 
         pch = pchs, col = param_palatte[param_info$colIdx], 
         cex = 2.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
         xlab = "mu.star", ylab = "sigma", main = get_group_long_names(code, FUNCTIONAL_GROUPS_FILE))
    text(mu.star[label_pts,i], sigma[label_pts,i], labels = param_info$longName[label_pts], 
         pos = t, cex = 1.5)
    
    dev.off()
    
    png(filename = file.path(plotfolder, paste0(code, "_morris_mu_text_", t, ".png")), width = 600, height = 600)
    plot(mu.star[,i], mu[,i], 
         pch = pchs, col = param_palatte[param_info$colIdx], 
         cex = 2.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
         xlab = "mu.star", ylab = "mu", main = get_group_long_names(code, FUNCTIONAL_GROUPS_FILE))
    text(mu.star[label_pts,i], mu[label_pts,i], labels = param_info$longName[label_pts], 
         pos = t, cex = 1.5)
    
    dev.off()
    
  }
  
  # print(paste(code, "mu thresh", round(mu_thresh), "pts >", sum(mu.star[,i] > mu_thresh),
  #             "sigma thresh", round(sigma_thresh), "pts >", sum(sigma[,i] > sigma_thresh)))
}

png(filename = file.path("plots", "legend_morris_ee.png"), width = 600, height = 350)

plot(0, 0, type = "n", xlim = c(0,1), ylim = c(0,0.5))
legend("topleft", pch = c(19, 19, 19, 1, 2), 
       col = c(param_palatte, "black", "black"), 
       legend = c("growth", "mortality", "recruitment", "invertebrate", "vertebrate"), 
       bty = "n", cex = 2)

dev.off()

table(row.names(mu)[most_influ])
table(unlist(param_info$name)[most_influ])
table(param_info$longName[most_influ])

most_most_influ = data.frame(table(most_influ))
most_most_influ$most_influ = as.numeric(as.character(most_most_influ$most_influ))
most_most_influ = most_most_influ[most_most_influ$Freq > 1,]
most_most_influ = cbind(group = param_info$longName[most_most_influ$most_influ], 
                        param = unlist(param_info$name[most_most_influ$most_influ]), 
                        most_most_influ)
most_most_influ[order(most_most_influ$Freq, decreasing = TRUE),]

####
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
cet_idx = grep("CET", colnames(design_matrix_sub))

apply(design_matrix_sub[consolid_results$CET < 0.5, cet_idx], 2, table)
apply(design_matrix_sub[consolid_results$CET > 1e9, cet_idx], 2, table)

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

