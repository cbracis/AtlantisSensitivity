# functions to plot output
# fancy heatmaps
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")

# old method for older R
#source("https://bioconductor.org/biocLite.R")
#biocLite("ComplexHeatmap")

library(ComplexHeatmap)
library(circlize)
library(dendextend)
library(RColorBrewer)

# some constants
param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
type_palatte = alpha(brewer.pal(3, "Dark2"), 0.8)

type_cols = c("growth" = type_palatte[1], "mortality" = type_palatte[2], "recruitment" = type_palatte[3])
age_cols = c("juv" = "gray100", "adult" = "gray90", "age" = "gray80", "single" = "gray70")

guild_order = c("Recycl. / Primary prod.", "Zooplankton", "Epibenthos", "Filter feeder", "Squid",
                "Fish - demersal", "Fish - pelagic", "Shark / ray", "Mammal / bird")
code_order = c("DET", "DL", "DR", "BB", "PB", "PP",
               "ZOO", "ZOC", "ZOG",
               "DEP", "ECH", "WHE", "SHP", "CRA", "LBE",
               "SUS", "SCE", "BIV",
               "CEP",
               "SMD", "GAD", "MUL", "GUX", "SPA", "OFF", "DAB", "PLE", "SOL", "BSS", "LBT", "POL", "WHG",
               "CLU", "MAC",
               "RAY", "SHK",
               "SB", "SXX", "CET")

plot_ee_heatmap = function(ee, name, param_info, group_info, 
                           col = gray_palette(), ...)
{
  param_info = param_info[match(rownames(ee), param_info$param_code),]
  group_info = group_info[match(colnames(ee), group_info$code),]
  
  map = Heatmap(ee, name = name, column_title = "Group", row_title = "Parameter",
                row_names_gp = gpar(fontsize = 7), column_names_gp = gpar(fontsize = 9),
                col = col,
                left_annotation = row_annot(param_info), top_annotation = col_annot(group_info), 
                ...)
  return(map)
}

plot_ee_heatmap_by_guild = function(ee, name, param_info, group_info, title, annotate_type = FALSE,
                                    col = gray_palette(), ...)
{
  param_info = param_info[match(rownames(ee), param_info$param_code),]
  group_info = group_info[match(colnames(ee), group_info$code),]
  
  
  # need to resort the matrix in order of the guilds and codes specified above
  group_info = group_info[order(match(group_info$guild, guild_order), match(group_info$code, code_order)),]
  ee = ee[,match(group_info$code, colnames(ee))]
  
  param_info = param_info[order(match(param_info$guild, guild_order), match(param_info$code, code_order)),]
  ee = ee[match(param_info$param_code, rownames(ee)),]
  
  guild_cols = brewer.pal(length(unique(c(param_info$guild, group_info$guild))), "Pastel1")
  names(guild_cols) = sort(unique(c(param_info$guild, group_info$guild)))
  
  row_annot = rowAnnotation(df = data.frame(guild = param_info$guild),
                            col = list(guild = guild_cols), show_annotation_name = FALSE)
  col_annot = columnAnnotation(df = data.frame(guild = group_info$guild),
                               col = list(guild = guild_cols), show_annotation_name = FALSE)
  type_annot = if (annotate_type) { rowAnnotation(df = data.frame(type = param_info$param_cat), col = list(type = type_cols))}
                                  else { NULL }
  
  map = Heatmap(ee, name = name, column_title = title, row_title = "",
                row_labels = param_info$displayName, 
                row_names_gp = gpar(fontsize = 7), column_names_gp = gpar(fontsize = 9),
                col = col, cluster_rows = FALSE, cluster_columns = FALSE,
                left_annotation = row_annot, right_annotation = type_annot, top_annotation = col_annot, 
                ...)
  
  return(map)
}

plot_corr_plot = function(consolid_results, group_info, add_missing_groups = FALSE)
{
  require(corrplot)
  if (add_missing_groups)
  {
    # for nums, add invertebrate groups too so plot is same size
    missing_groups = setdiff(group_info$code, names(consolid_results))
    row_names = c(names(consolid_results), missing_groups)
    consolid_results = cbind(consolid_results, rep(list(NA), length(missing_groups)))
    names(consolid_results) = row_names
  }
  
  group_info = group_info[order(match(group_info$guild, guild_order), match(group_info$code, code_order)),]
  consolid_results = consolid_results[,-1] # get rid of sim column
  consolid_results = consolid_results[,na.exclude(match(group_info$code, colnames(consolid_results)))] # reorder like heatmap by guild
  
  corrplot(cor(consolid_results), type = "full", order = "original",
           tl.col = "black", tl.srt = 90, diag = FALSE,
           na.label = "x", na.label.col = "gray85")
  
}

plot_mustar_mu_sigma = function(mu.star, mu, sigma, param_info, group_info)
{
  # make a dendogram based on mu.star to use for all
  row_dend = hclust(dist(mu.star))
  col_dend = hclust(dist(t(mu.star)))
  
  mu.star = plot_ee_heatmap(mu.star, "mu.star", param_info, group_info, col = gray_palette(range(mu.star)),
                            cluster_rows = color_branches(row_dend, k = 5), 
                            cluster_columns = color_branches(col_dend, k = 5))
  mu = plot_ee_heatmap(mu, "mu", param_info, group_info, col = diverging_palette(range(mu)),
                       cluster_rows = color_branches(row_dend, k = 5), 
                       cluster_columns = color_branches(col_dend, k = 5))
  sigma = plot_ee_heatmap(sigma, "sigma", param_info, group_info, col = gray_palette(range(sigma)),
                          cluster_rows = color_branches(row_dend, k = 5), 
                          cluster_columns = color_branches(col_dend, k = 5))
  
  mu.star + mu + sigma
}

row_annot = function(param_info)
{
  row_annot = rowAnnotation(df = data.frame(type = param_info$param, age = unlist(param_info$suffix)),
                            col = list(type = type_cols, age = age_cols))
  return(row_annot)
}

col_annot = function(group_info)
{
  col_annot = columnAnnotation(df = data.frame(vertebrate = group_info$invertIdx - 1),
                               col = list(vertebrate = c("0" = "khaki", "1" = "lightblue")))
  
}

gray_palette = function(range = c(0, 1))
{
  return( colorRamp2(breaks = range, colors = c("white", "black")) )
}

diverging_palette = function(range = c(-1, 1))
{
  # make sure range is symmetric around 0
  range[1] = min(range[1], -range[2])
  range[2] = max(-range[1], range[2])
  return( colorRamp2(breaks = seq(range[1], range[2], len = 11), brewer.pal(n = 11, name = "PiYG")) )
}

diverging_center_palette = function(range = c(0, 1), center = 0.5)
{
  delta = max(abs(center - range))
  range = center + c(-delta, delta)
  return( colorRamp2(breaks = c(-1, seq(range[1], range[2], len = 7)), c("#FFFFFF", brewer.pal(n = 7, name = "Spectral"))) )
}

plot_stability_last_ten_years = function(output_dir, plot_folder, sim_id, normalize = FALSE)
{
  pdf(file = file.path(plot_folder, paste0("stability_sim", sim_id, ".pdf")), width = 15, height = 8)
  
  last_ten_years = read.csv(file.path(output_folder, paste0("AEEC_SA_sim",
                                                   sim_id,
                                                   "_last_ten_years_avg_nonage.csv")), stringsAsFactors = FALSE)
  # plot last 10 years
  par(mfrow = c(4,5), mar = c(2, 2, 3, 1), mgp = c(2, 0.5, 0))
  for(i in 1:nrow(last_ten_years))
  {
    sp_long = last_ten_years$species[i]
    sp_data = unlist(last_ten_years[i, grepl("biomass", names(last_ten_years))]) # / 
    #   group_info$biomass.init[group_info$longName == sp_long]
    if (normalize)
    {
      sp_data = sp_data / mean(sp_data)
    }
    years = 91:100
    plot(x = years, y = sp_data, type = "l", main = sp_long)
    
    if (all(!is.na(sp_data)) & all(sp_data > 0.1))
    {
      data_mean = mean(sp_data)
      
      polygon(c(min(years), min(years), max(years), max(years)), 
              c(data_mean * c(0.975, 1.025, 1.025, 0.975)), 
              col = alpha("gray50", alpha = 0.3), border = NA)
      abline(h = data_mean, col = ifelse(max(diff(range(sp_data))) > 0.05 * data_mean, "red", "black"), lty = 2)
      
      sp_mod = lm(sp_data ~ years)
      p_val = summary(sp_mod)$coefficients[2,4]
      abline(sp_mod, col = ifelse(p_val < 0.05, "red", "black"))
      
      lines(x = years, y = sp_data)
    }
    else
    {
      text(x = mean(years), y = mean(sp_data), labels = "extinct", col = "red")
    }
  }
  dev.off()
}

plot_mu_vs_sigma_by_group = function(mu.star, sigma, param_info, group_info, 
                                     which_cols = 1:ncol(mu.star))
{
  for (i in which_cols)
  {
    code = colnames(mu.star)[i]
    param_info = param_info[match(rownames(mu.star), param_info$param_code),]
    pchs = param_info$invertIdx

    mu_thresh = mean(mu.star[,i]) + sd(mu.star[,i])
    sigma_thresh = mean(sigma[,i]) + sd(sigma[,i])
    label_pts = which(sigma[,i] > sigma_thresh | mu.star[,i] > mu_thresh)
    plot_lim = c(0, max(mu.star[,i], sigma[,i])) # to have asp = 1, using that makes x-axis neg in some cases

    plot(mu.star[,i], sigma[,i], 
         pch = pchs, col = param_palatte[param_info$colIdx], bty = "l", 
         xlim = plot_lim, ylim = plot_lim,
         cex = 2.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
         xlab = "", ylab = "", 
         main = paste0(code, " (", group_info$longName[group_info$code == code], ")"))
    abline(a = 0, b = 0.1, lty = 2, col = alpha("black", 0.5))
    abline(a = 0, b = 0.5, lty = 3, col = alpha("black", 0.5))
    abline(a = 0, b = 1, lty = 4, col = alpha("black", 0.5))
    text(mu.star[label_pts,i], sigma[label_pts,i], labels = param_info$code[label_pts], 
         pos = 2, cex = 1.5)
  }
  
  # legend("topleft", pch = c(19, 19, 19, 1, 2), 
  #        col = c(param_palatte, "black", "black"), 
  #        legend = c("growth", "mortality", "recruitment", "invertebrate", "vertebrate"), 
  #        bty = "n", cex = 2)
  

  mtext("mu.star", side = 1, line = 0, cex = 1.5, outer = TRUE)
  mtext("sigma", side = 2, line = 0, cex = 1.5, outer = TRUE)
  
}

# assume we have 3 diff sample sizes of 10, 20, 30
plot_subset_traj = function(mu.star, mu.star_10, mu.star_20, mu.star_30, mu.star_40, ylim = NULL, param_info)
{
  for (j in 1:ncol(mu.star)) # group
  {
    # note -4 not -5 to get 5th biggest
    topx = 5 # num of params to consider
    topx_param =  names(which(mu.star[,j] >= sort(mu.star[,j])[nrow(mu.star) - topx + 1]))
    topx_vals = mu.star[topx_param, j]
    topx_disp_name = param_info$displayName[match(topx_param, param_info$param_code)]
    topx_disp_name = sub(" ", "\n", topx_disp_name, fixed = TRUE) # write on 2 lines or too long
  
    sub_10 = t(mu.star_10[topx_param, j,])
    sub_20 = t(mu.star_20[topx_param, j,])
    sub_30 = t(mu.star_30[topx_param, j,])
    sub_40 = t(mu.star_40[topx_param, j,])
    # combine but first normalize by value from 50 traj, not R divides matricies by col
    out = cbind((sub_10 - topx_vals[col(sub_10)]) / topx_vals[col(sub_10)],
                (sub_20 - topx_vals[col(sub_20)]) / topx_vals[col(sub_20)],
                (sub_30 - topx_vals[col(sub_30)]) / topx_vals[col(sub_30)],
                (sub_40 - topx_vals[col(sub_40)]) / topx_vals[col(sub_40)])
  
    # now need to reorder by param and not by num samples 1, 6, 11, 2, 7, ... if x = 5 and 3 samples each
    out = out[, as.vector(sapply(1:topx, function(x) seq(from = x, by = topx, length = 4)))]
    
    if (is.null(ylim)) { ylim = range(out) }
    yax = if ((j %% 5) == 1) { list() } else { list(yaxt = "n") }

    boxplot(out, xlab = "", ylab = "", xaxt = "n", pars = yax, ylim = ylim,
         col = c("gray80", "gray60", "gray40", "gray20"))
    abline(h = 0, col = "red")
    #abline(h = mu.star[topx_param,j], lty = 1:5)
    mtext(topx_disp_name, side = 1, line = 1, at = seq(from = 2, by = 4, length = topx), cex = 0.5)
    mtext(colnames(mu.star)[j], side = 3, line = 0.5)
    
    if (j == 1)
    {
      legend("topleft", legend = c(10, 20, 30, 40), fill = c("gray80", "gray60", "gray40", "gray20"), bty = "n", x.intersp=0.6, y.intersp=0.7)
    }
  }
  mtext("error", side = 2, line = 0, cex = 1, outer = TRUE, las = 0)
  
}

plot_subset_order = function(order_10, order_20, order_30, order_40, legend = TRUE)
{
  means = data.frame(code = colnames(order_10))
  means$samp_10 = apply(order_10, 2, mean)
  means$samp_20 = apply(order_20, 2, mean)
  means$samp_30 = apply(order_30, 2, mean)
  means$samp_40 = apply(order_40, 2, mean)
  
  sds = data.frame(code = colnames(order_10))
  sds$samp_10 = apply(order_10, 2, sd)
  sds$samp_20 = apply(order_20, 2, sd)
  sds$samp_30 = apply(order_30, 2, sd)
  sds$samp_40 = apply(order_40, 2, sd)
  
  n_group = nrow(means)
  
  plot(1:n_group - 0.3, means$samp_10, pch = 15, bty = "l", cex = 0.8,
       xaxt = "n", xlim = c(1.5, n_group - 0.5), ylim = c(0, 1), xlab = "", ylab = "agreement")
  segments(x0 = 1:n_group - 0.3, x1 = 1:n_group - 0.3, y0 = means$samp_10 - sds$samp_10, y1 = means$samp_10 + sds$samp_10)

  points(1:n_group - 0.1, means$samp_20, pch = 16, cex = 0.8)
  segments(x0 = 1:n_group - 0.1, x1 = 1:n_group - 0.1, y0 = means$samp_20 - sds$samp_20, y1 = means$samp_20 + sds$samp_20)

  points(1:n_group + 0.1, means$samp_30, pch = 17,  cex = 0.8)
  segments(x0 = 1:n_group + 0.1, x1 = 1:n_group + 0.1, y0 = means$samp_30 - sds$samp_30, y1 = means$samp_30 + sds$samp_30)

  points(1:n_group + 0.3, means$samp_40, pch = 18)
  segments(x0 = 1:n_group + 0.3, x1 = 1:n_group + 0.3, y0 = means$samp_40 - sds$samp_40, y1 = means$samp_40 + sds$samp_40)
  
  abline(v = seq(from = 1.5, to = n_group, by = 1), col = "gray80")
  
  mtext(means$code, side = 1, line = 0.5, at = 1:n_group, cex = 0.9, las = 2)
  
  if (legend)
  {
    legend("bottomright", legend = c(10, 20, 30, 40), pch = 15:18, pt.cex = c(0.8, 0.8, 0.8, 1), lty = 1, box.col = "white", bg = "white" )
  }
  
  axis(1, lwd.tick=0, labels=FALSE) # redraw x-axis
  
}
