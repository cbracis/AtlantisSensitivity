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

plot_ee_heatmap = function(ee, name, param_info, group_info, 
                           col = gray_palette(), ...)
{
  map = Heatmap(ee, name = name, column_title = "Group", row_title = "Parameter",
                row_names_gp = gpar(fontsize = 7), column_names_gp = gpar(fontsize = 9),
                col = col,
                left_annotation = row_annot(param_info), top_annotation = col_annot(group_info), 
                ...)
  return(map)
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
  row_annot = rowAnnotation(df = data.frame(type = unlist(param_info$name), age = unlist(param_info$suffix)),
                            col = list(type = c("C" = "darkgreen", "mum" = "darkgreen", 
                                                "mL" = "darkorange", "mQ" = "darkorange3",
                                                "BHalpha" = "plum1", "KDENR" = "plum3"),
                                       age = c("juv" = "gray80", "adult" = "gray60", "age" = "gray40", "single" = "black")))
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
  # make sure range is symmetric
  range[1] = min(range[1], -range[2])
  range[2] = max(-range[1], range[2])
  return( colorRamp2(breaks = seq(range[1], range[2], len = 11), brewer.pal(n = 11, name = "PiYG")) )
}