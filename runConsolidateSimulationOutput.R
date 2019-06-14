source("consolidateSimulationOutput.R")
source("writeParams.R")
source("getGroups.R")
source("utilityFuncs.R")
library(RColorBrewer)
library(scales)


# start with first 6 trajectories, that is simulations 10001-10546
sims = 10092:10546
sims = 10001:14550
files = file.path("Y:/SA_first_try/output_done", paste0("AEEC_SA_sim",
                                                        sims,
                                                        "_ten_year_avg_nonage.csv"))
traj = rep(1:(length(sims)/91), each = 91)
# first do some clean up to run post processing on missing files
# this only needs to be done once, then no longer missing
missing = extract_sim_number(files[!file.exists(files)])

# try to recalcualte output, then re-run missing
#missing_retry = missing
missing_retry = 11634:11637
for (sim in missing_retry)
{
  print(sim)
  system2(command = "Rscript",
          args = paste("runProcessOutput.R",
                       "-a Y:/SA_first_try/datahome/atlantis-parameter-files/AEEC_F_UNIXv6290/simu -o Y:/SA_first_try/output_done", 
                       sim))
}


missing = as.numeric(missing)
sim_done = is.na(match(sims, missing))
tapply(sim_done, traj, sum)

complete_traj = which(tapply(sim_done, traj, sum) == 91)
complete_sims = sims[traj %in% complete_traj]

files = file.path("Y:/SA_first_try/output_done", paste0("AEEC_SA_sim",
                                                        complete_sims,
                                                        "_ten_year_avg_nonage.csv"))

# some are still missing because atlantis didn't finish
# delete those and retry
consolidate_output_avgs(files, "biomass.90.100", "code", 
                        "C:/Atlantis/AEEC_SA_test_output/output-consolid")

consolid_results = read.csv("C:/Atlantis/AEEC_SA_test_output/output-consolid/biomass.90.100.csv")
# TODO there are some NAs that should be 0
consolid_results = consolid_results %>% mutate_all(funs(replace(., which(is.na(.)), 0))) 

library(sensitivity)
design_matrix = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)

# for now subselect design matrix for just some sims
design_matrix_sub = as.matrix(design_matrix %>% filter(simID %in% complete_sims) %>% select(-simID))

# now update morris_full object with current data from createMorrisPlan.R
morris_full$r = nrow(design_matrix_sub) / (ncol(design_matrix_sub) + 1)
morris_full$X = design_matrix_sub
morris_out = tell(morris_full, as.matrix(consolid_results[,-1])) # -1 to remove sim col

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

param_palatte = alpha(brewer.pal(3, "Dark2"), 1)
i = 10
most_influ = NULL
for (i in 1:ncol(mu.star))
{
  code = colnames(mu.star)[i]
  pchs = param_info$invertIdx
  pchs[param_info$group == code] = ifelse(code %in% age_codes, 17, 19)
  
  mu_thresh = mean(mu.star[,i]) + sd(mu.star[,i])
  sigma_thresh = mean(sigma[,i]) + sd(sigma[,i])
  label_pts = which(sigma[,i] > sigma_thresh | mu.star[,i] > mu_thresh)
  most_influ = c(most_influ, label_pts)
  
  for (t in c(2, 4)) 
  {
    png(filename = file.path("plots", paste0(code, "_morris_ee_text_", t, ".png")), width = 600, height = 600)
    plot(mu.star[,i], sigma[,i], 
         pch = pchs, col = param_palatte[param_info$colIdx], 
         cex = 2.5, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
         xlab = "mu.star", ylab = "sigma", main = get_group_long_names(code, FUNCTIONAL_GROUPS_FILE))
    text(mu.star[label_pts,i], sigma[label_pts,i], labels = param_info$longName[label_pts], 
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


#### --------------- simulation clean up ----------------------------------------------------

# figure out which simulations are really done and just need to be copied
scratch_output_dir = "S:/output"
scratch_done_dir = "S:/output_done"
status_dirs = list.dirs(scratch_output_dir)
status_dirs = status_dirs[-1] # or else it will have S:/output too

for (simdir in status_dirs)
{
  print(simdir)
  status_text = readLines(gsub(basename(simdir), 
                               paste0("status_", basename(simdir), ".txt"),
                               simdir) )
  if (any(status_text == "R script finished"))
  {
    output_files = dir(simdir, pattern="AEEC_SA*")
    file.rename( file.path(simdir, output_files ), 
                 file.path(scratch_done_dir, output_files))
    if (length(dir(path = simdir, pattern = "AEEC_SA*")) == 0) # directory empty
    {
      unlink(simdir, recursive = TRUE)
    }
  }
}

# figure out which need to be re-run
sims = 10547:12548
no_file = NULL
not_done = NULL

for (s in sims)
{
  status_file = file.path(scratch_output_dir, paste0("status_sim", s, ".txt"))
  if (file.exists(status_file))
  {
    status_text = readLines(status_file)
    if ( ! any(status_text == "R script finished") )
    {
      not_done = c(not_done, s)
    }
  } else
  {
    no_file = c(no_file, s)
  }
}
