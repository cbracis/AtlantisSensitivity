
source("consolidateSimulationOutput.R")
#source("getGroups.R")
#source("utilityFuncs.R")

#-----------------------------------------------------------------------------------
# SA_first try

sims = 10001:14550
output_folder = "Y:/SA_first_try/output_done"
results_folder = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_first_try"

#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# SA_two_noPPP

sims = 10001:13400
output_folder = "Y:/SA_two_noPPP/output_done"
results_folder = "C:/Atlantis/AEEC_SA_test_output/output-consolid/SA_two_noPPP"

#-----------------------------------------------------------------------------------

missing = get_missing_sims(sims, output_folder, "_ten_year_avg_nonage.csv")

# TODO temporary circular workaround for problem of simulations that suddenly have all groups 0's
#missing = c(missing, consolid_results$sim[bad_rows])
# missing = sims_too_short

complete_sims = get_sims_for_complete_trajs(sims, 50, missing)

files = file.path(output_folder, paste0("AEEC_SA_sim",
                                        complete_sims,
                                        "_ten_year_avg_nonage.csv"))

# biomass
consolidate_output_avgs(files, "biomass.90.100", "code", results_folder)
consolidate_output_avgs(files, "biomass.60.70", "code", results_folder)

# nums
consolidate_output_avgs(files, "nums.90.100", "code", results_folder)
consolidate_output_avgs(files, "nums.60.70", "code", results_folder)

# stability
complete_sims = sims # will fill in not stable for crashing sims
files = file.path(output_folder, paste0("AEEC_SA_sim",
                                        complete_sims,
                                        "_last_ten_years_avg_nonage.csv"))
consolidate_stability(files, "biomass", results_folder)

#### --------------- simulation clean up ----------------------------------------------------

# verify all simulation files on the NAS
sims = 10001:14550
output_done = "Y:/SA_first_try/output_done"

prefix = "AEEC_SA_sim"
nc_postfix = c(".nc")
csv_postfix = c("_five_year_avg_age.csv", "_five_year_avg_nonage.csv", "_ten_year_avg_age.csv", "_ten_year_avg_nonage.csv")

complete = NULL
missing_nc = NULL
missing_csv = NULL

for (s in sims)
{
  nc_exists = file.exists(file.path(output_done, paste0(prefix, s, nc_postfix)))
  csv_exists = all(file.exists(file.path(output_done, paste0(prefix, s, csv_postfix))))
  
  if (nc_exists & csv_exists) { complete = c(complete, s) }
  else
  {
    if (!nc_exists) { missing_nc = c(missing_nc, s) }
    if (!csv_exists) { missing_csv = c(missing_csv, s) }
  }
}


########## which simulations ended early #############

sims_too_short = list.files(output_done, "*incorrect*")
sims_too_short = gsub(".*sim([0-9]+)\\..*$", "\\1", sims_too_short)
sims_too_short = as.numeric(sims_too_short)

# redo for other problem sims, those filled with zeros, list from consolid_results$sim[bad_rows] below
#sims_too_short = c(13126, 13179, 13197, 13657, 13679, 13692, 13718, 
#                   13727, 13764, 13802, 13850, 13870, 13993, 14233)


sims_too_short_output_nc = file.path(output_done, paste0("AEEC_SA_sim", sims_too_short, ".nc"))

sims_too_short_actual_length = rep(NA, length(sims_too_short))
param_changed = rep(NA, length(sims_too_short))
param_new_value = rep(NA, length(sims_too_short))
param_changed_after = rep(NA, length(sims_too_short))
param_new_value_after = rep(NA, length(sims_too_short))
design_matrix_ponly = design_matrix[,-1]

for( i in 1:length(sims_too_short_output_nc))
{
  sims_too_short_actual_length[i] = get_atlantis_output_length(sims_too_short_output_nc[i])
  rowIdx = sims_too_short[i] - 10000

  colIdx = which(apply(design_matrix_ponly[(rowIdx-1):rowIdx,], 2, diff) != 0)
  param_changed[i] = ifelse(length(colIdx) > 1, NA, names(design_matrix_ponly)[colIdx])
  param_new_value[i] = ifelse(length(colIdx) > 1, NA, design_matrix_ponly[rowIdx, colIdx])
  
  colIdx = which(apply(design_matrix_ponly[rowIdx:(rowIdx+1),], 2, diff) != 0)
  param_changed_after[i] = ifelse(length(colIdx) > 1, NA, names(design_matrix_ponly)[colIdx])
  param_new_value_after[i] = ifelse(length(colIdx) > 1, NA, design_matrix_ponly[rowIdx, colIdx])
  
}

# how many years?
(sims_too_short_actual_length - 1) / 5
traj_start = seq(from = 1, by = 91, length = 50) + 10000
is_traj_start = match(sims_too_short, traj_start)
is_traj_end = match(sims_too_short, traj_start + 90)
traj_num = traj[sims_too_short - 10000]

sims_short = cbind(sims_too_short, traj_num, (sims_too_short_actual_length - 1) / 5, 
                   param_changed, param_new_value, param_changed_after, param_new_value_after)

write.csv(sims_short, "files/problemSimulations.csv", quote = FALSE, row.names = FALSE)

#""""""""""""" figure out which simulations are really done and just need to be copied
scratch_output_dir = "S:/output"
scratch_done_dir = "S:/output_done"
status_dirs = list.dirs(scratch_output_dir)
status_dirs = status_dirs[-1] # or else it will have S:/output too

# to move files from early sims that didn't copy
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

# first do some clean up to run post processing on missing files
# this only needs to be done once, then no longer missing
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

# update to find all complete sims
scratch_output_dir = "S:/output"
nas_output_dir = "Y:/SA_first_try/output/done_status"
status_files = c(list.files(scratch_output_dir, recursive = TRUE, full.names = TRUE),
                 list.files(nas_output_dir, recursive = TRUE, full.names = TRUE))
#some of these might be duplicated between S and Y


complete = 10001:10273 # these were the first set run
incomplete = NULL

for (f in status_files)
{
  #match number between sim and .
  sim_num = gsub(".*sim([0-9]+)\\..*$", "\\1", f)
  status_text = readLines(f)
  
  if ( any(status_text == "R script finished") )
  {
    complete = c(complete, sim_num)
  } else
  {
    incomplete = c(incomplete, sim_num)
  }
}

incomplete_final = setdiff(incomplete, complete) #I remove those that were re-run successfully
missing_status = setdiff(10001:14450, c(complete, incomplete_final))
