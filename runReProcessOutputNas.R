# reprocess files on NAS
require(foreach)

# script location
source("Y:/SA_first_try/datahome/processOutput.R") # support functions
source("Y:/SA_first_try/datahome/getGroups.R") # support functions

# director of atlantis (biol and run prm files etc)
aeec_root_path = "Y:/SA_first_try/datahome/atlantis-parameter-files/AEEC_F_UNIXv6290/simu"
aeec_fgs = file.path(aeec_root_path, "SETasGroups.csv")  # functional groups file


# directory of atlantis output (.nc file and where to write results)
aeec_outdir = "Y:/SA_first_try/output_done" 

sims = 10001:14550

# second run
aeec_outdir = "Y:/SA_two_noPPP/output_done" 
sims = 10001:13400

foreach(s = sims, .errorhandling = 'pass') %do% 
{
  last_ten_years_age_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", s, "_last_ten_years_avg_age.csv"))
  last_ten_years_nonage_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", s, "_last_ten_years_avg_nonage.csv"))
  aeec_rdata = file.path(aeec_outdir, paste0("AEEC_SA_sim", s, ".rdata")) 
  load(aeec_rdata)
  
  last_ten_years = calculate_each_last_10_years(metrics, fgs = aeec_fgs)
  write.csv(last_ten_years$nonage, file = last_ten_years_nonage_file, quote = FALSE, row.names = FALSE)
  write.csv(last_ten_years$age, file = last_ten_years_age_file, quote = FALSE, row.names = FALSE)
  
  remove(metrics)
}
