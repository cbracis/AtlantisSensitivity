# some checks to verify the calculated values
source("processOutput.R") # support functions

# first run the code to analyse sim output

aeec_root_path = "C:/Atlantis/AEEC_SA_test/simu"
sim_results_root_path = "C:/Atlantis/AEEC_SA_test_output"
sim_id = 10003

# Atlantis run files
aeec_prm_biol = file.path(aeec_root_path, paste0("AEEC_biol", sim_id, ".prm"))

aeec_bgm = file.path( aeec_root_path, "..", "poly_atlantisEC35_projETRS89_LAEA_snapped0p002.bgm")
aeec_fgs = file.path(aeec_root_path, "..", "SETasGroups.csv")  # functional groups file
aeec_init = file.path(aeec_root_path, "..", "AEECF_200y_ini.nc") # initial conditions for groups, etc.
aeec_prm_run = file.path(aeec_root_path, "..", "AEEC_run.prm") # model run parameters 

# Atlantis output files
aeec_outdir = file.path(sim_results_root_path, "output-done") #file.path(sim_results_root_path, paste0("sim", sim_id))
aeec_output_nc = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, ".nc")) 
aeec_migr = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, "Migration.txt"))
aeec_biomass = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, "BiomIndx.txt")) 


metrics = calcualte_metrics(output = aeec_output_nc, biol_prm = aeec_prm_biol,
                            fgs = aeec_fgs, init = aeec_init, run_prm = aeec_prm_run, 
                            bgm = aeec_bgm, sediment_layer = 3)

# this only has time = 0 ??
biomass = read.table(aeec_biomass, header = TRUE)
migr = read.table(aeec_migr, header = TRUE)

# biomass$Time is in days, with a value at 0, then the end of yeach year, so 97:101 is last 5 years
# 2:14 are biomass columns for groups, then comes relative biomass and some others
avg_biom = colMeans(biomass[97:101, 2:41])
avg_biom = data.frame(Code = names(avg_biom), avg = avg_biom)
avg_metrics = calculate_avg_timespan_all(metrics, 76, 80, fgs = aeec_fgs) # see note below explaining years
avg_compare = full_join(avg_biom, 
                        avg_metrics$nonage, 
                        by = "Code")
plot((avg_compare$avg - avg_compare$biomass)/avg_compare$avg)

# next steps
# update calculation to handle migration
# do a test run of atlantis with the weird directory structure so I can test the script on it
# finish 5 and 10 year avgs and write them out
# write lots of prm files
# final test of script, look at atlantis tools needed, packages

five_year_avg = calculate_5_years(metrics, fgs = aeec_fgs)
ten_year_avg = calculate_10_years(metrics, fgs = aeec_fgs)


process_atlantis_results(aeec_root_path, aeec_outdir, sim_id)

# for files on NAS 
aeec_root_path = "Y:/SA_first_try/datahome/atlantis-parameter-files/AEEC_F_UNIXv6290/simu"
aeec_outdir = "Y:/SA_first_try/output_done" 
sim_id = 12113

#  13126 13179 13197 13657 13679 13692 13718 13727 13764 13802 13850 13870 13993 14233
for( sim_id in consolid_results$sim[bad_rows] )
{
  print(sim_id)
  process_atlantis_results(aeec_root_path, aeec_outdir, sim_id)
}

sim_id = 13126

for( sim_id in consolid_results$sim[bad_rows] )
{
  load(file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, ".rdata")))
  print(sim_id)
  print(tail( table(metrics$biomass$time), n = 20))
}

get_atlantis_output_length(file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, ".nc")))

# some important points
# nc files have explicit time (ie seconds since date) but when atlantistools reads them in, it 
# reconverts time to 0:n_timesteps, then calibrates to years with toutinc but doesn't add on toutstart
# so that it will always start with 0, 100 years starting at year 20 will be returned as 0 to 80