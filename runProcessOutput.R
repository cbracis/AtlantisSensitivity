# File to call from datarmor scripts after simulation runs to process output and copy it

source("processOutput.R") # support functions

#--- files and directories --------------------------------

# files/dirs that definitely need to be changed on datarmor!!!
# these are just test dirs on my machine for now

aeec_root_path = "C:/Atlantis/AEEC_trunk1_testP" 
sim_results_root_path = "C:/test_scratch_drive"

# other files/dirs for Atlantis (verify with Atlantis installation on datarmor)

aeec_outdir = file.path(aeec_root_path, "output")
aeec_output = file.path(aeec_outdir, "AEEC_F8test.nc")
aeec_prm_biol = file.path(aeec_root_path, "AEEC_biol.prm")
aeec_bgm = file.path(aeec_root_dir, "poly_atlantisEC35_projETRS89_LAEA_snapped0p002.bgm")
aeec_fgs = file.path(aeec_root_dir, "SETasGroups.csv")  # functional groups file
aeec_prm_run = file.path(aeec_root_dir, "AEEC_run_121217.prm") # model run parameters
aeec_init = file.path(aeec_root_dir, "AEECF_200y_ini.nc") # initial conditions for groups, etc.

# simulation ID
# for now I have the idea that we also copy a text file with the simulation id along with the biol prm file
# so for now I assume this file exists in the current 
# but we could change this to another method easily
sim_id_file = file.path(aeec_root_path, "simid.txt")
sim_id = get_sim_id(sim_id_file)

# other files/dirs for simulation results
metrics_file = file.path(aeec_outdir, paste0("sim_", sim_id, ".rdata"))
sim_results_path = file.path(sim_results_root_path, paste0("sim_", sim_id))
if (file.exists(subDir)){
  error(paste("Directory already exists for simulation", sim_id))
} else {
  dir.create(sim_results_path)
}


#--- process output ------------------------------------

# TODO: find sediment layer automatically?
# TODO: CEP not handled correctly
# TODO need to filter depending on how ofter we write data?
metrics = calcualte_metrics(output = aeec_output, biolPrm = aeec_prm_biol,
                            fgs = aeec_fgs, init = aeec_init, runPrm = aeec_prm_run, 
                            bgm = aeec_bgm, sedimentLayer = 3)
save(metrics, metrics_file)

# TODO: save 5 yr and 10 yr summary

# copy biol prm to output dir so it will get saved with output
file.copy(aeec_prm_biol, aeec_outdir)

#--- delete unneeded output files ----------------------

# CAREFUL!

# Rapheal, check which files we don't need
files_to_delete = c()

for (file in files_to_delete)
{
  file_path = file.path(aeec_outdir, file)
  if (file.exists(file_path))
  {
    file.remove(file_path)
  }
}

#--- copy output to scratch drive --------------------

files_to_copy = dir(path = aeec_outdir, all.files = TRUE)
file.copy(from = file.path(aeec_outdir, files_to_copy), to = sim_results_path)


