#!/usr/bin/Rscript
# File to call from datarmor scripts after simulation runs to process output and copy it

suppressMessages(library(docopt))
suppressMessages(require(atlantistools)) # note requires updated version of package from github (not CRAN version)
suppressMessages(require(dplyr))

source("processOutput.R") # support functions

#--- command line arguments -------------------------------
doc = "Usage: runProcessOutput.R [-h] [--atl ATLANTISDIR] [--out OUTDIR] (SIMID)

Options:
-a --atl ATLANTISDIR  Atlantis root directory [default: ~/simu]
-o --out OUTDIR       output directory root [default: /home1/scratch/rgirardi/output]
-h --help             show this help text"

opt = docopt(doc)

# files/dirs, the default values are for datarmor, change these for testing

aeec_root_path = opt$atl        # Atlantis root directory
sim_results_root_path = opt$out # root output directory, parent of simulation output directory
sim_id = opt$SIMID              # simulation id

#--- files and directories --------------------------------

# Atlantis run files
aeec_prm_biol = file.path(aeec_root_path, paste0("AEEC_biol", sim_id, ".prm"))

aeec_bgm = file.path( aeec_root_path, "..", "poly_atlantisEC35_projETRS89_LAEA_snapped0p002.bgm")
aeec_fgs = file.path(aeec_root_path,"..", "SETasGroups.csv")  # functional groups file
aeec_init = file.path(aeec_root_path, "..", "AEECF_200y_ini.nc") # initial conditions for groups, etc.
aeec_prm_run = file.path(aeec_root_path, "AEEC_run.prm") # model run parameters 

# Atlantis output files
aeec_outdir = file.path(sim_results_root_path, paste0("sim", sim_id)) # TODO match sh script
aeec_output_nc = file.path(aeec_outdir, paste0("AEEC_Fv6376_sim", sim_id, ".nc")) # TODO match sh script

# other files/dirs for simulation results
metrics_file = file.path(aeec_outdir, paste0("sim_", sim_id, ".rdata"))
# TODO other csv file for 5 yr / 10 yr biomass

#--- process output ------------------------------------

# TODO: find sediment layer automatically?
# TODO: CEP not handled correctly
# TODO need to filter depending on how ofter we write data?
metrics = calcualte_metrics(output = aeec_output_nc, biol_prm = aeec_prm_biol,
                            fgs = aeec_fgs, init = aeec_init, run_prm = aeec_prm_run, 
                            bgm = aeec_bgm, sediment_layer = 3)
save(metrics, file = metrics_file)

# TODO: save 5 yr and 10 yr summary




