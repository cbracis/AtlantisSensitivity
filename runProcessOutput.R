#!/usr/bin/Rscript
# File to call from datarmor scripts after simulation runs to process output and copy it

# subdirectory of datahome where R packages are installed
.libPaths( c(.libPaths(), "~/r-package/library") )

library(docopt)
require(atlantistools) # note requires updated version of package from cbracis github (not CRAN version)
require(dplyr)

source("~/processOutput.R") # support functions

#--- command line arguments -------------------------------
doc = "Usage: runProcessOutput.R [-h] [--atl ATLANTISDIR] [--out OUTDIR] (SIMID)

Options:
-a --atl ATLANTISDIR  Atlantis root directory [default: ~/atlantis-parameter-files/AEEC_F_UNIXv6290/simu]
-o --out OUTDIR       output directory root [default: /home1/scratch/cbracis/output]
-h --help             show this help text"

opt = docopt(doc)

# files/dirs, the default values are for datarmor, change these for testing

aeec_root_path = opt$atl        # Atlantis root directory
sim_results_root_path = opt$out # root output directory, parent of simulation output directory
sim_id = opt$SIMID              # simulation id

aeec_outdir = file.path(sim_results_root_path, paste0("sim", sim_id)) # TODO match sh script
#aeec_outdir = sim_results_root_path # for output already on the NAS

process_atlantis_results(aeec_root_path, aeec_outdir, sim_id)

# try to release some memory
gc()

