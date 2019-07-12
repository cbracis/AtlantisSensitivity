#!/usr/bin/Rscript
# File to re-run process output when already on NAS

require(foreach)
require(doParallel)

# TODO change the following as needed depending on where calling script from
# I have Y: mapped to \\EcosystemModel\AEEC_Sensi
#--------------------------------------------------------------------------
# script location
source("Y:/SA_first_try/datahome/processOutput.R") # support functions
source("Y:/SA_first_try/datahome/getGroups.R") # support functions

# how many to run in parallel
numClusters = 2

# director of atlantis (biol and run prm files etc)
aeec_root_path = "Y:/SA_first_try/datahome/atlantis-parameter-files/AEEC_F_UNIXv6290/simu"

# directory of atlantis output (.nc file and where to write results)
aeec_outdir = "Y:/SA_first_try/output_done" 

#--------------------------------------------------------------------------

# doparallel uses multicore linux and snow on windows
clust = makeCluster(numClusters)
registerDoParallel(clust)


#load required libraries in each cluster
# make sure to install the atlantistools version from cbracis' github (available in r-packages on the NAS)
clusterCall(clust, function() { require(atlantistools); require(dplyr); require(tidyr) } )

sims = 10001:14550

foreach (s = sims, .errorhandling = 'pass') %dopar% 
{
    process_atlantis_results(aeec_root_path, aeec_outdir, s)
}

stopCluster(clust)


