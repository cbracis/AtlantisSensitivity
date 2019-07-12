# create the parameter files for the sensitivity analysis using
# 1. the table of morris simulations
# 2. the table of parameter values
# 3. the base biol prm file
# 3. the species file?
source("utilityFuncs.R")
source("writeParams.R")

design_matrix = read.csv(DESIGN_MATRIX_FILE, stringsAsFactors = FALSE)
param_bounds = read.csv(PARAM_BOUNDS_FILE, stringsAsFactors = FALSE)


create_sim_param_files(design_matrix, param_bounds, BASE_BIOL_PRM_FILE, OUTPUT_DIR)


#create_sim_param_files(design_matrix, param_bounds, BASE_BIOL_PRM_FILE, OUTPUT_DIR, start = 7473)

##################################################################################################
### next version of SA without primary production / top predators
##  rempve CET, SB, SXX, PP, ZOO, ZOC, ZOG
##################################################################################################

design_matrix2 = read.csv(DESIGN_MATRIX_FILE_TWO, stringsAsFactors = FALSE)
# same param_bounds as above

create_sim_param_files(design_matrix2, param_bounds, BASE_BIOL_PRM_FILE, OUTPUT_DIR)


