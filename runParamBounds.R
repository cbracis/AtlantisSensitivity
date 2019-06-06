# run to create parameter bound file
source("utilityFuncs.R")
source("calculateParamBounds.R")

func_groups_data = load_fgs(FUNCTIONAL_GROUPS_FILE)
param_values = read.csv(EEC_PARAM_VALUES_FILE, stringsAsFactors = FALSE)
fitted_C_values = read.csv(FITTED_C_VALUES_FILE, stringsAsFactors = FALSE)
BHalpha_mult_factors = read.csv(BHALPHA_MULT_FACTORS_FILE, stringsAsFactors = FALSE)

bounds = create_param_bounds(func_groups_data, param_values, fitted_C_values, BHalpha_mult_factors)

# write out bounds, watch out! the code to write param files will read this in
write.csv(bounds, file = PARAM_BOUNDS_FILE, quote = FALSE, row.names = FALSE)

# how many parameters in the final analysis, everything but mum, which is there for convenience later
# - 1 don't count levels column
# + 1 but need to add back in mum for primary producers that don't have C!! (only phytoplankton, seagrass is turned off)
# and - 1 because juv/adult C are moved together
length(names(bounds)) - length(grep("mum", names(bounds))) - 1 + 1 - 1


