# get matrix from character vector where each entry is string of space separated values
getMatrixFromAgeParamVec = function(params)
{
  # as.vector incase pased single column df which gives errro
  return(as.matrix(read.table(text = as.character(params))))
}

# types of groups
# pool - those without age structured parameters
GROUPS_POOL = "pool"
# cohort - those wih juvenile and adult, like CEP, note these may need to be handled as 2 pool groups, one preceded by j??
GROUPS_COHORT = "cohort"
# age - those with age classes, generally 10
GROUPS_AGE = "age"

# suffixes for the parameter bounds file, not an exact match for group types
# used for age_structured groups (i.e. 10 age classes, juv/adult) for C, mum, mL, mQ (also CEP for mortality)
PARAM_SUFFIX_AGE = "age"
# used for pool groups that just have a single parameter or age-structured groups too for those params with just a single value
PARAM_SUFFIX_SINGLE = "single"
# used for *juvenile* mortality (parameters independent but need to be combined into age parameter to set values in param file)
PARAM_SUFFIX_JUVENILE = "juv"
# used for *adult* mortality (parameters independent but need to be combined into age parameter to set values in param file)
PARAM_SUFFIX_ADULT = "adult"

# specific groups
# TODO - consider looking these up in species group file (based on GroupType = CEP??)
# this is for groups that have an adult and juvenile C and mum (rather than the typical age-structured multiple values)
JUV_C_GROUPS = c("CEP")

# group types based on setting in species group file for how to set params
# age-structured groups (i.e. 10 age classes)
GROUP_TYPE_AGE = "age"
# just juvenile-adult (i.e. age structured inverts, cephalapods)
GROUP_TYPE_JUV_ADULT = "juv_adult"
# pool groups (invertebrates, no age stucture)
GROUP_TYPE_POOL = "pool"

# important files
# design matrix of senstivity analysis, simulation id and levels each param takes
DESIGN_MATRIX_FILE = "files/design_matrix.csv"
# parameter bounds, values of each parameter for each level
PARAM_BOUNDS_FILE = "files/param_bounds.csv"
# Atlantis species groups file
FUNCTIONAL_GROUPS_FILE = "files/SETasGroups.csv"
# base Atlantis biology parameterfile to modify
BASE_BIOL_PRM_FILE = "files/AEEC_biol.prm"
# initial conditions file, to use to calculate C from inital total N
INIT_COND_FILE = "files/AEEC35_final_ini.nc"
# output from PRMComparisons with all the parameters from Atlantis extracted, plus quantiles across models for mortality
EEC_PARAM_VALUES_FILE = "files/aeecParameterData.csv"
# fitted C values from function C = a totN^0.7 where totN = structN + resN from initial conditions
FITTED_C_VALUES_FILE = "files/fitted_C_values.csv"
# factor values for BHalpha calculated from stock assesments
BHALPHA_MULT_FACTORS_FILE = "files/BHalpha_mult_factors.csv"

# directories
OUTPUT_DIR = "output"

# names of columns
# the low and high values (level 3-4) to use from the parameter comparison for mortality when param is 0
COL_MORTALITY_QUANTILE_LOW = "quantile25"
COL_MORTALITY_QUANTILE_HIGH = "quantile75"
COL_BHALPHA_LEVEL_1 = "level1"
COL_BHALPHA_LEVEL_2 = "level2"
COL_BHALPHA_LEVEL_3 = "level3"
COL_BHALPHA_LEVEL_4 = "level4"


