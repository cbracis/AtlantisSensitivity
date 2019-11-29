# AtlantisSens: Sensitivity analysis of Atlantis models

This document describes how to run a sensitivity analysis (SA) on growth, mortality, and recruitment parameters using the Morris screening method. The code could be easily adapted to run a SA on different parameters as well.

The following list gives an overview of the main steps in the analysis, which are then described in greater detail below. Don't hesitate to examine the R code directly to answer any questions, but this attempts to provide a road map to tuning the analysis and to understand which code file is responsible for what. 

## Overview of steps
1. Obtain necessary data
    * Atlantis model
    * biological prm file (either calibrated or starting version)
    * groups file
    * (optional) recruitment variability data from stock assessment
1. Fit a function to existing C values (optional)
1. Calculate recruitment variability
1. Generate parameter bound (optional, they could be created by hand as well)
1. Generate Morris design matrix
1. Generate biological prm files for each simulation
1. Generate run files (optional)
1. Run simulations
1. Process Atlantis output (could be done as part of previous step)
1. Calculate Morris metrics from processed output
1. Pretty plots


## Necessary data

The Atlantis model needs to have the hydrodynamic portion of the model already completed (spatial structure, flows), the groups defined, and "reasonable" starting values for parameters (perhaps just taken from the literature or other models). Fishing can be disabled, included with constant mortality, or other setting.

The biological prm file and the groups file need to be copied to the files subdirectory of the R project.

## Specifying files

The file `utilityFuncs.R` lists constants used in the project, some of which may need to be modified for your specifics. Specifically, those constants ending with "_FILE" may need to be updated. The  constants that refer to Atlantis files and the file names likely need updating. These files should be copied to the files subdirectory of the project.

  * `FUNCTIONAL_GROUPS_FILE`
  * `BASE_BIOL_PRM_FILE`
  * `INIT_COND_FILE`

Other files are used in the project to both write files and also to read them in later steps. These file names don't necessarily need to be changed but the generated files should be examined and verified.

  * `DESIGN_MATRIX_FILE`
  * `PARAM_BOUNDS_FILE`
  * `EEC_PARAM_VALUES_FILE` ?? still used
  * `FITTED_C_VALUES_FILE`
  * `BHALPHA_MULT_FACTORS_FILE`
  * `PARAM_INFO_FILE`
  * `GROUP_INFO_FILE`

## Fitting function to C values

For already calibrated models, one can follow our approach of fitting a function to the calibrated C values using least squares. The code for doing the fitting is in `fitCFunction.R`, and an example of calling the fitting routine (as well as additional code for creating prm files with the fitted values for testing) is in the `runParamChange.R` file. Alternatively, uncalibrated models could simply generate values for C or mum based on the equations recommended in the Atlantis User Guide and use those directly.

To fit a function to calibrated C values and initial weights, several Atlantis files are needed: the biological parameters file, the groups file, the initial conditions file, and the bgm bounding boxes files.
Both functions suggested by the Atlantis user guide are included (an exponential function of the initial size per age and a function based on the difference (growth) of initial size between ages). A user could also create their own function and add a functionType to the `fit_C_from_init_biomass` function. Or alternatively, for a function not based on initial biomass, by modeling a new function on the existing code, where `Cfunc_exp` is the function to fit, `fit_Cfunc_exp` fits the function to a single group, and `fit_Cfunc_exp_to_groups` then applies the fitting across all the groups.

## Calculate recruitment variability

The file `ICES_recruitment_var.R` provides code to calculate a time series of normalized recruitment error from ICES stock assessment data, and this could be easily adapted to any time series of recruit data. We used the 5% and 95% quantiles to define the parameter bounds for BHalpha.


## Generate parmater bounds

This is a large and important part of the SA and shouldn't be underestimated. The code in `runParamBounds.R` calls the functions in `calculateParamBounds.R` to generate a csv file with the values to use for each parameter for each level. Note that there are more parameters in the parameter bounds file because some parameters (i.e. C and mum) are moved together. There are two important constants in `calculateParamBounds.R`

  * `param_mult_factors` This defines the values to use to multiply the baseline value by to get the parameter values used in the SA. This is a vector and the length of the vector determines the number of levels (8 in our case but this can be changed). In general parameter values should be evenly spaced for a Morris SA, though it is possible to use a pdf related to the parameter distribution (see TODO). The values are multiplied by the baseline value, so a value of 0.5 is a decrease of 50%, while a value of 2 is an increase of 100%.
  * `mum_func_of_C_mult_factor` This defines the relationship between C and mum for age-structured groups. That is, the values of C are determined from the fitted C values, then mum is set as a factor of C.
  * `bacteria_detritus_types` The group codes for groups representing bacteria, detritus, etc. that shouldn't be included in the SA since they don't have the growth, mortality, and reproduction parameters the SA focuses on.
  
The groups to be included are found in the groups file. Any group that is not turned on or that is included in `bacteria_detritus_types` is omitted from the analysis. The rest of the groups are included, and their group type is determined from setting in the groups file. Note that the group type here is not identical to the GroupType column in the groups file, though that is used to help determine the group type.

  * `GROUP_TYPE_POOL` These are the invertebrate groups represented by simple biomass pools, those with `NumCohorts` = 1.
  * `GROUP_TYPE_JUV_ADULT` These are stage-structured invertebrates (e.g. cephalopods) with juvenile and adult stages, determined by `NumCohorts` = 2 and the GroupType also not being FISH.
  * `GROUP_TYPE_AGE` These are the age-structured groups, generally vertebrates, which have `NumCohorts` > 2.

The output file is created with a column for each parameter and as many rows as there are levels. The parameters are named as ppp_ccc_aaa, where ppp gives the parameter name, ccc the group code, and aaa the age specifier (see `utilityFuncs.R`). Could construct this file by hand or by other means if desired. TODO

In general, parameter values for the SA are calculated based on the multiplication factor provided times the baseline value in the biological prm file. There are a few things to note for each parameter type.

### Growth parameters C and mum

This part is handled in the function `create_bounds_C_mum`. The inputs are the biological parameter file and the fitted function C values. For the invertebrate groups, baseline values are read in from the biological parameter file for both C and mum. Phytoplankton and any other primary producers only have a mum value. Stage-structured invertebrates have juvenile and adult values. For vertebrates, baseline values for C are read in from the fitted C values (this could be changed to the biological prm file if using a function to represent the age-structure of C isn't desired). These are then multiplied by `mum_func_of_C_mult_factor` to obtain mum. In all cases, the baseline C and mum values are then multiplied by `param_mult_factors` to obtain the parameter levels.

### Mortality parameters mL and mQ

This part is handled in the function `create_bounds_mL_mQ`. The input is the biological parameter file. The first thing that is important to note is that any group with a value of 0 for mL or mQ will not have that parameter included in the SA. Thus, we primarily used mQ for invertebrates and mL for top predator vertebrates, but this is controlled by the values in the biological parameter file, not in the parameter bounds. The mortality parameter values are extracted from the biological parameter file. For vertebrates and stage-structured invertebrates, there are both juvenile and adult values for the mortality parameter. In all cases, the baseline mortality parameter value is multiplied by `param_mult_factors` to obtain the parameter levels.

### Recruitment parameters BHalpha and KDENR

This part is handled in the function `create_bounds_reproduction`. The inputs are the biological parameter file and the normalized recruitment error values. Currently two flagrecruit values are supported (3 = Beverton Holt, 12 = KDENR), but more could be added. For Beverton Holt, the level1 (5% quantile) and level4 (95% quantile) in the BHalpha data file are used as the upper and lower limits for for the SA, thus the appropriate number of levels are interpolated between these limits. For KDENR, baseline values are read from the biological parameter file and multiplied by `param_mult_factors`.



## Generate Morris design matrix

The file `createMorrisPlan.R` provides sample code for creating the Morris design matrix, but this is something that needs to be adapted for each SA. Creating the design is relatively straightforward using the `morris` function in the `sensitivity` package. Unfortunately the list of parameters needs to be constructed by hand. This consists of listing all the parameters from the parameter bounds file, and deleting any that will not be moved independently, i.e. mum parameters for all non-primary production groups.

A few notes on selecting the settings for cresting the design:

  * `r` This specifies the number of trajectories. It's a good idea to specify 2 numbers to take the best spanning trajectories from the larger set.
  * `levels` The number of levels specified here should match the number of levels created when generating parameter bounds.
  * `grid.jump` This should generally be half the number of levels.
  * `binf`, `bsup` Here you can either specify the parameter bounds upper and lower limits for each parameter, or just use 1 and th enumber of levels and replace the true values later.

Make sure to save the design as a file to reference later. The entire Morris object could be saved, but it is simple to recreate and copy over the design as well (see `get_morris_output`). This was useful for to also be able to calculate Morris metrics for a subset of the design as well.


## Generate biological prm files for each simulation

The code in `runCreateParamFiles.R` calls the functions in `writeParams.R` to generate a biological prm file for each simulation in the design matrix. The input data are the design matrix, the parmater bound information, and a base version of the biological prm file to change. The code contains all the logic to translate the 1, 2, ..., n-levels in the design matrix to the levels specified in the parameter bounds, as well as logic to write parameters appropriately, such as combining juvenile and adult mortality parameters, etc. 

## Generate run files

This part of the process depends on the infrastructure you use to run all the simulations. In our case, we created a unique run file for each simulation which specified information like the specific biological prm file for that simulations, as well as the directory on the supercomputer to store output. THe file `runCreateShScripts.R` gives a simple appraoch to writing this files, though this will need to be modified to the unique needs of the particular model, and clearly other approaches are possible as well.

## Run simulations

We used the Ifremer Datarmor supercomputer, and thus created a pub script and Docker image with Atlantis for that context. In any case, run those simulations! Be sure to think if you want to process the output at the same time or as a subsequent step. Procesing the output takes just a few minutes per simulation, but this still quickly adds up across 1000's of simulations.

## Process Atlantis output

In order to take the output from the nc files and summerize it into a set of csv files ready for being combined across simulations, we created a script making use of the atlantistools package. We ran this script immediately after the Atlantis on the supercomputer. The script reads the output nc file to extract biomass and numbers (age-structured groups only) aggregated spatially and both aggregated by age and age-specific. These were averaged for the last 5 and 10 years of the simulation.

Note that this requires a modified version of the atlatistools package with several bug fixes available here: https://github.com/cbracis/atlantistools

