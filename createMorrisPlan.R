# code to run and create Morris file

library(sensitivity)

# small test
# TODO build parameter map (or take from param bounds)
paramCodes = c("C_COD_age", "C_ZOO_single", "C_SB_age", "mL_COD_juv", "mL_COD_adult", "mL_SB_juv", "mL_SB_adult", "mL_ZOO_single")
design_small = morris(model = NULL, factors = paramCodes, r = 10, 
                      design = list(type = "oat", levels = 4, grid.jump = 2),
                      binf = rep(1, length(paramCodes)), bsup = rep(4, length(paramCodes)))
design_small$X

design_matrix = cbind(simID = 1:nrow(design_small$X), design_small$X)
write.csv(design_matrix, file = "files/design_small_matrix.csv", row.names = FALSE, quote = FALSE)

# test of each parameter type and each group type (age) ZOO, CEP, COD, SB
paramCodes = c("C_ZOO_single", "C_CEP_adult", "C_COD_age", "C_SB_age",
               "mL_ZOO_single", "mL_CEP_juv", "mL_CEP_adult", "mL_COD_juv", "mL_COD_adult", "mL_SB_juv", "mL_SB_adult",
               "mQ_ZOO_single", "mQ_CEP_juv", "mQ_CEP_adult", "mQ_COD_juv", "mQ_COD_adult", "mQ_SB_juv", "mQ_SB_adult",
               "BHalpha_COD_single", "KDENR_SB_single")

design_test = morris(model = NULL, factors = paramCodes, r = 4, 
                      design = list(type = "oat", levels = 4, grid.jump = 2),
                      binf = rep(1, length(paramCodes)), bsup = rep(4, length(paramCodes)))
design = cbind(simID = 1:nrow(design_test$X), design_test$X)
write.csv(design, file = "files/design_matrix_test.csv", row.names = FALSE, quote = FALSE)

# TODO do full morris plan for all parameters
# need complete param list
# need to use r1 and r2 to get better coverage
# need to verify not duplicating designs if levels 1 and 2 are the same for mL and mQ
#   could check by replacing 2 with 1 for those params and checking for duplicate rows
# make sure only put CEP in for adult for C, then we move juv C the same way (similar to age groups)


##### for complete parameter list, start with all column names from param_bounds
# * delete mum_XXX for everything except PP (since it has no C, and for any other primary producer),
#   the rest will have mum added to match the levels for C
# * delete C_CEP_juv (and for any other state-structured invert), it will be added later to match levels for C_CEP_adult
cat(paste(names(param_bounds), collapse = "\", \""))
param_names = c( "C_SB_age", "mL_SB_juv", "mL_SB_adult", "KDENR_SB_single", 
                 "C_CET_age", "mL_CET_juv", "mL_CET_adult", "KDENR_CET_single", 
                 "C_SXX_age", "mL_SXX_juv", "mL_SXX_adult", "KDENR_SXX_single", 
                 "C_COD_age", "BHalpha_COD_single", 
                 "C_RAY_age", "mL_RAY_juv", "mL_RAY_adult", "BHalpha_RAY_single", 
                 "C_SHK_age", "mL_SHK_juv", "mL_SHK_adult", "BHalpha_SHK_single", 
                 "C_CEP_adult", 
                 "C_WHG_age", "BHalpha_WHG_single", 
                 "C_POL_age", "mL_POL_juv", "mL_POL_adult", "BHalpha_POL_single", 
                 "C_LBT_age", "mL_LBT_juv", "mL_LBT_adult", "BHalpha_LBT_single", 
                 "C_BSS_age", "BHalpha_BSS_single", 
                 "C_SOL_age", "BHalpha_SOL_single", 
                 "C_PLE_age", "BHalpha_PLE_single", 
                 "C_DAB_age", "BHalpha_DAB_single", 
                 "C_OFF_age", "BHalpha_OFF_single", 
                 "C_MAC_age", "BHalpha_MAC_single", 
                 "C_CLU_age", "mL_CLU_juv", "mL_CLU_adult", "mQ_CLU_juv", "mQ_CLU_adult", "BHalpha_CLU_single", 
                 "C_SPA_age", "BHalpha_SPA_single", 
                 "C_GUX_age", "BHalpha_GUX_single", 
                 "C_MUL_age", "BHalpha_MUL_single", 
                 "C_GAD_age", "BHalpha_GAD_single", 
                 "C_SMD_age", "BHalpha_SMD_single", 
                 "C_LBE_single", "mQ_LBE_single", 
                 "C_CRA_single", "mQ_CRA_single", 
                 "C_SHP_single", "mQ_SHP_single", 
                 "C_WHE_single", "mQ_WHE_single", 
                 "C_SUS_single", "mQ_SUS_single", 
                 "C_DEP_single", "mQ_DEP_single", 
                 "C_SCE_single", "mQ_SCE_single", 
                 "C_BIV_single", "mQ_BIV_single", 
                 "C_ECH_single", "mQ_ECH_single", 
                 "C_ZOO_single", "mL_ZOO_single", "mQ_ZOO_single", 
                 "C_ZOC_single", "mL_ZOC_single", "mQ_ZOC_single", 
                 "C_ZOG_single", "mL_ZOG_single", "mQ_ZOG_single", 
                 "mum_PP_single", "mL_PP_single" )
length(param_names)

morris_full = morris(model = NULL, factors = param_names, r = 50, 
                     design = list(type = "oat", levels = 4, grid.jump = 2),
                     binf = rep(1, length(param_names)), bsup = rep(4, length(param_names)))
design_full = cbind(simID = ( 1:nrow(morris_full$X) ) + 10000, morris_full$X)
write.csv(design_full, file = "files/design_matrix_full.csv", row.names = FALSE, quote = FALSE)
dim(design_full)

levels = 8
morris_opt = morris(model = NULL, factors = param_names, r = c(50, 200), 
                    design = list(type = "oat", levels = levels, grid.jump = levels / 2),
                    binf = rep(1, length(param_names)), bsup = rep(levels, length(param_names)))
design_opt = cbind(simID = ( 1:nrow(morris_opt$X) ) + 10000, morris_opt$X)
write.csv(design_opt, file = "files/design_matrix_opt.csv", row.names = FALSE, quote = FALSE)
dim(design_opt)


