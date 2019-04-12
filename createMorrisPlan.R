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
param_names = c( "C_SB_age", "mL_SB_juv", "mL_SB_adult", "mQ_SB_juv", "mQ_SB_adult", "KDENR_SB_single", 
                 "C_CET_age", "mL_CET_juv", "mL_CET_adult", "mQ_CET_juv", "mQ_CET_adult", "KDENR_CET_single", 
                 "C_SXX_age", "mL_SXX_juv", "mL_SXX_adult", "mQ_SXX_juv", "mQ_SXX_adult", "KDENR_SXX_single", 
                 "C_COD_age", "mL_COD_juv", "mL_COD_adult", "mQ_COD_juv", "mQ_COD_adult", "BHalpha_COD_single", 
                 "C_RAY_age", "mL_RAY_juv", "mL_RAY_adult", "mQ_RAY_juv", "mQ_RAY_adult", "BHalpha_RAY_single", 
                 "C_SHK_age", "mL_SHK_juv", "mL_SHK_adult", "mQ_SHK_juv", "mQ_SHK_adult", "BHalpha_SHK_single", 
                 "C_CEP_adult", "mL_CEP_juv", "mL_CEP_adult", "mQ_CEP_juv", "mQ_CEP_adult", 
                 "C_WHG_age", "mL_WHG_juv", "mL_WHG_adult", "mQ_WHG_juv", "mQ_WHG_adult", "BHalpha_WHG_single", 
                 "C_POL_age", "mL_POL_juv", "mL_POL_adult", "mQ_POL_juv", "mQ_POL_adult", "BHalpha_POL_single", 
                 "C_LBT_age", "mL_LBT_juv", "mL_LBT_adult", "mQ_LBT_juv", "mQ_LBT_adult", "BHalpha_LBT_single", 
                 "C_BSS_age", "mL_BSS_juv", "mL_BSS_adult", "mQ_BSS_juv", "mQ_BSS_adult", "BHalpha_BSS_single", 
                 "C_SOL_age", "mL_SOL_juv", "mL_SOL_adult", "mQ_SOL_juv", "mQ_SOL_adult", "BHalpha_SOL_single", 
                 "C_PLE_age", "mL_PLE_juv", "mL_PLE_adult", "mQ_PLE_juv", "mQ_PLE_adult", "BHalpha_PLE_single", 
                 "C_DAB_age", "mL_DAB_juv", "mL_DAB_adult", "mQ_DAB_juv", "mQ_DAB_adult", "BHalpha_DAB_single", 
                 "C_OFF_age", "mL_OFF_juv", "mL_OFF_adult", "mQ_OFF_juv", "mQ_OFF_adult", "BHalpha_OFF_single", 
                 "C_MAC_age", "mL_MAC_juv", "mL_MAC_adult", "mQ_MAC_juv", "mQ_MAC_adult", "BHalpha_MAC_single", 
                 "C_CLU_age", "mL_CLU_juv", "mL_CLU_adult", "mQ_CLU_juv", "mQ_CLU_adult", "BHalpha_CLU_single", 
                 "C_SPA_age", "mL_SPA_juv", "mL_SPA_adult", "mQ_SPA_juv", "mQ_SPA_adult", "BHalpha_SPA_single", 
                 "C_GUX_age", "mL_GUX_juv", "mL_GUX_adult", "mQ_GUX_juv", "mQ_GUX_adult", "BHalpha_GUX_single", 
                 "C_MUL_age", "mL_MUL_juv", "mL_MUL_adult", "mQ_MUL_juv", "mQ_MUL_adult", "BHalpha_MUL_single", 
                 "C_GAD_age", "mL_GAD_juv", "mL_GAD_adult", "mQ_GAD_juv", "mQ_GAD_adult", "BHalpha_GAD_single", 
                 "C_SMD_age", "mL_SMD_juv", "mL_SMD_adult", "mQ_SMD_juv", "mQ_SMD_adult", "BHalpha_SMD_single", 
                 "C_LBE_single", "mL_LBE_single", "mQ_LBE_single", 
                 "C_CRA_single", "mL_CRA_single", "mQ_CRA_single", 
                 "C_SHP_single", "mL_SHP_single", "mQ_SHP_single", 
                 "C_WHE_single", "mL_WHE_single", "mQ_WHE_single", 
                 "C_SUS_single", "mL_SUS_single", "mQ_SUS_single", 
                 "C_DEP_single", "mL_DEP_single", "mQ_DEP_single", 
                 "C_SCE_single", "mL_SCE_single", "mQ_SCE_single", 
                 "C_BIV_single", "mL_BIV_single", "mQ_BIV_single", 
                 "C_ECH_single", "mL_ECH_single", "mQ_ECH_single", 
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



