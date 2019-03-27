# code to run and create Morris file

library(sensitivity)

# small test
# TODO build parameter map (or take from param bounds)
paramCodes = c("C_COD_age", "C_ZOO_pool", "C_SB_age", "mL_COD_juv", "mL_COD_adult", "mL_SB_juv", "mL_SB_adult", "mL_ZOO_pool")
design_small = morris(model = NULL, factors = paramCodes, r = 10, 
                      design = list(type = "oat", levels = 4, grid.jump = 2),
                      binf = rep(1, length(paramCodes)), bsup = rep(4, length(paramCodes)))
design_small$X

design_matrix = cbind(simID = 1:nrow(design_small$X), design_small$X)
write.csv(design_matrix, file = "files/design_small_matrix.csv", row.names = FALSE, quote = FALSE)

# test of each parameter type and each group type (age) ZOO, CEP, COD, SB
paramCodes = c("C_ZOO_pool", "C_CEP_pool", "C_COD_age", "C_SB_age",
               "mL_ZOO_pool", "mL_CEP_juv", "mL_CEP_adult", "mL_COD_juv", "mL_COD_adult", "mL_SB_juv", "mL_SB_adult",
               "mQ_ZOO_pool", "mQ_CEP_juv", "mQ_CEP_adult", "mQ_COD_juv", "mQ_COD_adult", "mQ_SB_juv", "mQ_SB_adult",
               "BHalpha_COD_pool", "KDENR_SB_age")

design_test = morris(model = NULL, factors = paramCodes, r = 4, 
                      design = list(type = "oat", levels = 4, grid.jump = 2),
                      binf = rep(1, length(paramCodes)), bsup = rep(4, length(paramCodes)))
design = cbind(simID = 1:nrow(design_test$X), design_test$X)
write.csv(design, file = "files/design_matrix.csv", row.names = FALSE, quote = FALSE)

# TODO do full morris plan for all parameters
# need complete param list
# need to use r1 and r2 to get better coverage
# need to verify not duplicating designs if levels 1 and 2 are the same for mL and mQ
#   could check by replacing 2 with 1 for those params and checking for duplicate rows