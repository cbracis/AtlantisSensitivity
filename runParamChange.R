source("fitCFunction.R")
source("changeMum.R")
source("writeParams.R")
source("plotParamValues.R")

# this is for testing a few versions of new parameter values, not for SA

aeec_dir = "C:/Atlantis/AEEC_trunk1_testC" 
base_biol_prm = file.path(aeec_dir, "AEEC_biol.prm")
base_fgs = file.path(aeec_dir, "SETasGroups.csv")
base_init = file.path(aeec_dir, "AEEC35_final_ini.nc")
base_bgm = file.path(aeec_dir, "poly_atlantisEC35_projETRS89_LAEA_snapped0p002.bgm")

# exp function of init N
C_values_e = fit_C_from_init_biomass(base_init, base_biol_prm, base_fgs, base_bgm,
                                     functionType = "exp", useWeights = FALSE)
C_values_e$groupType = "age" # so far have just fit C for group type

change_C_biol_file = create_file(aeec_dir, base_biol_prm, "changeC")
write_params(change_C_biol_file, C_values_e, "C")

# growth diff function of init N
#C_values_g = fit_C_from_init_biomass(base_init, base_biol_prm, base_fgs, base_bgm,
 #                                    functionType = "growth")

write.csv(C_values_e, file = "files/fitted_C_values.csv", quote = FALSE, row.names = FALSE)

# stuff for mum
#-------------------------------------------------------------------------------------------
# note: here is where we add the column for the group type, should this all be done in a function??
# the group type is required because the write functions depend on it
# this all seems a little hacky as is

C_pool = get_C_for_groups(base_biol_prm, base_fgs, get_pool_predators_group_codes(base_fgs), FALSE)
C_pool = C_pool %>% select(Code, C = Value) %>% mutate(C = as.character(C))
C_pool$groupType = "pool"
C_values_e = bind_rows(C_values_e, C_pool)
C10mum_values = getMumAsFunctionofC(C_values_e, 10)

change_C10mum_biol_file = create_file(aeec_dir, base_biol_prm, "changeC10mum")
write_params(change_C10mum_biol_file, C10mum_values, "C")
write_params(change_C10mum_biol_file, C10mum_values, "mum")

C10mum_values = C10mum_values %>% left_join(get_mum_for_groups(base_biol_prm, base_fgs, get_pool_predators_group_codes(base_fgs), FALSE) %>%
                                               dplyr::rename(mumCalib = Value) %>% select(-Param),
                                            by = "Code")

# NOTE: previously done with 10 when in fact the EEC model uses 3 as the multiplication factor!! (though 10 for inverts except zooplankton)
# only use mum = 3 * C for age groups (trouble with inverts, see below for zooplankton)
C3mum_age_values = getMumAsFunctionofC(C_values_e %>% filter(groupType == GROUPS_AGE), 3)
change_C3mum_age__biol_file = create_file(aeec_dir, base_biol_prm, "changeC3mumAge")
write_params(change_C3mum_age__biol_file, C3mum_age_values, "C")
write_params(change_C3mum_age__biol_file, C3mum_age_values, "mum")

# write files for testing +/-80% C, with and without mum = 3C
# base file if fitted C with mum unchanged: C_values_e
for (sp in c("COD", "SOL", "SMD", "WHG"))
{
  for (change in c(1.8, 0.2))
  {
    updatedParams = multAgeParameter(C_values_e, "C", sp, change)
    updatedC_biol_file = create_file(aeec_dir, base_biol_prm, paste(sp, "C", change, sep = "_"))
    write_params(updatedC_biol_file, updatedParams, "C")
    
    updatedParams = getMumAsFunctionofC(updatedParams %>% filter(groupType == GROUPS_AGE), 3)
    updatedCmum_biol_file = create_file(aeec_dir, base_biol_prm, paste(sp, "C", change, "mum", sep = "_"))
    write_params(updatedCmum_biol_file, updatedParams, "C")
    write_params(updatedCmum_biol_file, updatedParams, "mum")
  }
}


# make some plots for potential C and mum values
C_values_plot = fit_C_from_init_biomass(base_init, base_biol_prm, base_fgs, base_bgm,
                                     functionType = "exp", returnAllInfo = TRUE)

pdf("plots/C_fit_init_std_factors.pdf", width = 12, height = 8)
#png("plots/C_fit_init_std_factors.png", width = 12, height = 8, units = "in", res = 300)
plot_C_for_age_groups_bounds(C_values_plot, c(-0.5, 1))
dev.off()

C_values_plot = left_join(C_values_plot,
                          get_mum_for_groups(base_biol_prm, base_fgs, get_age_group_codes(base_fgs), TRUE) %>% mutate(mum = Value),
                          by = c("Code", "Age"))

# plot C vs mum for inverts

with(filter(C10mum_values, groupType == "pool"), {
  plot(C10mum_values$C, C10mum_values$mumCalib, pch = 1:13)
  abline(a = 0, b = 10)
})

with(filter(C10mum_values, groupType == "pool"), {
     plot(C10mum_values$mum, C10mum_values$mumCalib, pch = 1:13)
     points(C10mum_values$mum, C10mum_values$mum, pch = 1:13, col = 2)
     })

# mum = 10 * C works for most but not Zooplankton
filter(C10mum_values, groupType == "pool")
