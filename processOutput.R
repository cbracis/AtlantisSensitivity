# supporting libraries
require(atlantistools) # note requires updated version of package from github (not CRAN version)
require(dplyr)

source("getGroups.R")

# gets the simulation id by looking in specified file (assumed to be copied as part of simulation initilization)
# file should just contain simulation id number and nothing else
get_sim_id = function(sim_id_file)
{
  id = as.numeric(readLines(sim_id_file))
}

# calculates a number of meterics (biomass, numbers, and biomass, numbers and length at age) for a simulation run
# issue is that structN and resN have default values in boundary boxes and the sediment layer (not an issue for Nums)
# includinging it affects results (esp when populations go extinct), so specify sedment layer to remove if necessary
# TODO: way to figure out sediment layer automatically? also, may be 2 sediment layers
calcualte_metrics = function(output, biol_prm, bgm, fgs, init, run_prm, sediment_layer = NA)
{
  groups_age = get_age_group_names(fgs) # the vertegrate groups with age structure
  groups_age_code = get_age_group_codes(fgs) # the 3 letter codes used in the prm file
  # TODO exclude cohort pool groups until we handle them properly
  groups_pool = get_pool_group_names(fgs, exclude_cohorts = TRUE) # the limited number of biomass pool groups to use 
  
  bboxes = get_boundary(boxinfo = load_box(bgm)) # boundary boxes to delete
  bps = load_bps(fgs, init) # list of epibenthic functional groups (only in sediment)
  bio_conv = get_conv_mgnbiot(biol_prm)
  
  lenWt_a = extract_prm(biol_prm, variables = paste0("li_a_", groups_age_code))
  lenWt_b = extract_prm(biol_prm, variables = paste0("li_b_", groups_age_code))
  ab.df = data.frame(species = get_group_long_names(groups_age, fgs), 
                     a = lenWt_a, b = lenWt_b, stringsAsFactors = FALSE)
  
  vars = list("Nums", "StructN", "ResN", "N")
  grps = list(groups_age, groups_age, groups_age, groups_pool)
  
  num_and_Ns = Map(load_nc, select_variable = vars, select_groups = grps,
                   MoreArgs = list(nc = output, bps = bps, fgs = fgs, prm_run = run_prm, bboxes = bboxes))
  init_vals = Map(load_init_age, select_variable = vars[-4], select_groups = grps[-4],
                  MoreArgs = list(init = init, fgs = fgs, bboxes = bboxes))
  # TODO need to handle Ceppalopods (and other multi-stage invertebrates, prawns) seperately since it's N1 and N2
  # somehow the other methods seem to aggregate this (load_nc) but not obvious how, check
  # then could call load_init, but will have to manually handle boundry boxes etc
  #init_vals[[4]] = load_init_nonage(select_variable = vars[4], select_groups = grps[4],
  #              init = init, fgs = fgs, bboxes = bboxes, bps = bps)
  
  # remove sediment layer
  # TODO read this in from a file (check again which) will say the max number of water layers per box
  num_and_Ns[[2]] = filter(num_and_Ns[[2]], layer != sediment_layer)
  num_and_Ns[[3]] = filter(num_and_Ns[[3]], layer != sediment_layer)
  
  vol_dz = load_nc_physics(nc = output, select_physics = c("volume", "dz"),
                           prm_run = run_prm, bboxes = bboxes)
  
  
  # now for biomass, etc
  # Calculate biomass spatially
  bio_sp = calculate_biomass_spatial(nums = num_and_Ns[[1]], sn = num_and_Ns[[2]], rn = num_and_Ns[[3]], n = num_and_Ns[[4]],
                                      vol_dz = vol_dz, bio_conv = bio_conv, bps = bps)
  
  # Aggregate biomass
  biomass = bio_sp %>% agg_data(groups = c("species", "time"), fun = sum)
  
  biomass_age = bio_sp %>%
    #   filter(agecl > 2) %>%   # why get rid of juveniles??
    agg_data(groups = c("species", "agecl", "time"), fun = sum)
  
  # Aggregate Numbers! This is done seperately since numbers need to be summed!
  nums     = agg_data(data = num_and_Ns[[1]], groups = c("species", "time"), fun = sum)
  nums_age = agg_data(data = num_and_Ns[[1]], groups = c("species", "agecl", "time"), fun = sum)
  
  # Aggregate the rest of the dataframes by mean!
  structn_age = agg_data(data = num_and_Ns[[2]],  groups = c("species", "time", "agecl"), fun = mean)
  resn_age    = agg_data(data = num_and_Ns[[3]],  groups = c("species", "time", "agecl"), fun = mean)
  
  # lengthAtAge
  length_at_age = calculate_length_at_age(ab.df, structN = num_and_Ns[[2]], reserveN =  num_and_Ns[[3]], bio_conv = bio_conv)
  
  result = list(
    "biomass"                = biomass,       
    "biomass_age"            = biomass_age,
    "nums"                   = nums,
    "nums_age"               = nums_age,
    "resn_age"               = resn_age,
    "structn_age"            = structn_age,    
    "length_age"             = length_at_age
  )
  
  return(result)
}

# calculates length at age for age-structured species through time based on mass and provided a and b parameters
calculate_length_at_age = function(ab_params, reserveN, structN, bio_conv)
{
  ton_to_g = 1e6
  names(structN)[names(structN) == "atoutput"] = "structN"
  names(reserveN)[names(reserveN) == "atoutput"] = "reserveN"
  
  weight_age_sp = dplyr::inner_join(reserveN, structN, by = c("species", "agecl", "polygon", "layer", "time")) %>% 
    dplyr::left_join(ab_params, by = "species") %>%
    dplyr::mutate(totalWeight_g = (structN + reserveN) * bio_conv * ton_to_g) %>%
    dplyr::mutate(length = (totalWeight_g / a)^(1 / b))
  
  length_at_age = agg_data(weight_age_sp, col = "length", groups = c("species", "agecl", "time"), fun = mean)
  #names(length_at_age)[names(length_at_age) == "atoutput"] = "length"
  
  return(dplyr::ungroup(length_at_age))
}

