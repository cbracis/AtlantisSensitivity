# supporting libraries
require(atlantistools) # note requires updated version of package from github (not CRAN version)
require(dplyr)

source("~/getGroups.R") # TODO move here and rename file

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
  groups_age = get_age_group_names(fgs) # the vertebrate groups with age structure
  groups_age_code = get_age_group_codes(fgs) # the 3 letter codes used in the prm file
  # TODO exclude cohort pool groups until we handle them properly, 
  # TODO TODO I just changed this, need to verify it
  groups_pool = get_pool_group_names(fgs) # the biomass pool groups to use (invertebrates)
  
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
  
  biomass_age = bio_sp %>% filter(species %in% get_group_long_names(groups_age, fgs)) %>%
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

# data is data.frame with columns species, time, and atoutput, and optionally agecl
# interval is open on left (from) and closed on right (to)
calculate_avg_timespan = function(data, from, to)
{
  grouping = if ("agecl" %in% names(data)) { c("species", "agecl") } else { "species"}
 
   species_mean = data %>% 
    filter(time > from & time <= to) %>%
    group_by(.dots = grouping) %>% 
    summarise(avg = mean(atoutput))
  return(species_mean)
}

# calculates averages for each metric, works on output of calculate_metrics
# intervals are as in calculate_avg_timespan, open on lower bound and closed on upper
calculate_avg_timespan_all = function(metric_list, from, to, fgs = NULL)
{
  metric_names = names(metric_list)
  metric_names_age = metric_names[grep("age", metric_names)]
  metric_names_nonage = setdiff(metric_names, metric_names_age)
  results = NULL
  
  avgs_nonage = lapply(metric_names_nonage, function(x) calculate_avg_timespan(metric_list[[x]], from, to) %>% 
                                                        rename(!!x := avg)) # the !! unquotes the x to use the varaible as the name and not x, see help(":=")
  results$nonage = Reduce(function(x, y) full_join(x, y, by = "species"), avgs_nonage) 

  avgs_age = lapply(metric_names_age, function(x) calculate_avg_timespan(metric_list[[x]], from, to) %>% 
                                                  rename(!!x := avg)) # the !! unquotes the x to use the varaible as the name and not x
  results$age = Reduce(function(x, y) full_join(x, y, by = c("species", "agecl")), avgs_age) 
  
  if (!is.null(fgs))
  {
    results = lapply(results,
                     function(x) cbind(x, code = get_codes_from_long_names(x$species, fgs)) )
  }
  
  return(results)
}

# calculates averages for each metric, *across a number of time intervals* 
# works on output of calculate_metrics
# be careful about how to specify from and to, remember intervals are open on lower bound and closed on upper
# thus from = 0 would include the first report interval but not the initial conditions 0
calculate_avg_timespan_intervals = function(metric_list, start, end, interval, fgs = NULL)
{
  n_intervals = floor((end - start) / interval)
  
  result_list = NULL
  interval_names = NULL
  from = start
  for (i in 1:n_intervals)
  {
    # not sure if this is right, think throguh it, yes just need to add note to explain how to pick values
    result_list[[i]] = calculate_avg_timespan_all(metric_list, from = from, to = from + interval)
    interval_names[i] = paste0(".", from, "-", from + interval)
    
    from = from + interval
  }
  
  # now we need to consolidate
  # cbind the columns with the year range appended, at the end add back the species, and if nec agecl and code
  results = NULL
  
  for (i in 1:n_intervals)
  {
    # nonage 
    old_names = setdiff(names(result_list[[i]]$nonage), c("species"))
    new_names = paste0(old_names, interval_names[i])
    results$nonage = bind_cols( results$nonage, 
                                result_list[[i]]$nonage %>% 
                                  rename( !!!syms(setNames(old_names, new_names)) ) %>%
                                  select(-species) )
    
    # age
    old_names = setdiff(names(result_list[[i]]$age), c("species", "agecl"))
    new_names = paste0(old_names, interval_names[i])
    results$age = bind_cols( results$age, 
                                result_list[[i]]$age %>% 
                                  rename( !!!syms(setNames(old_names, new_names)) ) %>%
                                  select(-c("species", "agecl")) )
    
  }
  
  # add back species and code columns (assume first list item)
  results$nonage$species = result_list[[1]]$nonage$species
  results$age$species = result_list[[1]]$age$species
  if (!is.null(fgs))
  {
    results$nonage$code = get_codes_from_long_names(results$nonage$species, fgs)
    results$age$code = get_codes_from_long_names(results$age$species, fgs)
  }
  return(results)
}

# calculates sets of 5 year avgs for 100 year simulation
calculate_5_years = function(metric_list, fgs = NULL)
{
  return(calculate_avg_timespan_intervals(metric_list, 0, 100, 5, fgs))
}

# calculates sets of 10 year avgs for 100 year simulation
calculate_10_years = function(metric_list, fgs = NULL)
{
  return(calculate_avg_timespan_intervals(metric_list, 0, 100, 10, fgs))
}

# verify atlantis output, that there is enough and it didn't crash
verify_atlantis_output = function(output_nc, run_prm)
{
  # note! we assume these are in units of days
  # and ignore any other run prm settings that could also change output length
  stop_days = extract_prm(prm_biol = run_prm, variables = "tstop")
  start_days = extract_prm(prm_biol = run_prm, variables = "toutstart")
  interval = extract_prm(prm_biol = run_prm, variables = "toutinc")
  
  expected_length = length(seq(from = start_days, to = stop_days, by = interval))
  
  # use RNetCDF since it's already there for atlantis tools
  output = RNetCDF::open.nc(con = aeec_output_nc)
  on.exit( RNetCDF::close.nc(output) )
  n_timesteps = RNetCDF::dim.inq.nc(output, 0)$length
  
  result = (expected_length == n_timesteps)
  return(result)
}
