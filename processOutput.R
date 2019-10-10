# supporting libraries
require(atlantistools) # note requires updated version of package from github (not CRAN version)
require(dplyr)
require(tidyr)

source("~/getGroups.R") # TODO move here and rename file

process_atlantis_results = function(aeec_root_path, aeec_outdir, sim_id)
{
  #--- files and directories --------------------------------
  
  # Atlantis run files
  aeec_prm_biol = file.path(aeec_root_path, paste0("AEEC_biol", sim_id, ".prm"))
  
  aeec_bgm = file.path( aeec_root_path, "..", "poly_atlantisEC35_projETRS89_LAEA_snapped0p002.bgm")
  aeec_fgs = file.path(aeec_root_path, "SETasGroups.csv")  # functional groups file
  aeec_init = file.path(aeec_root_path,  "AEEC35_final_ini.nc") # initial conditions for groups, etc.
  aeec_prm_run = file.path(aeec_root_path,  "AEEC_run.prm") # model run parameters 
  
  # Atlantis output files
  aeec_output_nc = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, ".nc")) # TODO match sh script
  
  # other files/dirs for simulation results
  metrics_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, ".rdata"))
  five_year_age_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, "_five_year_avg_age.csv"))
  five_year_nonage_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, "_five_year_avg_nonage.csv"))
  ten_year_age_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, "_ten_year_avg_age.csv"))
  ten_year_nonage_file = file.path(aeec_outdir, paste0("AEEC_SA_sim", sim_id, "_ten_year_avg_nonage.csv"))
  
  #--- process output ------------------------------------
  
  # TODO: find sediment layer automatically?
  # TODO: CEP not handled correctly
  
  metrics = calcualte_metrics(output = aeec_output_nc, biol_prm = aeec_prm_biol,
                              fgs = aeec_fgs, init = aeec_init, run_prm = aeec_prm_run, 
                              bgm = aeec_bgm, sediment_layer = 3)
  # no filtering since we write data 5x per year
  
  sim_good = verify_atlantis_output(aeec_output_nc, aeec_prm_run)
  if (!sim_good)
  {
    # atlantis crashed along the way
    file.create(file.path(aeec_outdir, paste0("atlantis_output_incorrect_length_sim", sim_id, ".txt")))
    
    num_years = attr(sim_good, 'n_years')
    print(num_years)
    print(floor(num_years / 5) * 5)
    print(floor(num_years / 10) * 10)
    
    five_year_avg = calculate_5_years(metrics, max_year = floor(num_years / 5) * 5, fgs = aeec_fgs)
    ten_year_avg = calculate_10_years(metrics, max_year = floor(num_years / 10) * 10, fgs = aeec_fgs)

  } else
  {
    # save 5 yr and 10 yr summary
    five_year_avg = calculate_5_years(metrics, fgs = aeec_fgs)
    ten_year_avg = calculate_10_years(metrics, fgs = aeec_fgs)
    
  }
  
  write.csv(five_year_avg$nonage, file = five_year_nonage_file, quote = FALSE, row.names = FALSE)
  write.csv(five_year_avg$age, file = five_year_age_file, quote = FALSE, row.names = FALSE)
  write.csv(ten_year_avg$nonage, file = ten_year_nonage_file, quote = FALSE, row.names = FALSE)
  write.csv(ten_year_avg$age, file = ten_year_age_file, quote = FALSE, row.names = FALSE)
  
  save(metrics, file = metrics_file)

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
  structn_age = agg_data(data = num_and_Ns[[2]],  groups = c("species", "agecl", "time"), fun = mean)
  resn_age    = agg_data(data = num_and_Ns[[3]],  groups = c("species", "agecl", "time"), fun = mean)
  
  # lengthAtAge
  length_at_age = calculate_length_at_age(ab.df, structN = num_and_Ns[[2]], reserveN =  num_and_Ns[[3]], bio_conv = bio_conv)
  
  # because 0's are missing after load_nc, fill them back in or else extinctions are unrecognized and 
  # data frame is not complete
  biomass = complete(biomass, species, time, fill = list(atoutput = 0))
  biomass_age = complete(biomass_age, species, agecl, time, fill = list(atoutput = 0))
  nums = complete(nums, species, time, fill = list(atoutput = 0))
  nums_age = complete(nums_age, species, agecl, time, fill = list(atoutput = 0))
  structn_age = complete(structn_age, species, agecl, time, fill = list(atoutput = 0))
  resn_age = complete(resn_age, species, agecl, time, fill = list(atoutput = 0))
  length_at__age = complete(length_at_age, species, agecl, time, fill = list(atoutput = 0))
  
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
  result_nonage = NULL
  result_age = NULL
  
  for (i in 1:n_intervals)
  {
    # nonage 
    old_names = setdiff(names(result_list[[i]]$nonage), c("species"))
    new_names = paste0(old_names, interval_names[i])
    
    result_nonage[[i]] = rename(result_list[[i]]$nonage, !!!syms(setNames(old_names, new_names)) )
    
    # age
    old_names = setdiff(names(result_list[[i]]$age), c("species", "agecl"))
    new_names = paste0(old_names, interval_names[i])

    result_age[[i]] = rename(result_list[[i]]$age, !!!syms(setNames(old_names, new_names)) )
  }
  
  results$nonage = Reduce(function(x, y) full_join(x, y, by = "species"), result_nonage) 
  results$age = Reduce(function(x, y) full_join(x, y, by = c("species", "agecl")), result_age) 
  
  if (!is.null(fgs))
  {
    results$nonage$code = get_codes_from_long_names(results$nonage$species, fgs)
    results$age$code = get_codes_from_long_names(results$age$species, fgs)
  }
  return(results)
}

# calculates sets of 5 year avgs for 100 year simulation
calculate_5_years = function(metric_list, max_year = 100, fgs = NULL)
{
  return(calculate_avg_timespan_intervals(metric_list, 0, max_year, 5, fgs))
}

# calculates sets of 10 year avgs for 100 year simulation
calculate_10_years = function(metric_list, max_year = 100, fgs = NULL)
{
  return(calculate_avg_timespan_intervals(metric_list, 0, max_year, 10, fgs))
}

# calculates each year of the last ten years for stability analysis
calculate_each_last_10_years = function(metric_list, max_year = 100, fgs = NULL)
{
  return(calculate_avg_timespan_intervals(metric_list, 90, max_year, 1, fgs))
}

# verify atlantis output, that there is enough and it didn't crash
# note attribute n_years may not be accurate if interval doesn't line up evenly with years
verify_atlantis_output = function(output_nc, run_prm)
{
  # note! we assume these are in units of days
  # and ignore any other run prm settings that could also change output length
  stop_days = extract_prm(prm_biol = run_prm, variables = "tstop")
  start_days = extract_prm(prm_biol = run_prm, variables = "toutstart")
  interval = extract_prm(prm_biol = run_prm, variables = "toutinc")
  
  expected_length = length(seq(from = start_days, to = stop_days, by = interval))
  n_timesteps = get_atlantis_output_length(output_nc)
  
  result = (expected_length == n_timesteps)
  attr(result, 'n_timesteps') = n_timesteps
  attr(result, 'n_years') = (n_timesteps + start_days - 1) / (365 / interval)
  
  return(result)
}

# gets the year of the last recorded data, i.e. should be 100
get_atlantis_output_length = function(output_nc, run_prm)
{
  # use RNetCDF since it's already there for atlantis tools
  output = RNetCDF::open.nc(con = output_nc)
  on.exit( RNetCDF::close.nc(output) )
  n_timesteps = RNetCDF::dim.inq.nc(output, 0)$length
  return(n_timesteps)
}


add_missing_rows = function(metrics_df, run_prm)
{
  # note! we assume these are in units of days
  # and ignore any other run prm settings that could also change output length
  stop_days = extract_prm(prm_biol = run_prm, variables = "tstop")
  start_days = extract_prm(prm_biol = run_prm, variables = "toutstart")
  interval = extract_prm(prm_biol = run_prm, variables = "toutinc")
  
  times = seq(from = start_days, to = stop_days, by = interval) / 365
  
  for (col in names(metrics_df))
  {
    if (grepl("age", col, fixed = TRUE))
    {
      metrics_df[[col]] = complete(metrics_df[[col]], species, agecl, time = times, fill = list(atoutput = 0))
    } else
    {
      metrics_df[[col]] = complete(metrics_df[[col]], species, time = times, fill = list(atoutput = 0))
    }
  }
  return(metrics_df)
}
