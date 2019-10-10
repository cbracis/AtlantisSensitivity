require(dplyr)
require(tidyr)
require(sensitivity)

consolidate_output_avgs = function(sim_output_files, data_columns, key_column, output_dir)
{
  # make as many data frames as columns
  consolidated_results = NULL
  n_cols = length(data_columns)
  
  # get the species names/codes from first file, then cycle through rest
  sim_output = read.csv(sim_output_files[1], stringsAsFactors = FALSE)
  sim_number = extract_sim_number(sim_output_files[1])

  for (col in 1:n_cols)
  {
    #transponse this so col names are from key_column, first adding simulation number
    consolidated_results[[col]] = sim_output %>% select(c(key_column, data_columns[col])) %>% 
      mutate(sim = sim_number) %>% group_by(sim) %>% spread( key_column, data_columns[col]) 
  }
  
  # now go through 2nd and on files
  for (f in 2:length(sim_output_files))
  {
    if (file.exists(sim_output_files[f]))
    {
      sim_output = read.csv(sim_output_files[f], stringsAsFactors = FALSE)
      sim_number = extract_sim_number(sim_output_files[f])
      
      for (col in 1:n_cols)
      {
        # now row bind the column, bind_rows will match columns 
        consolidated_results[[col]] = bind_rows(consolidated_results[[col]],
                                                sim_output %>% select(c(key_column, data_columns[col])) %>% 
                                                  mutate(sim = sim_number) %>% group_by(sim) %>% 
                                                  spread( key_column, data_columns[col]) )
      }
    } else
    {
      warning(paste("file", sim_output_files[f], "does not exist"))
    }
    
  }
  
  # save results
  for (col in 1:n_cols)
  {
    #transponse this so col names are from key_column
    write.csv(consolidated_results[[col]], 
              file = file.path(output_dir, paste0(data_columns[col], ".csv")),
              row.names = FALSE, quote = FALSE)
  }
  
}

consolidate_stability = function(sim_output_files, column_prefix, output_dir)
{
  # read first output file to make data.frame
  sim_output = read.csv(sim_output_files[1], stringsAsFactors = FALSE)
  n_sim = length(sim_output_files)
  
  pval_results = matrix(NA, nrow = n_sim, ncol = (length(sim_output$code) + 1),
                                dimnames = list(1:n_sim, c("sim", sim_output$code)))
  pval_results = data.frame(pval_results)
  slope_results = pval_results
  slope_norm_results = pval_results
  range_norm_results = pval_results
  slope_in_range_results = pval_results
  
  for (f in 1:n_sim)
  {
    if (file.exists(sim_output_files[f]))
    {
      sim_output = read.csv(sim_output_files[f], stringsAsFactors = FALSE)
      sim_number = extract_sim_number(sim_output_files[f])
      pval_results$sim[f] = sim_number
      
      if (nrow(sim_output) == 0)
      {
        # probably crashing sim w/o data for last 10 yrs
        print(paste("sim skipped", sim_number))
      } else
      {
        for(i in 1:nrow(sim_output))
        {
          sp_code = sim_output$code[i]
          sp_data = unlist(sim_output[i, grepl(column_prefix, names(sim_output))]) 
          years = 1:length(sp_data) 
          
          # check for NAs (sim crashed) or < 0.1 t (species essentially extinct)
          # if so, leave NA for p_val in results
          if (all(!is.na(sp_data)) & all(sp_data > 0.1))
          {
            sp_mod = lm(sp_data ~ years)
            p_val = summary(sp_mod)$coefficients[2,4] # 2nd col is for slope
            pval_results[f, sp_code] = p_val
            slope_results[f, sp_code] = coef(sp_mod)[2]
            
            # so 0.1 is like being with in +/- 5% of the mean, but not nec. perfectly symmetric and dont have to count pts inside
            # from https://stats.stackexchange.com/questions/11544/testing-for-stability-in-a-time-series
            # "The naive approach that first pops into my mind (which I have seen used as win conditions for some neural networks, for instance) is to pick to parameters T and E, then if for the last T timesteps there are not two points x and x′ such that x′−x>E then we conclude we have stabilized. "
            range_norm_results[f, sp_code] = diff(range(sp_data)) / mean(sp_data) 
            
            # now check and see if the lm regression line is within 5% of mean, this will be true when the datapoints are
            # all within the range (even if the slope is significant) but is more forgiving for big osscilations for NPZ/recycling
            pred_vals = predict(sp_mod)
            pred_stable = all( pred_vals > 0.95 * mean(sp_data)  ) & all( pred_vals < 1.05 * mean(sp_data)  )
            slope_in_range_results[f, sp_code] = pred_stable
            
            #now normalize by mean and refit model
            sp_data = sp_data / mean(sp_data)
            sp_mod = lm(sp_data ~ years)
            slope_norm_results[f, sp_code] = coef(sp_mod)[2]
          }
        }
      }
    }
  }
  slope_results$sim = pval_results$sim
  slope_norm_results$sim = pval_results$sim
  range_norm_results$sim = pval_results$sim
  slope_in_range_results$sim = pval_results$sim
  
  write.csv(pval_results, 
            file = file.path(output_dir, paste0("stability_", column_prefix, "_pvals.csv")),
            row.names = FALSE, quote = FALSE)
  write.csv(slope_results, 
            file = file.path(output_dir, paste0("stability_", column_prefix, "_slope.csv")),
            row.names = FALSE, quote = FALSE)
  write.csv(slope_norm_results, 
            file = file.path(output_dir, paste0("stability_", column_prefix, "_slope_norm.csv")),
            row.names = FALSE, quote = FALSE)
  write.csv(range_norm_results, 
            file = file.path(output_dir, paste0("stability_", column_prefix, "_range_norm.csv")),
            row.names = FALSE, quote = FALSE)
  write.csv(slope_in_range_results, 
            file = file.path(output_dir, paste0("stability_", column_prefix, "_slope_in_range.csv")),
            row.names = FALSE, quote = FALSE)
  
}

# vector of sim numbers, output folder, file suffix to check for (after AEEC_SA_simXXXXX part)
get_missing_sims = function(sims, output_folder, file_suffix)
{
  files = file.path(output_folder, paste0("AEEC_SA_sim", sims, file_suffix))
  missing = extract_sim_number(files[!file.exists(files)])
  missing = as.numeric(missing)
  
  return(missing)
}

# vector of sims, number of trajectories run, i.e. 50, vector of missing sims
get_sims_for_complete_trajs = function(sims, num_traj, missing_sims)
{
  sims_per_traj = length(sims)/num_traj
  traj = rep(1:num_traj, each = sims_per_traj)
  sim_done = is.na(match(sims, missing_sims))
  print(tapply(sim_done, traj, sum))
  
  complete_traj = which(tapply(sim_done, traj, sum) == sims_per_traj)
  complete_sims = sims[traj %in% complete_traj]
  
  return(complete_sims)
}

# exctracts simulation number from files with the format AEEC_SA_sim10001_ten_year_avg_age.csv
# where the part after the simulation number can vary
extract_sim_number = function(file_name)
{
  # look for number between sim and _, substituting captured string
  sim = gsub(".*sim([0-9]+)_.*$", "\\1", file_name)
  return(sim)
}

# design_matrix_sub
get_morris_output = function(design_matrix_full, consolidated_results)
{
  complete_sims = consolidated_results$sim
  design_matrix_sub = as.matrix(design_matrix_full %>% filter(simID %in% complete_sims) %>% dplyr::select(-simID)) # avoid conflict with MASS
  
  param_names = colnames(design_matrix_sub) # without simID
  levels = length(unique(c(design_matrix_sub))) # just numer of levels and assume same for all params
  
  morris_full = morris(model = NULL, factors = param_names, r = 50, 
                       design = list(type = "oat", levels = levels, grid.jump = levels / 2),
                       binf = rep(1, length(param_names)), bsup = rep(levels, length(param_names)))
  
  # now update morris_full object with current data 
  morris_full$r = nrow(design_matrix_sub) / (ncol(design_matrix_sub) + 1)
  morris_full$X = design_matrix_sub
  morris_out = tell(morris_full, as.matrix(consolidated_results[,-1])) # -1 to remove sim col
  
  return(morris_out)
}

# calculate Morris, mu, mu.star, and sigma from help file
calculate_mu = function(elementaryEffects)
{
  return(calculate_morris_metric(elementaryEffects, mean))
}
calculate_mu.star = function(elementaryEffects)
{
  return(calculate_morris_metric(abs(elementaryEffects), mean))
}
calculate_sigma = function(elementaryEffects)
{
  return(calculate_morris_metric(elementaryEffects, sd))
}

calculate_percent_positive = function(elementaryEffects)
{
  percent_pos = function(x) {sum(x > 0) / length(x) }
  
  return(calculate_morris_metric(elementaryEffects, percent_pos))
}

calculate_morris_metric = function(elementaryEffects, FUN)
{
  if (is.matrix(elementaryEffects))
  {
    metric = apply(elementaryEffects, 2, FUN)
  }
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 3)
  {
    metric =  apply(elementaryEffects, 3, function(M){
      apply(M, 2, FUN)
    })
  }   
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 4)  # results (y) a 3-dim array
  {
    metric = sapply(1:dim(elementaryEffects)[4], function(i){
      apply(elementaryEffects[, , , i, drop = FALSE], 3, function(M){
        apply(M, 2, FUN)
      })
    }, simplify = "array")
  }
  else { stop("elementaryEffects must be matrix, or 3- or 4-dimensional array") }
  return(metric)
}

normalize_effects_by_max = function(ee_matrix)
{
  group_max = apply(abs(ee_matrix), 2, max)
  ee_norm = ee_matrix / group_max[col(ee_matrix)] # because R divides by columns, not rows
  return(ee_norm)
}

# sim_results: matrix with rows for simulation and columns group codes, 
#   plus a sim column for the sim number
# init: df with code (species codes) and column name including 'init' (inital value)
normalize_results_by_init = function(sim_results, init)
{
  stopifnot(colnames(sim_results)[1] == "sim") # first column assumed to be sim number
  groups = colnames(sim_results)[-1]

  init_vals = c(1, init$val[match(groups, init$code)]) # divide sim number by 1
  sim_results = sim_results / init_vals[col(sim_results)]
  return(sim_results)
}

sample_morris_output = function(consolidated_results, design_matrix, n_traj, n_sample, n_rep,
                                effect = c("mu.star", "mu", "sigma"))
{
  sims = consolidated_results$sim
  sims_per_traj = length(sims)/n_traj
  traj = rep(1:n_traj, each = sims_per_traj)

  n_params = dim(design_matrix)[2] - 1  # subtract 1 for sim column
  n_groups = dim(consolidated_results)[2] - 1 # ibed
  results = array(NA, dim = c(n_params, n_groups, n_rep))
  
  for (i in 1:n_rep)
  {
    traj_sample = sort(sample(1:n_traj, size = n_sample, replace = FALSE))
    sims_sample = sims[traj %in% traj_sample]

    morris_sample = get_morris_output(design_matrix, 
                                      consolidated_results[consolidated_results$sim %in% sims_sample,])
    
    results[,,i] = calculate_mu.star(morris_sample$ee)
  }
  rownames(results) = dimnames(morris_sample$ee)[[2]] # traj are dim 1, params are dim 2, groups dim 3
  colnames(results) = dimnames(morris_sample$ee)[[3]] 
  
  return(results)
}

# summ_effects - mu or mu.star, the matrix of summarized effects
# metrics, the metrics calculated for each simulation to get initial biomass, nums...
# fgs - functional groups file
# param_info_file - where to save param_info
# group_info_file - where to save group info
create_group_and_param_info = function(summ_effects, metrics, fgs, param_info_file, group_info_file)
{
  # this has a weird structure of matrix of lists which makes the data frame have embedded sublists
  #param_info = data.frame( t( sapply(row.names(mu.star), split_param, simplify = TRUE) ) )
  param_info = lapply(sapply(row.names(summ_effects), split_param, simplify = FALSE), unlist)
  param_info = data.frame( do.call("rbind", param_info), stringsAsFactors = FALSE)
  age_codes = get_age_group_codes(FUNCTIONAL_GROUPS_FILE)
  
  param_info$colIdx = 0
  param_info$colIdx[param_info$param %in% c("C", "mum")] = 1
  param_info$colIdx[param_info$param %in% c("mL", "mQ")] = 2
  param_info$colIdx[param_info$param %in% c("BHalpha", "KDENR")] = 3
 
  param_info$param_cat = 0
  param_info$param_cat[param_info$param %in% c("C", "mum")] = "growth"
  param_info$param_cat[param_info$param %in% c("mL", "mQ")] = "mortality"
  param_info$param_cat[param_info$param %in% c("BHalpha", "KDENR")] = "recruitment"
  
  param_info$invertIdx = 1
  param_info$invertIdx[param_info$code %in% age_codes] = 2
  
  param_info$longName = get_group_long_names(param_info$code, fgs)
  param_info$param_code = rownames(param_info)
  param_info$guild = get_group_guild(param_info$code)

  param_info$displayName = paste(param_info$code, param_info$param)
  param_info$displayName[param_info$suffix %in% c("juv", "adult")] = paste( param_info$displayName[param_info$suffix %in% c("juv", "adult")],
                                                                            param_info$suffix[param_info$suffix %in% c("juv", "adult")])
  
  group_info = data.frame(code = colnames(summ_effects), stringsAsFactors = FALSE)
  group_info$longName = get_group_long_names(group_info$code, fgs)
  group_info$invertIdx = 1
  group_info$invertIdx[group_info$code %in% age_codes] = 2
  group_info$guild = get_group_guild(group_info$code)
  
  
  
  # now have object metrics
  # NOTE!!!! changing Carrion DET from 0 to 1 to avoid divide by 0, so it won't be normalized
  biomass0 = metrics$biomass %>% filter(time == 0) %>% 
    rename(biomass.init = atoutput) %>% 
    select(-time) %>% 
    mutate(biomass.init = ifelse(biomass.init == 0, 1, biomass.init))
  group_info = left_join(group_info, biomass0, by = c("longName" = "species"))
  
  # alternatively, add nums
  nums0 = metrics$nums %>% filter(time == 0) %>% 
    rename(nums.init = atoutput) %>% 
    select(-time)
  group_info = left_join(group_info, nums0, by = c("longName" = "species"))
  
  # note that the merge can re-order the rows! need to be careful with the heat map plotting
  group_info = group_info[match(colnames(summ_effects), group_info$code),]
  
  write.csv(param_info, param_info_file, row.names = FALSE, quote = FALSE)
  write.csv(group_info, group_info_file, row.names = FALSE, quote = FALSE)
  
}