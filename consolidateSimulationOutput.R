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
  complete_sims = consolid_results$sim
  design_matrix_sub = as.matrix(design_matrix %>% filter(simID %in% complete_sims) %>% select(-simID))
  
  param_names = colnames(design_matrix_sub) # without simID
  levels = length(unique(c(design_matrix_sub))) # just numer of levels and assume same for all params
  
  morris_full = morris(model = NULL, factors = param_names, r = 50, 
                       design = list(type = "oat", levels = levels, grid.jump = levels / 2),
                       binf = rep(1, length(param_names)), bsup = rep(levels, length(param_names)))
  
  # now update morris_full object with current data 
  morris_full$r = nrow(design_matrix_sub) / (ncol(design_matrix_sub) + 1)
  morris_full$X = design_matrix_sub
  morris_out = tell(morris_full, as.matrix(consolid_results[,-1])) # -1 to remove sim col
  
  return(morris_out)
}

# calculate Morris, mu, mu.star, and sigma from help file
calculate_mu = function(elementaryEffects)
{
  if (is.matrix(elementaryEffects)) # results (y) a vector
  {
    mu = apply(elementaryEffects, 2, mean)
  }
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 3)  # results (y) a matrix
  {
    mu = apply(elementaryEffects, 3, function(M){
      apply(M, 2, mean)
    })
  } 
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 4)  # results (y) a 3-dim array
  {
    mu = sapply(1:dim(elementaryEffects)[4], function(i){
      apply(elementaryEffects[, , , i, drop = FALSE], 3, function(M){
        apply(M, 2, mean)
      })
    }, simplify = "array")
  }
  else { stop("elementaryEffects must be matrix, or 3- or 4-dimensional array") }
  return(mu)
}
calculate_mu.star = function(elementaryEffects)
{
  if (is.matrix(elementaryEffects))
  {
    mu.star = apply(elementaryEffects, 2, function(x) mean(abs(x)))
  }
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 3)
  {
    mu.star = apply(abs(elementaryEffects), 3, function(M){
      apply(M, 2, mean)
    })
  } 
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 4)  # results (y) a 3-dim array
  {
    mu.star = sapply(1:dim(elementaryEffects)[4], function(i){
      apply(abs(elementaryEffects)[, , , i, drop = FALSE], 3, function(M){
        apply(M, 2, mean)
      })
    }, simplify = "array")
  }
  else { stop("elementaryEffects must be matrix, or 3- or 4-dimensional array") }
  return(mu.star)
}
calculate_sigma = function(elementaryEffects)
{
  if (is.matrix(elementaryEffects))
  {
    sigma = apply(elementaryEffects, 2, sd)
  }
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 3)
  {
    sigma =  apply(elementaryEffects, 3, function(M){
      apply(M, 2, sd)
    })
  }   
  else if (is.array(elementaryEffects) & length(dim(elementaryEffects)) == 4)  # results (y) a 3-dim array
  {
    sigma = sapply(1:dim(elementaryEffects)[4], function(i){
      apply(elementaryEffects[, , , i, drop = FALSE], 3, function(M){
        apply(M, 2, sd)
      })
    }, simplify = "array")
  }
  else { stop("elementaryEffects must be matrix, or 3- or 4-dimensional array") }
  return(sigma)
}

normalize_effects_by_max = function(ee_matrix)
{
  group_maxs = apply(abs(ee_matrix), 2, max)
  ee_norm = ee_matrix / group_maxs[col(ee_matrix)] # because R divides by columns, not rows
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
