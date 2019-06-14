require(dplyr)
require(tidyr)

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

# exctracts simulation number from files with the format AEEC_SA_sim10001_ten_year_avg_age.csv
# where the part after the simulation number can vary
extract_sim_number = function(file_name)
{
  # look for number between sim and _, substituting captured string
  sim = gsub(".*sim([0-9]+)_.*$", "\\1", file_name)
  return(sim)
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

