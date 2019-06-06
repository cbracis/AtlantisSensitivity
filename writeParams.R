require(atlantistools)
require(dplyr)
library(tools)
source("utilityFuncs.R")
# write some parameter files

create_sim_param_files = function(sa_design, param_bounds, base_biology_param_file, output_directory, start = 1)
{
  sa_design = add_juv_C(sa_design, JUV_C_GROUPS)
  sa_design = add_mum_from_c(sa_design)

  sa_values = get_values_from_levels(sa_design, param_bounds)
  sa_values = combine_mortality_age_params(sa_values)
  sa_values = translate_juv_C_mum(sa_values, JUV_C_GROUPS)
  sa_values = translate_KDENR(sa_values)
  
  params = names(sa_values %>% select(-simID)) 

  for (s in start:nrow(sa_values))
  {
    simID = sa_values$simID[s]
    param_file = create_file(output_directory, base_biology_param_file, simID)
    open_param_file = read_file(param_file)
    
    for (p in params)
    {
      param_value = sa_values[s, p]
      param_info = split_param(p)
      open_param_file = set_param(param_info$name, param_info$group, param_info$suffix, param_value, open_param_file)
    }
    
    write_file(open_param_file, param_file)
  }
}

# all the colums for C and add the same thing for mum (since we change these the same for pool groups
# and use a functional relationship for age groups), mum is already defined in param_bounds file
add_mum_from_c = function(sa_design)
{
  cIdxs = grep("^C_.*", names(sa_design)) # match C_ only at beginning followed by 0 or more characters
  mum_matrix = sa_design[,cIdxs, drop = FALSE]
  names(mum_matrix) = gsub("^C_", "mum_", names(mum_matrix))
  sa_design = cbind(sa_design, mum_matrix)
  return(sa_design)
}

# this is for things like CEP that have C_CEP_T15 and C_jCEP_T15 to have juvenile and adult C and mum
# maybe could look up groups in fgs file (but is NumCohorts = 2 vs 10 or GroupType = CEP that controls this?)
# this just adds a corresponding _juv for any _adult that is in the design matrix for the specified groups
add_juv_C = function(sa_design, group_codes)
{
  idxs = NULL
  for (code in group_codes)
  {
    idxs = c(idxs, grep(paste0("^C_", code, ".*"), names(sa_design)) )
  }
  juv_matrix = sa_design[,idxs, drop = FALSE]
  names(juv_matrix) = gsub("adult", "juv", names(juv_matrix))
  sa_design = cbind(sa_design, juv_matrix)

  return(sa_design)
}

# after parameter bounds are established, we need to change the _juv and _adult to the proper forms 
# to be written as parameters, this means making both end in _single and the code be XXX and jXXX
# needs to happen for C and mum
translate_juv_C_mum = function(sa_design, group_codes)
{
  for (code in group_codes)
  {
    names(sa_design) = gsub(paste0("^C_", code, "_", PARAM_SUFFIX_ADULT), 
                            paste0("C_", code, "_", PARAM_SUFFIX_SINGLE), names(sa_design))
    names(sa_design) = gsub(paste0("^C_", code, "_", PARAM_SUFFIX_JUVENILE), 
                            paste0("C_j", code, "_", PARAM_SUFFIX_SINGLE), names(sa_design))
    names(sa_design) = gsub(paste0("^mum_", code, "_", PARAM_SUFFIX_ADULT), 
                            paste0("mum_", code, "_", PARAM_SUFFIX_SINGLE), names(sa_design))
    names(sa_design) = gsub(paste0("^mum_", code, "_", PARAM_SUFFIX_JUVENILE), 
                            paste0("mum_j", code, "_", PARAM_SUFFIX_SINGLE), names(sa_design))
  }
  return(sa_design)
}

# KDENR is a single param (for how read in for the parameter compare) but technically an 
# age-structured parameter of length 1, so we need to change this to write it correctly
translate_KDENR = function(sa_design)
{
  names(sa_design) = gsub(paste0("^KDENR(.*)", PARAM_SUFFIX_SINGLE), 
                          paste0("KDENR\\1", PARAM_SUFFIX_AGE), names(sa_design))
  return(sa_design)
}

# substitite the 1, 2, 3, 4 from morris design for real parameter levels
get_values_from_levels = function(sa_design, param_bounds)
{
  params = names(sa_design)
  # assume sim id is first column
  for (i in 2:ncol(sa_design))
  {
#    print(paste(i, params[i]))
    sa_design[,i] = param_bounds[sa_design[,i], params[i]]
  }
  return(sa_design)
}

# for age groups, need mortality juvenile and adult values in single vector
combine_mortality_age_params = function(sa_values)
{
  for (m in c("mL", "mQ"))
  {
    params = names(sa_values)
    juv_idxs = grep(paste0("^", m, "_.*", PARAM_SUFFIX_JUVENILE), params)
    
    if (length(juv_idxs) > 0)
    {
      adult_params = gsub(PARAM_SUFFIX_JUVENILE, PARAM_SUFFIX_ADULT, params[juv_idxs]) # this way they are in the same order
      
      for (i in seq_along(juv_idxs))
      {
        #find the corresponding adult column, then combine and put in juv column
        sa_values[,juv_idxs[i]] = paste(sa_values[,juv_idxs[i]], sa_values[,adult_params[i]])
      }
      
      # clean up: rename juv to age, delete adult columns
      # match in 2 capture groups, \\1 refers to first group (don't want to change mQ at same time as mL)
      names(sa_values) = gsub(paste0("(^", m, "_.*)(", PARAM_SUFFIX_JUVENILE, ")"), 
                              paste0("\\1", PARAM_SUFFIX_AGE), 
                              params)
      sa_values = sa_values[,-which(params %in% adult_params)]
    }
  }
  return(sa_values)
}

split_param = function(param_str)
{
  param = strsplit(param_str, "_")[[1]]
  param = as.list(param)
  names(param) = c("name", "group", "suffix")
  return(param)
}

set_param = function(param, group, param_type, param_value, open_param_file)
{
  if (param_type == PARAM_SUFFIX_AGE)
  {
    param_value = getMatrixFromAgeParamVec(param_value)
    open_param_file = change_prm_cohort_file(prm_biol = open_param_file, select_acronyms = group,
                                             roc = param_value, parameter = param, relative = FALSE)
    
  } else if (param_type == PARAM_SUFFIX_SINGLE)
  {
    open_param_file = change_prm_file(prm_biol_new = open_param_file, select_acronyms = group, 
                                      roc = param_value, parameter = param, relative = FALSE)
    
  } else
  {
    stop(paste("Unknown parameter suffix type:", param_type))
  }
  return(open_param_file)
}

# this function used to change many parameters at once for testing 
# this function edits the parameter in the newly created dir
# question: how to loop over the params, will alos need to handle pool and age pool groups
write_params = function(biol_param_file, param_df, params_to_change)
{
  stopifnot("Code" %in% names(param_df))
  stopifnot("groupType" %in% names(param_df))

  param_df = param_df %>% arrange(Code)
  groups_age = unique(param_df$Code[param_df$groupType == GROUPS_AGE])
  groups_pool = unique(param_df$Code[param_df$groupType == GROUPS_POOL])
  
  for (param in params_to_change)
  {
    # age parameters that need to be matrixfied
    new_values_age = param_df %>% filter(Code %in% groups_age) %>% pull(param)
    new_values_age = getMatrixFromAgeParamVec(new_values_age)
    change_prm_cohort(prm_biol = biol_param_file, select_acronyms = groups_age,
                      roc = new_values_age, parameter = param, relative = FALSE, save_to_disc = TRUE)
    
    
    # TODO: change_prm() only adds _T15 for version_flag = 1 (becdev), but that is how aeec param file currently is??
    new_values_pool = param_df %>% filter(Code %in% groups_pool) %>% pull(param)
    change_prm(prm_biol = biol_param_file, select_acronyms = groups_pool, 
               roc = new_values_pool, parameter = param, relative = FALSE, 
               save_to_disc = TRUE, version_flag = 1)
  }
}



# makes copy of param file with the simulation id appended to the name
# for now go with the idea this will be an overlay, so just copy the biol prm, not everything
create_file = function(root_dir, base_biol_prm_file, simulation_id)
{
  new_file_name = paste0( file_path_sans_ext(basename(base_biol_prm_file)), simulation_id, ".", file_ext(base_biol_prm_file) )
  new_file_path = file.path(root_dir, new_file_name)
  
  file.copy(base_biol_prm_file, new_file_path)
  
  return(new_file_path)
}

# the following are copied from atlantis tools to allow changing multiple parameters before writing the file
# otherwise it takes forever to read/write the file so many times

# read in 
read_file = function(prm_biol)
{
  # Read in parameter file!
  prm_biol_new <- readLines(con = prm_biol)
  
  return(prm_biol_new)
}

write_file = function(prm_biol_new, prm_biol)
{
  writeLines(text = prm_biol_new, con = prm_biol, sep = "\n")
}

change_prm_cohort_file = function(prm_biol_new, select_acronyms, roc, parameter, relative = TRUE) {
  if (length(parameter) != 1) stop("Please suply only one parameter per function call.")
  
  # Convert to matrix if only one species is selected and roc is a vector!
  if (length(select_acronyms) == 1 & is.vector(roc)) roc <- matrix(roc, nrow = 1)
  
  # Convert matrix to list (it is not possible to programm indexing for both lists and matrices).
  # Therefore we convert every user input to a list.
  if (is.matrix(roc)) roc <- lapply(seq_len(nrow(roc)), function(i) roc[i,])
  
  # Check if all rocs equal to 1! leave function in this case!
  if (all(sapply(roc, function(x) all(x == 1))) & relative) {
    message("All rocs 1 no changes applied to prm-file.")
  } else {
    if (length(select_acronyms) != length(roc)) {
      stop("Dimensions of select_acronyms and roc do not match. Please supply one row of values per group.")
    }
    

    # Function to update a specific parameter composed of a parameter string
    # a group acronym and a seperator (by default "_") found in a prm file.
    update_prm_species <- function(prm_biol, acronym, roc, parameter, relative) {
      if (parameter %in% c("mL", "mQ")) {
        flag <- paste(acronym, parameter, sep = "_") # only works with trunc code!
      } else {
        flag <- paste(parameter, acronym, sep = "_")
      }
      pos <- scan_prm(chars = prm_biol, variable = flag)
      # Values are stored in the next row in the *.prm file.
      pos <- pos + 1
      # In case row is commented out use next column!
      while (substr(prm_biol[pos], 1, 1) == "#") pos <- pos + 1
      
      # Keep all numeric values
      old_value <- str_split_twice(char = prm_biol[pos], min_only = FALSE)
      if (length(old_value) != length(roc)) {
        stop(paste(length(old_value), "values found but only", length(roc), "new values supplied."))
      }
      
      if (relative) {
        new_value <- old_value * roc
      } else {
        new_value <- roc
      }
      
      prm_biol[pos] <- paste(new_value, collapse = "\t")
      return(prm_biol)
    }
    
    for (i in seq_along(select_acronyms)) {
      if (!(all(roc[[i]] == 1) & relative)) {
        prm_biol_new <- update_prm_species(prm_biol = prm_biol_new, acronym = select_acronyms[i],
                                           roc = roc[[i]], parameter = parameter, relative = relative)
      }
    }
    
    invisible(prm_biol_new)
  }
}

change_prm_file = function(prm_biol_new, select_acronyms, roc, parameter,
                       relative = TRUE, version_flag = 2) {
  if (length(parameter) != 1) stop("Please suply only one parameter per function call.")
  
  if (length(select_acronyms) != length(roc)) {
    stop("Length of select_acronyms and roc does not match.")
  }
  

  # Function to update a specific parameter composed of a parameter string
  # a group acronym and a seperator (by default "_") found in a prm file.
  update_prm_species <- function(prm_biol, acronym, roc, parameter, relative) {
    if (parameter == "AgeClassSize" | ( parameter %in% c("mL", "mQ") & version_flag == 2)) {
      flag <- paste(acronym, parameter, sep = "_")
    } else {
      flag <- paste(parameter, acronym, sep = "_")
    }
    pos <- scan_prm(chars = prm_biol, variable = flag)
    
    old_value <- str_split_twice(char = prm_biol[pos])
    
    if (relative) {
      new_value <- old_value * roc
    } else {
      new_value <- roc
    }
    
    # Update value. Some pesky expectations have to be added here.
    if (is.element(parameter, c("mum", "C")) |
        ( is.element(parameter, c("mQ", "mL", "jmL", "jmQ")) & version_flag == 1 )) {
      prm_biol[pos] <- paste(paste0(flag, "_T15"), new_value, sep = "\t")
    } else {
      prm_biol[pos] <- paste(flag, new_value, sep = "\t")
    }
    return(prm_biol)
  }
  
  for (i in seq_along(select_acronyms)) {
    if (!(roc[i] == 1 & relative)) {
      prm_biol_new <- update_prm_species(prm_biol = prm_biol_new, acronym = select_acronyms[i], roc = roc[i], parameter = parameter, relative = relative)
    }
  }
  
  invisible(prm_biol_new)
}
