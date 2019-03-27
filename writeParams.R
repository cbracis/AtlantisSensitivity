require(atlantistools)
require(dplyr)
source("utilityFuncs.R")
# write some parameter files

create_sim_param_files = function(sa_design, param_bounds, base_biology_param_file, output_directory)
{
  sa_design = add_juv_C(sa_design, JUV_C_GROUPS)
  sa_design = add_mum_from_c(sa_design)
  sa_values = get_values_from_levels(sa_design, param_bounds)
  sa_values = combine_mortality_age_params(sa_values)
  
  params = names(sa_values %>% select(-simID)) 

  for (s in 1:nrow(sa_values))
  {
    simID = sa_design$simID[s]
    param_file = create_file(output_directory, base_biology_param_file, simID)
    
    for (p in params)
    {
      param_value = sa_values[s, p]
      param_info = split_param(p)
      set_param(param_info$name, param_info$group, param_info$suffix, param_value, param_file)
    }
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
add_juv_C = function(sa_design, group_codes)
{
  idxs = NULL
  for (code in group_codes)
  {
    idxs = c(idxs, grep(paste0("^C_", code, ".*"), names(sa_design)) )
  }
  juv_matrix = sa_design[,idxs, drop = FALSE]
  names(juv_matrix) = gsub("^C_", "C_j", names(juv_matrix))
  sa_design = cbind(sa_design, juv_matrix)

  return(sa_design)
}

get_values_from_levels = function(sa_design, param_bounds)
{
  params = names(sa_design)
  # assume sim id is first column
  for (i in 2:ncol(sa_design))
  {
    sa_design[,i] = param_bounds[sa_design[,i], params[i]]
  }
  return(sa_design)
}

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
                              paste0("\\1", PARAM_SUFFIX_ADULT), 
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

set_param = function(param, group, param_type, param_value, param_file)
{
  if (param_type == PARAM_SUFFIX_AGE)
  {
    param_value = getMatrixFromAgeParamVec(param_value)
    change_prm_cohort(prm_biol = param_file, select_acronyms = group,
                      roc = param_value, parameter = param, relative = FALSE, save_to_disc = TRUE)
    
  } else if (param_type == PARAM_SUFFIX_SINGLE)
  {
    change_prm(prm_biol = param_file, select_acronyms = group, 
               roc = param_value, parameter = param, relative = FALSE, 
               save_to_disc = TRUE)
    
  } else
  {
    stop(paste("Unknown parameter suffix type:", param_type))
  }
  
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



# makes copy of param file in new folder
# for now go with the idea this will be an overlay, so just copy the biol prm, not everything
create_file = function(root_dir, base_biol_prm_file, simulation_id)
{
  dir_name = paste0("sim_", simulation_id)
  new_file_path = file.path(root_dir, dir_name)
  
  dir.create(new_file_path)
  file.copy(base_biol_prm_file, new_file_path)
  
  return(file.path(new_file_path, basename(base_biol_prm_file)))
}