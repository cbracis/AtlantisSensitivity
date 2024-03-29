# calculates all the parameter bounds
source("fitCFunction.R")
source("utilityFuncs.R")

library(dplyr)

# C - age groups - calculated from init values
# C - pool groups - calcualted from current calibrated value
# mum - age groups - calculated as mum = 3 * C
# mum - pool groups - calcualted from current calibrated value
# mL/mQ, first 2 values 0, then 2 other levels, based on calibrated value if non-0 or param comparison if 0
# BHalpha - from stock assesment
# KDENR - calcualted from current calibrated value

# number of levels hard-coded to be 8

# what values to multiply the baseline value by to get the parameter levels, the length determines the number of levels
param_mult_factors = seq(from = -0.5, to = 1, len = 8) + 1
# what value to multiply C by to get mum for age-structured groups
mum_func_of_C_mult_factor = 3
# which groups are bacteria, detritus, etc that shouldn't be included in the SA
bacteria_detritus_types = c("SED_BACT", "PL_BACT", "LAB_DET", "REF_DET", "CARRION")

# get age groups, ceph groups, and pool groups and need to know primary producers too

# for each group, look up info on it, e.g; group type etc, then make each type of parameter
create_param_bounds = function(func_groups_data, param_values, fitted_C_values, BHalpha_mult_factors)
{
  codes = get_group_codes(func_groups_data)
  
  param_bounds_df = data.frame(levels = 1:length(param_mult_factors))

  for (code in codes)
  {
    group_info = get_group_info(code, func_groups_data)
    param_bounds_df = bind_cols(param_bounds_df, 
                                create_bounds_C_mum(code, group_info$Group_Type, group_info$Is_Primary_Producer,
                                                    param_values, fitted_C_values))
    param_bounds_df = bind_cols(param_bounds_df, 
                                create_bounds_mL_mQ(code, group_info$Group_Type, group_info$Is_Primary_Producer, param_values))
    
    param_bounds_df = bind_cols(param_bounds_df, create_bounds_reproduction(code, group_info$Group_Type, 
                                                                            param_values, BHalpha_mult_factors))
    
  }
  return(param_bounds_df)
}

get_group_codes = function(species_groups_df)
{
  # don't include disabled groups or bacteria, detritus, carrion in the analysis
  codes = species_groups_df %>% filter(IsTurnedOn == 1, !(GroupType %in% bacteria_detritus_types)) %>% pull(Code)
  return(codes)
}

create_bounds_C_mum = function(code, group_type, group_is_primary_producer, param_values, fitted_C_values)
{
  bounds = NULL
  if (group_is_primary_producer) 
  { 
    # no C for primary producers
    values = param_mult_factors * filter(param_values, Param == "mum", Code == code) %>% pull(Value)
    bounds = data.frame(x = values)
    names(bounds) = paste("mum", code, PARAM_SUFFIX_SINGLE, sep = "_")
  }
  else if (group_type == GROUP_TYPE_POOL)
  {
    values_C = param_mult_factors * filter(param_values, Param == "C", Code == code) %>% pull(Value)
    values_mum = param_mult_factors * filter(param_values, Param == "mum", Code == code) %>% pull(Value)

    bounds = data.frame(x = values_C, y = values_mum)
    names(bounds) = paste(c("C", "mum"), code, PARAM_SUFFIX_SINGLE, sep = "_")
  } 
  else if (group_type == GROUP_TYPE_JUV_ADULT)
  {
    values_C_j = param_mult_factors * filter(param_values, Param == "C", Code == code, Age == 1) %>% pull(Value)
    values_C_a = param_mult_factors * filter(param_values, Param == "C", Code == code, Age == 2) %>% pull(Value)
    values_mum_j = param_mult_factors * filter(param_values, Param == "mum", Code == code, Age == 1) %>% pull(Value)
    values_mum_a = param_mult_factors * filter(param_values, Param == "mum", Code == code, Age == 2) %>% pull(Value)
    
    bounds = data.frame(x1 = values_C_j, x2 = values_C_a, y1 = values_mum_j, y2 = values_mum_a)
    names(bounds) = paste(c("C", "C", "mum", "mum"), code, c(PARAM_SUFFIX_JUVENILE, PARAM_SUFFIX_ADULT), sep = "_")
  }
  else if (group_type == GROUP_TYPE_AGE)
  {
    values_C = filter(fitted_C_values, Code == code) %>% pull(C)
    values_C = getMatrixFromAgeParamVec(values_C)
    values_C = outer(param_mult_factors, values_C) # 10 x 4 matrix
    values_mum = mum_func_of_C_mult_factor * values_C
    
    values_C = apply(values_C, 1, paste, collapse = " ")
    values_mum = apply(values_mum, 1, paste, collapse = " ")
    bounds = data.frame(x = values_C, y = values_mum)
    names(bounds) = paste(c("C", "mum"), code, PARAM_SUFFIX_AGE, sep = "_")
  } else
  {
    stop(paste("Unknown group type", group_type))
  }
  return(bounds)
}

create_bounds_mL_mQ = function(code, group_type, group_is_primary_producer, param_values)
{
  result = NULL
  for(m_param in c("mL", "mQ"))
  {
    bounds = NULL
    m_values = filter(param_values, Param == m_param, Code == code)
    
    # now filter those out that have value = 0, we don't include these params in the SA
    #  only juv or adult mortality is non-0 could be possible
    if (!all(m_values %>% pull(Value) == 0)) 
    { 
      if (group_type == GROUP_TYPE_POOL)
      {
        if ( !group_is_primary_producer | (group_is_primary_producer & m_param == "mL") ) # no quadratic mortality for primary producers
        {
          values =  param_mult_factors * m_values %>% pull(Value)
          bounds = data.frame(x = values)
          names(bounds) = paste(m_param, code, PARAM_SUFFIX_SINGLE, sep = "_")
        }
      } 
      else if (group_type %in% c(GROUP_TYPE_JUV_ADULT, GROUP_TYPE_AGE))
      {
        values_j =  param_mult_factors * filter(m_values, Age == 1) %>% pull(Value)
        values_a =  param_mult_factors * filter(m_values, Age == 2) %>% pull(Value)
        bounds = data.frame(x1 = values_j, x2 = values_a)
        names(bounds) = paste(m_param, code, c(PARAM_SUFFIX_JUVENILE, PARAM_SUFFIX_ADULT), sep = "_")
      } else
      {
        stop(paste("Unknown group type", group_type))
      }
      result = bind_cols(result, bounds)
    }
  }
  return(result)
} 


create_bounds_reproduction = function(code, group_type, param_values, BHalpha_mult_factors)
{
  interpolate_mult_factor = function(bounds_lower_upper)
  {
    mult_factor = seq(from = bounds_lower_upper[1], to = bounds_lower_upper[2], 
                      length.out = length(param_mult_factors))
    return(mult_factor)
  }
  bounds = NULL
  if (group_type == GROUP_TYPE_AGE)
  {
    flagrecruit = filter(param_values, Param == "flagrecruit", Code == code) %>% pull(Value)
    
    if (flagrecruit == 3) # Beverton-Holt
    {
      # COL_BHALPHA_LEVEL_1, COL_BHALPHA_LEVEL_2, COL_BHALPHA_LEVEL_3, COL_BHALPHA_LEVEL_4
      # which represent 5, 25, 75, 95 percentile, therefore we now use levels 1 and 4
      # and interpolate the others evenly between it
      cols = c(COL_BHALPHA_LEVEL_1, COL_BHALPHA_LEVEL_4)
      default_mult_factor = interpolate_mult_factor( unlist(filter(BHalpha_mult_factors, Code == "ALL") %>% select(cols)) )
      mult_factor = NULL
      
      if (code %in% BHalpha_mult_factors)
      {
        mult_factor = interpolate_mult_factor( unlist(filter(BHalpha_mult_factors, Code == code) %>% select(cols)) )
      } else
      {
        mult_factor = default_mult_factor
      }
      
      values = mult_factor * filter(param_values, Param == "BHalpha", Code == code) %>% pull(Value)
      bounds = data.frame(x = values)
      names(bounds) = paste("BHalpha", code, PARAM_SUFFIX_SINGLE, sep = "_")
    }
    else if (flagrecruit == 12) # fixed number of offspring
    {
      values = param_mult_factors * filter(param_values, Param == "KDENR", Code == code) %>% pull(Value)
      bounds = data.frame(x = values)
      names(bounds) = paste("KDENR", code, PARAM_SUFFIX_SINGLE, sep = "_")
    } 
    else
    {
      # would need to add others if they are used 
      stop(paste("flagrecruit not implemented", flagrecruit))
    }
  }
  return(bounds)
}

get_group_info = function(code, species_groups_df)
{
  row_idx = which(code == species_groups_df$Code)
  group_info = list(
    Code = species_groups_df$Code[row_idx],
    Name = species_groups_df$Name[row_idx],
    # note, this system of rules borrowed from atlantistools get-groups.R, correct?
    # why are only FISH age groups if cohort num is 2, really also SHARK, MAMMAL, REPTILE,...?
    Group_Type = ifelse(species_groups_df$NumCohorts[row_idx] == 1, GROUP_TYPE_POOL,
                        ifelse(species_groups_df$NumCohorts[row_idx] > 2 | species_groups_df$GroupType[row_idx] == "FISH", 
                               GROUP_TYPE_AGE, GROUP_TYPE_JUV_ADULT)),
    Is_Primary_Producer = !as.logical(species_groups_df$isPredator[row_idx])
  )
  return(group_info)
}