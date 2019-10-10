require(atlantistools)

# gets Name from functional groups file of all age structured groups
get_age_group_names = function(functional_groups_file)
{
  fgs = load_fgs(functional_groups_file)
  groups_off = fgs$Name[fgs$IsTurnedOn == 0]
  groups_age = get_age_groups(functional_groups_file)
  groups_age = groups_age[!groups_age %in% groups_off]
  
  return(groups_age)
}

# gets Code (the abbreviated 2 or 3 letter name) from functional groups file of all age structured groups
get_age_group_codes = function(functional_groups_file)
{
  fgs = load_fgs(functional_groups_file)
  groups_off = fgs$Code[fgs$IsTurnedOn == 0]
  groups_age_code = get_age_acronyms(functional_groups_file)
  groups_age_code = groups_age_code[!groups_age_code %in% groups_off]
  
  return(groups_age_code)
}

# gets Name from functional groups file of all biomass pool groups
# exclude_cohorts: exclude cohort (juvenile/adult) pool groups like cephalopods
get_pool_group_names = function(functional_groups_file, exclude_cohorts = FALSE)
{
  fgs = load_fgs(functional_groups_file)
  groups_off = fgs$Name[fgs$IsTurnedOn == 0]
  
  groups = get_groups(functional_groups_file)
  groups = groups[!groups %in% groups_off]
  
  groups_age = get_age_groups(functional_groups_file)
  groups_pool = groups[!groups %in% groups_age]
  
  if (exclude_cohorts)
  {
    # exclude things like ceph, prawns because they have 2 stages, thus tricky
    groups_cohort = fgs$Name[fgs$NumCohorts > 1]
    groups_pool = groups_pool[!groups_pool %in% groups_cohort]
  }
  
  return(groups_pool)
}

# gets Code (the abbreviated 2 or 3 letter name) from functional groups file 
get_pool_group_codes = function(functional_groups_file, exclude_cohorts = FALSE)
{
  fgs = load_fgs(functional_groups_file)
  groups_pool = get_pool_group_names(functional_groups_file, exclude_cohorts)
  groups_pool_code = fgs$Code[fgs$Name %in% groups_pool]
  
  return(groups_pool_code)
}

# gets Code (the abbreviated 2 or 3 letter name) from functional groups file for same filtered list as getPoolGroups()
# this also omits the nonPredator groups (for which there is no C among other things)
get_pool_predators_group_codes = function(functional_groups_file)
{
  fgs = load_fgs(functional_groups_file)
  groups_pool_code = get_pool_group_codes(functional_groups_file)
  is_pred_idx = which(names(fgs) %in% c("isPredator", "IsPredator")) # col names not case sensitive!
  
  groups_pool_code = fgs$Code[fgs$Code %in% groups_pool_code & fgs[,is_pred_idx] == 1]
  
  return(groups_pool_code)
}

# gets the LongName (used by atlantistools in the summary data frames) from the Name or abbreviated name (Code)
get_group_long_names = function(groups, functional_groups_file)
{
  fgs = load_fgs(functional_groups_file)
  
  if (all( nchar(groups) <= 3 ))
  {
    # using code (2 or 3 letter abbreviation)
    long_names = fgs$LongName[match(groups, fgs$Code)]
    
  } else
  {
    # using name (separated with underscores)
    long_names = fgs$LongName[match(groups, fgs$Name)]
  }
  return(long_names)
}

# get the 2 or 3 letter code for species groups from the LongName (used by atlantistools in the summary data frames)
get_codes_from_long_names = function(groups, functional_groups_file)
{
  fgs = load_fgs(functional_groups_file)
  
  codes = fgs$Code[match(groups, fgs$LongName)]
  
  return(codes)
}

# these are hard-coded guilds (i.e. categories) of groups
get_group_guild = function(group_codes)
{
  # question, should DEP be infauna, epibenthos, or something else?
  guild = c("BB"  = "Recycl. / Primary prod.", "BIV" = "Filter feeder", "BSS" = "Fish - demersal", "CEP" = "Squid", 
            "CET" = "Mammal / bird", "CLU" = "Fish - pelagic", "COD" = "Fish - demersal", "CRA" = "Epibenthos", 
            "DAB" = "Fish - demersal", "DEP" = "Epibenthos", "DET" = "Recycl. / Primary prod.", "DL" = "Recycl. / Primary prod.", 
            "DR" = "Recycl. / Primary prod.", "ECH" = "Epibenthos", "GAD" = "Fish - demersal", "GUX" = "Fish - demersal",
            "LBE" = "Epibenthos", "LBT" = "Fish - demersal", "MAC" = "Fish - pelagic", "MUL" = "Fish - demersal", 
            "OFF" = "Fish - demersal", "PB" = "Recycl. / Primary prod.",  "PLE" = "Fish - demersal", "POL" = "Fish - demersal", 
            "PP" = "Recycl. / Primary prod.", "RAY" = "Shark / ray", "SB" = "Mammal / bird", "SCE" = "Filter feeder", 
            "SHK" = "Shark / ray", "SHP" = "Epibenthos", "SMD" = "Fish - demersal", "SOL" = "Fish - demersal", 
            "SPA" = "Fish - demersal", "SUS" = "Filter feeder", "SXX" = "Mammal / bird", "WHE" = "Epibenthos", 
            "WHG" = "Fish - demersal", "ZOC" = "Zooplankton", "ZOG" = "Zooplankton", "ZOO" = "Zooplankton")
  
 idxs = match(group_codes, names(guild))
 return(guild[idxs])
}
