require(atlantistools) # note requires updated version of package (not CRAN version)
require(dplyr)

# gets inital conditions from the init.nc file
# atlantistools has a function convert_relative_initial() which divides by the initial value, but this only works
# for age-structured groups as biomass pools have a value of 0 at time=0 for some reason
# note! can't combine age stuctures and pool/stanza groups, plus they will have different variables anyway
get_initial_values = function(initFile, functionalGroupsFile, variable, groups, aggregateAge = FALSE, bboxes, bps)
{
  # determine number of cohorts to pull info from init
  fgs_data = load_fgs(functionalGroupsFile)
  groups_numcohort = fgs_data$NumCohorts[fgs_data$Code %in% groups]
  groups_name = fgs_data$Name[fgs_data$Code %in% groups]
  
  # results
  rNonage = NULL
  rStanza = NULL
  rAge = NULL
  
  # need to aggregate over polygon and layer, also agecl?
  aggGroups = if(aggregateAge) { "species" } else { c("species", "agecl") }
  
  
  # age structured groups
  if (any(groups_numcohort > 2 ))
  {
    rAge = load_init_age(initFile, functionalGroupsFile, variable, groups_name[groups_numcohort > 2], bboxes)
    rAge = agg_data(rAge, groups = aggGroups, fun = sum)
    
  }
  
  # stanza (juvenile/adult) groups
  if (any( groups_numcohort == 2 ))
  {
    rStanza = load_init_stanza(initFile, functionalGroupsFile, variable, groups_name[groups_numcohort == 2], bboxes)
    rStanza = agg_data(rStanza, groups = aggGroups, fun = sum)
  }
  
  # biopool groups
  if (any( groups_numcohort == 1 ))
  {
    rNonage = load_init_nonage(initFile, functionalGroupsFile, variable, groups_name[groups_numcohort == 1], bboxes, bps)
    rNonage = agg_data(rNonage, groups = "species", fun = sum)
    
    if (!is.null(rStanza) & "agecl" %in% names(rStanza))
    {
      rNonage$agecl = NA #so it can be combined with the rStanza later
    }
  }
  
  
  result = rbind(rNonage, rStanza, rAge)
  result = left_join(result, select(fgs_data, Code, Name, LongName), by = c("species" = "LongName"))
  
  return(result)
  
}
