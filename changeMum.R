source("utilityFuncs.R")

# get mum as function of C, TODO generalize from one param to another
getMumAsFunctionofC = function(param_df, multFactor)
{
  # because of how we store multi-age params, the column of values will be character
  
  # start with just as.numeric, this will convert pool groups
  param_df$mum = multFactor * as.numeric(param_df$C) #TODO way to ignore warning about NAs?
  
  # now find, extract, multiply, and recombine age values
  idxsAge = which(is.na(param_df$mum))
  Cvals = extractupdateAgeParamVals(param_df, "C", idxsAge, multFactor)
  param_df$mum[idxsAge] = Cvals
  return(param_df)
}

multAgeParameter = function(param_df, param, groups, multFactor)
{
  idxs = which(param_df$groupType == GROUPS_AGE & param_df$Code %in% groups )
  colIdx = which(names(param_df) == param)
  param_df[idxs, colIdx] = extractupdateAgeParamVals(param_df, param, idxs, multFactor)
  return(param_df)
}

extractupdateAgeParamVals = function(param_df, param, rowIdxs, multFactor)
{
  colIdx = which(names(param_df) == param)
  vals = getMatrixFromAgeParamVec(param_df[rowIdxs, colIdx, drop = TRUE])
  vals = multFactor * vals
  vals = apply(vals, 1, function(x) paste(x, collapse = " "))
  return(vals)
}