source("getInitialValues.R")
source("getGroups.R")

library(plyr)
library(dplyr)
library(tidyr)

# function to fit function to calculate C from init biomass, 
# based on exisiting calibration of C
fit_C_from_init_biomass = function(initFile, biolFile, functionalGroupsFile, bgmFile, 
                                   functionType = c("exp", "growth"), useWeights = FALSE, returnAllInfo = FALSE)
{
  bps = load_bps(functionalGroupsFile, initFile)
  bboxes = get_boundary(load_box(bgmFile)) # ids of bounding boxes
  
  groupsAge = get_age_group_codes(functionalGroupsFile) 
  
  resN = get_initial_values(initFile, functionalGroupsFile, "ResN", groupsAge, FALSE, bboxes, bps)
  structN = get_initial_values(initFile, functionalGroupsFile, "StructN", groupsAge, FALSE, bboxes, bps)
  totN = resN
  totN$atoutput = resN$atoutput + structN$atoutput
  
  groups_C = get_C_for_groups(biolFile, functionalGroupsFile, groupsAge)
  groups_C = right_join(groups_C, totN, by = c("Code" = "Code", "Age" = "agecl"))
  groups_C = groups_C %>% select(-species) %>% dplyr::rename(totN = atoutput) 
  
  if (functionType == "exp")
  {
    # exp function
    groups_C = fit_Cfunc_exp_to_groups(groups_C, useWeights)
    show(plot_C_for_age_groups(groups_C))
    
    # now need to group by species to combine Cpred values
    values_C = groups_C %>% group_by(Code) %>% dplyr::summarize(C = paste(round(Cpred, digits = 2), collapse = " "),
                                                                a = paste(unique(a), collapse = " ")) # should only be one value of a per group
    
  } else
  {
    # growth function
    recruitN = get_totN_Recruit_for_groups(biolFile, functionalGroupsFile, groupsAge)
    recruitN = recruitN  %>% dplyr::rename(y1N = Value)
    groups_C = left_join(groups_C, recruitN, by = "Code")

    groups_C = fit_Cfunc_growth_to_groups(groups_C) # TODO useWeights option
    
    
    # now need to group by species to combine Cpred values
    values_C = groups_C %>% group_by(Code) %>% dplyr::summarize(C = paste(round(Cpred, digits = 2), collapse = " "),
                                                                b = paste(unique(b), collapse = " ")) # should only be one value of a per group
  }
  
  if (returnAllInfo)
  {
    # all infor, e.g. for plotting
    return(groups_C)
  }
  else
  {
    return(values_C)
  }
}

Cfunc_exp = function(totN, a, exp = 0.7) { a * totN ^ exp }

# to calculate difference version, get input parameters KWSR (struct N) and KWRR (reserve N) for recruits
# this is size of recruits after larval stage, can use for first year C, then get next 9 from diff eq
# also might need flat option

Cfunc_growth = function(totN, y1N, b) { c(y1N[1], b * diff(totN)) }

fit_Cfunc_exp_to_groups = function(C_data, useWeights)
{
  # use do instead of mutate since fit returns dataframe not vector
  out = C_data %>% group_by(Code) %>% do({
    cbind(., fit_Cfunc_exp(.$totN, .$Value, useWeights)) # Value was C
  }) %>% ungroup()
  return(out)
}

fit_Cfunc_growth_to_groups = function(C_data)
{
  # use do instead of mutate since fit returns dataframe not vector
  out = C_data %>% group_by(Code) %>% do({
    cbind(., fit_Cfunc_growth(.$totN, .$y1N, .$Value)) # Value was C
  }) %>% ungroup()
  return(out)
}


fit_Cfunc_exp = function(totN, C, useWeights)
{
  if (useWeights)
  {
    # simply weight adult age classes higher than juvenile (assume first 2 years, correct for most sp)
    weights = c(1, 1, rep(5, length(C) - 2))
  } else
  {
    weights = rep(1, length(C))
  }
  
  out = tryCatch(
    {
      startA = mean( C / totN^0.7 )
      model = nls(C ~ Cfunc_exp(totN, a),
                  start = list(a = startA),
                  weights = weights,
                  control = nls.control(maxiter = 1000, minFactor = 1 / (2^14))) # deaults 50 and 1/2^10
      
      out = data.frame(matrix(rep(coef(model), each = length(totN)), ncol = 1))
      names(out) = names(coef(model))
      out = cbind(Cpred = predict(model, totN),  out)
    },
    error=function(message) {
      message("Failed to fit nls model")
      message(message)
      
      out = data.frame(matrix(NA, nrow = length(totN), ncol = 2))
      names(out) = c("Cpred", "a")
      return(out)
    }  
  )
  
  return(out)
}

fit_Cfunc_growth = function(totN, y1N, C)
{
  out = tryCatch(
    {
      startB = 10 # from atlantis user guide
      model = nls(C ~ Cfunc_growth(totN, y1N, b),
                  start = list(b = startB),
                  control = nls.control(maxiter = 1000, minFactor = 1 / (2^14))) # deaults 50 and 1/2^10
      
      out = data.frame(matrix(rep(coef(model), each = length(totN)), ncol = 1))
      names(out) = names(coef(model))
      out = cbind(Cpred = predict(model, totN, y1N),  out)
    },
    error=function(message) {
      message("Failed to fit nls model")
      message(message)
      
      out = data.frame(matrix(NA, nrow = length(totN), ncol = 2))
      names(out) = c("Cpred", "b")
      return(out)
    }  
  )
  
  return(out)
}

get_C_for_groups = function(prm_biol, fgs, groups, isCohort = TRUE)
{
  variables = paste("C", groups, sep = "_")
  if (!isCohort) { variables = paste(variables, "T15", sep = "_") }
  return( get_param_for_groups(prm_biol, "C", variables, fgs, groups, isCohort) )
}

get_mum_for_groups = function(prm_biol, fgs, groups, isCohort = TRUE)
{
  if (length(groups) == 0) { return(NULL) }
  variables = paste("mum", groups, sep = "_")
  if (!isCohort) { variables = paste(variables, "T15", sep = "_") }
  return( get_param_for_groups(prm_biol, "mum", variables, fgs, groups, isCohort) )
}

# structual N weight of recruits
get_KWSR_for_groups = function(prm_biol, fgs, groups)
{
  variables = paste("KWSR", groups, sep = "_")
  return( get_param_for_groups(prm_biol, "KWSR", variables, fgs, groups, FALSE) )
}

# Reserve N weight of recruits
get_KWRR_for_groups = function(prm_biol, fgs, groups)
{
  variables = paste("KWRR", groups, sep = "_")
  return( get_param_for_groups(prm_biol, "KWRR", variables, fgs, groups, FALSE) )
}

# total N weight of recruits
get_totN_Recruit_for_groups = function(prm_biol, fgs, groups)
{
  structN = get_KWSR_for_groups(prm_biol, fgs, groups)
  resN = get_KWRR_for_groups(prm_biol, fgs, groups)
  structN$Value = structN$Value + resN$Value
  return( structN )
}


get_param_for_groups = function(prm_biol, param, variableSet, fgs, groups, isCohort = TRUE)
{
  if (isCohort)
  {
    groups_C = extract_prm_cohort(prm_biol, variables = variableSet)
    groups_C = ldply(groups_C)
    names(groups_C) = c("Param", 1:(ncol(groups_C) - 1))
    
    # see Unquting names in help for quasiquotation for the !!
    groups_C = gather(groups_C, "Age", !!param, 2:ncol(groups_C), convert = TRUE)
    names(groups_C)[3] = "Value" # make so we can combine params df's together
    
  } else
  {
    # also pool with stage (e.g. CEP and PWN) also need jCEP, jPWN
    groups_C = extract_prm(prm_biol, variables = variableSet)
    groups_C = data.frame(Param = groups, C = ldply(groups_C), stringsAsFactors = FALSE)
    names(groups_C)[2] = "Value" # param
  }
  
  groups_C$Param = param
  groups_C$Code = groups
  groups_C$longName = get_group_long_names(groups, fgs)
  return(groups_C)
}

plot_C_for_age_groups = function(df)
{
  require(ggplot2)
  theplot = ggplot(data = df, aes(x = Age, y = Value)) +
    ylab("C") +
    geom_line() + geom_point() + 
    geom_line(aes(x = Age, y = Cpred), col = "blue") +
    theme_classic() +
    facet_wrap(~ longName, scales = "free")
  return(theplot)
}
