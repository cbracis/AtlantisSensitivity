# some functions for plotting potential parameter values, i.e. the fit C and mum

# plot C from long form df, Value and Cpred
# paramIncDec - how much to increase/decrease a by to get levels for sensitivity analysis 
# such as c(-0.8, -0.2, 0.2, 0.8) means -80%, -20%, +20%, +80%
plot_C_for_age_groups_levels = function(df, paramIncDec)
{
  cols = factor(as.character(paramIncDec), ordered = TRUE)
  paramIncDec = 1 + paramIncDec # can now be multiplies directly by Cpred value
  
  theplot = ggplot(data = df, aes(x = Age, y = Value)) +
    ylab("C") +
    geom_line() + geom_point() + 
    geom_line(aes(x = Age, y = Cpred), col = "purple") +
    geom_line(aes(y = paramIncDec[1] * Cpred, color = cols[1])) +
    geom_line(aes(y = paramIncDec[2] * Cpred, color = cols[2])) +
    geom_line(aes(y = paramIncDec[3] * Cpred, color = cols[3])) +
    geom_line(aes(y = paramIncDec[4] * Cpred, color = cols[4])) +
    scale_colour_manual(name="Param level", values = c("darkblue", "blue", "blue", "darkblue")) +
    theme_classic() +
   # theme(legend.position = c(0.3, 0.05))
    facet_wrap(~ longName, scales = "free")
  return(theplot)
}

plot_mum_for_age_groups = function(df, CmultFactor, paramIncDec)
{
  cols = factor(as.character(paramIncDec), ordered = TRUE)
  paramIncDec = 1 + paramIncDec # can now be multiplies directly by Cpred value
  
  theplot = ggplot(data = df, aes(x = Age, y = CmultFactor * Value)) +
    ylab("mum") +
    geom_line() + geom_point() + 
    geom_line(aes(x = Age, y = Cpred), col = "purple") +
    geom_line(aes(y = paramIncDec[1] * CmultFactor * Cpred, color = cols[1])) +
    geom_line(aes(y = paramIncDec[2] * CmultFactor * Cpred, color = cols[2])) +
    geom_line(aes(y = paramIncDec[3] * CmultFactor * Cpred, color = cols[3])) +
    geom_line(aes(y = paramIncDec[4] * CmultFactor * Cpred, color = cols[4])) +
    scale_colour_manual(name="Param level", values = c("darkblue", "blue", "blue", "darkblue")) +
    theme_classic() +
    # theme(legend.position = c(0.3, 0.05))
    facet_wrap(~ longName, scales = "free")
  return(theplot)
}