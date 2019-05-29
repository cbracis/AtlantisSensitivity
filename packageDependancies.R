# find pacakges that need to be installed on datarmor
library(tools)

# base packages that are already there
i = installed.packages() 
base = i[ i[,"Priority"] %in% c("base","recommended"), c("Package", "Priority")] 
base

# dependancies of packages used
dplyr_dep = tools::package_dependencies(packages = "dplyr", which = c("Depends", "Imports"), recursive = TRUE)
setdiff(dplyr_dep$dplyr, base[,1])

tools::package_dependencies(packages = "docopt", which = c("Depends", "Imports"), recursive = TRUE)

# dependancies for packages imported by atlantistools
# circlize,curl,dplyr,ggplot2,gridExtra,lazyeval,magrittr,proj4,purrr,RColorBrewer,RNetCDF,rvest,scales,stringr,tibble,tidyr,xml2

setdiff(unlist(package_dependencies(packages = "circlize", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "curl", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "ggplot2", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "gridExtra", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "lazyeval", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "proj4", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "RColorBrewer", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "RNetCDF", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "rvest", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "scales", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "stringr", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "tidyr", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])
setdiff(unlist(package_dependencies(packages = "xml2", which = c("Depends", "Imports"), recursive = TRUE)), base[,1])



