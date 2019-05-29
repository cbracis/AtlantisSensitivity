# install packages to datarmor home directory and verifies packages can be loaded
# need to update libPaths with local library so that dependancies can be searched there as well
lib_dir = "r-package/library"
.libPaths( c(.libPaths(), lib_dir) )


# dplyr and dependancies
install.packages("r-package/source/assertthat_0.2.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/glue_1.3.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/magrittr_1.5.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/pkgconfig_2.0.2.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/R6_2.4.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/Rcpp_1.0.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/rlang_0.3.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/purrr_0.3.2.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/tidyselect_0.2.5.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/crayon_1.3.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/cli_1.1.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/fansi_0.4.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/utf8_1.1.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/backports_1.1.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/digest_0.6.19.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/zeallot_0.1.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/vctrs_0.1.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/pillar_1.4.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/tibble_2.1.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/BH_1.69.0-1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/plogr_0.2.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/dplyr_0.8.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)

# docopt for command line args
install.packages("r-package/source/docopt_0.6.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)

library(dplyr, lib.loc = lib_dir)
library(docopt, lib.loc = lib_dir)

# and now for the packages atlantis tools needs :o
# note: RNetCDF is build from github due to issue https://github.com/mjwoods/RNetCDF/issues/49
# note: atlantistools is build from the fork in cbracis's github that fix some issues and remove circlize dependancy
# which reqires a more recent R version than on Datarmor
install.packages("r-package/source/shape_1.4.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/colorspace_1.4-1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/curl_3.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/gtable_0.3.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/lazyeval_0.2.2.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/plyr_1.8.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/stringi_1.4.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/stringr_1.4.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/reshape2_1.4.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/viridisLite_0.3.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/labeling_0.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/munsell_0.5.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/RColorBrewer_1.1-2.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/scales_1.0.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/withr_2.1.2.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/ggplot2_3.1.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/gridExtra_2.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/proj4_1.0-8.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/RNetCDF_2.0-1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/sys_3.2.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/askpass_1.1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/openssl_1.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/mime_0.6.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/jsonlite_1.6.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/selectr_0.4-1.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/httr_1.4.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/xml2_1.2.0.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/rvest_0.3.4.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/tidyr_0.8.3.tar.gz", repos = NULL, type = "source", lib = lib_dir)
install.packages("r-package/source/atlantistools_0.4.3.9000.tar.gz", repos = NULL, type = "source", lib = lib_dir)

library(atlantistools, lib.loc = lib_dir)



