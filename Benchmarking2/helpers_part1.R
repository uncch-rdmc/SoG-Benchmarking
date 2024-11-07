# Overall structure
# common IO block
#
# others blokc
# bi block
# wsww block

# library calls

library(fst)
library(tibble)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyquant)
library(ggplot2)
library(scales)
#library(rjson)
library(Rmisc)
library(stringr)
library(RColorBrewer)
library(sysfonts)
#library(gfonts)
library(showtext)
library(ggpubr)



dataUpdatedDayStamp <- "November 1, 2024"




# unlike the previous all-in-one version
# 3 service groups load their own data and 
# prepare their data/hash objects
# 
# 
#
# =================================================================

# common block
# constant regardless of a given service group
maxPeerSelection4I <- 5

# data-file loading 

# make the others case default so that references outside the conditional block
# read back others' serialized files
# for their generation steps, 
# see `~/Documents/experiments/visualization/benchmarking/data/2024-08-06/dryrun/bd_data_update_fy2025_test_w_2024-09-18-data.R`

# 1 data file

# 6 lookup tables/lists -------------------------------------------------------
# all_varNameToLabel: df: name, acr, order, label, def, all in one
# all_vname2def: list:metric name => its definition
# v2lallinOne: list: metric name => its label
# srvclngToShrtRefLst: list: full service name to its acronym
# srvclngToShrtRefLstWoc: list: ditto without census
# srv2varlbllst: list: service-acronym => its member-metric-label list 


# others case------------------------------------------------------------------
# 
# data themselves


# bd_data_completed6.rds => bd_data_completed7_others.rds

bd_data_others <- readr::read_rds(file="bd_data_completed7_others.rds")


# lookup tables/lists

# all_varNameToLabel6.rds => all_varNameToLabel7_others.rds

all_varNameToLabel_others <- readr::read_rds(file = "all_varNameToLabel7_others.rds") # 


# all_vname2def.rds => all_vname2def_others.rds
all_vname2def_others <- readr::read_rds(file="all_vname2def_others.rds") 

# v2lallinOne.rds => v2lallinOne_others.rds
v2lallinOne_others <- readr::read_rds(file = "v2lallinOne_others.rds")

#  srvclngToShrtRefLst.rds => srvclngToShrtRefLst_others.rds
srvclngToShrtRefLst_others <- readr::read_rds(file = "srvclngToShrtRefLst_others.rds")

# srvclngToShrtRefLstWoc.rds => srvclngToShrtRefLstWoc_others.rds
srvclngToShrtRefLstWoc_others <- readr::read_rds(file = "srvclngToShrtRefLstWoc_others.rds")

# srv2varlbllst.rds => srv2varlbllst_others.rds
srv2varlbllst_others <- readr::read_rds(file = "srv2varlbllst_others.rds") 

# internal objects to be generated on the fly
## List of fiscal years
y_list_others <- bd_data_others |>
  dplyr::distinct(Year) |>
  dplyr::pull() |>
  as.character()

# list of municipalities
citylabel_others <-
  bd_data_others |>
  dplyr::select(Municipality) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  as.vector()

# default selected service
default_selected_service_others <-"am"

varset4numerator_others <- c(srv2varlbllst_others[[default_selected_service_others]])
varset4numerator_others <- varset4numerator_others[order(names(varset4numerator_others))]



varset4denominator_others <- varset4numerator_others[-c(1)]






# -----------------------------------------------------------------------------
# bi case
# 
# data themselves


# bd_data_completed6.rds => bd_data_completed7_bi.rds

bd_data_bi <- readr::read_rds(file="bd_data_completed7_bi.rds")


# lookup tables/lists

# all_varNameToLabel6.rds => all_varNameToLabel7_bi.rds

all_varNameToLabel_bi <- readr::read_rds(file = "all_varNameToLabel7_bi.rds") # 


# all_vname2def.rds => all_vname2def_bi.rds
all_vname2def_bi <- readr::read_rds(file="all_vname2def_bi.rds") 

# v2lallinOne.rds => v2lallinOne_bi.rds
v2lallinOne_bi <- readr::read_rds(file = "v2lallinOne_bi.rds")

#  srvclngToShrtRefLst.rds => srvclngToShrtRefLst_bi.rds
srvclngToShrtRefLst_bi <- readr::read_rds(file = "srvclngToShrtRefLst_bi.rds")

# srvclngToShrtRefLstWoc.rds => srvclngToShrtRefLstWoc_bi.rds
srvclngToShrtRefLstWoc_bi <- readr::read_rds(file = "srvclngToShrtRefLstWoc_bi.rds")

# srv2varlbllst.rds => srv2varlbllst_bi.rds
srv2varlbllst_bi <- readr::read_rds(file = "srv2varlbllst_bi.rds") 

# internal objects to be generated on the fly
## List of fiscal years
y_list_bi <- bd_data_bi |>
  dplyr::distinct(Year) |>
  dplyr::pull() |>
  as.character()

# list of municipalities
citylabel_bi <-
  bd_data_bi |>
  dplyr::select(Municipality) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  as.vector()

# default selected service
default_selected_service_bi <-"bi"


varset4numerator_bi <- c(srv2varlbllst_bi[[default_selected_service_bi]])
varset4numerator_bi <- varset4numerator_bi[order(names(varset4numerator_bi))]

varset4denominator_bi <- varset4numerator_bi[-c(1)]

# -----------------------------------------------------------------------------
# wsww case 
# 
# 
# data themselves


# bd_data_completed6.rds => bd_data_completed7_wsww.rds

bd_data_wsww <- readr::read_rds(file="bd_data_completed7_wsww.rds")


# lookup tables/lists

# all_varNameToLabel6.rds => all_varNameToLabel7_wsww.rds

all_varNameToLabel_wsww <- readr::read_rds(file = "all_varNameToLabel7_wsww.rds") # 


# all_vname2def.rds => all_vname2def_wsww.rds
all_vname2def_wsww <- readr::read_rds(file="all_vname2def_wsww.rds") 

# v2lallinOne.rds => v2lallinOne_wsww.rds
v2lallinOne_wsww <- readr::read_rds(file = "v2lallinOne_wsww.rds")

#  srvclngToShrtRefLst.rds => srvclngToShrtRefLst_wsww.rds
srvclngToShrtRefLst_wsww <- readr::read_rds(file = "srvclngToShrtRefLst_wsww.rds")

# srvclngToShrtRefLstWoc.rds => srvclngToShrtRefLstWoc_wsww.rds
srvclngToShrtRefLstWoc_wsww <- readr::read_rds(file = "srvclngToShrtRefLstWoc_wsww.rds")

# srv2varlbllst.rds => srv2varlbllst_wsww.rds
srv2varlbllst_wsww <- readr::read_rds(file = "srv2varlbllst_wsww.rds") 

# internal objects to be generated on the fly
## List of fiscal years
y_list_wsww <- bd_data_wsww |>
  dplyr::distinct(Year) |>
  dplyr::pull() |>
  as.character()

# list of municipalities
citylabel_wsww <-
  bd_data_wsww |>
  dplyr::select(Municipality) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  as.vector()


# default selected service
default_selected_service_wsww <-"ww"

varset4numerator_wsww <- c(srv2varlbllst_wsww[[default_selected_service_wsww]])
varset4numerator_wsww <- varset4numerator_wsww[order(names(varset4numerator_wsww))]


varset4denominator_wsww <- varset4numerator_wsww[-c(1)]
#-----------------------------------------------------------------------------

starting_default <-"others"
if (is.null(starting_default) || is.na(starting_default)){
  starting_default <-"others"
}
# set the startup default -----------------------------------------------------


if (starting_default == "others") {
  print("= helpers_part1: others is loaded  ==================================================")
  bd_data <- bd_data_others
  
  all_varNameToLabel <- all_varNameToLabel_others
  
  all_vname2def <- all_vname2def_others
  
  y_list <- y_list_others
  
  v2lallinOne <- v2lallinOne_others
  
  srvclngToShrtRefLst <- srvclngToShrtRefLst_others
  
  srvclngToShrtRefLstWoc <- srvclngToShrtRefLstWoc_others
  
  srv2varlbllst <- srv2varlbllst_others
  
  default_selected_service <- default_selected_service_others
  
  citylabel <- citylabel_others
  
  sizeOfMuniucipalities <- length(citylabel)
  
  rvllabel <- citylabel[-c(1)]
  
  varset4numerator <- varset4numerator_others
  
  
  varset4denominator <- varset4numerator_others
  
} else if (starting_default == "bi") {
  # bi
  print("= helpers_part1: bi is loaded  ==================================================")
  bd_data <- bd_data_bi
  
  all_varNameToLabel <- all_varNameToLabel_bi
  
  all_vname2def <- all_vname2def_bi
  
  y_list <- y_list_bi
  
  v2lallinOne <- v2lallinOne_bi
  
  srvclngToShrtRefLst <- srvclngToShrtRefLst_bi
  
  srvclngToShrtRefLstWoc <- srvclngToShrtRefLstWoc_bi
  
  srv2varlbllst <- srv2varlbllst_bi
  
  default_selected_service <- default_selected_service_bi
  
  citylabel <- citylabel_bi
  
  sizeOfMuniucipalities <- length(citylabel)
  
  rvllabel <- citylabel[-c(1)]
  
  varset4numerator <- varset4numerator_bi
  
  varset4denominator <- varset4numerator_bi
  
} else if (starting_default == "wsww") {
  print("wsww is called")
  # wsww
  print("= helpers_part1: wsww is loaded  ==================================================")
  bd_data <- bd_data_wsww
  
  all_varNameToLabel <- all_varNameToLabel_wsww
  
  all_vname2def <- all_vname2def_wsww
  
  y_list <- y_list_wsww
  
  v2lallinOne <- v2lallinOne_wsww
  
  srvclngToShrtRefLst <- srvclngToShrtRefLst_wsww
  
  srvclngToShrtRefLstWoc <- srvclngToShrtRefLstWoc_wsww
  
  srv2varlbllst <- srv2varlbllst_wsww
  
  default_selected_service <- default_selected_service_wsww
  
  citylabel <- citylabel_wsww
  
  sizeOfMuniucipalities <- length(citylabel)
  
  rvllabel <- citylabel[-c(1)]
  
  varset4numerator <- varset4numerator_wsww
  
  varset4denominator <- varset4numerator_wsww
  
} else {
  
  bd_data <- bd_data_others
  
  all_varNameToLabel <- all_varNameToLabel_others
  
  all_vname2def <- all_vname2def_others
  
  y_list <- y_list_others
  
  v2lallinOne <- v2lallinOne_others
  
  srvclngToShrtRefLst <- srvclngToShrtRefLst_others
  
  srvclngToShrtRefLstWoc <- srvclngToShrtRefLstWoc_others
  
  srv2varlbllst <- srv2varlbllst_others
  
  default_selected_service <- default_selected_service_others
  
  citylabel <- citylabel_others
  
  sizeOfMuniucipalities <- length(citylabel)
  
  rvllabel <- citylabel[-c(1)]
  
  varset4numerator <- varset4numerator_others
  
  varset4denominator <- varset4numerator_others
}

welcomeToken <-list("bi"="Building Inspection", "wsww"="Water&#47;Wastewater services", "others"="Other Services")
