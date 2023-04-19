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
# library(sysfonts)
#library(gfonts)
library(showtext)



# -----------------------------------------------------------------------------
# read back the saved benchmarking data files
# -----------------------------------------------------------------------------

# completed benchmarking data file
bd_data <-readr::read_rds(file="bd_data_completed5.rds")

# lookup-table file
all_service_abbrev_to_full<-readr::read_rds(file = "all_service_abbrev_to_full.rds")

# variable-name-to-label data files
all_varNameToLabel<-readr::read_rds(file = "all_varNameToLabel5.rds")

# -----------------------------------------------------------------------------
# creating static objects 
# -----------------------------------------------------------------------------
# Coding Note:
# creating various static files that depends on the contents of the latest
# benchmarking data files; 
# While some of these objects can be pre-processed and hard-coded here,
# for some cases, the ease of maintenance rather than performance gains
# was chosen.

# years
y_list <- bd_data %>% dplyr::distinct(Year) %>% dplyr::pull() %>% as.character()


# all participating municipalities names vector
citylabel <-c("Apex", "Chapel Hill", "Charlotte", "Concord", "Goldsboro", 
        "Greensboro", "Hickory", "Raleigh", "Wilson", "Winston-Salem")

# Apex's peer-group vector as the initial choice set
rvllabel <- c("Chapel Hill",   "Charlotte", "Concord", "Goldsboro", 
        "Greensboro", "Hickory", "Raleigh", "Wilson", "Winston-Salem")



# usage: srv2varlbllst[["amr"]] returns its named variable list
# where name is their label

# all vars named list
v2lallinOne <-list()
for (row in 1:nrow(all_varNameToLabel)) {
  valueN <- all_varNameToLabel[row, "var_name"]
  valueL <- all_varNameToLabel[row, "var_label"]
  vl <- stats::setNames(as.list(valueL), valueN)
  v2lallinOne <- append(v2lallinOne, vl)
}
# usage: v2lallinOne[["qamr01"]]




# The update of 2022-10-27 
# long name to acronym list for selectInput
# replaces the above s_list
srvclngToShrtRefLst <-list()
for (i in 1:dim(all_service_abbrev_to_full)[1]) {
  
  srvclngToShrtRefLst[[all_service_abbrev_to_full$Full[i]]] <- all_service_abbrev_to_full$lc[i]
}
# without census
srvclngToShrtRefLstWoc <-list()
for (i in 1:dim(all_service_abbrev_to_full)[1]) {
  if (all_service_abbrev_to_full$lc[[i]]=="census"){
    next
  }
  srvclngToShrtRefLstWoc[[all_service_abbrev_to_full$Full[i]]] <- all_service_abbrev_to_full$lc[i]
}


# replacement of the above service2var
srv2varlbllst <-list()
for (srv in srvclngToShrtRefLst){
  # value is short form such as amr
  tmp <- all_varNameToLabel %>%
    dplyr::filter(var_acr==srv) %>% dplyr::select(var_name, var_label)
  name_vec  <-tmp %>% dplyr::pull(var_label)
  # print(name_vec)
  value_vec <- tmp %>% dplyr::pull(var_name) 
  # print(value_vec)
  valueLst<-stats::setNames(as.list(value_vec), name_vec)
  srv2varlbllst[[srv]]  <- valueLst
}
###############################################################################
# data-manipulation functions
###############################################################################
get_numerator_data <- function(dt, selectedService, 
                               selectedVar4num, 
                               group,
                               selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>
    filter(Variable==selectedVar4num)   |>
    filter(Municipality %in% group) |> 
    arrange(Municipality, Year) |>
    spread(key=Year, value=Value)       |>
    select(selectedYears) |> 
    as.matrix()
  
  
}

get_bd_matrix_data <- function(dt, selectedService, 
                               selectedVar4num, 
                               group,
                               selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>
    filter(Variable==selectedVar4num)   |>
    filter(Municipality %in% group) |> 
    arrange(Municipality, Year) |>
    spread(key=Year, value=Value)       |>
    select(selectedYears) |> 
    as.matrix()
  
  
}


# -----------------------------------------------------------------------------
# Graph-rendering-related settings 
# -----------------------------------------------------------------------------
# 
# custom palettes
# This is a static approach, i.e., colors are pre-assigned to all participating 
# cities and these city-color pairs are fixed 
# For more realistic settings, the palettes must be dynamically subset
pairedPalette <- RColorBrewer::brewer.pal(n=length(citylabel), name="Paired")
# print("pairedPalette=")
# print(pairedPalette)
lvlcl <- levels(factor(citylabel, ordered = T))
# print("levelsCityLabel=")
# print(lvlcl)
names(pairedPalette) <- lvlcl
# print("pairedPalette=")
# print(pairedPalette)
shapeNoList <- seq(1:length(citylabel))
names(shapeNoList)  <- lvlcl

# for bar/column plot
fixed_f_scale <- ggplot2::scale_fill_manual(name="Legend", values=pairedPalette)
# for line and point
fixed_c_scale <- ggplot2::scale_color_manual(name="Legend", values = pairedPalette)
# for shapes
fixed_s_scale <- ggplot2::scale_shape_manual(name="Legend", values = shapeNoList)




# default page layout
paperWidth <- 11
paerHeight <- 8.5


# -----------------------------------------------------------------------------
# custom Google-font setting
# -----------------------------------------------------------------------------
# The following setting is expected to download font files from a remote site
# sysfonts::font_add_google(name = "Barlow Semi Condensed",family =  "barlow")

# This setting loads locally saved ttf files in www/fonts 
sysfonts::font_add(family = "barlow", 
        regular = "www/fonts/barlow-semi-condensed-v14-latin-regular.ttf", 
        italic = "www/fonts/barlow-semi-condensed-v14-latin-italic.ttf")

# the above setting was supposed to be replaced with a new css setting with 
# gfonts::includeCSS() in app.R; see the manual of an R package, gfonts, esp.,
# setup_font() function; however, this approach did not work with ggplot2 
# for some # unknown reason (naming?) and was abandoned 
