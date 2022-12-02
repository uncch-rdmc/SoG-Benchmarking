library(tibble)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyquant)
library(scales)
#library(rjson)
library(Rmisc)
library(stringr)
library(RColorBrewer)
library(showtext)



# -----------------------------------------------------------------------------
# read back the saved benchmarking data files
# -----------------------------------------------------------------------------

# completed benchmarking data file
bd_data <-read_rds(file="bd_data_completed5.rds")

# lookup-table file
all_service_abbrev_to_full<-read_rds(file = "all_service_abbrev_to_full.rds")

# variable-name-to-label data files
all_varNameToLabel<-read_rds(file = "all_varNameToLabel5.rds")

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
y_list <- bd_data %>% distinct(Year) %>% pull() %>% as.character()


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
  vl <- setNames(as.list(valueL), valueN)
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
    filter(var_acr==srv) %>% select(var_name, var_label)
  name_vec  <-tmp %>% pull(var_label)
  # print(name_vec)
  value_vec <- tmp %>% pull(var_name) 
  # print(value_vec)
  valueLst<-setNames(as.list(value_vec), name_vec)
  srv2varlbllst[[srv]]  <- valueLst
}

# -----------------------------------------------------------------------------
# Graph-rendering-related settings 
# -----------------------------------------------------------------------------
# 
# custom palettes
# This is a static approach, i.e., colors are pre-assigned to all participating 
# cities and these city-color pairs are fixed 
# For more realistic settings, the palettes must be dynamically subset
pairedPalette <- brewer.pal(n=length(citylabel), name="Paired")
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
fixed_f_scale <- scale_fill_manual(name="Legend", values=pairedPalette)
# for line and point
fixed_c_scale <- scale_color_manual(name="Legend", values = pairedPalette)
# for shapes
fixed_s_scale <- scale_shape_manual(name="Legend", values = shapeNoList)


# -----------------------------------------------------------------------------
# custom Google-font setting
# -----------------------------------------------------------------------------
#sysfonts::font_add_google(name = "Barlow Semi Condensed",family =  "barlow")
#
# the above setting was replaced with a new css setting, includeCSS(), 
# in the tags$head of app.R; see the manual of an R package, gfonts, esp.,
# setup_font() function
# 
# 