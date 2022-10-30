#library(tidyverse)
library(plyr)
library(dplyr)
library(tidyquant)
library(plotly)
library(viridis)
library(leaflet)
library(DT)
library(scales)
library(rjson)
library(Rmisc)
library(stringr)
#bd_data<-read_rds(file="./Benchmarking_2_0_Data_Long_Form.rds")
# raw dataset
# bd_data <-read_rds(file="df_combined_uc.rds")
# completed dataset
bd_data <-read_rds(file="bd_data_completed.rds")

# label data files
all_service_abbrev_to_full<-read_rds(file = "all_service_abbrev_to_full.rds")

all_varNameToLabel<-read_rds(file = "all_varNameToLabel.rds")




m_tbl  <- bd_data %>% distinct(Municipality)
m_list <- bd_data %>% distinct(Municipality) %>% pull() 

# to be replaced with a named list below: srvclngToShrtRefLst
s_list <- bd_data %>% distinct(Service) %>% filter(Service !="Census") %>% pull()

# to be repaced with a named list 
s_all_list <- bd_data %>% distinct(Service) %>% pull()



y_list <- bd_data %>% distinct(Year) %>% pull() %>% as.character()


# all M names vector
citylabel <-c("Apex", "Chapel Hill", "Charlotte", "Concord", "Goldsboro", 
              "Greensboro", "Hickory", "Raleigh", "Wilson", "Winston-Salem")

# Apex's peergroup vector
rvllabel <- c("Chapel Hill",   "Charlotte", "Concord", "Goldsboro", 
              "Greensboro", "Hickory", "Raleigh", "Wilson", "Winston-Salem")

rvllabellist <- setNames(as.list(rvllabel), rvllabel)

v_list <- bd_data %>% 
  filter(Municipality=="Apex") %>% 
  group_by(Service) %>% 
  distinct(Variable)

#s1numlist   <- bd_data %>% filter(Municipality=="Apex") %>% group_by(Service) %>% distinct(Variable) %>% filter(Service == s_list[1] ) %>% pull()
#s1denomlist <- bd_data %>% filter(Municipality=="Apex") %>% group_by(Service) %>% distinct(Variable) %>% filter(Service == s_list[1] | Service=="Census") %>% pull()
#s1numlist   <- v_list %>% filter(Service == s_list[1] ) %>% pull()
#s1denomlist <- v_list %>% filter(Service == s_list[1] | Service=="Census") %>% pull()

service2var <- list()
valueLst<-list()
for (srv in s_all_list) {
  service2var[[srv]]  <-bd_data  %>% 
    group_by(Service) %>% 
    distinct(Variable) %>% 
    filter(Service == srv) %>% pull()
}

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

censusVars <- bd_data  %>% 
  filter(Service=="Census") %>% 
  distinct(Variable) %>% pull()



# 2022-10-27 
# long name to acronym list for selectInput
# replaces the above s_list
srvclngToShrtRefLst <-list()
for (i in 1:dim(all_service_abbrev_to_full)[1]) {
  
  srvclngToShrtRefLst[[all_service_abbrev_to_full$Full[i]]] <- all_service_abbrev_to_full$lc[i]
}
# without census
srvclngToShrtRefLstWoc <-list()
for (i in 1:dim(all_service_abbrev_to_full)[1]) {
  if (all_service_abbrev_to_full$lc[[i]]=="Census"){
    next
  }
  srvclngToShrtRefLstWoc[[all_service_abbrev_to_full$Full[i]]] <- all_service_abbrev_to_full$lc[i]
}


# reference table: abbrev to each row
# srvcRefTable <- list()
# for (i in 1:dim(all_service_abbrev_to_full)[1]) {
#   srvcRefTable[[all_service_abbrev_to_full$lc[i]]] <- all_service_abbrev_to_full[i, ]
# }

# initial selections

initialNumeratorValue <- list("Lane miles"="qamr01")
initialDenominatorValue<-list("Population, 2020"="census_01")


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