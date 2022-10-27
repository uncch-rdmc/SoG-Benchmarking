library(tidyverse)
#library(plyr)
#library(dplyr)
library(tidyquant)
library(plotly)
library(viridis)
library(leaflet)
library(DT)
library(scales)
library(rjson)
library(Rmisc)
#bd_data<-read_rds(file="./Benchmarking_2_0_Data_Long_Form.rds")
bd_data <-read_rds(file="df_combined_uc.rds")
m_tbl  <- bd_data %>% distinct(Municipality)
m_list <- bd_data %>% distinct(Municipality) %>% pull() 
s_list <- bd_data %>% distinct(Service) %>% filter(Service !="Census") %>% pull()
s_all_list <- bd_data %>% distinct(Service) %>% pull()
y_list <- bd_data %>% distinct(Year) %>% pull() 
citylabel <-c("Apex", "Chapel Hill",   "Charlotte", "Concord", "Goldsboro", "Greensboro", "Hickory", "Raleigh", "Wilson", "Winston-Salem")
rvllabel <- c("Chapel Hill",   "Charlotte", "Concord", "Goldsboro", "Greensboro", "Hickory", "Raleigh", "Wilson", "Winston-Salem")
rvllabellist <- setNames(as.list(rvllabel), rvllabel)

v_list <- bd_data %>% filter(Municipality=="Apex") %>% group_by(Service) %>% distinct(Variable)

#s1numlist   <- bd_data %>% filter(Municipality=="Apex") %>% group_by(Service) %>% distinct(Variable) %>% filter(Service == s_list[1] ) %>% pull()
#s1denomlist <- bd_data %>% filter(Municipality=="Apex") %>% group_by(Service) %>% distinct(Variable) %>% filter(Service == s_list[1] | Service=="Census") %>% pull()
#s1numlist   <- v_list %>% filter(Service == s_list[1] ) %>% pull()
#s1denomlist <- v_list %>% filter(Service == s_list[1] | Service=="Census") %>% pull()

service2var <- list()
for (srv in s_all_list) {
  service2var[[srv]]  <-bd_data  %>% group_by(Service) %>% distinct(Variable) %>% filter(Service == srv) %>% pull()
}


censusVars <- bd_data  %>% filter(Service=="Census") %>% distinct(Variable) %>% pull()
# label data files
all_service_abbrev_to_full.rds<-read_rds(file = "all_service_abbrev_to_full.rds")

all_varNameToLabel.rds<-read_rds(file = "all_varNameToLabel.rds")