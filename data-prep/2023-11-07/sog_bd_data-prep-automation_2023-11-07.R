
# 2023-11-02 edition
# mainly copied from the edition of 2023-10-31
# differeces are data themselves not script
###############################################################################
# as of 2023-10-31: after the newly updated benchmarking data-file was received
# !!!! warning: 
# This version uses a new coding scheme that differs from last year's one
# metric-name tokens are now 2-letter rather than 2/3-letter mixed.
# 
# Sheet names
# 2023-10-31
# 
# Asphalt Maintenance and Repair
# Building Inspection
# Central Human Resources
# Emergency Communications
# Fire Service
# Fleet Maintenance
# Household Recycling
# Parks and Recreation
# Police Service
# Residential Refuse Collection
# Wastewater Service
# Water Service
# Yard Waste and Leaf Collection

# 2023-11-02
# 
# Asphalt Maintenance and Repair
# Building Inspection
# Central Human Resources
# Emergency Communications
# Fire Service
# Fleet Maintenance
# Household Recycling
# Parks and Recreation
# Police Service
# Residential Refuse Collection
# Wastewater Service
# Water Service
# Yard Waste and Leaf Collection





#------------------------------------------------------------------------------
# Part 1: Benchmark Data
#------------------------------------------------------------------------------
# step 0: set the storage directory as the current working directory, e.g., 
# setwd("~/Documents/experiments/visualization/benchmarking/2023-10-31")
# 
# Required libraries 
# library(tidyverse)
# library(readxl)
library(magrittr)

# office script to dump sheet names
# worksheet names were dumped by running the following office script ["script1"]
# 
# function main(workbook: ExcelScript.Workbook) {
#   // Your code here
#   let sheet = workbook.getActiveWorksheet();
#   let wss = workbook.getWorksheets();
#   wss.forEach((ws) => {
#     console.log(ws.getName());
#   })
# }
# 
# 
# 
# 
# -----------------------------------------------------------------------------
# step 1: read the workdbook and create service-wise objects
# -----------------------------------------------------------------------------


am_1 <- readxl::read_excel('bm_data.xlsx', sheet='Asphalt Maintenance and Repair')
bi_2 <- readxl::read_excel('bm_data.xlsx', sheet='Building Inspection')
hr_3 <- readxl::read_excel('bm_data.xlsx', sheet='Central Human Resources')
ec_4 <- readxl::read_excel('bm_data.xlsx', sheet='Emergency Communications')
fs_5 <- readxl::read_excel('bm_data.xlsx', sheet='Fire Service')
fm_6 <- readxl::read_excel('bm_data.xlsx', sheet='Fleet Maintenance')
re_7 <- readxl::read_excel('bm_data.xlsx', sheet='Household Recycling')
pr_8 <- readxl::read_excel('bm_data.xlsx', sheet='Parks and Recreation')
ps_9 <- readxl::read_excel('bm_data.xlsx', sheet='Police Service')
rr_10 <- readxl::read_excel('bm_data.xlsx', sheet='Residential Refuse Collection')
ww_11 <- readxl::read_excel('bm_data.xlsx', sheet='Wastewater Service')
ws_12 <- readxl::read_excel('bm_data.xlsx', sheet='Water Service')
yw_13 <- readxl::read_excel('bm_data.xlsx', sheet='Yard Waste and Leaf Collection')


# add service column 2023-10
am_1 <- am_1 |> dplyr::mutate(service='am')
bi_2 <- bi_2 |> dplyr::mutate(service='bi')
hr_3 <- hr_3 |> dplyr::mutate(service='hr')
ec_4 <- ec_4 |> dplyr::mutate(service='ec')
fs_5 <- fs_5 |> dplyr::mutate(service='fs')
fm_6 <- fm_6 |> dplyr::mutate(service='fm')
re_7 <- re_7 |> dplyr::mutate(service='re')
pr_8 <- pr_8 |> dplyr::mutate(service='pr')
ps_9 <- ps_9 |> dplyr::mutate(service='ps')
rr_10 <- rr_10 |> dplyr::mutate(service='rr')
ww_11 <- ww_11 |> dplyr::mutate(service='ww')
ws_12 <- ws_12 |> dplyr::mutate(service='ws')
yw_13 <- yw_13 |> dplyr::mutate(service='yw')



# step 1-2 revised
df_am<-tidyr::gather(am_1, key=s_var, value=s_value, -c(Municipality, service, Year))
df_bi<-tidyr::gather(bi_2, key=s_var, value=s_value, -c(Municipality, service, Year))
df_hr<-tidyr::gather(hr_3, key=s_var, value=s_value, -c(Municipality, service, Year))
df_ec<-tidyr::gather(ec_4, key=s_var, value=s_value, -c(Municipality, service, Year))
df_fs<-tidyr::gather(fs_5, key=s_var, value=s_value, -c(Municipality, service, Year))
df_fm<-tidyr::gather(fm_6, key=s_var, value=s_value, -c(Municipality, service, Year))
df_re<-tidyr::gather(re_7, key=s_var, value=s_value, -c(Municipality, service, Year))
df_pr<-tidyr::gather(pr_8, key=s_var, value=s_value, -c(Municipality, service, Year))
df_ps<-tidyr::gather(ps_9, key=s_var, value=s_value, -c(Municipality, service, Year))
df_rr<-tidyr::gather(rr_10, key=s_var, value=s_value, -c(Municipality, service, Year))
df_ww<-tidyr::gather(ww_11, key=s_var, value=s_value, -c(Municipality, service, Year))
df_ws<-tidyr::gather(ws_12, key=s_var, value=s_value, -c(Municipality, service, Year))
df_yw<-tidyr::gather(yw_13, key=s_var, value=s_value, -c(Municipality, service, Year))

#rm(df_am, df_bi, df_hr, df_ec, df_fs, df_fm, df_re, df_pr, df_ps, df_rr, df_ww, df_ws, df_yw)


# -----------------------------------------------------------------------------
# step 2: row-binding service-wise data
# -----------------------------------------------------------------------------
# step 2-0: combine all service-wise tibble into one 
# row-bind all services

df_list <- list(df_am, df_bi, df_hr, df_ec, df_fs, df_fm, df_re, df_pr, df_ps, 
                df_rr, df_ww, df_ws, df_yw)
# row-wise bind
df_all <- dplyr::bind_rows(df_list)
df_all
df_service_all <- df_all |> 
  dplyr::rename( 
    Variable=s_var,  
    Service=service, Value=s_value) |>
  dplyr::select(Municipality, Variable, Year, Service, Value)
df_service_all
# 2023-10 update
df_service_all <- df_service_all |> dplyr::mutate_at(c('Year'), as.double)
df_service_all
# -----------------------------------------------------------------------------
# step 3: read back census data and combine it with all-service-data
# -----------------------------------------------------------------------------
# 

bd_census_data <- readr::read_rds(file="bd_census_data_2023.rds")
bd_census_data
# step 3-1: row-bind (benchmark and census data)
df_combined <- dplyr::bind_rows(list(df_service_all, bd_census_data))
df_combined



# -----------------------------------------------------------------------------
# step 4: complete rows, i.e., creating missing rows with NA 
# -----------------------------------------------------------------------------
# step 4-1: apply tidyr::complete()
# 
bd_data_imp <- df_combined |> tidyr::complete(Municipality, Variable, Year)
bd_data_imp

#write_rds(bd_data_imp, file="bd_data_completed.rds")

# step 4-2: replace with NAs with correct ones in Service column
# this step requires the following helper function 
# mutate-supplement function
service_token <-function(x){
  token <- stringr::str_match(x, "^(census)_\\d|^q([a-z]+)\\d")[2]
  
  if( is.na(token)){
    token <-  stringr::str_match(x, "^(census)_\\d|^q([a-z]+)\\d")[3]
  }
  token
}

tmp_result <- bd_data_imp |> dplyr::rowwise() |> 
  dplyr::mutate(Service = service_token(Variable)) 
tmp_result


# step 4-3: check results by getting a frequency table
tmp_result |> dplyr::summarize(count_na = sum(is.na(Service)))


# step 4-4: saving the completed file as an rds file
# 2023-10 update
# rename from 5 to 6 due to breaking changes
readr::write_rds(tmp_result, file="bd_data_completed6.rds")
#tmp_result <- readr::read_rds(file="bd_data_completed6.rds")


# rm(df_all, df_service_all, bd_census_data, df_combined, bd_data_imp, dflst, tmp_result)
# 
# 
# ##################################################
# testing results
df_service_all |> dplyr::select(Service, Variable) |>
  dplyr::group_by(Service) |> 
  dplyr::distinct(Variable) |>
  dplyr::summarize(Freq=dplyr::n())

# df_service_all |> dplyr::distinct(Municipality) |> dplyr::arrange(Municipality)
# tmp_result |> dplyr::distinct(Municipality) |> dplyr::arrange(Municipality) |>
#   paste(sep=",")

# checking the contents of the combined data
tmp_result |> dplyr::select(Service, Variable) |>
  dplyr::group_by(Service) |> 
  dplyr::distinct(Variable) |>
  dplyr::summarize(Freq=dplyr::n())

df_service_to_var_in_data <- tmp_result |> dplyr::select(Service, Variable) |>
  dplyr::group_by(Service) |> 
  dplyr::distinct(Variable)

df_bi |> dplyr::select(service, s_var) |>
  dplyr::group_by(service) |> 
  dplyr::distinct(s_var) |> print(n=110)

#------------------------------------------------------------------------------
# Part 2: var-name-to-label data file
#------------------------------------------------------------------------------

# Required libraries 
library(tidyverse)
library(readxl)

# step 1 : read all-in-one metadata file
# 
# all-in-one Excel file: MetricNames_2023-10-31.xlsx
# sheet name: MetricNames
# The above includes census data; so no need to read the census metadata file
# 
# census metadata file: census_data_2023.xlsx
# sheet name : census_label_data
# header row: var_name	var_acr	var_order	var_label	var_def
# 
# old census_14: header info
# # A tibble: 34 × 4
# var_name  var_label  var_acr var_order
# <chr>     <chr>      <chr>       <dbl>

# !!!! warning
# before this step, var_acr and var_order must be inserted in the worksheet
# "MetricNames"
# step 0: insert 2 columns after variable name
# step 1: add a header row: var_name	var_acr	var_order	var_label	var_def
# step 2: fill var_acr and var_order columns

all_label_def_data_csv <- 
  readr::read_csv(col_names = c("var_name", "var_label", "var_def"),
                  "./Updated Data/MetricNames.csv")
all_label_def_data_csv

key2value =c('am'=1,'bi'=2,'hr'=3,'ec'=4,'fs'=5,'fm'=6,'re'=7,'pr'=8,'ps'=9,'rr'=10,'ww'=11,'ws'=12,'yw'=13,
'census'=14)

var2varvalue <-list()
key2value['am']
names(key2value)[1]
for (row in 1:length(names(key2value))){
  var2varvalue[[ names(key2value)[row] ]] <- row
}
var2varvalue[["census"]]
#-----------------------------

all_label_def_data <- 
  all_label_def_data_csv |> 
  dplyr::mutate(var_acr = stringr::str_extract(
  var_name, "^(census)_\\d|^q([a-z]+)\\d", group =2)) |>
  tidyr::replace_na(list(var_acr ='census')) |>
  dplyr::rowwise() |>
  dplyr::mutate(var_order = var2varvalue[[ var_acr ]]) |>
  dplyr::ungroup() |>
  dplyr::select(var_name, var_acr, var_order, var_label, var_def)
  
# rm(test_var_acr)

all_label_def_data <- readxl::read_excel("MetricNames_2023-11-02.xlsx", 
                                  sheet = "MetricNames")

# census_label_def_data <- readxl::read_excel("census_data_2023_added_more.xlsx", 
#                                   sheet = "census_label_data")

# The previous steps 1 and 6 can be merged into one step
# 
# how to re-arrange the columns according to the previous column-order
# if necessary 
# all_label_def_data <- all_label_def_data |> dplyr::select(var_name, var_label, 
# var_def, var_acr, var_order)

# step 7: check the above result by getting a frequency table
all_label_def_data |> dplyr::select(var_acr, var_name) |>
  dplyr::group_by(var_acr) |> 
  dplyr::distinct(var_name) |>
  dplyr::summarize(Freq=dplyr::n())

# serialize the data
readr::write_rds(all_label_def_data, "all_label_def_data6.rds")

defdf_all <- all_label_def_data |> 
  dplyr::select(var_name, var_label, var_def)


# intermediary files no need to store
# readr::write_rds(defdf_all, 
#                  file="metric_def_data_all.rds")
# metric_def_data <- readr::read_rds(file="metric_def_data_all.rds")
# defdf_all=> 

# for the hash table

all_varNameToLabel <- all_label_def_data |> 
dplyr::select(var_name, var_label, var_acr, var_order)

# serialize
readr::write_rds(all_label_def_data, "all_varNameToLabel6.rds")

all_varNameToLabel |> dplyr::group_by(var_acr) |> 
  dplyr::summarize(Freq=dplyr::n())

#------------------------------------------------------------------------------
# Part 3: var-name-to-def hash table
#------------------------------------------------------------------------------
#
# this section covers the previous pre-processing section in helper.R and
# those in `~/data-prep/metric-definition-data-prep.R`
# 
defdf_all 
# A tibble: 850 × 3
# var_name var_label var_def
# <chr>    <chr>     <chr>  

which(is.na(defdf_all$var_label))
which(is.na(defdf_all$var_def))
which(is.null(defdf_all$var_def))

# it seems latest version (2023-10-31) has no NA, NULL cells
# 
# no need to serialize because defdf_all is not the one to be called back
# readr::write_rds(defdf_all, file="metric_def_data.rds")
# defdf_all <- readr::read_rds(file="metric_def_data.rds")




# processing concerning all_varNameToLabel 
# This object must be serialized later
v2lallinOne <-list()
for (row in 1:nrow(all_varNameToLabel)) {
  valueN <- all_varNameToLabel[row, "var_name"]
  valueL <- all_varNameToLabel[row, "var_label"]
  vl <- stats::setNames(as.list(valueL), valueN)
  v2lallinOne <- append(v2lallinOne, vl)
}
# serialized
# help.R reads back the saved file
readr::write_rds(v2lallinOne, file = "v2lallinOne.rds")
#v2lallinOne <- readr::read_rds(file = "v2lallinOne.rds")


# in helper.R, all_vname2def is called back as follows
# all_vname2def <- readr::read_rds(file="all_vname2def.rds")
# previous all_metric_def_data is metric_def_data, i.e., defdf_all
# The above rds file is generated as follows: 
all_vname2def <- list()
for (row in 1:nrow(defdf_all)) {
  valueN <- defdf_all[row, "var_name"]
  valueD <- defdf_all[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  all_vname2def <- append(all_vname2def, vl)
}
#all_vname2def$qyl12
# serializaton
readr::write_rds(all_vname2def, file = "all_vname2def.rds")
# all_vname2def <- readr::read_rds(file="all_vname2def.rds")



# -----------------------------------------------------------------------------
# creating all_service_abbrev_to_full in helper.R
# -----------------------------------------------------------------------------
all_service_abbrev_to_full <- 
  readxl::read_excel("MetricNames.xlsx",
             sheet = "all_service_info")
# sheet name: all_service_info
# all_vname2def.rds
readr::write_rds(all_service_abbrev_to_full, file="all_service_abbrev_to_full.rds")

# 3 lists to be serialized and read back
#  1 srvclngToShrtRefLst <-list()
srvclngToShrtRefLst <-list()
for (i in 1:dim(all_service_abbrev_to_full)[1]) {
  srvclngToShrtRefLst[[all_service_abbrev_to_full$Full[i]]] <- all_service_abbrev_to_full$lc[i]
}
srvclngToShrtRefLst

readr::write_rds(srvclngToShrtRefLst, file="srvclngToShrtRefLst.rds")
srvclngToShrtRefLst <- readr::read_rds(file = "srvclngToShrtRefLst.rds")

# 2 srvclngToShrtRefLstWoc, i.e., without census, service only 
# 
srvclngToShrtRefLstWoc <-list()
for (i in 1:dim(all_service_abbrev_to_full)[1]) {
  if (all_service_abbrev_to_full$lc[[i]]=="census"){
    next
  }
  srvclngToShrtRefLstWoc[[all_service_abbrev_to_full$Full[i]]] <- all_service_abbrev_to_full$lc[i]
}

readr::write_rds(srvclngToShrtRefLstWoc, file="srvclngToShrtRefLstWoc.rds")
srvclngToShrtRefLstWoc <- readr::read_rds(file = "srvclngToShrtRefLstWoc.rds")


# 3 srv2varlbllst
srv2varlbllst <-list()
for (srv in srvclngToShrtRefLst){
  # value is short form such as am
  tmp <- all_varNameToLabel |>
    dplyr::filter(var_acr==srv) |> dplyr::select(var_name, var_label)
  name_vec  <-tmp |> dplyr::pull(var_label)
  # print(name_vec)
  value_vec <- tmp |> dplyr::pull(var_name) 
  # print(value_vec)
  valueLst<-stats::setNames(as.list(value_vec), name_vec)
  srv2varlbllst[[srv]]  <- valueLst
}

readr::write_rds(srv2varlbllst, file="srv2varlbllst.rds")
srv2varlbllst <- readr::read_rds(file = "srv2varlbllst.rds")




