###############################################################################
# data prep steps of 2023-02-02 revision
#
# This revision starts from the following conditions where
# each worksheet is saved in a workbook
# for both benchmarking data and variable list
# ane their file names are given by:
#
# bm_data.xlsx
# vnl.xlsx
#
# For the benchmarking data, the last column of each worksheet, 
# `yr_mun_ser` must be skipped; this last column position varies sheet by sheet,
# it would be better to dplyr to filter out this column 
# 
# For variable-list, each sheet must remove the following irrelevant 4 rows, 
#   a_municipality	Municipality
#   a_service	Service
#   a_year	Service Year
#   yr_mun_ser	Service Year_Municipality_Service
# 
# and add two new columns: `var_acr` and `var_order` 
# 
# mutate(var_acr="amr", var_order="1")

# Remove rows based on condition
# 
# dplyr::filter(name=="a_municipality" | name=="a_service" | name=="a_year" |
# name=="yr_mun_ser")
###############################################################################


#------------------------------------------------------------------------------
# Part 1: Benchmark Data
#------------------------------------------------------------------------------

# step 0: set the storage directory as the current working directory, e.g., 
setwd("~/Documents/experiments/visualization/benchmarking/2023-02-02")


# Required libraries 
# library(tidyverse)
# library(readxl)
library(magrittr)
# -----------------------------------------------------------------------------
# step 1: read the workdbook and create service-wise objects
# -----------------------------------------------------------------------------

# step 1-0: read all worksheets of the dashboard-data workbook

amr_1 <- readxl::read_excel('bm_data.xlsx', sheet='1')
bi_2 <- readxl::read_excel('bm_data.xlsx', sheet='2')
hr_3 <- readxl::read_excel('bm_data.xlsx', sheet='3')
pr_4 <- readxl::read_excel('bm_data.xlsx', sheet='4')
ec_5 <- readxl::read_excel('bm_data.xlsx', sheet='5')
fs_6 <- readxl::read_excel('bm_data.xlsx', sheet='6')
fm_7 <- readxl::read_excel('bm_data.xlsx', sheet='7')
hore_8 <- readxl::read_excel('bm_data.xlsx', sheet='8')
ps_9 <- readxl::read_excel('bm_data.xlsx', sheet='9')
rrc_10 <- readxl::read_excel('bm_data.xlsx', sheet='10')
wws_11 <- readxl::read_excel('bm_data.xlsx', sheet='11')
ws_12 <- readxl::read_excel('bm_data.xlsx', sheet='12')
yl_13 <- readxl::read_excel('bm_data.xlsx', sheet='13')



# step 1-1: remove irrelevant column (`yr_mun_ser`) and recode `a_service`
# from a long name to its short name to save memory
# 
# amr_1 <- amr_1 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service, a_service=='Asphalt Maintenance and Repair', "amr"))

amr_1 <- amr_1 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Asphalt Maintenance and Repair','amr'))
bi_2 <- bi_2 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Building Inspection','bi'))
hr_3 <- hr_3 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Central Human Resources','hr'))
pr_4 <- pr_4 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Core Parks and Recreation','pr'))
ec_5 <- ec_5 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Emergency Communications','ec'))
fs_6 <- fs_6 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Fire Service','fs'))
fm_7 <- fm_7 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Fleet Maintenance','fm'))
hore_8 <- hore_8 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Household Recycling','hore'))
ps_9 <- ps_9 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Police Service','ps'))
rrc_10 <- rrc_10 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Residential Refuse Collection','rrc'))
wws_11 <- wws_11 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Wastewater Service','wws'))
ws_12 <- ws_12 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Water Service','ws'))
yl_13 <- yl_13 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=replace(a_service,a_service == 'Yard Waste/Leaf Collection','yl'))








# bi_2 <- bi_2 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Building Inspection'='bi'))
# hr_3 <- hr_3 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Central Human Resources'='hr'))
# pr_4 <- pr_4 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Core Parks and Recreation'='pr'))
# ec_5 <- ec_5 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Emergency Communications'='ec'))
# fs_6 <- fs_6 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Fire Service'='fs'))
# fm_7 <- fm_7 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Fleet Maintenance'='fm'))
# hore_8 <- hore_8 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Household Recycling'='hore'))
# ps_9 <- ps_9 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Police Service'='ps'))
# rrc_10 <- rrc_10 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Residential Refuse Collection'='rrc'))
# wws_11 <- wws_11 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Wastewater Service'='wws'))
# ws_12 <- ws_12 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Water Service'='ws'))
# yl_13 <- yl_13 |> dplyr::select(-c(yr_mun_ser)) |> dplyr::mutate(a_service=recode(a_service,'Yard Waste/Leaf Collection'='yl'))

# command to remove intermediary objects later
# rm (amr_1, bi_2, hr_3, pr_4, ec_5,fs_6, fm_7, hore_8, ps_9,rrc_10, wws_11, ws_12, yl_13)

# step 1-2: create the long form of each service tibble
df_amr<-tidyr::gather(amr_1, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_bi<-tidyr::gather(bi_2, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_hr<-tidyr::gather(hr_3, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_pr<-tidyr::gather(pr_4, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_ec<-tidyr::gather(ec_5, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_fs<-tidyr::gather(fs_6, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_fm<-tidyr::gather(fm_7, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_hore<-tidyr::gather(hore_8, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_ps<-tidyr::gather(ps_9, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_rrc<-tidyr::gather(rrc_10, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_wws<-tidyr::gather(wws_11, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_ws<-tidyr::gather(ws_12, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))
df_yl<-tidyr::gather(yl_13, key=s_var, value=s_value, -c(a_municipality, a_service, a_year))

# command to remove intermediary objects later
# rm(df_amr, df_bi, df_hr, df_pr, df_ec, df_fs, df_fm, df_hore, df_ps, df_rrc, df_wws, df_ws, df_yl)

# -----------------------------------------------------------------------------
# step 2: row-binding service-wise data
# -----------------------------------------------------------------------------
# step 2-0: combine all service-wise tibble into one 
# row-bind all services

dflst<- list(df_amr, df_bi, df_hr, df_pr, df_ec, df_fs, df_fm, df_hore, df_ps, df_rrc, df_wws, df_ws, df_yl)
df_all <- dplyr::bind_rows(dflst)

# step 2-1: rename columns
df_service_all <- df_all |> 
  dplyr::rename( Municipality=a_municipality,
                 Variable=s_var,  
                 Year = a_year,
                 Service=a_service, Value=s_value) |>
  dplyr::select(Municipality, Variable, Year, Service, Value)

df_service_all
# -----------------------------------------------------------------------------
# step 3: read back census data and combine it with all-service-data
# -----------------------------------------------------------------------------
# step 3-0: read back census data

bd_census_data <- readr::read_rds(file="../bd_census_data_uc.rds")
bd_census_data
# step 3-1: row-bind (benchmark and census data)
df_combined <- dplyr::bind_rows(list(df_service_all, bd_census_data))
df_combined

# -----------------------------------------------------------------------------
# step 4: complete rows, i.e., creating missing rows with NA 
# -----------------------------------------------------------------------------
# step 4-1: apply tidyr::complete()

bd_data_imp <- df_combined |> tidyr::complete(Municipality, Variable, Year)

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



# step 4-3: check results by getting a frequency table
tmp_result |> dplyr::summarize(count_na = sum(is.na(Service)))


# step 4-4: saving the completed file as an rds file
readr::write_rds(tmp_result, file="bd_data_completed5.rds")
tmp_result <- readr::read_rds(file="bd_data_completed5.rds")


# rm(df_all, df_service_all, bd_census_data, df_combined, bd_data_imp)


#------------------------------------------------------------------------------
# Part 2: var-name-to-label data file
#------------------------------------------------------------------------------

# Required libraries 
library(tidyverse)
library(readxl)

# step 1: read all worksheets in the workbook
amr_p1<- readxl::read_excel('vnl.xlsx', sheet='1')
bi_p2<- readxl::read_excel('vnl.xlsx', sheet='2')
hr_p3<- readxl::read_excel('vnl.xlsx', sheet='3')
pr_p4<- readxl::read_excel('vnl.xlsx', sheet='4')
ec_p5<- readxl::read_excel('vnl.xlsx', sheet='5')
fs_p6<- readxl::read_excel('vnl.xlsx', sheet='6')
fm_p7<- readxl::read_excel('vnl.xlsx', sheet='7')
hore_p8<- readxl::read_excel('vnl.xlsx', sheet='8')
ps_p9<- readxl::read_excel('vnl.xlsx', sheet='9')
rrc_p10<- readxl::read_excel('vnl.xlsx', sheet='10')
wws_p11<- readxl::read_excel('vnl.xlsx', sheet='11')
ws_p12<- readxl::read_excel('vnl.xlsx', sheet='12')
yl_p13<- readxl::read_excel('vnl.xlsx', sheet='13')


# step 2: add two columns (var_acr and var_order) to each sheet

amr_p1 <- amr_p1 |> dplyr::mutate(var_acr='amr', var_order=1)
bi_p2 <- bi_p2 |> dplyr::mutate(var_acr='bi', var_order=2)
hr_p3 <- hr_p3 |> dplyr::mutate(var_acr='hr', var_order=3)
pr_p4 <- pr_p4 |> dplyr::mutate(var_acr='pr', var_order=4)
ec_p5 <- ec_p5 |> dplyr::mutate(var_acr='ec', var_order=5)
fs_p6 <- fs_p6 |> dplyr::mutate(var_acr='fs', var_order=6)
fm_p7 <- fm_p7 |> dplyr::mutate(var_acr='fm', var_order=7)
hore_p8 <- hore_p8 |> dplyr::mutate(var_acr='hore', var_order=8)
ps_p9 <- ps_p9 |> dplyr::mutate(var_acr='ps', var_order=9)
rrc_p10 <- rrc_p10 |> dplyr::mutate(var_acr='rrc', var_order=10)
wws_p11 <- wws_p11 |> dplyr::mutate(var_acr='wws', var_order=11)
ws_p12 <- ws_p12 |> dplyr::mutate(var_acr='ws', var_order=12)
yl_p13 <- yl_p13 |> dplyr::mutate(var_acr='yl', var_order=13)

# step 3: read-back the pre-processed census data as an rds file
# census_14<- read_excel('census_vnl_data.xlsx', sheet='Sheet1')
# write_rds(census_14, file = "census_14.rds")
census_14 <-readr::read_rds(file = "../census_14.rds")

# step 4: row-bind service-wise tibbles and rename two columns
dflst2 <-list(amr_p1, bi_p2, hr_p3, pr_p4, ec_p5, fs_p6, fm_p7,
    hore_p8, ps_p9, rrc_p10, wws_p11, ws_p12, yl_p13)

# rm(amr_p1, bi_p2, hr_p3, pr_p4, ec_p5, fs_p6, fm_p7, hore_p8, ps_p9, rrc_p10, wws_p11, ws_p12, yl_p13)

df_all2 <- dplyr::bind_rows(dflst2) |> 
  dplyr::rename(var_name = name, var_label= varlab )


# step 5: row-bind with census data
df_all2 <- dplyr::bind_rows(list(df_all2, census_14))

# step 6: remove irrelevant rows in each service-wise rows: 4 rows per service
all_varNameToLabel <- df_all2 |> 
  dplyr::filter(
    !(var_name == "a_municipality" | var_name == "a_service" |
        var_name == "a_year" | var_name =="yr_mun_ser"
     )
  )

# step 7: check the above result by getting a frequency table
all_varNameToLabel |> dplyr::group_by(var_acr) |> dplyr::summarize(Freq=dplyr::n())

# step 8: save the varName-varLabel data as an rds file
all_varNameToLabel
readr::write_rds(all_varNameToLabel, "all_varNameToLabel5.rds")

# rm(census_14, df_all2 )


