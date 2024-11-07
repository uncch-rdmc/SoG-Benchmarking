
# version: This version deals with the data files of 2024-11-01


# data files to be used
#
# benchmarking_project_bi_2024-11-01.csv
# benchmarking_project_others_2024-11-01.csv
# benchmarking_project_wsww_2024-11-01.csv
# 'Metric Codes, Names, and Definitions_2024-11-01.csv'

# 
# all_service_abbrev_to_full_others_updated.csv
# Census_Data_2020-2022_others.csv
# Census_Data_2020-2022_bi.csv
# Metric Codes, Names, and Definitions_2024-11-01.csv
# all_service_lookup_table_fy2025_1.csv



# required library
library(tidyverse)
#library(readr)


# =============================================================================
# Part I loading data 
# =============================================================================


# loading data

# read each data file
# bi case
bi_2024_11_01 <- readr::read_csv("../../2024-11-01/benchmarking_project_bi_2024-11-01.csv",
                                 col_types = readr::cols(
                                   .default = "n",
                                   ResponseId = readr::col_skip(),
                                   a_fiscal_year = readr::col_integer(),
                                   a_jurisdiction = readr::col_character(),
                                   a_service = readr::col_character()))

# wsww case


wsww_2024_11_01 <- readr::read_csv("../../2024-11-01/benchmarking_project_wsww_2024-11-01.csv",
                                   col_types = readr::cols(
                                     .default = "n",
                                     ResponseId = readr::col_skip(),
                                     a_fiscal_year = readr::col_integer(),
                                     a_jurisdiction = readr::col_character(),
                                     a_service = readr::col_character() ))



# others case
others_2024_11_01<- readr::read_csv("../../2024-11-01/benchmarking_project_others_2024-11-01.csv",
                                    col_types = readr::cols(
                                      .default = "n",
                                      ResponseId = readr::col_skip(),
                                      a_fiscal_year = readr::col_integer(),
                                      a_jurisdiction = readr::col_character(),
                                      a_service = readr::col_character()
                                    ))


# -----------------------------------------------------------------------------
# the actual-set vs pop-set for providers
#
sp_set_bi <-
  bi_2024_11_01 |>
  dplyr::select(a_jurisdiction) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  as.vector()
sp_set_bi
length(sp_set_bi) # 62

sp_set_bi[duplicated(sp_set_bi)] 


sp_set_wsww <-
 wsww_2024_11_01 |>
  dplyr::select(a_jurisdiction) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  as.vector()
sp_set_wsww
length(sp_set_wsww) #112
sp_set_wsww[duplicated(sp_set_wsww)] 

sp_set_others <-
  others_2024_11_01 |>
  dplyr::select(a_jurisdiction) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  as.vector()
sp_set_others
length(sp_set_others) #16
sp_set_others[duplicated(sp_set_others)] 


bi_2024_11_01 |>
  dplyr::group_by(a_service) |>
  dplyr::count()  


# wsww: just 2 services
wsww_2024_11_01 |>
  dplyr::group_by(a_service) |>
  dplyr::count()

# others: 11 services; Budgeting was added after 2024-09-30
# A problem here is that they are not used for GUI, i.e., they must be 
# related to each respective service;
# these "Budgeting" rows must be re-coded to their respective service

others_2024_11_01 |>
  dplyr::group_by(a_service) |>
  dplyr::count() 


all_service_fy2025 <-
  others_2024_11_01 |>
  dplyr::select(a_service) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  sort()
all_service_fy2025 # 11


# Budgeting-data manipulating block:


# 2024-11-01 update
# budget only data
others_2024_11_01_budget <- others_2024_11_01 |>
  dplyr::filter(stringr::str_detect(a_service, "^Budget"))
others_2024_11_01_budget[sapply(others_2024_11_01_budget, function(x) all(is.na(x)))] <- NULL
dim(others_2024_11_01_budget) # 63 23


# 2024-11-01 update
# creating a hash table from service-acronym to service-full name
all_service_abbrev_to_full_others <- readr::read_csv("all_service_abbrev_to_full_others_updated.csv", 
                                              col_types = readr::cols(OrderNo = readr::col_integer()))
# or
#all_service_abbrev_to_full_others <- readr::read_rds(file="all_service_abbrev_to_full_others.rds")

# creating a hash table from service-full name to service acronym
srvclngToShrtRefLstWoc_others <-list()
for (i in 1:dim(all_service_abbrev_to_full_others)[1]) {
  if (all_service_abbrev_to_full_others$lc[[i]]=="census"){
    next
  }
  srvclngToShrtRefLstWoc_others[[all_service_abbrev_to_full_others$Full[i]]] <- all_service_abbrev_to_full_others$lc[i]
}
srvclngToShrtRefLstWoc_others


# creating a list of rows that indicates which service (row) should be updated
# > row_id2row_numbers
# [[1]]
# am  ec  fs  fm  hr  pr  ps  re  rr  yw 
# 1 126 163 227 277 336 391 445 501 563 
# ...

row_id2row_numbers <-list()
for (i in 1:dim(others_2024_11_01_budget)[1]){
  bdgt_row <- others_2024_11_01_budget[i,]
  # print(paste(c(bdgt_row$a_fiscal_year, bdgt_row$a_jurisdiction),collapse = "-"))
  # 
  # print(bdgt_row$a_fiscal_year)
  # print(bdgt_row$a_jurisdiction)
  # print(bdgt_row$a_service)
  # 
  # relevant_rows contains non-budgeting rows under the above 3 combination
  # probably not all of the known services might be found, some might be missing
  print("i-th row =")
  print(i)
  relevant_rows <-
    which(others_2024_11_01$a_fiscal_year== bdgt_row$a_fiscal_year &
            others_2024_11_01$a_jurisdiction==bdgt_row$a_jurisdiction  &
            others_2024_11_01$a_service != bdgt_row$a_service
    )
  print(relevant_rows)
  print(length(relevant_rows))
  
  # for each of relevant_rows, let's find its service token
  # note: what happens if Budgeting
  # 
  service_acro_set <- c()
  for (rw in relevant_rows){
    # see line 2313 for srvclngToShrtRefLstWoc_others
    service_acro <- srvclngToShrtRefLstWoc_others[[dplyr::pull(others_2024_11_01[rw, "a_service"]) ]] 
    service_acro_set <- append(service_acro_set, service_acro)
  }
  print(service_acro_set)
  names(relevant_rows)<- service_acro_set
  row_id2row_numbers[[i]] <- relevant_rows
}
row_id2row_numbers


# ----------------------------------------------------------------------------
# How to replace Budgeting-related cells with their respective cells from the 
# budgeting rows
# -----------------------------------------------------------------------------
# budgeting row => related/available rows 
# for each of the above affected rows, get relevant 

# create a test data file


others_2024_11_01_test <- others_2024_11_01

# loop through: budget_df_row: others_2024_10_04_budget
#   find i-th row's related row vector => c(556 557 558 559)
#   and also its service => ("yw" "rr" "re" "fm")
# the above vector gives rows to be updated and each row's service is known,
# the columns to be update in the parent d.f. are given by the following way:
# e.g., am => (qam17, qam18)
# 
# next to be found is its corresponding columns in the all
# this step is done by service => service_column: am => data_budget(4, 5) or qamxx, qamyy
# 21, 22 : 
# service2b_mtrcs: service_acro to its relevant metric name
# 



# how many columns in Budgeting rows


budget_metrics <- grep("^q", colnames(others_2024_11_01_budget), value=TRUE, invert = FALSE) |> sort()

budget_metrics
length(budget_metrics) #: 20 so far

# unique service acronyms are
budget_metric_services <- budget_metrics |> stringr::str_extract("^q(\\D+)\\d+", group=1) |> unique() |> sort()
# service acronym
budget_metric_services

budget_metric_services_wq <- budget_metrics |> stringr::str_extract("^(\\D+)\\d+", group=1) |> unique() |> sort()
# "q" + service acronym
budget_metric_services_wq
length(budget_metric_services) # 10 => each service has two budgeting metrics columns


# creating service2b_mtrcs such as
# > service2b_mtrcs
# $am
# [1] "qam17" "qam18"
# 
# $ec
# [1] "qec40" "qec41"

# create an empty list first
service2b_mtrcs <-list()

# initialize the list
for (service_acro in budget_metric_services){
  service2b_mtrcs[[service_acro]] <- c("")
}
service2b_mtrcs

# add the contents
for (mtrc in budget_metrics){
  service_tag <- mtrc |> stringr::str_extract("^q(\\D+)\\d+", group=1)
  print(service_tag)
  # if (service_tag %in% names(service2b_mtrcs)){
  #   service2b_mtrcs[[service_tag]] <-c()
  # }
  if (length(service2b_mtrcs[[service_tag]]==1) & service2b_mtrcs[[service_tag]][1] == "" ){
    print("first if")
    service2b_mtrcs[[service_tag]][1] <- mtrc
  } else  if (length(service2b_mtrcs[[service_tag]]==1) & service2b_mtrcs[[service_tag]][1] != "" ){
    print("2nd if")
    service2b_mtrcs[[service_tag]] <- append(service2b_mtrcs[[service_tag]], mtrc)
  }

}

service2b_mtrcs

# 
# The following block copies data in budgeting rows to their respective 
# columns' row 
counter_replacement <-0
for (ri in 1:dim(others_2024_11_01_budget)[1]){
  row_i_set <- row_id2row_numbers[[ri]]
  
  # row_i_set, e.g,
  #  ps  hr  re  fs  pr  yw  am  ec  rr  hr 
  # 465 492 505 506 507 508 509 510 511 542
  # 
  # print("ri i-th=")
  # print(ri)
  for (k in 1:length(row_i_set)){
    # el points to each location (row) of the parent data.frame,
    # such as 465 (ps)
    # print("parent d.f. row #:k =")
    # print(row_i_set[k])
    # print(names(row_i_set[k]))s
    service_acro <- names(row_i_set[k])
    # e.g., "ps"
    # print("service name:service_acro=")
    # print(service_acro)
    for (bdgt_col in service2b_mtrcs[[service_acro]]){ # budgeting columns
      
      if (is.na(dplyr::pull(others_2024_11_01_budget[ri, c(bdgt_col)]))){
        next
      }
      
      # such as "qyw11" "qyw12"
      # left-hand-side
      
      others_2024_11_01_test[row_i_set[k], c(bdgt_col)]
      # print("current value=")
      # print(others_2024_11_01_test[el, c(bdgt_col)])
      # 
      # right-hand-side
      dplyr::pull(others_2024_11_01_budget[ri, c(bdgt_col)])
      # print("new value=")
      # print(dplyr::pull(others_2024_11_01_budget[ri, c(bdgt_col)]))
      
      # replacement
      others_2024_11_01_test[row_i_set[k], c(bdgt_col)] <- 
        dplyr::pull(others_2024_11_01_budget[ri, c(bdgt_col)])
      # print("value check=")
      # print(others_2024_11_01_test[row_i_set[k], c(bdgt_col)])
      counter_replacement <- counter_replacement+1
    }
  }
}
print("how many replacement=")
print(counter_replacement)


others_2024_11_01_test <- 
  others_2024_11_01_test |>
  dplyr::filter(a_service !="Budgeting")


# checking after removing "budgeting": should be no budgeting
others_2024_11_01_test |>
  # dplyr::filter(a_service !="Budgeting") |>
  dplyr::group_by(a_service) |>
  dplyr::count()



# -----------------------------------------------------------------------------
# read back the current census data file
# 
# Pop_others
# note:  bi and others have a population file

# 2024-11-01 update
# the following census files assume that they do not have a row of labels, i.e.,
# it has been removed
# 
# Pop_others
# this is wide-form
Pop_others_2024_11_01 <- readr::read_csv("./Census_Data_2020-2022_others.csv", 
 col_types = readr::cols (.default = "n", a_jurisdiction = "c"))

# generate its long-form 
bd_census_data_others_2024_11_01 <- 
  Pop_others_2024_11_01 |> tidyr::pivot_longer(cols = (tidyselect::starts_with("census_")),
                                               names_to = "Variable", values_to = "Value")
# rename/arrange columns
bd_census_data_others_2024_11_01 <-
  bd_census_data_others_2024_11_01 |> dplyr::mutate(Service="census") |>
  dplyr::relocate(Service, .before = Value) |>
  dplyr::relocate(a_fiscal_year, .before = Service) |>
  dplyr::rename(Municipality=a_jurisdiction, Year=a_fiscal_year)
bd_census_data_others_2024_11_01

# sort by Variable column
bd_census_data_others_2024_11_01  <- bd_census_data_others_2024_11_01 |> 
  dplyr::arrange(Variable)
bd_census_data_others_2024_11_01 # dim 425 x5





# Pop_bi
# census for BI
# column header: a_jurisdiction,census_01

# 2024-11-01 update
Pop_bi_2024_11_01 <- readr::read_csv("./Census_Data_2020-2022_bi.csv", 
                                     col_types = readr::cols (.default = "n", a_jurisdiction = "c"))
#    a_jurisdiction   a_fiscal_year census_01
dim(Pop_bi_2024_11_01) # 1045 x 3

# -----------------------------------------------------------------------------
# read back the metric definition file
# 
# MetricNames

# 2024-11-01 updated
# Code_New,Metrics,Definitions  => code_new,metrics,definitions
# duplicated metrics qre28_01 => qre28_02
MetricNames_2024_11_01 <- readr::read_csv("./Metric Codes, Names, and Definitions_2024-11-01.csv", 
col_types = readr::cols(code_new = readr::col_character(), metrics = readr::col_character(), 
definitions = readr::col_character()), trim_ws = FALSE)

# duplication check
MetricNames_2024_11_01$code_new[duplicated(MetricNames_2024_11_01$code_new)] # should return character(0) otherwise duplicated entries


MetricNames <- MetricNames_2024_11_01
dim(MetricNames) # 872 x 3



# dividing the all-in-one file into 3 service groups
# note: no census data for bi
MetricNames_bi <- MetricNames |> dplyr::filter(stringr::str_detect(code_new, "^(qbi)")) 
dim(MetricNames_bi) # 139 x 3

# note: no census data for wsww
MetricNames_wsww <- MetricNames |> dplyr::filter(stringr::str_detect(code_new, "^(qws|qww)")) 
dim(MetricNames_wsww) # 155 x 3

MetricNames_others <- MetricNames |> dplyr::filter(stringr::str_detect(code_new, "^(qws|qww|qbi)", negate=TRUE)) 
dim(MetricNames_others) # 578 x 3

#------------------------------------------------------------------------------
# reference table to be used for generating data as an Excel workbook, etc.
# all_service_lookup_table_fy2025 is used in this block only
# 
# read back the reference table
all_service_lookup_table_fy2025 <- readr::read_csv("all_service_lookup_table_fy2025_1.csv",
  col_types =  readr::cols(OrderNo =  readr::col_integer(),
  Order_25 =  readr::col_integer(), Order_25x =  readr::col_integer()))


# creating acronym to fullname mappingtable
srvcShrtToLngRefLst_all <-list()
for (i in 1:dim(all_service_lookup_table_fy2025)[1]) {
  srvcShrtToLngRefLst_all[[all_service_lookup_table_fy2025$lc[i]]] <- all_service_lookup_table_fy2025$Full[i]
}
srvcShrtToLngRefLst_all

# The above table without census
service_all_woc <- head(all_service_lookup_table_fy2025$Full, -1)
service_all_woc


# -----------------------------------------------------------------------------
# population-related operation and renaming objects

# 2024-11-01
# # bi
data_bi_pop_added <-
  dplyr::left_join(bi_2024_11_01, Pop_bi_2024_11_01, 
                   by =c("a_jurisdiction"="a_jurisdiction", "a_fiscal_year" = "a_fiscal_year"))
data_bi <- data_bi_pop_added


# wsww
data_wsww <- wsww_2024_11_01


# others
data_others <- others_2024_11_01_test
data_others_pop_test <- others_2024_11_01_test


Pop_others_pop_only <- 
Pop_others_pop_test |> dplyr::select(a_jurisdiction, a_fiscal_year, census_01)



data_others_pop_added <-
  dplyr::left_join(others_2024_11_01_test, Pop_others_pop_only, 
                   by =c("a_jurisdiction"="a_jurisdiction", "a_fiscal_year" = "a_fiscal_year"))
data_others_pop_added

data_others <- data_others_pop_added

# check the above result
data_others |>
  # dplyr::filter(a_service !="Budgeting") |>
  dplyr::group_by(a_service) |>
  dplyr::count()

  
# =============================================================================
# Part II metrics definitions
# =============================================================================
# metric formula: as fo 2024-11-01
# -----------------------------------------------------------------------------
# bi
data_bi$qbi01_02 <- (data_bi$qbi01/data_bi$census_01)*1000
data_bi$qbi02_01 <- (data_bi$qbi02/data_bi$qbi01)
data_bi$qbi02_02 <- (data_bi$qbi02/data_bi$census_01)*1000
data_bi$qbi06_01 <- rowSums(data_bi[, c('qbi01', 'qbi02', 'qbi05', 'qbi06')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi01', 'qbi02', 'qbi05', 'qbi06')])) == 0)
data_bi$qbi01_01 <- (data_bi$qbi01/data_bi$qbi06_01)*100
data_bi$qbi06_02 <- (data_bi$qbi06_01/data_bi$census_01)
data_bi$qbi06_03 <- (data_bi$qbi06/data_bi$qbi67)
data_bi$qbi07_01 <- (data_bi$qbi07/data_bi$census_01)*1000
data_bi$qbi08_01 <- (data_bi$qbi08/data_bi$qbi07)
data_bi$qbi08_02 <- (data_bi$qbi08/data_bi$census_01)
data_bi$qbi10_01 <- rowSums(data_bi[, c('qbi07', 'qbi08', 'qbi09')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi07', 'qbi08', 'qbi09')])) == 0)
data_bi$qbi10_02 <- (data_bi$qbi10_01/data_bi$qbi19)*100
data_bi$qbi14_01 <- (data_bi$qbi14/data_bi$qbi19)*100
data_bi$qbi15_01 <- (data_bi$qbi15/data_bi$qbi19)*100
data_bi$qbi16_01 <- (data_bi$qbi16/data_bi$qbi19)*100
data_bi$qbi17_01 <- (data_bi$qbi17/data_bi$qbi19)*100
data_bi$qbi19_01 <- (data_bi$qbi19/data_bi$census_01)
data_bi$qbi19_02 <- (data_bi$qbi19/data_bi$qbi67)

data_bi$qbi38_02 <- rowSums(data_bi[, c('qbi34', 'qbi35', 'qbi36', 'qbi37', 'qbi38')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi34', 'qbi35', 'qbi36', 'qbi37', 'qbi38')])) == 0)

data_bi$qbi22_01 <- (data_bi$qbi22/data_bi$qbi38_02)*100
data_bi$qbi23_01 <- (data_bi$qbi23/data_bi$qbi38_02)*100
data_bi$qbi26_01 <- rowSums(data_bi[, c('qbi22', 'qbi26')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi22', 'qbi26')])) == 0)
data_bi$qbi27_01 <- rowSums(data_bi[, c('qbi23', 'qbi27')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi23', 'qbi27')])) == 0)
data_bi$qbi28_01 <- rowSums(data_bi[, c('qbi25', 'qbi28')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi25', 'qbi28')])) == 0)
data_bi$qbi29_01 <- (data_bi$qbi29/data_bi$qbi26_01)*100
data_bi$qbi30_01 <- (data_bi$qbi30/data_bi$qbi27_01)*100
data_bi$qbi31_01 <- (data_bi$qbi31/data_bi$qbi26_01)*100
data_bi$qbi32_01 <- (data_bi$qbi32/data_bi$qbi27_01)*100
data_bi$qbi33_01 <- (data_bi$qbi33/data_bi$qbi28_01)*100

data_bi$qbi34_01 <- (data_bi$qbi34/data_bi$qbi38_02)*100
data_bi$qbi35_01 <- (data_bi$qbi35/data_bi$qbi38_02)*100
data_bi$qbi36_01 <- (data_bi$qbi36/data_bi$qbi38_02)*100
data_bi$qbi37_01 <- (data_bi$qbi37/data_bi$qbi38_02)*100
data_bi$qbi38_01 <- (data_bi$qbi38/data_bi$qbi38_02)*100
data_bi$qbi39_01 <- (data_bi$qbi39/data_bi$qbi34)*100
data_bi$qbi40_01 <- (data_bi$qbi40/data_bi$qbi35)*100
data_bi$qbi41_01 <- (data_bi$qbi41/data_bi$qbi36)*100
data_bi$qbi42_01 <- (data_bi$qbi42/data_bi$qbi37)*100
data_bi$qbi43_01 <- (data_bi$qbi43/data_bi$qbi38)*100
data_bi$qbi45_01 <- (data_bi$qbi45/data_bi$qbi44)*100
data_bi$qbi47_01 <- (data_bi$qbi47/data_bi$census_01)
data_bi$qbi48_01 <- (data_bi$qbi48/data_bi$census_01)
data_bi$qbi49_01 <- (data_bi$qbi49/data_bi$census_01)
data_bi$qbi10_01 <- rowSums(data_bi[, c('qbi07', 'qbi08', 'qbi09')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi07', 'qbi08', 'qbi09')])) == 0)
data_bi$qbi56_01 <- rowSums(data_bi[, c('qbi52', 'qbi56')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi52', 'qbi56')])) == 0)
data_bi$qbi59_01 <- rowSums(data_bi[, c('qbi56', 'qbi57','qbi58', 'qbi59')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi56', 'qbi57','qbi58', 'qbi59')])) == 0)
data_bi$qbi60_01 <- rowSums(data_bi[, c('qbi53', 'qbi57','qbi60')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi53', 'qbi57','qbi60')])) == 0)
data_bi$qbi61_01 <- rowSums(data_bi[, c('qbi54', 'qbi58','qbi61')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi54', 'qbi58','qbi61')])) == 0)
data_bi$qbi62_01 <- rowSums(data_bi[, c('qbi60', 'qbi61', 'qbi62')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi60', 'qbi61', 'qbi62')])) == 0)
data_bi$qbi64_01 <- rowSums(data_bi[, c('qbi55', 'qbi59', 'qbi62', 'qbi64')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi55', 'qbi59', 'qbi62', 'qbi64')])) == 0)
data_bi$qbi66_01 <- rowSums(data_bi[, c('qbi63', 'qbi64', 'qbi65', 'qbi66')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi63', 'qbi64', 'qbi65', 'qbi66')])) == 0)
data_bi$qbi66_03 <- rowSums(data_bi[, c('qbi52', 'qbi53', 'qbi54','qbi55', 'qbi56', 'qbi57','qbi58', 'qbi59', 'qbi60','qbi61', 'qbi62', 'qbi63', 'qbi64','qbi65', 'qbi66')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi52', 'qbi53', 'qbi54','qbi55', 'qbi56', 'qbi57','qbi58', 'qbi59', 'qbi60','qbi61', 'qbi62', 'qbi63', 'qbi64', 'qbi65', 'qbi66')])) == 0) 
data_bi$qbi55_01 <- rowSums(data_bi[, c('qbi52', 'qbi53', 'qbi54', 'qbi55')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi52', 'qbi53', 'qbi54', 'qbi55')])) == 0)
data_bi$qbi55_02 <- (data_bi$qbi55_01/data_bi$qbi66_03)*100
data_bi$qbi56_02 <- (data_bi$qbi56_01/data_bi$qbi66_03)*100
data_bi$qbi59_02 <- (data_bi$qbi59_01/data_bi$qbi66_03)*100
data_bi$qbi60_02 <- (data_bi$qbi60_01/data_bi$qbi66_03)*100
data_bi$qbi61_02 <- (data_bi$qbi61_01/data_bi$qbi66_03)*100
data_bi$qbi62_02 <- (data_bi$qbi62_01/data_bi$qbi66_03)*100
data_bi$qbi64_02 <- (data_bi$qbi64_01/data_bi$qbi66_03)*100
data_bi$qbi66_02 <- (data_bi$qbi66_01/data_bi$qbi66_03)*100
data_bi$qbi68_01 <- (data_bi$qbi68/data_bi$qbi67)
data_bi$qbi69_01 <- (data_bi$qbi69/data_bi$qbi67)
data_bi$qbi69_02 <- rowSums(data_bi[, c('qbi67', 'qbi68', 'qbi69')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi67', 'qbi68', 'qbi69')])) == 0)
data_bi$qbi69_03 <- (data_bi$qbi69_02/data_bi$census_01)*10000

data_bi$qbi71_01 <- rowSums(data_bi[, c('qbi70', 'qbi71')], na.rm = TRUE)*NA^(rowSums(!is.na(data_bi[, c('qbi70', 'qbi71')])) == 0)

data_bi$qbi70_01 <- (data_bi$qbi70/data_bi$qbi71_01)*100

data_bi$qbi66_04 <- (data_bi$qbi66_03/data_bi$qbi71_01)*100
data_bi$qbi71_02 <- (data_bi$qbi71_01/data_bi$census_01)

# wsww
# ws
data_wsww$qws05_01 <- rowSums(data_wsww[, c('qws01', 'qws03', 'qws05')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qws01', 'qws03', 'qws05')])) == 0)
data_wsww$qws07_01 <- rowSums(data_wsww[, c('qws05_01', 'qws07')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qws05_01', 'qws07')])) == 0)
data_wsww$qws07_02 <- (data_wsww$qws07/data_wsww$qws07_01)*100
data_wsww$qws09_01 <- rowSums(data_wsww[, c('qws08', 'qws09')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qws08', 'qws09')])) == 0)
data_wsww$qws09_03 <- (data_wsww$qws09/data_wsww$qws37)
data_wsww$qws10_01 <- (data_wsww$qws10/data_wsww$qws08)*100
data_wsww$qws11_01 <- (data_wsww$qws11/data_wsww$qws08)*100
data_wsww$qws12_01 <- rowSums(data_wsww[, c('qws11', 'qws12')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qws11', 'qws12')])) == 0)
data_wsww$qws12_02 <- (data_wsww$qws12/data_wsww$qws12_01)*100
data_wsww$qws13_01 <- (data_wsww$qws13/data_wsww$qws09_01)*100
data_wsww$qws14_01 <- (data_wsww$qws14/data_wsww$qws09_01)*100
data_wsww$qws14_02 <- (data_wsww$qws14/data_wsww$qws39)
data_wsww$qws15_01 <- (data_wsww$qws09_01/data_wsww$qws15)*100
data_wsww$qws16_01 <- (data_wsww$qws07_01/data_wsww$qws16)
data_wsww$qws17_01 <- (data_wsww$qws09_01/data_wsww$qws17)
data_wsww$qws18_01 <- (data_wsww$qws09_01/data_wsww$qws18)
data_wsww$qws19_01 <- (data_wsww$qws19/data_wsww$qws09_01)
data_wsww$qws31_01 <- (data_wsww$qws31/data_wsww$qws09_01)

data_wsww$qws35_01 <- rowSums(data_wsww[, c('qws34', 'qws35')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qws34', 'qws35')])) == 0)

data_wsww$qws32_01 <- (data_wsww$qws32/data_wsww$qws35_01)
data_wsww$qws33_01 <- (data_wsww$qws33/data_wsww$qws32)

data_wsww$qws09_02 <- (data_wsww$qws09_01/data_wsww$qws35_01)*10000
data_wsww$qws36_01 <- (data_wsww$qws36/data_wsww$qws39)
data_wsww$qws40_01 <- (data_wsww$qws40/data_wsww$qws41)*100
data_wsww$qws43_01 <- (data_wsww$qws43/data_wsww$qws42)*100
data_wsww$qws44_01 <- (data_wsww$qws44/data_wsww$qws35_01)
data_wsww$qws46_01 <- rowSums(data_wsww[, c('qws45', 'qws46')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qws45', 'qws46')])) == 0)
data_wsww$qws09_04 <- (data_wsww$qws46_01/data_wsww$qws08)
data_wsww$qws46_02 <- (data_wsww$qws45/data_wsww$qws46_01)*100
data_wsww$qws46_03 <- (data_wsww$qws46_01/data_wsww$qws35_01)

# ww
data_wsww$qww02_01 <- rowSums(data_wsww[, c('qww01', 'qww02')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qww01', 'qww02')])) == 0)
data_wsww$qww02_02 <- (data_wsww$qww02_01/data_wsww$qww34)
data_wsww$qww03_01 <- (data_wsww$qww03/data_wsww$qww02_01)*10000
data_wsww$qww04_01 <- (data_wsww$qww04/data_wsww$qww03)*100
data_wsww$qww04_02 <- (data_wsww$qww04/data_wsww$qww36)
data_wsww$qww05_01 <- (data_wsww$qww05/data_wsww$qww02_01)*10000
data_wsww$qww05_02 <- (data_wsww$qww05/data_wsww$qww04)*100
data_wsww$qww06_01 <- (data_wsww$qww06/data_wsww$qww05)*100
data_wsww$qww07_01 <- (data_wsww$qww04/data_wsww$qww07)
data_wsww$qww08_01 <- (data_wsww$qww04/data_wsww$qww08)
data_wsww$qww10_01 <- rowSums(data_wsww[, c('qww09', 'qww10')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qww09', 'qww10')])) == 0)
data_wsww$qww10_02 <- (data_wsww$qww09/data_wsww$qww10)
data_wsww$qww12_01 <- (data_wsww$qww12/data_wsww$qww11)*100
data_wsww$qww15_01 <- (data_wsww$qww15/data_wsww$qww04)
data_wsww$qww16_01 <- (data_wsww$qww16/data_wsww$qww04)
data_wsww$qww19_01 <- (data_wsww$qww19/data_wsww$qww10_01)
data_wsww$qww21_01 <- (data_wsww$qww21/data_wsww$qww10_01)
data_wsww$qww23_01 <- (data_wsww$qww23/data_wsww$qww04)
data_wsww$qww24_01 <- (data_wsww$qww24/data_wsww$qww02_01)*10000
data_wsww$qww25_01 <- (data_wsww$qww25/data_wsww$qww04)
data_wsww$qww26_01 <- (data_wsww$qww26/data_wsww$qww04)
data_wsww$qww27_01 <- (data_wsww$qww27/data_wsww$qww10_01)
data_wsww$qww28_01 <- (data_wsww$qww28/data_wsww$qww10_01)
data_wsww$qww30_01 <- (data_wsww$qww30/data_wsww$qww04)
data_wsww$qww31_01 <- (data_wsww$qww31/data_wsww$qww04)
data_wsww$qww32_01 <- (data_wsww$qww32/data_wsww$qww04)
data_wsww$qww33_01 <- (data_wsww$qww33/data_wsww$qww02_01)
data_wsww$qww37_01 <- (data_wsww$qww37/data_wsww$qww38)*100
data_wsww$qww39_01 <- (data_wsww$qww39/data_wsww$qww02_01)
data_wsww$qww41_01 <- (data_wsww$qww41/data_wsww$qww40)*100

data_wsww$qww43_02 <- rowSums(data_wsww[, c('qww42', 'qww43')], na.rm = TRUE)*NA^(rowSums(!is.na(data_wsww[, c('qww42', 'qww43')])) == 0)

data_wsww$qww43_01 <- (data_wsww$qww42/data_wsww$qww43_02)*100

data_wsww$qww41_02 <- (data_wsww$qww41/data_wsww$qww43_02)*100
data_wsww$qww29_01 <- (data_wsww$qww29/data_wsww$qww43_02)*100
data_wsww$qww43_03 <- (data_wsww$qww43/data_wsww$qww10_01)
data_wsww$qww43_04 <- (data_wsww$qww43/data_wsww$qww04)

# others
# am
data_others$qam01_01 <- (data_others$qam01/data_others$census_01)*1000
data_others$qam01_02 <- (data_others$qam01/data_others$qam16)
data_others$qam02_01 <- (data_others$qam02/data_others$census_01)*1000
data_others$qam02_02 <- (data_others$qam02/data_others$qam16)
data_others$qam04_01 <- (data_others$qam04/data_others$qam01)*100
data_others$qam05_01 <- (data_others$qam05/data_others$qam01)*100
data_others$qam06_01 <- (data_others$qam06/data_others$qam01)
data_others$qam07_01 <- (data_others$qam07/data_others$qam01)
data_others$qam08_01 <- (data_others$qam08/data_others$qam06)*100
data_others$qam10_01 <- (data_others$qam10/data_others$qam16)
data_others$qam11_01 <- (data_others$qam11/data_others$qam07)
data_others$qam16_01 <- (data_others$qam16/data_others$census_01)*10000
data_others$qam18_04 <- rowSums(data_others[, c('qam17', 'qam17')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qam18', 'qam18')])) == 0)
data_others$qam18_03 <- (data_others$qam18_04/data_others$qam01)
data_others$qam18_02 <- (data_others$qam18_04/data_others$census_01)
data_others$qam18_01 <- (data_others$qam17/data_others$qam18_04)*100
data_others$qam14_01 <- (data_others$qam14/data_others$qam18_04)*100



# ec
data_others$qec01_01 <- (data_others$qec01/data_others$census_01)
data_others$qec01_02 <- (data_others$qec01/data_others$qec38)
data_others$qec02_01 <- (data_others$qec02/data_others$qec01)
data_others$qec03_01 <- (data_others$qec03/data_others$qec01)
data_others$qec04_01 <- (data_others$qec04/data_others$qec01)
data_others$qec09_01 <- (data_others$qec09/data_others$qec08)*100
data_others$qec10_01 <- (data_others$qec10/data_others$qec08)*100
data_others$qec11_01 <- (data_others$qec11/data_others$qec08)*100
data_others$qec12_01 <- (data_others$qec12/data_others$qec08)*100
data_others$qec13_01 <- (data_others$qec13/data_others$qec08)*100
data_others$qec20_01 <- (data_others$qec20/data_others$qec08)*100
data_others$qec25_01 <- (data_others$qec25/data_others$qec11)*100
data_others$qec26_01 <- (data_others$qec26/data_others$qec10)*100
data_others$qec27_01 <- (data_others$qec27/data_others$qec09)*100
data_others$qec28_01 <- (data_others$qec28/data_others$qec02)*100
data_others$qec30_01 <- (data_others$qec30/data_others$qec38)*100
data_others$qec31_01 <- (data_others$qec31/data_others$qec38)*100
data_others$qec32_01 <- (data_others$qec32/data_others$qec31)
data_others$qec35_01 <- (data_others$qec35/data_others$qec38)
data_others$qec36_01 <- (data_others$qec36/data_others$qec38)
data_others$qec38_01 <- (data_others$qec38/data_others$qec37)
data_others$qec38_02 <- (data_others$qec38/data_others$census_01)*10000

data_others$qec41_03 <- rowSums(data_others[, c('qec40', 'qec41')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qec40', 'qec41')])) == 0)

data_others$qec39_01 <- (data_others$qec39/data_others$qec41_03)*100
data_others$qec41_01 <- (data_others$qec40/data_others$qec41_03)*100
data_others$qec41_02 <- (data_others$qec41/data_others$census_01)

data_others$qec08_01 <- (data_others$qec41_03/data_others$qec08)

# fm

data_others$qfm72_03 <- rowSums(data_others[, c('qfm71', 'qfm72')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qfm71', 'qfm72')])) == 0)

data_others$qfm72_01 <- (data_others$qfm71/data_others$qfm72_03)*100

data_others$qfm72_02 <- (data_others$qfm72_03/data_others$census_01)

# fs
data_others$qfs01_01 <- (data_others$qfs01/data_others$census_01)*1000
data_others$qfs01_02 <- (data_others$qfs01/data_others$qfs42)
data_others$qfs02_01 <- (data_others$qfs02/data_others$census_01)*1000
data_others$qfs02_02 <- (data_others$qfs02/data_others$qfs42)
data_others$qfs03_01 <- (data_others$qfs03/data_others$qfs02)*100
data_others$qfs04_01 <- (data_others$qfs04/data_others$qfs03)*100
data_others$qfs05_01 <- (data_others$qfs05/data_others$qfs02)*100
data_others$qfs06_01 <- (data_others$qfs06/data_others$qfs02)*100
data_others$qfs07_01 <- (data_others$qfs07/data_others$qfs02)*100
data_others$qfs18_01 <- (data_others$qfs18/data_others$qfs03)*100
data_others$qfs19_01 <- (data_others$qfs19/data_others$qfs03)*100
data_others$qfs20_01 <- (data_others$qfs20/data_others$census_01)
data_others$qfs21_01 <- (data_others$qfs21/data_others$qfs02)*100
data_others$qfs22_01 <- (data_others$qfs22/data_others$qfs02)*100
data_others$qfs23_01 <- (data_others$qfs23/data_others$census_01)*100
data_others$qfs24_01 <- (data_others$qfs24/data_others$qfs23)*100
data_others$qfs25_01 <- (data_others$qfs25/data_others$census_01)*1000
data_others$qfs26_01 <- (data_others$qfs26/data_others$qfs25)*100
data_others$qfs27_01 <- (data_others$qfs27/data_others$census_01)*100
data_others$qfs28_01 <- (data_others$qfs28/data_others$census_01)*100
data_others$qfs29_01 <- (data_others$qfs29/data_others$census_01)*100
data_others$qfs32_01 <- (data_others$qfs32/data_others$qfs41)
data_others$qfs33_01 <- (data_others$qfs33/data_others$qfs32)*100
data_others$qfs34_01 <- (data_others$qfs34/data_others$census_01)
data_others$qfs35_01 <- (data_others$qfs35/data_others$qfs34)*100
data_others$qfs36_01 <- (data_others$qfs36/data_others$qfs42)
data_others$qfs37_01 <- (data_others$qfs37/data_others$qfs45)*100
data_others$qfs38_01 <- (data_others$qfs38/data_others$qfs42)
data_others$qfs39_01 <- (data_others$qfs39/data_others$qfs02)
data_others$qfs40_01 <- (data_others$qfs40/data_others$qfs02)
data_others$qfs42_01 <- (data_others$qfs42/data_others$census_01)*1000
data_others$qfs43_01 <- (data_others$qfs43/data_others$qfs42)
data_others$qfs44_01 <- (data_others$qfs44/data_others$qfs46)*100
data_others$qfs46_03 <- rowSums(data_others[, c('qfs45', 'qfs46')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qfs45', 'qfs46')])) == 0)
data_others$qfs46_01 <- (data_others$qfs45/data_others$qfs46_03)*100
data_others$qfs46_02 <- (data_others$qfs46_03/data_others$census_01)
data_others$qfs01_03 <- (data_others$qfs46_03/data_others$qfs01)

# hr
data_others$qhr01_01 <- (data_others$qhr01/data_others$census_01)*10000
data_others$qhr02_01 <- (data_others$qhr01/data_others$qhr02)
data_others$qhr03_01 <- (data_others$qhr03/data_others$qhr01)*100
data_others$qhr04_01 <- (data_others$qhr04/data_others$qhr02)*100
data_others$qhr05_01 <- (data_others$qhr05/data_others$qhr01)
data_others$qhr09_01 <- (data_others$qhr09/data_others$qhr01)*100
data_others$qhr10_01 <- (data_others$qhr10/data_others$qhr02)*100
data_others$qhr13_01 <- (data_others$qhr13/data_others$qhr03)*100
data_others$qhr14_01 <- (data_others$qhr14/data_others$qhr03)*100
data_others$qhr18_01 <- (data_others$qhr18/data_others$qhr01)*100
data_others$qhr19_01 <- (data_others$qhr19/data_others$qhr02)*100
data_others$qhr20_01 <- (data_others$qhr20/data_others$qhr18)*100
data_others$qhr21_01 <- (data_others$qhr21/data_others$qhr19)*100
data_others$qhr22_01 <- (data_others$qhr22/data_others$qhr18)*100
data_others$qhr23_01 <- (data_others$qhr23/data_others$qhr19)*100
data_others$qhr28_01 <- (data_others$qhr28/data_others$qhr03)*100
data_others$qhr31_01 <- (data_others$qhr31/data_others$qhr01)
data_others$qhr32_01 <- (data_others$qhr32/data_others$census_01)*1000
data_others$qhr33_01 <- (data_others$qhr32/data_others$qhr33)
data_others$qhr37_03 <- rowSums(data_others[, c('qhr36', 'qhr37')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qhr36', 'qhr37')])) == 0)
data_others$qhr37_01 <- (data_others$qhr36/data_others$qhr37_03)*100
data_others$qhr37_02 <- (data_others$qhr37_03/data_others$census_01)


# pr
data_others$qpr01_01 <- (data_others$qpr01/data_others$census_01)*10000
data_others$qpr02_01 <- (data_others$qpr02/data_others$census_01)*10000
data_others$qpr03_01 <- (data_others$qpr03/data_others$census_01)*10000
data_others$qpr04_01 <- (data_others$qpr04/data_others$census_01)*10000
data_others$qpr05_01 <- (data_others$qpr05/data_others$census_01)*10000
data_others$qpr06_01 <- (data_others$qpr06/data_others$census_01)*10000
data_others$qpr07_01 <- (data_others$qpr07/data_others$census_01)*10000
data_others$qpr09_01 <- (data_others$qpr09/data_others$census_01)*10000
data_others$qpr10_01 <- (data_others$qpr10/data_others$census_01)*10000
data_others$qpr14_01 <- (data_others$qpr14/data_others$census_01)*10000
data_others$qpr19_01 <- (data_others$qpr19/data_others$census_01)
data_others$qpr20_01 <- (data_others$qpr20/data_others$census_01)
data_others$qpr21_01 <- (data_others$qpr21/data_others$census_01)
data_others$qpr22_01 <- (data_others$qpr22/data_others$census_01)
data_others$qpr23_01 <- (data_others$qpr23/data_others$census_01)
data_others$qpr26_01 <- (data_others$qpr26/data_others$census_01)*10000
data_others$qpr29_01 <- (data_others$qpr29/data_others$qpr32)
data_others$qpr30_01 <- (data_others$qpr30/data_others$qpr32)
data_others$qpr31_01 <- (data_others$qpr31/data_others$qpr32)
data_others$qpr32_01 <- (data_others$qpr32/data_others$census_01)*10000
data_others$qpr33_01 <- (data_others$qpr33/data_others$qpr32)
data_others$qpr37_03 <- rowSums(data_others[, c('qpr36', 'qpr37')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qpr36', 'qpr37')])) == 0)
data_others$qpr34_01 <- (data_others$qpr34/data_others$qpr37_03)*100
data_others$qpr35_01 <- (data_others$qpr35/data_others$qpr37_03)*100
data_others$qpr37_01 <- (data_others$qpr36/data_others$qpr37_03)*100
data_others$qpr37_02 <- (data_others$qpr37_03/data_others$census_01)

# ps
data_others$qps01_01 <- (data_others$qps01/data_others$census_01)*10000
data_others$qps02_01 <- (data_others$qps02/data_others$census_01)*10000
data_others$qps03_01 <- (data_others$qps03/data_others$census_01)*10000
data_others$qps04_01 <- (data_others$qps04/data_others$census_01)*10000
data_others$qps04_02 <- rowSums(data_others[, c('qps01', 'qps02','qps03', 'qps04')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qps01', 'qps02','qps03', 'qps04')])) == 0)
data_others$qps05_01 <- (data_others$qps05/data_others$census_01)*10000
data_others$qps06_01 <- (data_others$qps06/data_others$census_01)*10000
data_others$qps07_01 <- (data_others$qps07/data_others$census_01)*10000
data_others$qps08_01 <- (data_others$qps08/data_others$census_01)*10000
data_others$qps08_02 <- rowSums(data_others[, c('qps05', 'qps06','qps07', 'qps08')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qps05', 'qps06','qps07', 'qps08')])) == 0)
data_others$qps09_01 <- (data_others$qps09/data_others$census_01)*1000
data_others$qps10_01 <- (data_others$qps10/data_others$census_01)*1000
data_others$qps11_01 <- (data_others$qps11/data_others$census_01)*1000
data_others$qps12_01 <- (data_others$qps12/data_others$qps39)*1000
data_others$qps13_01 <- (data_others$qps13/data_others$qps12)*100
data_others$qps14_01 <- (data_others$qps14/data_others$qps12)*100
data_others$qps15_01 <- (data_others$qps15/data_others$qps04_02)*100
data_others$qps16_01 <- (data_others$qps16/data_others$qps08_02)*100
data_others$qps17_01 <- (data_others$qps17/data_others$qps04_02)
data_others$qps18_01 <- (data_others$qps18/data_others$qps08_02)*100
data_others$qps20_01 <- (data_others$qps19/data_others$qps20)
data_others$qps20_02 <- rowSums(data_others[, c('qps19', 'qps20')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qps19', 'qps20')])) == 0)
data_others$qps21_01 <- (data_others$qps21/data_others$qps20_02)*100
data_others$qps22_01 <- (data_others$qps22/data_others$census_01)*1000
data_others$qps23_01 <- (data_others$qps23/data_others$census_01)*1000
data_others$qps24_01 <- (data_others$qps24/data_others$census_01)*1000
data_others$qps25_01 <- (data_others$qps25/data_others$census_01)*1000
data_others$qps26_01 <- (data_others$qps26/data_others$census_01)*1000
data_others$qps26_02 <- (data_others$qps26/data_others$qam01)
data_others$qps27_01 <- (data_others$qps27/data_others$qps39)
data_others$qps28_01 <- (data_others$qps28/data_others$census_01)
data_others$qps29_01 <- (data_others$qps29/data_others$qps39)
data_others$qps30_01 <- (data_others$qps30/data_others$qps29)*100
data_others$qps31_01 <- (data_others$qps31/data_others$qps29)*100
data_others$qps32_01 <- (data_others$qps32/data_others$qps30)*100
data_others$qps34_01 <- (data_others$qps34/data_others$qps28)*100
data_others$qps35_01 <- (data_others$qps35/data_others$qps39)
data_others$qps35_02 <- (data_others$qps35/data_others$qps29)*10000
data_others$qps36_01 <- (data_others$qps36/data_others$qps39)
data_others$qps37_01 <- (data_others$qps37/data_others$qps39)
data_others$qps38_01 <- (data_others$qps38/data_others$qps39)
data_others$qps39_01 <- (data_others$qps39/data_others$census_01)*1000
data_others$qps40_01 <- (data_others$qps39/data_others$qps40)
data_others$qps43_03 <- rowSums(data_others[, c('qps42', 'qps43')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qps42', 'qps43')])) == 0)
data_others$qps41_01 <- (data_others$qps41/data_others$qps43_03)*100
data_others$qps43_01 <- (data_others$qps42/data_others$qps43_03)*100
data_others$qps43_02 <- (data_others$qps43_03/data_others$census_01)

# re
data_others$qre01_01 <- (data_others$qre01/data_others$census_01)
data_others$qre01_02 <- (data_others$qre01/data_others$qre28)
data_others$qre13_01 <- rowSums(data_others[, c('qre02', 'qre03','qre04', 'qre05','qre06', 'qre07','qre08', 'qre09','qre10', 'qre11','qre12', 'qre13')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qre02', 'qre03','qre04', 'qre05','qre06', 'qre07','qre08', 'qre09','qre10', 'qre11','qre12', 'qre13')])) == 0)
data_others$qre16_01 <- (data_others$qre16/data_others$qre15)
data_others$qre17_01 <- (data_others$qre17/data_others$census_01)
data_others$qre18_01 <- (data_others$qre18/data_others$qre17)
data_others$qre21_01 <- (data_others$qre21/data_others$qre01)
data_others$qre22_01 <- (data_others$qre22/data_others$qre17)*100
data_others$qre24_01 <- (data_others$qre24/data_others$qre17)
data_others$qre25_01 <- (data_others$qre25/data_others$qre17)
data_others$qre27_01 <- (data_others$qre27/data_others$qre28)
data_others$qre28_01 <- (data_others$qre13_01/data_others$qre28)
data_others$qre28_02 <- (data_others$census_01/data_others$qre28)

data_others$qre30_03 <- rowSums(data_others[, c('qre29', 'qre30')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qre29', 'qre30')])) == 0)

data_others$qre30_01 <- (data_others$qre29/data_others$qre30_03)*100
data_others$qre30_02 <- (data_others$qre30_03/data_others$census_01)

data_others$qre01_03 <- (data_others$qre30_03/data_others$qre01)

# rr part 1
data_others$qrr01_01 <- (data_others$qrr01/data_others$census_01)
data_others$qrr01_02 <- (data_others$qrr01/data_others$qrr12)
data_others$qrr02_01 <- (data_others$qrr02/data_others$qrr01)
data_others$qrr03_01 <- (data_others$qrr03/data_others$census_01)
data_others$qrr05_01 <- (data_others$qrr05/data_others$qrr04)
data_others$qrr06_01 <- (data_others$qrr06/data_others$qrr03)
data_others$qrr07_01 <- (data_others$qrr07/data_others$qrr03)


data_others$qrr11_01 <- (data_others$qrr11/data_others$qrr12)
data_others$qrr12_01 <- (data_others$census_01/data_others$qrr12)

data_others$qrr14_03 <- rowSums(data_others[, c('qrr13', 'qrr14')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qrr13', 'qrr14')])) == 0)

data_others$qrr01_03 <- (data_others$qrr14_03/data_others$qrr01)

data_others$qrr14_01 <- (data_others$qrr13/data_others$qrr14_03)*100
data_others$qrr14_02 <- (data_others$qrr14_03/data_others$census_01)


# yw

data_others$qyw01_01 <- (data_others$qyw01/data_others$census_01)
data_others$qyw01_02 <- (data_others$qyw01/data_others$qyw10)
data_others$qyw02_01 <- (data_others$qyw02/data_others$census_01)
data_others$qyw04_01 <- (data_others$qyw03/data_others$qyw04)
data_others$qyw05_01 <- (data_others$qyw05/data_others$qyw02)
data_others$qyw06_01 <- (data_others$qyw06/data_others$qyw02)
data_others$qyw07_01 <- (data_others$qyw07/data_others$qyw10)
data_others$qyw10_01 <- (data_others$census_01/data_others$qyw10)

data_others$qyw12_03 <- rowSums(data_others[, c('qyw11', 'qyw12')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qyw11', 'qyw12')])) == 0)
data_others$qyw01_03 <- (data_others$qyw12_03/data_others$qyw01)
data_others$qyw12_02 <- (data_others$qyw12_03/data_others$census_01)

data_others$qyw12_01 <- (data_others$qyw11/data_others$qyw12_03)*100

# rr part 2

data_others$qrr14_04 <- rowSums(data_others[, c('qyw12_03', 'qrr14_03', 'qre30_03')], na.rm = TRUE)*NA^(rowSums(!is.na(data_others[, c('qyw12_03', 'qrr14_03', 'qre30_03')])) == 0)

data_others$qrr08_01 <- (data_others$qrr08/data_others$qrr14_04)*100




# checking the above result
data_others |>
  # dplyr::filter(a_service !="Budgeting") |>
  dplyr::group_by(a_service) |>
  dplyr::count()


# ----------------------------------------------------------------------------
# NA summention issue
# -----------------------------------------------------------------------------
# sum1 <-1 + NA
# sum1 #== NA
# sum2 <- sum(c(1, NA), na.rm = TRUE)
# sum2 #== 1



# save the 3 data objects for not repeating the above long steps
readr::write_rds(data_bi, file="data_bi.rds")
readr::write_rds(data_wsww, file="data_wsww.rds")
readr::write_rds(data_others, file = "data_others.rds")




# remove population column
data_others <- data_others |> dplyr::select(!c(census_01)) 
data_bi     <- data_bi     |> dplyr::select(!c(census_01)) 



# rename the following columns
colnames(data_others)[1:3] <- c("Year", "Municipality", "Service")
colnames(data_bi)[1:3] <- c("Year", "Municipality", "Service")
colnames(data_wsww)[1:3] <- c("Year", "Municipality", "Service")


# Remove all infinite values
# does the below actually work?
#data_others[data_others == "Inf"] <- NaN

# community-accepted solution
# df[sapply(df, is.infinite)] <- NA
# there are Inf and NaN in the wsww data file
data_others[sapply(data_others, is.infinite)] <- NA
data_bi[sapply(data_bi, is.infinite)] <- NA
data_wsww[sapply(data_wsww, is.infinite)] <- NA


# qws31_1 has several NaN
data_others[sapply(data_others, is.nan)] <- NA
data_bi[sapply(data_bi, is.nan)] <- NA
data_wsww[sapply(data_wsww, is.nan)] <- NA



# =============================================================================
# Part III
# =============================================================================
# via data set
# original way and still used
# 
qrr <- names(data_others[1,grepl("qrr", names(data_others))])
qre <- names(data_others[1,grepl("qre", names(data_others))])
qyw <- names(data_others[1,grepl("qyw", names(data_others))])
qps <- names(data_others[1,grepl("qps", names(data_others))])
qec <- names(data_others[1,grepl("qec", names(data_others))])
qam <- names(data_others[1,grepl("qam", names(data_others))])
qfs <- names(data_others[1,grepl("qfs", names(data_others))])
qfm <- names(data_others[1,grepl("qfm", names(data_others))])
qhr <- names(data_others[1,grepl("qhr", names(data_others))])
qpr <- names(data_others[1,grepl("qpr", names(data_others))])
# qbf <- names(data_others[1,grepl("qbf", names(data_others))]) # no longer used

qbi <- names(data_bi[1,grepl("qbi", names(data_bi))])

qws <- names(data_wsww[1,grepl("qws", names(data_wsww))])
qww <- names(data_wsww[1,grepl("qww", names(data_wsww))])



# Objects required for audit-file generation 
# A list of all metrics in each service to generate the audit file

# others
# 
# full-name case
columns_per_service_list_others <- list(
  "Asphalt Maintenance" = qam,
  # "Budgeting" = qbf, # no longer used
  # "Building Inspection" = qbi,
  "Human Resources" = qhr,
  "Emergency Communications" = qec,
  "Fire" = qfs,
  "Fleet Maintenance" = qfm,
  "Recycling" = qre,
  "Parks and Recreation" = qpr,
  "Police Service" = qps,
  "Residential Refuse Collection" = qrr,
  # "Wastewater Service" = qww,
  # "Water Utility" = qws,
  "Yard Waste" = qyw
)

# acronym case
columns_per_service_list_others_x <- list(
  "am" = qam,
  #"bf" = qbf, no longer used
  # "Building Inspection" = qbi,
  "hr" = qhr,
  "ec" = qec,
  "fs" = qfs,
  "fm" = qfm,
  "re" = qre,
  "pr" = qpr,
  "ps" = qps,
  "rr" = qrr,
  # "Wastewater Service" = qww,
  # "Water Utility" = qws,
  "yw" = qyw
)

# bi
# full-name case
columns_per_service_list_bi <- list(
  "Building Plan Review, Permit, and Inspections" = qbi
)

# acronym case
columns_per_service_list_bi_x <- list(
  "bi" = qbi
)

# wsww
# full-name case
columns_per_service_list_wsww <- list(
  "Wastewater Service" = qww,
  "Water Utility" = qws
)

# acronym case
columns_per_service_list_wsww_x <- list(
  "ww" = qww,
  "ws" = qws
)

# acronym case for all



columns_per_service_list <- list(
  "am" = qam,
  "bi" = qbi,
  # "bf" = qbf, # no longer used
  "hr" = qhr,
  "ec" = qec,
  "fs" = qfs,
  "fm" = qfm,
  "re" = qre,
  "pr" = qpr,
  "ps" = qps,
  "rr" = qrr,
  "ww" = qww,
  "ws" = qws,
  "yw" = qyw
)


metrics_per_service_counter <-list()
for (srvc in names(columns_per_service_list)){
  metrics_per_service_counter[[srvcShrtToLngRefLst_all[[srvc]]]] =
   length(columns_per_service_list[[srvc]])
}
metrics_per_service_counter



# replace labels with 2-letter acronyms to reduce the size of a data file
# others case
data_others$Service <-  dplyr::recode(data_others$Service , 
                                      
                                      `Asphalt Maintenance`='am',
                                      # `Building Inspection`='bi',
                                      # `Budgeting` ='bf',
                                      `Human Resources`='hr',
                                      `Emergency Communications`='ec',
                                      `Fire`='fs',
                                      `Fleet Maintenance`='fm',
                                      `Recycling`='re',
                                      `Parks and Recreation`='pr',
                                      `Police Service`='ps',
                                      `Residential Refuse Collection`='rr',
                                      # `Wastewater Service`='ww',
                                      # `Water Utility`='ws',
                                      `Yard Waste`='yw')


# checking the above result
data_others |>
  # dplyr::filter(a_service !="Budgeting") |>
  dplyr::group_by(Service) |>
  dplyr::count()


# bi case
data_bi$Service <-  dplyr::recode(data_bi$Service, 
                                  
                                  # `Asphalt Maintenance`='am',
                                  `Building Plan Review, Permit, and Inspections`='bi'
                                  # `Central Human Resources`='hr',
                                  # `Emergency Communications`='ec',
                                  # `Fire`='fs',
                                  # `Fleet Maintenance`='fm',
                                  # `Household Recycling`='re',
                                  # `Parks and Recreation`='pr',
                                  # `Police Service`='ps',
                                  # `Residential Refuse Collection`='rr',
                                  # `Wastewater Service`='ww',
                                  # `Water Utility`='ws',
                                  # `Yard Waste/Leaf Collection`='yw'
)
# wsww case
data_wsww$Service <-  dplyr::recode(data_wsww$Service , 
                                    
                                    # `Asphalt Maintenance`='am',
                                    # `Building Inspection`='bi',
                                    # `Central Human Resources`='hr',
                                    # `Emergency Communications`='ec',
                                    # `Fire`='fs',
                                    # `Fleet Maintenance`='fm',
                                    # `Household Recycling`='re',
                                    # `Parks and Recreation`='pr',
                                    # `Police Service`='ps',
                                    # `Residential Refuse Collection`='rr',
                                    `Wastewater Service`='ww',
                                    `Water Utility`='ws',
                                    # `Yard Waste/Leaf Collection`='yw'
)


# !!!! caution !!!!
# The below step requires the magrittr pipe (%>%), i.e., the native pipe does not work
# library(tidyverse)
# 
# This block is for generating an audit file
data_others_round <- data_others %>% dplyr::mutate_if(is.numeric, ~ round(., digits = 3))
data_bi_round <- data_bi %>% dplyr::mutate_if(is.numeric, ~ round(., digits = 3))
data_wsww_round <- data_wsww %>% dplyr::mutate_if(is.numeric, ~ round(., digits = 3))



# 
# to generate an audit file,
# see ./support_fy2025.R
# -----------------------------------------------------------------------------
# required files
# 
# Data
# data_bi
# data_wsww
# data_others


# metric-definition file
# MetricNames_bi
# MetricNames_wsww
# MetricNames_others
# 
# 
# hash table
# full-name type
# columns_per_service_list_bi
# columns_per_service_list_wsww
# columns_per_service_list_others

# or 
# acronym type
# columns_per_service_list_others_x
# columns_per_service_list_bi_x
# columns_per_service_list_wsww_x

# =============================================================================
# Part IV
# =============================================================================

# -----------------------------------------------------------------------------
# others
# -----------------------------------------------------------------------------



key2value  <- c('am'=1,'hr'=2,'ec'=3,'fs'=4,'fm'=5,'re'=6,'pr'=7,'ps'=8,
                'rr'=9,'yw'=10)
key2value


qcl2srvc <- paste("q", names(key2value), sep="")
qcl2srvc

list_tbbl1 <-list()
for (i in key2value){
  list_tbbl1[[i]] <- data_others |> dplyr::filter(Service == names(key2value)[i] ) |> 
    dplyr::select(Year, Municipality, Service, tidyselect::starts_with(qcl2srvc[i])) |>
    tidyr::pivot_longer(cols = (tidyselect::starts_with("q")), names_to = "Variable", values_to = "Value")
}




combined <- dplyr::bind_rows(list_tbbl1)
combined
# check the dimension
dim(combined) # 31472 x 5



### census data to be added to the final dataset

# 2024-11-01 update
# to serialize bd_census_data_others_2024_11_01 object above for future uses
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# note: this file is not used when the dashboard is running
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
readr::write_rds(bd_census_data_others_2024_11_01, file="bd_census_data_others.rds")
bd_census_data_others <- bd_census_data_others_2024_11_01

### correct the data-type disagreements


combined <- combined |> 
  dplyr::select(Municipality, Variable, Year, Service, Value) |>
  dplyr::mutate(Year = as.integer(Year))


### final step


df_combined_others <- dplyr::bind_rows(list(combined, bd_census_data_others))
# df_combined

# checking the result
df_combined_others |> dplyr::group_by(Service) |> dplyr::tally() 


### To complete the combined data


bd_data_imp_others <- df_combined_others |> tidyr::complete(Municipality, Variable, Year)
# bd_data_imp

# checking the result
bd_data_imp_others |> dplyr::group_by(Service) |> dplyr::tally() 

### To correct Service == NA rows, replace them with data from Variable column


# the above complete create a row whose service column is NA like below
# (Asheville did not report its data of 2020 for am01)
# 3029 Asheville  qam01 2020 NA  NA 
# 3030 Asheville  qam01 2021 am  8.110000e+02
# Thus, service column whose value is NA must be filled by using data in 
# variable column

# function to be used for the above replacement
service_token <-function(x){
  token <- stringr::str_match(x, "^(census)_\\d|^q([a-z]+)\\d")[2]
  
  if( is.na(token)){
    token <-  stringr::str_match(x, "^(census)_\\d|^q([a-z]+)\\d")[3]
  }
  token
}

# replacing NAs
tmp_result_others <- bd_data_imp_others |> dplyr::rowwise() |> 
  dplyr::mutate(Service = service_token(Variable)) 


# checking the result
tmp_result_others |> dplyr::group_by(Service) |> dplyr::tally() 

# dim(bd_data_imp)
# dim(tmp_result)
# tmp_result


### step 4-4: saving the completed file as an rds file


readr::write_rds(tmp_result_others, file="bd_data_completed7_others.rds")
#tmp_result_others <- readr::read_rds(file="bd_data_completed7_others.rds")




### the finalized data: contents check


# tmp_result_others |> dplyr::select(Service, Variable) |>
#   dplyr::group_by(Service) |>
#   dplyr::distinct(Variable) |>
#   dplyr::summarize(Freq=dplyr::n()) |> knitr::kable()

# the of others part
# -----------------------------------------------------------------------------
# bi case
# -----------------------------------------------------------------------------
key2value_bi  <- c('bi'=1)
key2value_bi
qcl2srvc_bi <- paste("q", names(key2value_bi), sep="")
qcl2srvc_bi

list_tbbl1_bi <-list()
for (i in key2value_bi){
  list_tbbl1_bi[[i]] <- data_bi |> dplyr::filter(Service == names(key2value_bi)[i] ) |> 
    dplyr::select(Year, Municipality, Service, tidyselect::starts_with(qcl2srvc_bi[i])) |>
    tidyr::pivot_longer(cols = (tidyselect::starts_with("q")), names_to = "Variable", values_to = "Value")
}


combined_bi <- dplyr::bind_rows(list_tbbl1_bi)
combined_bi


### correct the data-type disagreements
combined_bi <- combined_bi |> 
  dplyr::select(Municipality, Variable, Year, Service, Value) |>
  dplyr::mutate(Year = as.integer(Year))
combined_bi


df_combined_bi <- combined_bi

### to complete the combined data
bd_data_imp_bi <- df_combined_bi |> tidyr::complete(Municipality, Variable, Year)
# bd_data_imp
bd_data_imp_bi |> dplyr::group_by(Service) |> dplyr::tally() 

### Mine: To correct Service == NA rows; replace them with data from Variable column
tmp_result_bi <- bd_data_imp_bi |> dplyr::rowwise() |> 
  dplyr::mutate(Service = service_token(Variable)) 

tmp_result_bi |> dplyr::group_by(Service) |> dplyr::tally() 
dim(tmp_result_bi) ## 43090 x 5

### saving the completed file as an rds file

readr::write_rds(tmp_result_bi, file="bd_data_completed7_bi.rds")
#tmp_result_bi <- readr::read_rds(file="bd_data_completed7_bi.rds")


# -----------------------------------------------------------------------------
# wsww
# -----------------------------------------------------------------------------
key2value_wsww  <- c('ww'=1,'ws'=2)
key2value_wsww
qcl2srvc_wsww <- paste("q", names(key2value_wsww), sep="")
qcl2srvc_wsww

list_tbbl1_wsww <-list()
for (i in key2value_wsww){
  list_tbbl1_wsww[[i]] <- data_wsww |> dplyr::filter(Service == names(key2value_wsww)[i] ) |> 
    dplyr::select(Year, Municipality, Service, tidyselect::starts_with(qcl2srvc_wsww[i])) |>
    tidyr::pivot_longer(cols = (tidyselect::starts_with("q")), names_to = "Variable", values_to = "Value")
}

combined_wsww <- dplyr::bind_rows(list_tbbl1_wsww)
combined_wsww


### correct the data-type disagreements
combined_wsww <- combined_wsww |> 
  dplyr::select(Municipality, Variable, Year, Service, Value) |>
  dplyr::mutate(Year = as.integer(Year))
combined_wsww

df_combined_wsww <- combined_wsww

### To complete the combined data

bd_data_imp_wsww <- df_combined_wsww |> tidyr::complete(Municipality, Variable, Year)
# bd_data_imp
bd_data_imp_wsww |> dplyr::group_by(Service) |> dplyr::tally() 

### To correct Service == NA rows; replace them with data from Variable column

tmp_result_wsww <- bd_data_imp_wsww |> dplyr::rowwise() |> 
  dplyr::mutate(Service = service_token(Variable)) 

tmp_result_wsww |> dplyr::group_by(Service) |> dplyr::tally() 


### saving the completed file as an rds file

readr::write_rds(tmp_result_wsww, file="bd_data_completed7_wsww.rds")
#tmp_result_wsww <- readr::read_rds(file="bd_data_completed7_wsww.rds")

#------------------------------------------------------------------------------

# =============================================================================
# Part V
# =============================================================================
# creating auxiliary files, which were previously included in date_update.R as
# the last block

# The following steps generate auxiliary key-value objects




# all_label_def_data_csv
# 
# intermediary object
# not serialized
# This object is partially synced with the survey-data-update cycle
# This object cannot be immediately used;
# see the below transformation when all_label_def_data is derived
# all_label_def_data_csv <- MetricNames |>
#   dplyr::rename(var_name = Code, var_label = Name, var_def = Definition)


# others
all_label_def_data_csv_others <- MetricNames_others |>
  dplyr::rename(var_name = code_new, var_label = metrics, var_def = definitions)
all_label_def_data_csv_others


# bi
all_label_def_data_csv_bi <- MetricNames_bi |>
  dplyr::rename(var_name = code_new, var_label = metrics, var_def = definitions)
all_label_def_data_csv_bi


# wsww
all_label_def_data_csv_wsww <- MetricNames_wsww |>
  dplyr::rename(var_name = code_new, var_label = metrics, var_def = definitions)
all_label_def_data_csv_wsww




#### Step 2 var2varvalue
# intermediary use; 
# not serialized
# This object is relatively stable
# can be read back from an external file but this size may be too small and
# reading-back may be costly
# key2value <- c('am'=1,'bi'=2,'hr'=3,'ec'=4,'fs'=5,'fm'=6,'re'=7,'pr'=8,'ps'=9,
#                'rr'=10,'ww'=11,'ws'=12,'yw'=13,'census'=14)

# others
# 
key2value_others  <- c('am'=1,'hr'=2,'ec'=3,'fs'=4,'fm'=5,'re'=6,'pr'=7,'ps'=8,
                'rr'=9,'yw'=10,'census'=11)
key2value_others

var2varvalue_others <-list()
# key2value['am']
# names(key2value)[1]
for (row in 1:length(names(key2value_others))){
  var2varvalue_others[[ names(key2value_others)[row] ]] <- row
}
var2varvalue_others

# bi
key2value_bi  <- c('bi'=1)
key2value_bi

var2varvalue_bi <-list()
# key2value['am']
# names(key2value)[1]
for (row in 1:length(names(key2value_bi))){
  var2varvalue_bi[[ names(key2value_bi)[row] ]] <- row
}
var2varvalue_bi

# wsww
key2value_wsww  <- c('ww'=1,'ws'=2)
key2value_wsww

var2varvalue_wsww <-list()
# key2value['am']
# names(key2value)[1]
for (row in 1:length(names(key2value_wsww))){
  var2varvalue_wsww[[ names(key2value_wsww)[row] ]] <- row
}
var2varvalue_wsww


#### Step 3 all_label_def_data
# this object depends on how often all_label_def_data_csv (MetricNames) is updated
# currently helper.R does not read back this file
# intermediary object to generate other hash tables
# 
# adding two new columns (var_acr, var_order) to the original 
# This object is used for generating various lists
all_label_def_data_others <- 
  all_label_def_data_csv_others |> 
  dplyr::mutate(var_acr = stringr::str_extract(
    var_name, "^(census)_\\d|^q([a-z]+)\\d", group =2)) |>
  tidyr::replace_na(list(var_acr ='census')) |>
  dplyr::rowwise() |>
  dplyr::mutate(var_order = var2varvalue_others[[ var_acr ]]) |>
  dplyr::ungroup() |>
  dplyr::select(var_name, var_acr, var_order, var_label, var_def)
all_label_def_data_others
# no need to serialize this object
# readr::write_rds(all_label_def_data, "all_label_def_data6.rds")


# bi
all_label_def_data_bi <- 
  all_label_def_data_csv_bi |> 
  dplyr::mutate(var_acr = stringr::str_extract(
    var_name, "^(census)_\\d|^q([a-z]+)\\d", group =2)) |>
  tidyr::replace_na(list(var_acr ='census')) |>
  dplyr::rowwise() |>
  dplyr::mutate(var_order = var2varvalue_bi[[ var_acr ]]) |>
  dplyr::ungroup() |>
  dplyr::select(var_name, var_acr, var_order, var_label, var_def)
all_label_def_data_bi

# wsww
all_label_def_data_wsww <- 
  all_label_def_data_csv_wsww |> 
  dplyr::mutate(var_acr = stringr::str_extract(
    var_name, "^(census)_\\d|^q([a-z]+)\\d", group =2)) |>
  tidyr::replace_na(list(var_acr ='census')) |>
  dplyr::rowwise() |>
  dplyr::mutate(var_order = var2varvalue_wsww[[ var_acr ]]) |>
  dplyr::ungroup() |>
  dplyr::select(var_name, var_acr, var_order, var_label, var_def)
all_label_def_data_wsww


#### all_varNameToLabel
# serialized
# called back by helper.R
# derived from all_label_def_data
# 
# others
all_varNameToLabel_others <- all_label_def_data_others |> 
  dplyr::select(var_name, var_label, var_acr, var_order)
all_varNameToLabel_others

# bi
all_varNameToLabel_bi <- all_label_def_data_bi |> 
  dplyr::select(var_name, var_label, var_acr, var_order)
all_varNameToLabel_bi

# wsww
all_varNameToLabel_wsww <- all_label_def_data_wsww |> 
  dplyr::select(var_name, var_label, var_acr, var_order)
all_varNameToLabel_wsww



# serialize the object
# all_label_def_data_others: this objects includes definitions
# var_name  var_acr var_order var_label var_def   
# 
# all_varNameToLabel_others: this objects lacks definitions
# var_name  var_label var_acr var_order
# The following line suggests that in spite of its name, its contents include
# definition data, more that its name suggests.
# 
# others
readr::write_rds(all_varNameToLabel_others, "all_varNameToLabel7_others.rds")

# bi
readr::write_rds(all_varNameToLabel_bi, "all_varNameToLabel7_bi.rds")

# wsww
readr::write_rds(all_varNameToLabel_wsww, "all_varNameToLabel7_wsww.rds")

#### v2lallinOne
#
# serialize
# called back by helper.R

# processing concerning all_varNameToLabel 
# This object must be serialized later

# others
v2lallinOne_others <- list()
for (row in 1:nrow(all_label_def_data_others)) {
  valueN <- all_label_def_data_others[row, "var_name"]
  valueL <- all_label_def_data_others[row, "var_label"]
  vl <- stats::setNames(as.list(valueL), valueN)
  v2lallinOne_others <- append(v2lallinOne_others, vl)
}

# bi
v2lallinOne_bi <- list()
for (row in 1:nrow(all_label_def_data_bi)) {
  valueN <- all_label_def_data_bi[row, "var_name"]
  valueL <- all_label_def_data_bi[row, "var_label"]
  vl <- stats::setNames(as.list(valueL), valueN)
  v2lallinOne_bi <- append(v2lallinOne_bi, vl)
}

# wsww
v2lallinOne_wsww <- list()
for (row in 1:nrow(all_label_def_data_wsww)) {
  valueN <- all_label_def_data_wsww[row, "var_name"]
  valueL <- all_label_def_data_wsww[row, "var_label"]
  vl <- stats::setNames(as.list(valueL), valueN)
  v2lallinOne_wsww <- append(v2lallinOne_wsww, vl)
}

#v2lallinOne_others
# serialize this object

# others
readr::write_rds(v2lallinOne_others, file = "v2lallinOne_others.rds")

# bi
readr::write_rds(v2lallinOne_bi, file = "v2lallinOne_bi.rds")

# wsww
readr::write_rds(v2lallinOne_wsww, file = "v2lallinOne_wsww.rds")

#### all_vname2def
#
# This table relies on only all_label_def_data that may be regularly updated
# to be read back after each data update
# 
# serialize
# called back by helper.R
# 
# others
all_vname2def_others <- list()
for (row in 1:nrow(all_label_def_data_others)) {
  valueN <- all_label_def_data_others[row, "var_name"]
  valueD <- all_label_def_data_others[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  all_vname2def_others <- append(all_vname2def_others, vl)
}

# bi
all_vname2def_bi <- list()
for (row in 1:nrow(all_label_def_data_bi)) {
  valueN <- all_label_def_data_bi[row, "var_name"]
  valueD <- all_label_def_data_bi[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  all_vname2def_bi <- append(all_vname2def_bi, vl)
}

# wsww
all_vname2def_wsww <- list()
for (row in 1:nrow(all_label_def_data_wsww)) {
  valueN <- all_label_def_data_wsww[row, "var_name"]
  valueD <- all_label_def_data_wsww[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  all_vname2def_wsww <- append(all_vname2def_wsww, vl)
}

# serialize this object
# others
readr::write_rds(all_vname2def_others, file = "all_vname2def_others.rds")

# bi
readr::write_rds(all_vname2def_bi, file = "all_vname2def_bi.rds")

# wsww
readr::write_rds(all_vname2def_wsww, file = "all_vname2def_wsww.rds")

# -----------------------------------------------------------------------------
# less frequently updated hash tables  
# -----------------------------------------------------------------------------
#### all_service_abbrev_to_full
# read back from an rds file
# previously read from an Excel worksheet
# The contents of this file is relatively stable, rarely updated 
# it is unlikely to update this object when new survey data are available


# call back the reference table
# others
# note: for FY 2025 "Recycling" instead of "Household Recycling"
# also "Human Resources" and "Yard Waste" 
# 
all_service_abbrev_to_full_others <- read_csv("all_service_abbrev_to_full_others_updated.csv", 
col_types = cols(OrderNo = col_integer()))
readr::write_rds(all_service_abbrev_to_full_others, file="all_service_abbrev_to_full_others.rds")
#all_service_abbrev_to_full_others <- readr::read_rds(file="all_service_abbrev_to_full_others.rds")

# bi
all_service_abbrev_to_full_bi <- read_csv("all_service_abbrev_to_full_bi.csv", 
                                              col_types = cols(OrderNo = col_integer()))
readr::write_rds(all_service_abbrev_to_full_bi, file="all_service_abbrev_to_full_bi.rds")
#all_service_abbrev_to_full_bi <- readr::read_rds(file="all_service_abbrev_to_full_bi.rds")


# wsww
all_service_abbrev_to_full_wsww <- read_csv("all_service_abbrev_to_full_wsww.csv", 
                                              col_types = cols(OrderNo = col_integer()))
readr::write_rds(all_service_abbrev_to_full_wsww, file="all_service_abbrev_to_full_wsww.rds")
#all_service_abbrev_to_full_wsww <- readr::read_rds(file="all_service_abbrev_to_full_wsww.rds")



# others
srvcShrtToLngRefLst_others <-list()
for (i in 1:dim(all_service_abbrev_to_full_others)[1]) {
  srvcShrtToLngRefLst_others[[all_service_abbrev_to_full_others$lc[i]]] <- all_service_abbrev_to_full_others$Full[i]
}
srvcShrtToLngRefLst_others

# bi

srvcShrtToLngRefLst_bi <-list()
for (i in 1:dim(all_service_abbrev_to_full_bi)[1]) {
  srvcShrtToLngRefLst_bi[[all_service_abbrev_to_full_bi$lc[i]]] <- all_service_abbrev_to_full_bi$Full[i]
}
srvcShrtToLngRefLst_bi


# wsww
srvcShrtToLngRefLst_wsww <-list()
for (i in 1:dim(all_service_abbrev_to_full_wsww)[1]) {
  srvcShrtToLngRefLst_wsww[[all_service_abbrev_to_full_wsww$lc[i]]] <- all_service_abbrev_to_full_wsww$Full[i]
}
srvcShrtToLngRefLst_wsww



# 1 srvclngToShrtRefLst
# This list relies on only all_service_abbrev_to_full that is less frequently
# updated
# others
srvclngToShrtRefLst_others <-list()
for (i in 1:dim(all_service_abbrev_to_full_others)[1]) {
  srvclngToShrtRefLst_others[[all_service_abbrev_to_full_others$Full[i]]] <- all_service_abbrev_to_full_others$lc[i]
}
srvclngToShrtRefLst_others
# bi
srvclngToShrtRefLst_bi <-list()
for (i in 1:dim(all_service_abbrev_to_full_bi)[1]) {
  srvclngToShrtRefLst_bi[[all_service_abbrev_to_full_bi$Full[i]]] <- all_service_abbrev_to_full_bi$lc[i]
}
srvclngToShrtRefLst_bi

# wsww
srvclngToShrtRefLst_wsww <-list()
for (i in 1:dim(all_service_abbrev_to_full_wsww)[1]) {
  srvclngToShrtRefLst_wsww[[all_service_abbrev_to_full_wsww$Full[i]]] <- all_service_abbrev_to_full_wsww$lc[i]
}
srvclngToShrtRefLst_wsww

# serialize this object
# others
readr::write_rds(srvclngToShrtRefLst_others, file="srvclngToShrtRefLst_others.rds")
# bi
readr::write_rds(srvclngToShrtRefLst_bi,     file="srvclngToShrtRefLst_bi.rds")
# wsww
readr::write_rds(srvclngToShrtRefLst_wsww,   file="srvclngToShrtRefLst_wsww.rds")



# 2 srvclngToShrtRefLstWoc, i.e., without census, service only 
# The above list without census-part
# others; this list is used to remove budget rows above line 366
srvclngToShrtRefLstWoc_others <-list()
for (i in 1:dim(all_service_abbrev_to_full_others)[1]) {
  if (all_service_abbrev_to_full_others$lc[[i]]=="census"){
    next
  }
  srvclngToShrtRefLstWoc_others[[all_service_abbrev_to_full_others$Full[i]]] <- all_service_abbrev_to_full_others$lc[i]
}

# bi
srvclngToShrtRefLstWoc_bi <-list()
for (i in 1:dim(all_service_abbrev_to_full_bi)[1]) {
  if (all_service_abbrev_to_full_others$lc[[i]]=="census"){
    next
  }
  srvclngToShrtRefLstWoc_bi[[all_service_abbrev_to_full_bi$Full[i]]] <- all_service_abbrev_to_full_bi$lc[i]
}
srvclngToShrtRefLstWoc_bi


# wsww
srvclngToShrtRefLstWoc_wsww <-list()
for (i in 1:dim(all_service_abbrev_to_full_wsww)[1]) {
  if (all_service_abbrev_to_full_wsww$lc[[i]]=="census"){
    next
  }
  srvclngToShrtRefLstWoc_wsww[[all_service_abbrev_to_full_wsww$Full[i]]] <- all_service_abbrev_to_full_wsww$lc[i]
}
srvclngToShrtRefLstWoc_wsww

# serialize this object
# others
readr::write_rds(srvclngToShrtRefLstWoc_others, file="srvclngToShrtRefLstWoc_others.rds")

# bi
readr::write_rds(srvclngToShrtRefLstWoc_bi, file="srvclngToShrtRefLstWoc_bi.rds")
# wsww
readr::write_rds(srvclngToShrtRefLstWoc_wsww, file="srvclngToShrtRefLstWoc_wsww.rds")

# 3 srv2varlbllst
# This object relies on all_varNameToLabel that is frequently updated
# 
# others
srv2varlbllst_others <-list()
for (srv in srvclngToShrtRefLst_others){
  # value is short form such as am
  tmp <- all_varNameToLabel_others |>
    dplyr::filter(var_acr==srv) |> dplyr::select(var_name, var_label)
  name_vec  <-tmp |> dplyr::pull(var_label)
  # print(name_vec)
  value_vec <- tmp |> dplyr::pull(var_name) 
  # print(value_vec)
  valueLst<-stats::setNames(as.list(value_vec), name_vec)
  srv2varlbllst_others[[srv]]  <- valueLst
}
srv2varlbllst_others

# bi
srv2varlbllst_bi <-list()
for (srv in srvclngToShrtRefLst_bi){
  # value is short form such as am
  tmp <- all_varNameToLabel_bi |>
    dplyr::filter(var_acr==srv) |> dplyr::select(var_name, var_label)
  name_vec  <-tmp |> dplyr::pull(var_label)
  # print(name_vec)
  value_vec <- tmp |> dplyr::pull(var_name) 
  # print(value_vec)
  valueLst<-stats::setNames(as.list(value_vec), name_vec)
  srv2varlbllst_bi[[srv]]  <- valueLst
}
#srv2varlbllst_bi

# wsww
srv2varlbllst_wsww <-list()
for (srv in srvclngToShrtRefLst_wsww){
  # value is short form such as am
  tmp <- all_varNameToLabel_wsww |>
    dplyr::filter(var_acr==srv) |> dplyr::select(var_name, var_label)
  name_vec  <-tmp |> dplyr::pull(var_label)
  # print(name_vec)
  value_vec <- tmp |> dplyr::pull(var_name) 
  # print(value_vec)
  valueLst<-stats::setNames(as.list(value_vec), name_vec)
  srv2varlbllst_wsww[[srv]]  <- valueLst
}
srv2varlbllst_wsww

# serialize this object
# others
readr::write_rds(srv2varlbllst_others, file="srv2varlbllst_others.rds")

# bi
readr::write_rds(srv2varlbllst_bi, file="srv2varlbllst_bi.rds")

# wsww
readr::write_rds(srv2varlbllst_wsww, file="srv2varlbllst_wsww.rds")
