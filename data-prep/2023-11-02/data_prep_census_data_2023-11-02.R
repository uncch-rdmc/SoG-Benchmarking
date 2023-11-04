###############################################################################
# as of 2023-10-31: after the newly updated census data-file was received

# the number of census variables was reduced to 9
# 
# file name: census_data_2023.xlsx
# sheet name: transposed

library(tidyverse)
library(readxl)
# 14 cities x 9 metrics
census_data <- readxl::read_excel("census_data_2023.xlsx", 
                                  sheet = "transposed")
View(census_data)
census_data

# step 1: create a long form of this dataset multipled by 4 years from 2020
# 
df_census <- census_data |> tidyr::gather(
  key=s_var, 
  value = s_value, 
  -c(municipality))
df_census


year_2020 <- df_census |> 
  dplyr::mutate(Service = "census", .before = s_value) |>
  dplyr::mutate(ServiceYear = strtoi("2020"), .before= Service)
year_2020
year_2021 <- df_census |> 
  dplyr::mutate(Service = "census", .before = s_value) |>
  dplyr::mutate(ServiceYear = strtoi("2021"), .before= Service)
year_2021
year_2022 <- df_census |> 
  dplyr::mutate(Service = "census", .before = s_value) |>
  dplyr::mutate(ServiceYear = strtoi("2022"), .before= Service)
year_2022
year_2023 <- df_census |> 
  dplyr::mutate(Service = "census", .before = s_value) |>
  dplyr::mutate(ServiceYear = strtoi("2023"), .before= Service)
year_2023

censusLst <-list(year_2020, year_2021, year_2022, year_2023)

df_census <- dplyr::bind_rows(censusLst)
df_census
df_census <- df_census |> dplyr::rename("Variable"="s_var", 
"Municipality"="municipality",
"Value"="s_value", "Year"="ServiceYear")
df_census

readr::write_rds(df_census, file="bd_census_data_2023.rds")

