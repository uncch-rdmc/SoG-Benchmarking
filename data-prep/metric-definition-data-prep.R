# metric-definition data-capture 
# metrics_def_data.xlsx
library(magrittr)
# -----------------------------------------------------------------------------
# step 1: read the workdbook and create service-wise objects
# -----------------------------------------------------------------------------

amr_1 <-
  readxl::read_excel("~/github/sog/bm2/SoG-Benchmarking/data-prep/metrics_def_data.xlsx",
                     sheet='Asph.')
View(amr_1)
# Code `Metric(s)` `DEFINITION(S)`
#   var_name var_label var_def
# Asph.
# Bldg.
# CHR
# Parks
# Emerg.
# Fire
# Fleet
# Recyclng
# Police
# Res. Refuse
# Wste Wtr
# Water
# Yrd Wste
# 
# step 1-0: read all worksheets of the metric-defintion workbook
setwd("~/github/sog/bm2/SoG-Benchmarking/data-prep")
amr_1 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Asph.')
bi_2 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Bldg.')
hr_3 <- readxl::read_excel('metrics_def_data.xlsx', sheet='CHR')
pr_4 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Parks')
ec_5 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Emerg.')
fs_6 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Fire')
fm_7 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Fleet')
hore_8 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Recyclng')
ps_9 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Police')
rrc_10 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Res. Refuse')
wws_11 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Wste Wtr')
ws_12 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Water')
yl_13 <- readxl::read_excel('metrics_def_data.xlsx', sheet='Yrd Wste')
deflst<- list(amr_1, bi_2, hr_3, pr_4, ec_5, fs_6, fm_7,
             hore_8, ps_9, rrc_10, wws_11, ws_12, yl_13)

def_all <- dplyr::bind_rows(deflst)

# Code `Metric(s)` `DEFINITION(S)`
#   var_name var_label var_def


defdf_all <- def_all |> 
  dplyr::rename( var_name=Code,
                 var_label=`Metric(s)`,  
                 var_def = `DEFINITION(S)`)
View(defdf_all)
# |>
#   dplyr::select(Municipality, Variable, Year, Service, Value)
# /home/asone/github/sog/bm2/SoG-Benchmarking/data-prep
write_rds(defdf_all, file="~/github/sog/bm2/SoG-Benchmarking/data-prep/metric_def_data_all.rds")

defdf_all_new <- readr::read_rds(file="~/github/sog/bm2/SoG-Benchmarking/data-prep/metric_def_data_all.rds")
view(defdf_all_new)

# var_def has many missing columns and it must be filled with var_label beforehand
# 
defdf_all <- defdf_all_new |> 
  dplyr::mutate(across(var_def, coalesce, var_label))
view(defdf_all)

which(is.na(defdf_all$var_label))

defdf_all <- defdf_all_new |> 
  tidyr::replace_na(list(var_label ="not available", var_def="not available")) |>
  dplyr::select(var_name, var_def)

which(is.na(defdf_all$var_def))
metric_def_data<- defdf_all

write_rds(metric_def_data, file="~/github/sog/bm2/SoG-Benchmarking/data-prep/metric_def_data.rds")
metric_def_data <- readr::read_rds(file="~/github/sog/bm2/SoG-Benchmarking/data-prep/metric_def_data.rds")


# checking the contents?
vname <- metric_def_data |> pull (var_name) 
#vname2def <- as.list(metric_def_data["var_name"])
vdef <- metric_def_data |> pull(var_def) 
result <- stats::setNames(as.list(vdef), vname)
result$qyl12



v2lallinOne <-list()
for (row in 1:nrow(all_varNameToLabel)) {
  valueN <- all_varNameToLabel[row, "var_name"]
  valueL <- all_varNameToLabel[row, "var_label"]
  vl <- stats::setNames(as.list(valueL), valueN)
  v2lallinOne <- append(v2lallinOne, vl)
}


vname2def <- list()
for (row in 1:nrow(metric_def_data)) {
  valueN <- metric_def_data[row, "var_name"]
  valueD <- metric_def_data[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  vname2def <- append(vname2def, vl)
}
vname2def$qyl12
################################################################################
# copy var_label as var_def 
censusOnly <- all_varNameToLabel |> 
  base::subset(all_varNameToLabel$var_order >= 14) |> 
  dplyr::select(var_name, var_label) |>
  dplyr::rename("var_def"="var_label")

all_metric_def_data <- dplyr::bind_rows(metric_def_data, censusOnly)

all_vname2def <- list()
for (row in 1:nrow(all_metric_def_data)) {
  valueN <- all_metric_def_data[row, "var_name"]
  valueD <- all_metric_def_data[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  all_vname2def <- append(all_vname2def, vl)
}
all_vname2def$qyl12
base::saveRDS(all_metric_def_data, file = "~/github/sog/bm2/SoG-Benchmarking/data-prep/all_metric_def_data.rds")

###############################################################################
# var_def is "not available"

censusOnlyNA<- all_varNameToLabel |> 
  base::subset(all_varNameToLabel$var_order >= 14) |> 
  dplyr::select(var_name) |>
  dplyr::mutate(var_def = "not available")

all_metric_def_data <- dplyr::bind_rows(metric_def_data, censusOnlyNA)

all_vname2def <- list()
for (row in 1:nrow(all_metric_def_data)) {
  valueN <- all_metric_def_data[row, "var_name"]
  valueD <- all_metric_def_data[row, "var_def"]
  vl <- stats::setNames(as.list(valueD), valueN)
  all_vname2def <- append(all_vname2def, vl)
}
# test
# all_vname2def$qyl12
# all_vname2def$census_04
# all_vname2def[["census_04"]]
# all_vname2def[["qbi12_05"]]


# base::saveRDS(all_metric_def_data, file = "~/github/sog/bm2/SoG-Benchmarking/data-prep/all_metric_def_data.rds")
base::saveRDS(all_vname2def, file = "~/github/sog/bm2/SoG-Benchmarking/data-prep/all_vname2def.rds")
all_vname2def <- readr::read_rds(file="all_vname2def.rds")

###############################################################################
df4DefTable1 <- tibble(
  Metric=c("Total aggregate of lane miles for the municipallity", ""),
  Definition=c("Lane miles", "")
)

print(df4DefTable1)


df4DefTable2 <- tibble(
  Metrics=c("Total aggregate of lane miles for the municipallity", "Land area in square miles, 2020"),
  Definitions=c("Lane miles", "not available")
)
print(df4DefTable2)

library(ggpubr)


# solution 
ggpubr::ggtexttable(df4DefTable2, rows = NULL,
                    
theme = ttheme(base_style = "blank",
               tbody.style = tbody_style(
                 size=8,
                 color="blue",
                 # family = "barlow",
                 hjust=0, x=0.01, fill = NA),
               colnames.style = colnames_style(
                 size = 8,
                 color="red",
                 # family = "barlow",
                 hjust=0, x=0.01, fill = NA))) |>
  ggpubr::tab_add_hline(at.row = c(1, 2), 
                        row.side = "top", linewidth = 2) |>
  ggpubr::tab_add_hline(at.row = c(3), 
                        row.side = "bottom", linewidth = 2)


ggpubr::ggtexttable(df4DefTable1, rows = NULL,
theme = ttheme("blank", tbody.style = tbody_style(hjust=0, x=0.01, fill = NA),
              colnames.style = colnames_style(hjust=0, x=0.01, fill = NA))) |>
  ggpubr::tab_add_hline(at.row = c(1, 2), 
                        row.side = "top", linewidth = 2) |>
  ggpubr::tab_add_hline(at.row = c(3), 
                        row.side = "top", linewidth = 2)

###############################################################################
createTextualTable <- function(dt, denominator=FALSE){
  if (denominator) {
    # denominator available
    
    ggpubr::ggtexttable(dt, rows = NULL,
        theme = ttheme(base_style="blank", 
            tbody.style = tbody_style(size=9, hjust=0, x=0.01, fill = NA),
            colnames.style = colnames_style(size=9, hjust=0, x=0.01, fill = NA))) |>
      ggpubr::tab_add_hline(at.row = c(1, 2), 
                            row.side = "top", linewidth = 2) |>
      ggpubr::tab_add_hline(at.row = c(3), 
                            row.side = "bottom", linewidth = 2)
    
    
  } else {
    # no denominator
    ggpubr::ggtexttable(dt, rows = NULL,
        theme = ttheme(base_style="blank", 
            tbody.style = tbody_style(size=9, hjust=0, x=0.01, fill = NA),
            colnames.style = colnames_style(siz=9, hjust=0, x=0.01, fill = NA))) |>
      ggpubr::tab_add_hline(at.row = c(1, 2), 
                            row.side = "top", linewidth = 2) |>
      ggpubr::tab_add_hline(at.row = c(3), 
                            row.side = "top", linewidth = 2)
    
  }
    
  
  
}

###############################################################################
ggpubr::ggtexttable(df4DefTable2, rows = NULL,
                    theme = ttheme("mBlue")) 

ggpubr::ggtexttable(df4DefTable2, rows = NULL,
                    theme = ttheme("light")) |>
  ggpubr::tab_add_border()

ggpubr::ggtexttable(df4DefTable2, rows = NULL,
                    theme = ttheme("light")) |>
  ggpubr::tab_add_hline(at.row = c(1), 
                        row.side = c("top"),
                        linewidth = 10)

###############################################################################
letters[1:5]
letters[c(1:3)]

str(all_vname2def)
len_vec <- nchar(all_vname2def)
sort(len_vec)
histogram(len_vec)
median(len_vec) # 70
mean(len_vec)   # 150
boxplot(len_vec)



