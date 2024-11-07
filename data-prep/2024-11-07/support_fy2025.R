# This is an updated version (2024-09) of dumpDataAsExcelWorkbook, which was coded in
# April 2024, for FY2025 data

# definition
# assumptions here:
# if the Service column of a dataset to be serialized uses each service's acronym
# rather than its fully spelled name, the keys of a list for `var_list` argument must be
# each service's acronym rather than its full name; if not, the keys must be 
# each service's full name, i.e., data-var_list consistency must be maintained
# 
# data$Service       var_list
# if acronym      => keys must be acronym too
# if full name    => keys must be full name too
# 
# This script is expected to run after the following objects are updated
# 
# data_others_round
# data_bi_round
# data_wsww_round
# 
# MetricNames_others
# MetricNames_bi
# MetricNames_wsww

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
# 


library(openxlsx2)
# important openxlsx2-related options
# openxlsx2
options("openxlsx2.string_nums" = TRUE)
options("openxlsx2.numFmt" = "#,##0.000")
options("openxlsx2.na.strings" = "")

# =============================================================================
# Functions
# 
#------------------------------------------------------------------------------
# new function that generates a list of data.frames to be dumped
# -----------------------------------------------------------------------------

generateListOfDataFrames <- function(srvyRspData, metricMetadata, var_list) {

  list_of_dfs <- list()
  
  # List of municipalities
  municipalities <- unique(srvyRspData$Municipality)
  #print(municipalities)
  
  # List of services
  services <- unique(srvyRspData$Service)
  service_fullname_set <- c()
  is_service_acronym <- ifelse(nchar(services[1])== 2, TRUE, FALSE)

  print(services)
  target_list <- 
    c("am", "hr", "ec", "fs", "fm", "pr", "ps", "re", "rr", "yw", "bi", "ws", "ww")

  target_list <- target_list[! target_list %in% c("am", "bi", "ws", "ww")]
  
  
  for (service in services) {

    # Select which set of questions to use based on the municipality
    print("service=")
    print(service)
    # Create an empty dataset with just the questions
    temp <-
      srvyRspData[srvyRspData$Service == service &
                    srvyRspData$Municipality == "None",]
    
    
    # Create an empty list
    
    list <- list()
    
    # Cycle through municipalities
    for (municipality in municipalities) {
      # If there exists any rows for that municipality/service combo, add it to that list
      if (nrow(srvyRspData[srvyRspData$Service == service &
                           srvyRspData$Municipality == municipality,]) > 0) {
        list <- append(list, municipality)
      }
    }
    
    
    for (municipality in municipalities) {
      # If it's in the list, merge them together
      if (municipality %in% unlist(list)) {
        temp2 <- srvyRspData[srvyRspData$Service == service &
                               srvyRspData$Municipality == municipality,]
        temp2 <-
          temp2[, names(temp2) %in% c("Year", "Municipality", var_list[[service]])]
        # Remove empty rows (rows that have more NAs than length of the row, accounting for Year + Municipality)
        isNonEmptyRow <- rowSums(is.na(temp2)) < (length(temp2) - 2)
        if (!all(isNonEmptyRow ==TRUE)){
          print("An empty row(s) found=")
          print(municipality)
          print("Is this non-empty?")
          print(isNonEmptyRow)
          print(temp2[c(1:10)])
        }

        
        
        temp2 <- temp2[rowSums(is.na(temp2)) < (length(temp2) - 2),]
        temp <- rbind(temp, temp2)
      }
    }
    
    # # original solution
    # # 
    # # sorting metric-columns 
    # # Budgeting-related columns are gathered in one place after asphalt(am)
    # # extract metric columns only
    # names_list2 <- names(temp)[-c(1,2)]
    # # sort metric columns
    # names_list2 <- sort(names_list2)
    # # get the first two columns
    # names_list1 <- names(temp)[ c(1,2)]
    # # combine the two (non-metric and metric columns)
    # names_list_sorted <- c(names_list1,names_list2 )
    
    # latest revised solution 
    # 
    # just move the budgeting-related two columns to the end of metric columns
    # not sort the non-budgeting columns
    
    # The following relocation is not applicable for service == am|bi|wsww

    if (service %in% target_list){
      # extract all metric columns
      names_list2all <-    names(temp)[-c(1,2)]
      # remove the first 2 budgeting columns
      names_list2_1  <- names_list2all[-c(1,2)]
      # slice the first 2-budgeting columns
      names_list2_2  <- names_list2all[ c(1,2)]
      # slice the non-metric columns: Year and Municipality
      names_list1    <- names(temp)[ c(1,2)]
      # combine all these 3 parts into one
      names_list_sorted <- c(names_list1,names_list2_1, names_list2_2)
      # sort columns according the above column name list
      temp <- temp[, names_list_sorted]
    }

    temp <-
      rbind(metricMetadata$metrics[match(names(temp), metricMetadata$code_new)], temp)
    # print(temp)
    temp[temp == "NaN"] <- ""
    #print(is.data.frame(temp))
    
    # print(str(temp))
    # Write the merged dataset for that service onto the sheet created
    #
    # AS: as.data.frame() seems necessary for Ubuntu environment
    # error message: if (is.na(value)) { : the condition has length > 1
    # see https://github.com/colearendt/xlsx/issues/129
    # esp. https://github.com/colearendt/xlsx/issues/129#issuecomment-1683996775
    # 
    service_string <- service
    if (is_service_acronym){
      service_string <- srvcShrtToLngRefLst_all[[service]]
    }
    
    # the following mapping may be irrelevant when the data had been updated
    if (service_string == "Yard Waste/Leaf Collection") {
      service_string <-"Yard Waste"
    } else if ( service_string == "Building Plan Review, Permit, and Inspections") {
      service_string <- "Building Inspection"
    } else if (service_string == "Central Human Resources") {
      service_string <- "Human Resources"
    } else if (service_string == "Core Parks and Recreation"){
      service_string <- "Parks and Recreation"
    }
    
    current <-list(as.data.frame(temp))
    
    names(current) <-c(service_string)
    list_of_dfs <-append(list_of_dfs, current)
    
  } # loop of service
  list_of_dfs
} # end of generateListOfDataFrames()

# -----------------------------------------------------------------------------
# function to generate a manifest worksheet
# -----------------------------------------------------------------------------
# 
generateManifestDataAsDataFrame <-function(dataUpdateDate_others="", 
dataUpdateDate_bi="",dataUpdateDate_wsww="", updateDate_metrics_definition="",
metricCounterList){
  fileMetadata <- tibble::tibble(
    metadata = c(
      "Survey Data Updated Date: Building Inspection",
      "Survey Data Updated Date: Utilities",
      "Survey Data Updated Date: Others",
      "Metrics: Last Updated Date",
      "Excel File Creation Date",
      "",
      "Service",
      names(metricCounterList)
    ),
    value =   c(
      dataUpdateDate_bi,
      dataUpdateDate_wsww,
      dataUpdateDate_others,
      updateDate_metrics_definition,
      
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "",
      "Metrics",
      unname(unlist(metricCounterList))
    )
  )
  list("manifest"= as.data.frame(fileMetadata))
  
} 

# =============================================================================
# end of functions
# =============================================================================


#------------------------------------------------------------------------------
# using the above functions
# -----------------------------------------------------------------------------

# prepare the manifest page
# update the date values when you run this function
# 
manifestDataFrame <- 
  generateManifestDataAsDataFrame(metricCounterList = metrics_per_service_counter,
  dataUpdateDate_bi ="2024-11-01" , dataUpdateDate_wsww = "2024-11-01",
  dataUpdateDate_others = "2024-11-01", updateDate_metrics_definition="2024-11-01")



# the following line may fail if "Budgeting" in Service column is not replaced
# with their respective service
# 
# Others
list_df4Excel_sg1 <-
generateListOfDataFrames(srvyRspData = data_others_round, 
                        metricMetadata = MetricNames_others,
                        var_list= columns_per_service_list_others_x)



# bi

list_df4Excel_sg2 <-
  generateListOfDataFrames(srvyRspData = data_bi_round, 
                           metricMetadata = MetricNames_bi,
                           var_list= columns_per_service_list_bi_x)


# wsww
list_df4Excel_sg3 <-
  generateListOfDataFrames(srvyRspData = data_wsww_round, 
                           metricMetadata = MetricNames_wsww,
                           var_list= columns_per_service_list_wsww_x)



# binding the above 3 data.frames and sorting worksheets by their name 
# except for the manifest worksheet
list_of_dfs <-c(list_df4Excel_sg1, list_df4Excel_sg2, list_df4Excel_sg3)
list_of_dfs <- list_of_dfs[order(names(list_of_dfs))]
list_of_dfs <-c(list_of_dfs, manifestDataFrame)
names_list_of_dfs <- names(list_of_dfs)


# create a workbook
audit_workbook <- openxlsx2::wb_workbook()

# add a worksheet
for (dfi in 1:length(list_of_dfs)){
  print(names_list_of_dfs[[dfi]])
  audit_workbook <-
  audit_workbook |>
    openxlsx2::wb_add_worksheet(sheet = names_list_of_dfs[[dfi]]) |>
    openxlsx2::wb_add_data_table(x = list_of_dfs[[dfi]])
}

# save the workbook
# update the date value when you run this line
openxlsx2::wb_save(wb = audit_workbook, file="CompleteAudited_2024-11-01.xlsx")

