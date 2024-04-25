# 
# 
# 
# # A loop that creates a different workbook for each municipality, and a different sheet for each service
# aggregations  srvyRspData 
# MetricNames   metricMetadata
# next step: add the overwrite option (default: TRUE, if FALSE, append the current
# time-stamp to the specified filename)
# 
# Function definition 
# The following arguments must be not NULL
# srvyRspData
# metricMetadata
# var_list
library(xlsx)

dumpDataAsExcelWorkbook <- function(srvyRspData, metricMetadata, filename=NULL, 
                                    creatorEmail="", var_list) {
  
  # List of municipalities
  municipalities <- unique(srvyRspData$Municipality)
  
  # List of services
  services <- unique(srvyRspData$Service)
  
  
  columns_per_s_counter <-list()
  for (srvc in names(var_list)){
    columns_per_s_counter[[srvc]] = length(var_list[[srvc]])
  }
  
  
  
  
  excelFileName <- ifelse(is.null(filename), paste0("CompleteAudited_",
                                                    format(Sys.time(), "%Y-%m-%d_%H%M%S"),
                                                    ".xlsx"), filename)
  
  fileMetadata <-tibble::tibble(
    metadata = c("Survey Data Updated Date", "File Creation Date", "File Creator", names(columns_per_s_counter)),
    value =   c("2024-04-09", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), creatorEmail, unname(unlist(columns_per_s_counter)))
  )
  
  
  xlsx::write.xlsx(
    as.data.frame(fileMetadata),
    file = excelFileName,
    sheetName = "manifest",
    append = TRUE,
    row.names = FALSE,
    col.names = TRUE,
    showNA = FALSE
  )
  
  
  
  for (service in services) {
    # Select which set of questions to use based on the municipality
    
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
        temp2 <- temp2[rowSums(is.na(temp2)) < (length(temp2) - 2),]
        temp <- rbind(temp, temp2)
      }
    }
    
    # temp is list not data.frame
    temp <-
      rbind(metricMetadata$Name[match(names(temp), metricMetadata$Code)], temp)
    temp[temp == "NaN"] <- ""
    
    
    # Write the merged dataset for that service onto the sheet created
    #
    # AS: as.data.frame() seems necessary for Ubuntu environment
    # error message: if (is.na(value)) { : the condition has length > 1
    # see https://github.com/colearendt/xlsx/issues/129
    # esp. https://github.com/colearendt/xlsx/issues/129#issuecomment-1683996775
    xlsx::write.xlsx(
      as.data.frame(temp),
      file = excelFileName,
      sheetName = service,
      append = TRUE,
      row.names = FALSE,
      col.names = TRUE,
      showNA = FALSE
    )
  }
  
}