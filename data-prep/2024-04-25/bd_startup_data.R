# How to serialize data that cannot be stored on GitHub such as credentials to
# access the Qualtrics server

# required library
library(readr)

# The structure of start-up object: list
bd_startup_data_list <- list(
  dataCenterId = "",
  API_TOKEN = "",
  exportSurveyId = "",
  lastModifiedDateTimeZ = "2024-02-02T00:00:00Z"
)
# how to serialize the above table
readr::write_rds(bd_startup_data_list, file="bd_startup_data.rds")

# how to read back the object
bd_startup_data <- readr::read_rds(file="bd_startup_data.rds")

# test
# bd_startup_data
# bd_startup_data$API_TOKEN
# bd_startup_data$dataCenterId
# bd_startup_data$exportSurveyId
# bd_startup_data$lastModifiedDateTimeZ

# function to update the object
update_last_modified_date <-function(startUpDataList, newDate=NULL){
  if (is.null(newDate)){
  startUpDataList$lastModifiedDateTimeZ <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  } else {
    startUpDataList$lastModifiedDateTimeZ <- newDate
  }
  return(startUpDataList)
}


# how to use the above update function 

bd_startup_data <- update_last_modified_date(startUpDataList = bd_startup_data, 
newDate = "2024-04-09 00:00:00 EDT")
# check the update
# bd_startup_data$lastModifiedDateTimeZ
