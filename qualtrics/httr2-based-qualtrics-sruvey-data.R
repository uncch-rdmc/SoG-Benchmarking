library(httr2)
library(future)
library(readr)
future::plan(multisession)


dataCenterId <- "data_center_id"
API_TOKEN <- "api_token"
exportSurveyId <- "survey_id"

qsd_url <- paste("https://", dataCenterId, ".qualtrics.com/API/v3/surveys/",
                 exportSurveyId, "/export-responses",
                 sep = ""
)
qsd_url

respj1 <- httr2::request(base_url = qsd_url) |>
  httr2::req_headers(
    Accept = "application/json",
    "Content-Type" = "application/json",
    "X-API-TOKEN" = API_TOKEN
  ) |>
  httr2::req_body_json(list(format = "csv")) |>
  httr2::req_perform() |>
  httr2::resp_body_json()


respj1
exportProgressId <- respj1$result$progressId
exportProgressId

progress <-0
data_future <- future::future({

  while (progress < 100 ){
  
  response <- httr2::request(base_url = paste(qsd_url, "/", exportProgressId, sep="")) |>
    httr2::req_headers(Accept= "application/json",
                       'Content-Type' = "application/json", 'X-API-TOKEN' = API_TOKEN) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
   progress <- response$result$percentComplete
   #print(progress)
  }
  response
}) %plan% future::multisession


count <- 0
while (!future::resolved(data_future)) {
  count <- count +1
  #print(paste("still working:iteration=", count, sep = ""))
}
print(paste("step 2 ended after :iteration=", count, sep = ""))
respj2 <- future::value(data_future)
str(respj2)

respj2
respj2$result$status
respj2$result$percentComplete

exportFileId <- respj2$result$fileId
exportFileId
# -----------------------------------------------------------------------------
# step 3 request

temp <- tempfile()
step3_data_future <- future::future({
  resp3 <-
    httr2::request(base_url = paste(qsd_url, "/", exportFileId, "/file", sep = "")) |>
    httr2::req_headers("X-API-TOKEN" = API_TOKEN) |>
    httr2::req_perform(path = temp)
})

count3 <- 0
while (!resolved(step3_data_future)) {
  count3 <- count3 + 1
  #print(paste("still working on step 3:iteration=", count3, sep = ""))
}
print(paste("step 3 ended after iteration=", count3, sep = ""))

respj3 <- future::value(step3_data_future)

str(respj3)
print("status_code=")
print(respj3$status_code)


csv_file_info <- file.info(temp)

if (csv_file_info$size > 0 && respj3$status_code == 200) {
  print("joutput file is not empty: it size=")
  print(csv_file_info$size)
} else {
  print("output file is empty/status_code is not 200")
}

surveyData <- readr::read_csv(file = temp, 
    col_types = readr::cols(.default = "n", Finished = "c", RecordedDate = "T",
    a_year = "n", a_municipality = "c", a_service = "c"))
head(surveyData)

