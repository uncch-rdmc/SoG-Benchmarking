# package qualtRic-based approach 
# source
# https://docs.ropensci.org/qualtRics/reference/fetch_survey.html
if (FALSE) {
  # Register your Qualtrics credentials if you haven't already
  qualtrics_api_credentials(
    api_key = "<YOUR-API-KEY>",
    base_url = "<YOUR-BASE-URL>"
  )
  
  # Retrieve a list of surveys
  surveys <- all_surveys()
  
  # Retrieve a single survey
  my_survey <- fetch_survey(surveyID = surveys$id[6])
  
  my_survey <- fetch_survey(
    surveyID = surveys$id[6],
    start_date = "2018-01-01",
    end_date = "2018-01-31",
    limit = 100,
    label = TRUE,
    unanswer_recode = 999,
    verbose = TRUE,
    # Manually override EndDate to be a character vector
    col_types = readr::cols(EndDate = readr::col_character())
  )
  
}
