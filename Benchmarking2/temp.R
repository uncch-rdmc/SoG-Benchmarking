# Coding warnings
# 
# 1. Because multiple services have the same variable name,
# service-wise filtering must come ahead of variable-wise one to avoid
# filtering mistakes  




# data_sc_nm <- bd_data |> 
#   filter(Service == input$selectedService | Service =="census")   |>
#   filter(Variable==input$selectedVar4num)    |>
#   filter(Municipality == baseCity) |>
#   spread(key=Year, value=Value)        |>
#   select(selectedYearsC)        |> 
#   as.matrix()

# print("data_sc_nm: afer as.matrix()=")
# print(data_sc_nm)

#' get a selected municipality's data for the numerator
#'
#' @param dt benchmarking data as a tibble
#' @param selectedService the name of the selected service
#' @param selectedVar4num the name of a variable selected as
#'   the numerator
#' @param selectedCity the name of the selected municipality
#' @param selectedYears selected years
#'
#' @return A matrix of 1 x n where n is the number of selected years
#' @export
#'
#' @examples
get_selectedCity_numerator_data <- function(dt, selectedService, 
                                            selectedVar4num, 
                                            selectedCity,
                                            selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>
    filter(Variable==selectedVar4num)    |>
    filter(Municipality == selectedCity) |>
    spread(key=Year, value=Value)        |>
    select(selectedYears)        |> 
    as.matrix()
}


# data_sc_dm <- bd_data |> 
#   filter(Service == input$selectedService | Service =="census")   |>  
#   filter(Variable==input$selectedVar4denom)  |>
#   filter(Municipality == baseCity) |>
#   spread(key=Year, value=Value)        |>
#   select(selectedYearsC)        |> 
#   as.matrix()

#' get a selected municipality's data for the denominator
#'
#' @param dt benchmarking data as a tibble
#' @param selectedService the name of the selected service
#' @param selectedVar4denom the name of a variable selected as
#'   the denominator
#' @param selectedCity the name of the selected municipality
#' @param selectedYears selected years
#'
#' @return A matrix of 1 x n where n is the number of selected years
#'
#' @return
#' @export
#'
#' @examples
get_selectedCity_denominator_data <- function(dt, selectedService, 
                                              selectedVar4denom, 
                                              selectedCity,
                                              selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>  
    filter(Variable==selectedVar4denom)  |>
    filter(Municipality == selectedCity) |>
    spread(key=Year, value=Value)        |>
    select(selectedYears)        |> 
    as.matrix()
    
}

# data_pg_nm <- bd_data |> 
#   filter(Service == input$selectedService | Service =="census")   |>
#   filter(Variable==input$selectedVar4num)   |>
#   filter(Municipality %in% checkedM) |> 
#   arrange(Municipality, Year) |>
#   spread(key=Year, value=Value)       |>
#   select(selectedYearsC) |> 
#   as.matrix()


#' get the current peer-group's data for the numerator
#'
#' @param dt benchmarking data as a tibble
#' @param selectedService the name of the selected service
#' @param selectedVar4num the name of a variable selected as
#'   the numerator
#' @param peerGroup the current peer-group's member names
#' @param selectedYears selected years
#'
#' @return A matrix of m x n where m is the number of peer 
#'   municipalities and n is the number of selected years
#' @export
#'
#' @examples
get_peerGroup_numerator_data <- function(dt, selectedService, 
                                         selectedVar4num, 
                                            peerGroup,
                                            selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>
    filter(Variable==selectedVar4num)   |>
    filter(Municipality %in% peerGroup) |> 
    arrange(Municipality, Year) |>
    spread(key=Year, value=Value)       |>
    select(selectedYears) |> 
    as.matrix()
  
  
}


# data_pg_dm <- bd_data |> 
#   filter(Service == input$selectedService | Service =="census")   |>
#   filter(Variable==input$selectedVar4denom) |>
#   #filter(Municipality %in% updatedPeerGroup) |> 
#   filter(Municipality %in% checkedM) |> 
#   spread(key=Year, value=Value)       |>
#   select(selectedYearsC) |> 
#   as.matrix()























#' get the current peer-group's data for the denominator
#'
#' @param dt benchmarking data as a tibble
#' @param selectedService the name of the selected service
#' @param selectedVar4denom the name of a variable selected as
#'   the denominator
#' @param peerGroup the current peer-group's member names
#' @param selectedYears selected years
#'
#' @return A matrix of m x n where m is the number of peer 
#'   municipalities and n is the number of selected years
#' @export
#'
#' @examples
get_peerGroup_denominator_data <- function(dt, selectedService, 
                                           selectedVar4denom, 
                                           peerGroup,
                                           selectedYears){
  dt |> 
    filter(Service == selectedService | Service =="census")   |>
    filter(Variable==selectedVar4denom) |>
    #filter(Municipality %in% updatedPeerGroup) |> 
    filter(Municipality %in% peerGroup) |> 
    spread(key=Year, value=Value)       |>
    select(selectedYears) |> 
    as.matrix()
}

# how to unify the above four fuctions into one
# 
# 

get_selectedCity_numerator_data   <- function(dt, selectedService, 
                                            selectedVar4num, 
                                            selectedCity,
                                            selectedYears)

get_selectedCity_denominator_data <- function(dt, selectedService, 
                                                selectedVar4denom, 
                                                selectedCity,
                                                selectedYears)

get_peerGroup_numerator_data <-      function(dt, selectedService, 
                                           selectedVar4num, 
                                           peerGroup,
                                           selectedYears)

get_peerGroup_denominator_data <-    function(dt, selectedService, 
                                             selectedVar4denom, 
                                             peerGroup,
                                             selectedYears)

#1st : dt
#2nd: selected ervice 
#3rd: variable as filter
#4th: group as selector
#5th: selected years
#
#
#
#
# -----------------------------------------------------------------------------
library(profvis)
profvis::profvis(runApp())


library(gfonts)
# For example, we use a temporary directory
path_to_www <- tempfile()
dir.create(path_to_www)

# all_fonts <- get_all_fonts()
# all_fonts
# In a Shiny app, you can use the www/ directory
# in Markdown, use a subfolder of your Rmd directory
setup_font(
  id = "barlow-semi-condensed",
  output_dir = path_to_www
)








# Please use `use_font("barlow-semi-condensed", "file33262e29d3e633/css/barlow-semi-condensed.css")` to import the font in Shiny or Markdown.


# Clean up
unlink(path_to_www, recursive = TRUE)



tmstmp <- as.POSIXct(Sys.time(), tz = "EST5EDT")
tmstmp <- as.character(Sys.time(), usetz=TRUE); str(tmstmp)
paste("generated at: ", tmstmp, sep="")

paste("generated at: ", as.character(Sys.time(), usetz=TRUE), sep="")
paste("generated at: ", as.character(as.POSIXct(Sys.time(), tz = "EST5EDT"), usetz=TRUE), sep="")
library(lubridate)
lubridate::with_tz(Sys.time(), tzone="EST5EDT")
paste("generated at: ", as.character(lubridate::with_tz(Sys.time(), tzone="EST5EDT"),usetz=TRUE), sep="")
