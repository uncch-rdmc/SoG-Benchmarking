library(shiny)
library(shinythemes)
library(plotly)
source("helpers.R")
library(shinyjs)
library(showtext)
library(grDevices)
library(jcolors)
#install.packages("ggchicklet", repos = "https://cinc.rud.is")
#library("ggchicklet")
################################################################################
# ui defintion
################################################################################
ui <- fluidPage(
#  shinythemes::themeSelector(),
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    font-size: 16px
    }
    "))
  ),
  
  
  
  titlePanel("Benchmarking 2.0"),

  
  sidebarLayout(
    
    
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Home",
                 
                 
                 plotOutput(outputId= "scatterplot" )

                 ),

        tabPanel("About",
          tags$h3(
            "Benchmarking 2.0: Project Overview"),
          
          tags$p("Established in 1995, the North Carolina Benchmarking Project ",
                 "allows municipalities to compare their service ", 
"and performance trends with other participating units.",  
"Benchmarking 2.0, launched in 2022",
"streamlines data collection and analysis and ",
"expands opportunities for robust peer-to-peer conversations and best-practice development."
          )
        )
      )
      

    ),
    
    
sidebarPanel(
# select a base city
selectInput(inputId='selectedCity', 
              label='Select Base Municipality', 
              choices=c(citylabel)
              ),

# select-all-municipalities checkbox
checkboxInput(inputId ="selectAllpeer", 
              label= "Select all comparison municipalities",
              value=TRUE),

# checkbox group for the municipality list
checkboxGroupInput(inputId = "peerGroup", 
                   label = "Select Comparison Municipalities", 
                   choices = rvllabel, 
                   selected=rvllabel),

# select a service 
# s_list is replaced by a named list, srvclngToShrtRefLstWoc
selectInput(inputId='selectedService', 
            label='Service', 
            choices=srvclngToShrtRefLstWoc,
            selected = srvclngToShrtRefLstWoc[1] ), 

# select years: checkboxes 
checkboxGroupInput(inputId="selectedYears", 
                   label = "Years", 
                   choices = y_list,
                   selected = y_list),

# select a numerator(metric) variable:pulldown menu

selectInput(inputId='selectedVar4num', 
            label = 'Select Service Metric', 
            choices = c(srv2varlbllst[["amr"]]),
            #selected = initialNumeratorValue
            selected = c()
            ),
# srv2varlbllst[["amr"]][1]
# checkbox to hide/show the denominator variable
checkboxInput(inputId='selectedUseDenominator', 
              label = 'Use Denominator|Context Variable', 
              value = FALSE),

# show/hide for the denominator 
conditionalPanel(
  condition = "input.selectedUseDenominator == true",

# select a denominator variable: pulldwon menu
selectInput(inputId='selectedVar4denom', 
            label = 'Select Denominator/Context Variable', 
            choices = c()
            )
),


# radio-button for Adjustment choices 

radioButtons(inputId = "selectMultiplier", 
              label = "Select a multiplier", 
              choices = list("None"=0, "x 100"=2, 
               "x 100K"=5, "x 1M"=6),
              selected=0),


# Checkbox for the Average 
checkboxInput(inputId ="selectAvg", 
              label= "Average of Comparison Municipalities",
              value=FALSE),

# CI-panel and Checkbox for CIs
conditionalPanel(
    condition = "input.selectAvg == true",
    checkboxInput(inputId ="selectCI", 
                label= "Add Confidence Interval (95%)",
                value=FALSE
    )
),
# Graph-Downloading button 
downloadButton('downloadGraph'),

# radio-button for page layout
radioButtons(inputId = "selectPageLayout", 
             label = "Select Page Layout", 
             choices = list("Portrait"=0, "Landscape"=1),
             selected=1)


)




) # sidebarLayout

) # fluidPage

################################################################################
# server definition
################################################################################

server <- function(input, output) {
  
  
#------------------------------------------------------------------------------
# observeEvent blocks 
#------------------------------------------------------------------------------
# select-all or not peer group

  # when the selectAllpeer box is checked, all cities' boxes are checked
  observeEvent(input$selectAllpeer, {
    if (input$selectAllpeer) {
      # select all state
      #freezeReactiveValue(input, "peerGroup")
      updateCheckboxGroupInput(inputId = "peerGroup",
        choices = citylabel[!citylabel %in% c(input$selectedCity)],
        selected = citylabel[!citylabel %in% c(input$selectedCity)])
      # average button is allowed
      shinyjs::enable(id="selectAvg")
      
    } else {
      # there are two possible states of peerGroup checkboxGroup
      # one state is all unselected 
      # the other is some are unselected
      # these two states must be separate handling
      print("input$peerGroup: size=")
      print(length(input$peerGroup))
      if (length(input$peerGroup) == 0){
        print("empty peerGroup case")
        # unselect-all case: all-checked to all-unselected transition
        # base city only selected
        # 
        # disable the average button because no member to calculate
        #freezeReactiveValue(input, "peerGroup")
        updateCheckboxGroupInput(inputId = "peerGroup",
                                 choices = citylabel[!citylabel %in% c(input$selectedCity)],
                                 selected = character(0))
        updateCheckboxInput(inputId = "selectAvg", value = FALSE)
        shinyjs::disable(id="selectAvg")

      } else  {
        print("non-empty peerGroup case")
        
        if (length(input$peerGroup) == (length(citylabel)-1)){
          #freezeReactiveValue(input, "peerGroup")
          updateCheckboxGroupInput(inputId = "peerGroup",
                                   choices = citylabel[!citylabel %in% c(input$selectedCity)],
                                   selected = character(0))
          shinyjs::disable(id="selectAvg")
          updateCheckboxInput(inputId = "selectAvg", value = FALSE)
        } else {
          #freezeReactiveValue(input, "peerGroup")
        updateCheckboxGroupInput(inputId = "peerGroup",
                                 choices = citylabel[!citylabel %in% c(input$selectedCity)],
                                 selected = c(input$peerGroup))        
      }
      }

    }
    
  })
  
  # observeEvent(input$peerGroup,{
    # if (!input$selectAllpeer){
    #   updateCheckboxGroupInput(inputId ="peerGroup",
    #         choices=citylabel[!citylabel %in% c(input$selectedCity)],
    #            selected=c(input$peerGroup))
    #   }

    

  # })
  
  
  # observe({
  # 
  #   updateCheckboxGroupInput(
  #      inputId="peerGroup",
  #       choices=citylabel[!citylabel %in% c(input$selectedCity)],
  #       selected=citylabel[!citylabel %in% c(input$selectedCity)])
  # 
  # })
  

  
  
  
  # behavior of comparison municipalities' checkbox group
  # (1) this block deals with "all cities" to "some cities" transition
  # uncheck the selectAllpeer box
  # (2) When there is still one checked box, the average-option checkbox
  # can be updated; when there is no checked box, it seems too late to 
  # update the average-option checkbox, i.e., cannot update its state.
  observeEvent(input$peerGroup, { 
    print("observe:updateCheckbox(Input/selectAllpeer")
    
    if (identical(input$peerGroup, citylabel[!citylabel %in% c(input$selectedCity)])){
      print("state: select all cities")
      print(input$peerGroup)
      #freezeReactiveValue(input, "selectAllpeer")
      updateCheckboxInput(inputId = "selectAllpeer", value = TRUE)
      shinyjs::enable(id="selectAvg")
      
      
      
    } else {
      print("state: not select all cities")
      print("current peerGroup members=")
      print(input$peerGroup)
      
      if (is.null(input$peerGroup)) {
        print("state: input$peerGroup is null:empty")
        # do nothing;
        # the average checkbox must have been unchecked
        
      } else {
        print("state: input$peerGroup is not null:not-empty")
        #freezeReactiveValue(input, "selectAllpeer")
        # keep the check-all checkbox unchecked
        updateCheckboxInput(inputId = "selectAllpeer", value = FALSE)
        #freezeReactiveValue(input, "peerGroup")
        
        # update the state of the checkbox group
        updateCheckboxGroupInput(
          inputId = "peerGroup",
          choices = citylabel[!citylabel %in% c(input$selectedCity)],
          selected = c(input$peerGroup)
        )
        
        # update the state for the average-option checkbox
        if (length(input$peerGroup) == 1) {
          updateCheckboxInput(inputId = "selectAvg", value = FALSE)
          shinyjs::disable(id = "selectAvg")
        } else {
          shinyjs::enable(id = "selectAvg")
        }
        
        
      }
      
      
    }
  })
#------------------------------------------------------------------------------
# other input fields
#------------------------------------------------------------------------------
  # observe({
  #   
  #   updateSelectInput(inputId ="" , choices = )
  # })

# selection of a service
# affects the set of service metrics and its selected one

  
  
# updating the set of numerators (service metrics)
# 
# trigger: whenever the current service has been updated, i.e.,
# when a user made a new choice, the set of service metrics must be updated
#
# side-effects: a change to this UI component is self-contained
# this change in service must be propagated to 
# related UI parts: the current set of 
  
  
# city-invoked side-effects
# change of city should not affect service and variables
  
  observeEvent(input$selectedCity, {
    print("= observeEvent(input$selectedCity: start ===============")
    print("observeEvent:city change: current settings at the begginin=")
    print("current city=")
    print(input$selectedCity)
    print("current service=")
    
    updatedPeerGroup <- citylabel[!citylabel %in% c(input$selectedCity)]
    
    print(input$selectedService)
    print("current metric=")
    print(input$selectedVar4num)
    print("current peerGroup=")
    print(input$peerGroup)
    #freezeReactiveValue(input, "peerGroup")
    updateCheckboxGroupInput(inputId = "peerGroup",
                             choices = updatedPeerGroup,
                             selected = c(updatedPeerGroup))
    print("= observeEvent(input$selectedCity: end ===============")
  })
  
# service-invoked side-effects
  # change in service => 
  # 1. switch a list of a set of vars(varset4numerator)
  # 2. set the selected var to the first of the above new list
  # a change in input$selectedVar4num does not affect its service
  #
  observeEvent(input$selectedService, {
    print("= observeEvent(input$selectedService: start ===============")
    print("observeEvent(service): current input$selectedService=")
    print(input$selectedService)
    
    print("observeEvent(service): current metric=input$selectedVar4num=")
    print(input$selectedVar4num)


    
    # new settings
    # change the set of metrics according to a newly selected service
    varset4numerator <- srv2varlbllst[[input$selectedService]]
    print("observeEvent(service): check new the changedvarset4numerator=")
    print(varset4numerator)
    print("observeEvent(service): to-be-assgined value for input$selectedVar4num=")
    print(varset4numerator[1])
    
    
    freezeReactiveValue(input, "selectedVar4num")
    updateSelectInput(inputId = "selectedVar4num",
                      choices=c(varset4numerator),
                      selected = c(varset4numerator[1]))
    
    
    print("observeEvent(service): post-update-check")
    print("observeEvent(service): updated metric=input$selectedVar4num=")
    print(input$selectedVar4num)
    
    print("observeEvent(service): updated list=varset4numerator=")
    print(varset4numerator)
    
    print("= observeEvent(input$selectedService: end ===============")
      })
  
  
  
  
  

  # observe({
  # 
  #   print("within observe(numerator): current input$selectedService=")
  #   print(input$selectedService)
  # 
  #   print("within observe: input$selectedVar4num1=")
  #   print(input$selectedVar4num)
  # 
  #   varset4numerator <- srv2varlbllst[[input$selectedService]]
  # 
  #   print("within observe block: varset4numerator=")
  #   print(varset4numerator)
  #   updateSelectInput(inputId = "selectedVar4num",
  #                     choices=c(varset4numerator))
  # })

  # observeEvent(input$selectedVar4num
  #   {
  #     
  #     
  #   }
  # )
  
  
  
  
  # The behavior of the numerator set
  # This check is meaningful when the denominator option is on;
  # when the denomintor-option is not used, there is no need to 
  # update the list of denominator variables
  # 
  # When the denominator-option is on, 
  # the selected numerator variable decides the set of denominator ones, 
  # any change to this UI modifies the set of denominator variables
  # and the currently selected denominator, 
  # either the top of the updated list because the previous one has
  # been selected as the new numerator
  # or keeping the current one if it was not chosen as
  # the newly selected numerator.
  observeEvent(input$selectedVar4num,{

    print("within observeEvent: input$selectedVar4num")
    print("input$selectedVar4num=")
    print(input$selectedVar4num)
    print("input$selectedUseDenominator=")
    print(input$selectedUseDenominator)
    if (input$selectedUseDenominator) {
      print("input$selectedVar4denom=")
      print(input$selectedVar4denom)
      
      
      
      # get the current list of **all** variables for the current service
      # note: this list does not include census variables
      rawlist <- srv2varlbllst[[input$selectedService]]
      # print("rawlist=")
      # print(rawlist)
      # the list of denominators must excluded the variable
      # that is currently selected as the numerator
      # so exclude the numerator from the above list
      netlist <- rawlist[!rawlist %in% c(input$selectedVar4num)]
      # print("netlist=")
      # print(netlist)
      # add the list of denominators and list of census-variables
      varset4denominator <- c(netlist, srv2varlbllst[["census"]])
      # print("varset4denominator=")
      # print(varset4denominator)
      #freezeReactiveValue(input, "selectedVar4denom")
      # update the list of denominator (choices)
      # and the selected one (selected)
      
      # denominator is on
      if (input$selectedVar4num == input$selectedVar4denom) {
        print("numerator is the previously selected denominator")
        print("reset the selected one for the denominator")
        updateSelectInput(
          inputId = "selectedVar4denom",
          choices = c(varset4denominator),
          selected = c(varset4denominator[1])
        )
      } else {
        print("numerator is NOT the previously selected denominator")
        print("keep the current choice of denominator")
        updateSelectInput(
          inputId = "selectedVar4denom",
          choices = c(varset4denominator),
          selected = c(input$selectedVar4denom)
        )
      }
      
    } else {
      # denominator is off
      # do nothing
      print("denominator is off; do nothing here")
    }
    
  })
  
  
  
  
  
  # This block prepares the denominator Ui 
  # 
  # 
  # 
  observeEvent(input$selectedUseDenominator, {
    
    print("within observeEvent: input$selectedUseDenominator")
    print("input$selectedService=")
    print(input$selectedService)
    print("input$selectedVar4num=")
    print(input$selectedVar4num)
    
    # get the current list of **all** variables for the current service
    # note: this list does not include census variables
    rawlist <- srv2varlbllst[[input$selectedService]]
    # print("rawlist=")
    # print(rawlist)
    # the list of denominators must excluded the variable
    # that is currently selected as the numerator
    # so exclude the numerator from the above list
    netlist <- rawlist[!rawlist %in% c(input$selectedVar4num)]
    # print("netlist=")
    # print(netlist)
    # add the list of denominators and list of census-variables
    varset4denominator <- c(netlist, srv2varlbllst[["census"]])
    print("update the UI: selectedVar4denom ")
    freezeReactiveValue(input, "selectedVar4denom")
    updateSelectInput(
      inputId = "selectedVar4denom",
      choices = c(varset4denominator),
      selected = c(varset4denominator[1])
    )
    
    print("input$selectedVar4denom=")
    print(input$selectedVar4denom)
  })
  
  
  
  
  
  
  # observeEvent(input$selectAvg, {
  #   if (!input$selectAvg){
  #     updateCheckboxInput(inputId = "selectCI", 
  #       label = "Add Confidence Interval (95%)", value = FALSE )
  #   }
  #   
  # })
  
# when the checkbox of denominator|context var is turned off,
# chart returns to normal   
  observeEvent(input$selectedUseDenominator,{
    if (input$selectedUseDenominator){
      updateRadioButtons(inputId = "selectMultiplier", selected = 2)
    } else {
      updateRadioButtons(inputId = "selectMultiplier", selected = 0)
    }
    
    
    
  })
  
################################################################################

  output$scatterplot <- renderPlot({  
#  output$scatterplot <- renderPlotly({
    base::message("= renderPlotly: START ================================================")
    base::message("output$scatterplot: start-time=",as.POSIXct(Sys.time(), tz = "EST5EDT"))
    
    # input: sanity check
    # input$selectedVar4denom was removed
#     req(input$selectedCity, 
#         input$selectedService,
#         input$selectedYears,
#         input$selectedVar4num
# )

    
    
    
    
    
    
# plot for tab 1: start ========================================================
# data steps

# ==============================================================================
# matrix of data for the numerator and denominator for municipalities
# ==============================================================================
# 
# step 1: selected city
# base-line data for the numerator 
# warning: since the same variable name is used in more than one services
# the service filter must be applied first

print("selected city=")
print(input$selectedCity)
    
print("peerGroup=")
print(input$peerGroup)
    
print("selected Service=")
print(input$selectedService)

print("full service name=")
serviceNameFull<- names(srvclngToShrtRefLstWoc)[which(srvclngToShrtRefLstWoc == input$selectedService)]
print(serviceNameFull)


print("currently selected numerator=")
print(input$selectedVar4num)


# define a coding-friendly var
useDenominator<-FALSE
if (input$selectedUseDenominator){
  useDenominator<-TRUE
  print("use a denominator")
  print(input$selectedUseDenominator)
  
  ## base municipality: numerator 
  print("denominator=")
  print(input$selectedVar4denom)
  
  # update the multiplier value to 
  
} else {
  print("Does not use a denominator")
}





print("selectedYears=")
print(input$selectedYears)
print(typeof(input$selectedYears))
print(str(input$selectedYears))
# dplyr::select() failed to evaluate input$selectedYears as a vector
selectedYearsC<- as.character(input$selectedYears)
print("selectedYearsC=")
print(selectedYearsC)

valueAvailableCities <- bd_data %>% 
  filter(Service == input$selectedService)   %>%
  filter(Variable==input$selectedVar4num)    %>%
  filter(!is.na(Value)) %>%
  distinct(Municipality) %>%
  pull()
print("valueAvailableCities=")
print(valueAvailableCities)
print("selected city=")
print(input$selectedCity)

msg_no_base_m_data <-"" 
if (is.na(match(input$selectedCity, valueAvailableCities))){
  msg_no_base_m_data <-  paste(
    "\nData for the selected base municipality (", 
input$selectedCity,
") is not available for these years.")
}

# validate(
#   need(!is.na(match(input$selectedCity, valueAvailableCities)),
#        paste("\n\nThe selected base municipality (", 
#              input$selectedCity
#              ,") has not yet reported data about\n\t",serviceNameFull,
#              "\nfor these years.\n","\nPlease choose a different base municipality.")
#        )
# )


tmp_data_sc_nm <- bd_data %>%
  filter(Service == input$selectedService)   %>%
  filter(Variable==input$selectedVar4num)    %>%
  filter(Municipality == input$selectedCity) %>%
  spread(key=Year, value=Value)  %>%
  select(selectedYearsC)
print("tmp_data_sc_nm=")
print(tmp_data_sc_nm)

# if (all(is.na(tmp_data_sc_nm))){
#   message("all NA case ======================================")
# }

# the following select() return an error message 
# all-NA tibble seems to be rejected
# the following is a roundabout solution
data_sc_nm <- bd_data %>% 
  filter(Service == input$selectedService)   %>%
  filter(Variable==input$selectedVar4num)    %>%
  filter(Municipality == input$selectedCity) %>%
  spread(key=Year, value=Value)        %>%
  select(selectedYearsC)        %>% 
  as.matrix()
print("data_sc_nm: afer as.matrix()=")
print(data_sc_nm)

# The following c(4:6) would fail if not all 3 years are selected
#data_sc_nm <- t(as.matrix(data_sc_nm[c(4:6)]))
# data_sc_nm <- t(as.matrix(data_sc_nm))
# print("data_sc_nm: afer subset and t()=")
# print(data_sc_nm)

colnames(data_sc_nm) <- selectedYearsC
print("data_sc_nm: afer colnames=")
print(data_sc_nm)

suppressWarnings(storage.mode(data_sc_nm)<-"numeric")
print("data_sc_nm:after changing stroage mode=")
print(data_sc_nm)
print(str(data_sc_nm))
numerator_is_all_na <- FALSE
if (all(is.na(data_sc_nm))){
  message("numerator: all NA case")
  numerator_is_all_na<-TRUE
  msg_no_base_m_data <-  paste(
    "\nData for the selected base municipality (", 
    input$selectedCity,") is not available for these years.")
} else {
  data_sc_nm <- 10^as.integer(input$selectMultiplier) *  data_sc_nm
}
print("data_sc_nm: after multiplier is aplied=")
print(data_sc_nm)


#dimnames(data_sc_nm)[[1]] <- input$selectedCity
print("length(input$selectedCity)=")
print(length(input$selectedCity))
rownames(data_sc_nm)<-  input$selectedCity
print("data_sc_nm: after adding rowname=")
print(data_sc_nm)

## ----------------------------------------------------------------------------
## base municipality: denominator: to be ignored if no denominator
## ----------------------------------------------------------------------------
# base-line data for the denominator
#  variables must be added

if (useDenominator){ 
  print("base m: denominator data block: ========== start ==========")
  print("current city=")
  print(input$selectedCity)
  print("current service=")
  print(input$selectedService)
  print("current metric=")
  print(input$selectedVar4denom)  
  
  
  
data_sc_dm <- bd_data %>% 
  filter(Service == input$selectedService | Service =="census")   %>%  
  filter(Variable==input$selectedVar4denom)  %>%
  filter(Municipality == input$selectedCity) %>%
  spread(key=Year, value=Value)        %>%
  select(selectedYearsC)        %>% 
  as.matrix()
print("data_sc_dm=1")
print(data_sc_dm)
dimnames(data_sc_dm)[[1]] <- input$selectedCity
print("data_sc_dm=2")
print(data_sc_dm)


if (all(is.na(data_sc_dm))){
  message("base city: denominator: all NA case")
  numerator_is_all_na<-TRUE
  msg_no_base_m_data <-  paste(
    "\nDenominator data for the selected base municipality (", 
    input$selectedCity,") is not available for these years.")
}


}
## ----------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# step 2: peer group
# warning: since the same variable name is used in more than one services
# the service filter must be applied first
# base-line data for the numerator
test_i_selectedService <- input$selectedService
test_i_selectedVar4num <- input$selectedVar4num
test_i_peerGroup       <- input$peerGroup
# tmp_pg_nm <- bd_data %>% 
#   filter(Service == input$selectedService)   %>%
#   filter(Variable==input$selectedVar4num)   %>%
#   filter(Municipality %in% input$peerGroup) %>% 
#   arrange(Municipality, Year)
print("filters for the peergroup=")
print(test_i_selectedService)
print(test_i_selectedVar4num)
print("test_i_peerGroup=")
print(test_i_peerGroup)

if (!is.null(input$peerGroup)){ 
  
  # tmp_pg_nm <- bd_data %>% 
  #   filter(Service == test_i_selectedService)   %>%
  #   filter(Variable==test_i_selectedVar4num)   #%>%
  # filter(Municipality %in% test_i_peerGroup) %>% 
  # arrange(Municipality, Year)
  # print("tmp_pg_nm=")
  # print(tmp_pg_nm)
}

if (!is.null(input$peerGroup)){ 
  
  data_pg_nm <- bd_data %>% 
    filter(Service == input$selectedService)   %>%
    filter(Variable==input$selectedVar4num)   %>%
    filter(Municipality %in% input$peerGroup) %>% 
    arrange(Municipality, Year) %>%
    spread(key=Year, value=Value)       %>%
    select(selectedYearsC) %>% 
    as.matrix()
  
  # print("data_pg_nm(s)=")
  # print(data_pg_nm)
  
  data_pg_nm <- 10^as.integer(input$selectMultiplier) * data_pg_nm
  
  # print("data_pg_nm(b)=")
  # print(data_pg_nm)
  # print("input$peerGroup=")
  # print(input$peerGroup)
  
  # if a dataset is complete,
  # input$peerGroup can be used;
  # if a dataset is not complete,
  # i.e., some municipalities did not report, 
  # a peer-member set based on an actual data is necessary
  
  # updatedPeerGroup <- bd_data %>% 
  #   filter(Service == input$selectedService)   %>%
  #   filter(Variable== input$selectedVar4num)   %>% 
  #   filter(Municipality %in%  input$peerGroup) %>% 
  #   arrange(Municipality) %>%
  #   distinct(Municipality) %>% pull(Municipality)
  # 
  # print("updatedPeerGroup=")
  # print(updatedPeerGroup)
  
  rownames(data_pg_nm) <- input$peerGroup

  # print("data_pg_nm(a)=")
  # print(data_pg_nm)
  
}
## peer group: denominator specific
## ----------------------------------------------------------------------------
# base-line data for the denominator
#  variables must be added
if (!is.null(input$peerGroup)){ 
  if (useDenominator){
    
    data_pg_dm <- bd_data %>% 
      filter(Service == input$selectedService | Service =="census")   %>%
      filter(Variable==input$selectedVar4denom) %>%
      #filter(Municipality %in% updatedPeerGroup) %>% 
      filter(Municipality %in% input$peerGroup) %>% 
      spread(key=Year, value=Value)       %>%
      select(selectedYearsC) %>% 
      as.matrix()
    rownames(data_pg_dm) <- input$peerGroup
    # rownames(data_pg_dm) <- updatedPeerGroup
    print("data_pg_dm=")
    print(data_pg_dm)
  }
}
# ------------------------------------------------------------------------------
# working on peer group
if (!is.null(input$peerGroup)){ 
  print("selectedYears=")
  print(selectedYearsC)
  print("selectedYears:length=")
  print(length(selectedYearsC))
  
  
  
  data4EachPeerCity_raw_m <- data_pg_nm
  ## if denominoatr is used
  if (useDenominator){
    data4EachPeerCity_raw_m <- data4EachPeerCity_raw_m / data_pg_dm
  }
  
  ## peer-city case
  
  
  print("data4EachPeerCity_raw_m=")
  print(data4EachPeerCity_raw_m)
  print(str(data4EachPeerCity_raw_m))
  print("has_rownames: data4EachPeerCity_raw_m=")
  print(has_rownames(data4EachPeerCity_raw_m))
  
  
  
  # data4plot <- rownames_to_column(as.data.frame(data4plot_raw), var="catgry") %>% as_tibble()
  data4EachPeerCity_rawt <- 
    rownames_to_column(as.data.frame(data4EachPeerCity_raw_m),
                       var="catgry") %>%
    as_tibble()
  
  print("data4EachPeerCity_rawt=")
  print(data4EachPeerCity_rawt)
  
  data4EachPeerCity_rawt <- data4EachPeerCity_rawt %>%
    gather(selectedYearsC, key="Year", value = "quotient")
  print("data4EachPeerCity_rawt=")
  print(data4EachPeerCity_rawt)
  
  
  tmpTibblePg <- NULL
  if (useDenominator){
    tmpTibblePg <- as_tibble(data_pg_nm / data_pg_dm)
  } else {
    tmpTibblePg <- as_tibble(data_pg_nm)
  }
  
  
  
  
  data4EachPeerCity <- tmpTibblePg  %>% 
    gather(selectedYearsC, key="Year", value = "quotient")
  
  
  print("data4EachPeerCity=")
  print(data4EachPeerCity)
  
  
  
  summarizedPG <- tmpTibblePg %>% 
    gather(selectedYearsC, key="Year", value = "quotient") %>%
    summarySE(measurevar = "quotient", groupvars = "Year", na.rm = TRUE)
  
  
  
  
  print("summarizedPG=")
  print(summarizedPG)

} # end of non-empty peerGroup-case


tmpMatrixScQ <- NULL
if (!is.null(input$peerGroup)){ 
  if (useDenominator){
    tmpMatrixScQ <- data_sc_nm / data_sc_dm
  } else{
    tmpMatrixScQ <- data_sc_nm
  }

} else {
  # no peer case
  
  if (useDenominator){
    tmpMatrixScQ <- data_sc_nm / data_sc_dm
  } else{
    tmpMatrixScQ <- data_sc_nm
    print("tmpMatrixScQ=")
    print(tmpMatrixScQ)
    print("t(tmpMatrixScQ)[, 1]=")
    print(t(tmpMatrixScQ)[, 1])
  }
  
}


# last step: selected City 
if (!is.null(input$peerGroup)){ 
  data4plot_raw <- rbind(t(tmpMatrixScQ)[, 1], t(summarizedPG[, 3]))
  rownames(data4plot_raw ) <- c(input$selectedCity, "Average")
} else {
  # do nothing?
  data4plot_raw <- tmpMatrixScQ
  # data4plot_raw <- t(as.matrix(t(tmpMatrixScQ)[, 1]))
  # print("data4plot_raw=")
  # print(data4plot_raw)
  # print(str(data4plot_raw))
  # print("input$selectedCity=")
  # print(input$selectedCity)
  # rownames(data4plot_raw ) <- c(input$selectedCity)
}

print("data4plot_raw=")
print(data4plot_raw)
#-------------------------------------------------------------------------------

data4plot <- rownames_to_column(as.data.frame(data4plot_raw), 
                                var="catgry") %>% 
  as_tibble()
if (ncol(data4plot) == 2){
  print("***** column is 3 *****")
  yr <- selectedYearsC[1]
  print(yr)
  if (is.null(input$peerGroup)){
    # do nothing
  } else {
  data4plot <- data4plot %>% dplyr::rename(!!yr:= "V1")
}
  
} else {
  print("***** column is not 2 *****")
  print(ncol(data4plot))
}
print("data4plot=")
print(data4plot)



# the following line fails if the number of years is 1
data4plot_sc <- data4plot %>% 
  gather(selectedYearsC, key = "Year", value = "quotient") %>%
  filter(catgry== input$selectedCity)
print("data4plot_sc=")
print(data4plot_sc)

if (!is.null(input$peerGroup)){ 
  data4plot_pg <- data4plot %>% 
    gather(selectedYearsC, key = "Year", value = "quotient") %>%
    filter(catgry != input$selectedCity)
  print("data4plot_pg=")
  print(data4plot_pg)
  
  data4plot_pgx <- data4plot_pg %>% add_column(ci = summarizedPG[["ci"]])
  print("==================== end of data-prep for plot ==================")
  
}
# -----------------------------------------------------------------------------
# plot
# -----------------------------------------------------------------------------
# An if-block after ggplot() seems not to accept more than 1 geom_xxx 
# statements within it and the following incremental approach was used


print("==================== beginning of plot ==================== ")

sysfonts::font_add_google(name = "Barlow Semi Condensed",family =  "barlow")
showtext_auto()
# baseline rendering 
plt1 <- data4plot_sc %>%
  ggplot(aes(x=Year, y=quotient)) +
       geom_bar(stat = 'identity', 
                position = 'dodge', 
                fill="#D3D3D3", 
                color="#D3D3D3") + 
#  geom_chicklet(fill="#D3D3D3") +
  scale_y_continuous(name="Value", labels = comma)

if (input$selectAvg){
  print("extra-step for adding average")
  # add an average line
  plt1 <- plt1 + 
            geom_line(data = data4plot_pg, 
            aes(x = Year, y = quotient, group = catgry, color = catgry),
            size=1
            ) + 
            geom_point(
              data = data4plot_pg, 
              aes(x = Year, y = quotient, group = catgry, color = catgry),
              size=3
            ) +
            scale_color_discrete(name = "Legend", 
            labels = c("average of \ncomparison \nmunicipalities"))
  
  if (input$selectCI) {
    # add CI-bands
    plt1 <- plt1 + geom_errorbar(data = data4plot_pgx,
      aes(ymin = quotient - ci, ymax = quotient + ci), 
      width = .1, color = "#696969", position = position_dodge(0.1)) 
  }
  
} else {
  # each municipality's line is added 
  # tibble to be used
  print("no average, etc.")
  
  if (!is.null(input$peerGroup)){ 
    if (length(selectedYearsC) >1) {
      plt1 <- plt1 + 
        geom_line(data = data4EachPeerCity_rawt, 
                  aes(x = Year, y = quotient, 
                      group = catgry, color = catgry),
                  size=1) +
        geom_point(data = data4EachPeerCity_rawt, 
                   aes(x = Year, y = quotient, 
                       group = catgry, color = catgry),
                   size=3) +
        scale_color_discrete(name = "Legend")
      
    } else {
      # single-year cases
      plt1 <- plt1 +
        geom_point(data = data4EachPeerCity_rawt,
                   aes(x=Year, y= quotient, 
                       group=catgry, color=catgry),
                   size=3) +
        scale_color_discrete(name="Legend")
    }

  }
}


print("numerator=")
print(input$selectedVar4num)
print("numerator's name(label) check=")
metricVarLabel <- 
  names(srv2varlbllst[[input$selectedService]])[which(srv2varlbllst[[input$selectedService]]== input$selectedVar4num)]
metricVarLabel<- stringr::str_wrap(metricVarLabel, width = 80)
print(metricVarLabel)
  
# print("varset4denominator=") 
# print(varset4denominator)
print("denominator=")
print(input$selectedVar4denom)
print("name check: denominator: input$selectedVar4denom=")
print("current input$selectedService is=")
print(input$selectedService)
contextVarLabel <- v2lallinOne[[input$selectedVar4denom]]
contextVarLabel <- stringr::str_wrap(contextVarLabel, width = 80)
print("contextVarLabel=")
print(contextVarLabel)

if (useDenominator){
  print("use denominator case: variable-name")
  contextVarLabel <- paste("\n/",contextVarLabel)
} else{
  print("no denominator case: use blank")
  contextVarLabel<-""
}


titleText <- paste(c("" , 
                     metricVarLabel,  contextVarLabel), collapse = "")
print("titleText=")
print(titleText)


print("multiplier=")
print(input$selectMultiplier)
multiplierValue <- as.character( 10^as.integer(input$selectMultiplier) )


peerGroupList <-""
if (!is.null(input$peerGroup)) {
  peerGroupList <- paste(input$peerGroup,collapse=", ")
  # peerGroupList <- paste(updatedPeerGroup, collapse = ", ")
  peerGroupList <- stringr::str_wrap(peerGroupList, width = 80)
}


subtitleText <-paste(c("Base Municipality: ",input$selectedCity,
                      "\nService: ",  serviceNameFull,
                      "\nComparison Municipalities: ", peerGroupList,
                      "\nMultiplier: ", multiplierValue
                      ), 
                      collapse = "")
print("subtitleText=")
print(subtitleText)


# default page layout
paperWidth<-10
paerHeight<-8.5


if (input$selectPageLayout == 0){
  # portrait case
  print("portrait request")
  paperWidth<-8.5
  paerHeight<-10
}
print("paperWidth=")
print(paperWidth)
print("paerHeight=")
print(paerHeight)
# ========================================================================
# furnish the graph with its title, etc.
# ========================================================================
# adding title/caption data
plt1 <- plt1 +  labs(
    title=titleText, subtitle = msg_no_base_m_data,
    caption = subtitleText)

# text-tweaking 
plt1 <- plt1 +
  scale_color_jcolors(palette = "pal8") +
#  theme(text = element_text(family = "balow"))+
  theme_bw() +
  theme(plot.title =   element_text(family = "balow", size=20, vjust = 5))+
  theme(plot.subtitle =element_text(family = "balow", color="red") ) +
  theme(plot.margin =  margin(t=40, l=20)) +
  theme(plot.caption = element_text(family = "balow", size = 12, hjust = 0)) +
  theme(legend.title = element_text(family = "balow", size=14)) +
  theme(legend.text =  element_text(family = "balow", size=12)) +
  theme(axis.title.y = element_blank()  ) +
  theme(axis.title.x=  element_text(family = "balow", size=14)) +
  theme(axis.text.y=   element_text(family = "balow", size=12)) +
  theme(axis.text.x=   element_text(family = "balow", size=12))

# saving the PDF version for a downloading request

ggsave(filename = "graph.pdf",plot =  plt1, device = cairo_pdf, 
       width = paperWidth, 
       height = paerHeight, units = "in")

base::message("= renderPlotly: END ==========================================")
base::message("rquest endtime=",as.POSIXct(Sys.time(), tz = "EST5EDT"))

plt1
# hide plotly's modebar
#ggplotly(plt1) %>% config(displayModeBar = FALSE)
}) # end of tab 1's plot
  

# ------------------------------------------------------------------------------
output$downloadGraph <- downloadHandler(

  filename = function(){
    "graph.pdf"
  }, 
  content = function(file) {
    file.copy('graph.pdf', file, overwrite = TRUE)
  }
)
# ------------------------------------------------------------------------------
} # end of server



shinyApp(ui = ui, server = server)
