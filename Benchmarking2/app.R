library(shiny)
#library(shinythemes)
source("helpers.R")
library(shinyjs)
library(grDevices)
library(shinyWidgets)
library(waiter)
library(lubridate)
#library(shinyFeedback)

waiting_screen <- tagList(
  spin_wave(),
  h4("loading ...")
) 


################################################################################
# ui defintion
################################################################################
ui <- fluidPage(
  waiter::useWaiter(),
  # waiter::autoWaiter(c("benchmarkingDB")),
  # waiter::useWaitress(),
  # waiter::useHostess(),
  # waiter::useAttendant(),
  # waiter::attendantBar("progress-bar"),
  
  # waiterShowOnLoad(
  #   color = "#f7fff7",
  #   hostess_loader(
  #     "loader", 
  #     preset = "circle", 
  #     text_color = "blue",
  #     class = "label-center",
  #     center_page = TRUE
  #   )
  # ),
  
  
  
  shinyFeedback::useShinyFeedback(),
  #  shinythemes::themeSelector(),
  shinyjs::useShinyjs(),
  
  # The following css setting was ignored by ggplot2
  # includeCSS("www/css/barlow-semi-condensed.css"),
  
  # The following setting was ignored by ggplot2
  # gfonts::use_font(
  #   id = "barlow-semi-condensed",
  #   css_path = file.path("www", "css/barlow-semi-condensed.css")
  # ),
  
  #
  tags$head(# tags$link(rel = "stylesheet", type = "text/css",
    #           # href = "www/css/barlow-semi-condensed.css"),
    tags$style(
      HTML(
        "
    .shiny-output-error-validation {
    color: red;
    font-size: 16px
    }
    #goButton{
    margin-bottom: 20px;}
    "
      )
    )),
  
  
  
  titlePanel("Benchmarking 2.0"),
  
  # sidebarLayout = mainPanel + sidebarPanel
  sidebarLayout(
    ###########################################################################
    # main panel
    ###########################################################################
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(
        "Home",
        br(),
        htmlOutput(outputId = "initialMessage",
                   container = h3),
        
        plotOutput(outputId = "benchmarkingDB"),
        
      ),
      
      tabPanel(
        "About",
        tags$h3("Benchmarking 2.0: Project Overview"),
        
        tags$p(
          "Established in 1995, the North Carolina Benchmarking Project ",
          "allows municipalities to compare their service ",
          "and performance trends with other participating units.",
          "Benchmarking 2.0, launched in 2022",
          "streamlines data collection and analysis and ",
          "expands opportunities for robust peer-to-peer conversations ",
          "and best-practice development."
        )
      )
    )),
    
    ###########################################################################
    # side panel
    ###########################################################################
    sidebarPanel(
      # select a base city
      selectInput(
        inputId = 'selectedCity',
        label = 'Select base municipality',
        choices = c(citylabel)
      ),
      
      # select comparison municipalities
      pickerInput(
        inputId = "peerGroup",
        label = "Select comparison municipalities",
        choices = rvllabel,
        selected = rvllabel,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `virtualScroll` = (length(citylabel) - 1)
        )
      ),
      
      # select years
      pickerInput(
        inputId = "selectedYears",
        label = "Select years",
        choices = y_list,
        selected = y_list,
        multiple = TRUE,
        options = list(# `actions-box` = TRUE,
          `virtualScroll` = length(y_list))
      ),
      
      
      
      # Button that submit the above choice of base/comparison municipalities
      actionButton("goButton", "Submit"),
      
      
      
      
      # select a service
      selectInput(
        inputId = 'selectedService',
        label = 'Select service',
        choices = srvclngToShrtRefLstWoc,
        selected = srvclngToShrtRefLstWoc[1]
      ),
      
      
      
      
      
      
      # select a numerator(metric) variable:pulldown menu
      selectInput(
        inputId = 'selectedVar4num',
        label = 'Select service metric',
        choices = c(srv2varlbllst[["amr"]]),
        #selected = initialNumeratorValue
        selected = c()
      ),
      
      # show/hide the denominator-pane
      checkboxInput(
        inputId = 'selectedUseDenominator',
        label = 'Use denominator|context Variable',
        value = FALSE
      ),
      
      # the denominator pane
      conditionalPanel(
        condition = "input.selectedUseDenominator == true",
        
        # select a denominator variable: pulldwon menu
        selectInput(
          inputId = 'selectedVar4denom',
          label = 'Select denominator/context variable',
          choices = c()
        )
      ),
      
      
      # select a multiplier
      
      radioButtons(
        inputId = "selectMultiplier",
        label = "Select a multiplier",
        choices = list(
          "None" = 0,
          "x 100" = 2,
          "x 100K" = 5,
          "x 1M" = 6
        ),
        selected = 0
      ),
      
      
      # add the Average line or not
      checkboxInput(
        inputId = "selectAvg",
        label = "Average of comparison municipalities",
        value = FALSE
      ),
      
      # show/hide the CI-panel and add CIs or not
      conditionalPanel(
        condition = "input.selectAvg == true",
        checkboxInput(
          inputId = "selectCI",
          label = "Add confidence interval (95%)",
          value = FALSE
        )
      ),
      
      # Graph-Downloading button
      # downloadButton('downloadGraph'),
      downloadButton('dwonloadImage'),
      
      # radio-button for page layout
      radioButtons(
        inputId = "selectPageLayout",
        label = "Select page layout",
        choices = list("Portrait" = 0, "Landscape" = 1),
        selected = 1
      )
      
      
    ) # sidePanel
    
    
    
    
  ) # sidebarLayout
  
) # fluidPage

###############################################################################
# server definition
###############################################################################

server <- function(input, output, session) {
  # the following rv is used to store the choice of a service over time
  rv <- reactiveValues()
  
  # w <- waiter::Waiter$new()

  #----------------------------------------------------------------------------
  # observeEvent blocks
  #----------------------------------------------------------------------------

  # behavior of comparison municipalities' checkbox group
  # 
  # (1) this block deals with "all cities" to "some cities" transition
  # uncheck the selectAllpeer box
  # (2) When there is still one checked box, the average-option checkbox
  # can be updated; when there is no checked box, it seems too late to 
  # update the average-option checkbox, i.e., cannot update its state.
  observeEvent(input$peerGroup, { 
    base::message("===== observe:updateCheckbox(Input/selectAllpeer")
    
    if (identical(input$peerGroup, citylabel[!citylabel %in% c(input$selectedCity)])){
      
      # print("state: select all cities")
      # print(input$peerGroup)
      
      # updateCheckboxInput(inputId = "selectAllpeer", value = TRUE)
      shinyjs::enable(id="selectAvg")
      
      
      
    } else {
      # print("state: not select all cities")
      # print("current peerGroup members=")
      # print(input$peerGroup)
      
      if (is.null(input$peerGroup)) {
        # print("state: input$peerGroup is null:empty")
        # do nothing;
        # the average checkbox must have been unchecked
        updateCheckboxInput(inputId = "selectAvg", value = FALSE)
        shinyjs::disable(id="selectAvg")
      } else {
        # print("state: input$peerGroup is not null:not-empty")
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
  
  
  # A-change-of-the-base-city-invoked side-effects
  # the peer-group's membership must be updated as the base-city changes
  # However, a change of the base city should not affect service and variables
  
  observeEvent(input$selectedCity, {
    
    base::message("===== observeEvent(input$selectedCity: start ===============")
    # print("observeEvent:city change: current settings at the begginin=")
    # print("current city=")
    # print(input$selectedCity)
    # print("current service=")
    
    updatedPeerGroup <- citylabel[!citylabel %in% c(input$selectedCity)]
    
    # print(input$selectedService)
    # print("current metric=")
    # print(input$selectedVar4num)
    # print("current peerGroup=")
    # print(input$peerGroup)

    updatePickerInput(session=session, inputId = "peerGroup",
                             choices = updatedPeerGroup,
                             selected = c(updatedPeerGroup))
    base::message("===== observeEvent(input$selectedCity: end ===============")
  })

  
  
  
  
  
  
  
  
  # a-change-of-service-invoked side-effects
  # a change in service => 
  # 1. switch a list of a set of vars(varset4numerator)
  # 2. set the selected var to the first of the above new list
  # a change in input$selectedVar4num does not affect its service
  #
  observeEvent(input$selectedService, {
    
    base::message("===== observeEvent(input$selectedService: start ===============")
    print("observeEvent(service): current input$selectedService=")
    print(input$selectedService)
    rv$lastService <- rv$currentService
    rv$currentService <- input$selectedService;
    print("rv$lastService=")
    print(rv$lastService)
    
    
    
    
    
     
    
    
    
    
    
    
    
    
    
    
    
    
    
    print("observeEvent(service): current metric=input$selectedVar4num=")
    print(input$selectedVar4num)


    
    # new settings
    # change the set of metrics according to a newly selected service
    varset4numerator <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    
    
    print("observeEvent(service): check new the changedvarset4numerator=")
    # print(varset4numerator)
    # print("observeEvent(service): to-be-assgined value for input$selectedVar4num=")
    # print(varset4numerator[1])
    # 
    print("freezeReactiveValue: start")
    freezeReactiveValue(input, "selectedVar4num")
    print("freezeReactiveValue: end")
    updateSelectInput(inputId = "selectedVar4num",
                      choices=c(varset4numerator),
                      selected = c(varset4numerator[1]))
    

    print("observeEvent(service): post-update-check")
    print("observeEvent(service): updated metric=input$selectedVar4num=")
    print(input$selectedVar4num)
    # # 
    # print("observeEvent(service): updated list=varset4numerator[1]=")
    # print(varset4numerator[1])
    # 


    # rawlist <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    # varset4denominator <- rawlist[!rawlist %in% c(input$selectedVar4num)]
    # 
    # print("update the UI: selectedVar4denom ")
    # # 
    # freezeReactiveValue(input, "selectedVar4denom")
    # updateSelectInput(
    #   inputId = "selectedVar4denom",
    #   choices = c(varset4denominator),
    #   selected = c(varset4denominator[1])
    # )
    
    
    
    
    # 
    # 
    base::message("===== observeEvent(input$selectedService: end ===============")
      })
  
  
  
  
  

  
  
  
  # The behavior of the numerator set
  # 
  # This check is meaningful when the denominator option is on;
  # when the denominator-option is not used, there is no need to 
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

    base::message("==== within observeEvent: input$selectedVar4num:start=",
                  as.POSIXct(Sys.time(), tz = "EST5EDT"))
    print("input$selectedVar4num=")
    print(input$selectedVar4num)
    print("input$selectedUseDenominator=")
    print(input$selectedUseDenominator)
    
    
    
    rv$lastServiceWN <- rv$currentServiceWN
    rv$currentServiceWN <- input$selectedService;
    
    print("rv$lastServiceWN=")
    print(rv$lastServiceWN)
    print("rv$currentServiceWN=")
    print(rv$currentServiceWN)
    
    print("rv$lastService=")
    print(rv$lastService)
    print("rv$currentService=")
    print(rv$currentService)
    
    
    
    # get the current list of **all** variables for the current service
    # note: this list now includes census variables
    rawlist <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    # print("rawlist=")
    # print(rawlist)
    # the list of denominators must excluded the variable
    # that is currently selected as the numerator
    # so exclude the numerator from the above list
    
    varset4denominator <- rawlist[!rawlist %in% c(input$selectedVar4num)]
    
    print("current varset4denominator[1]=")
    print(varset4denominator[1])
    
    if (input$selectedUseDenominator) {
      # denominator-option is on
      
      # there are two situations to pass through this block
      # 
      # (1) While the same service is still kept,
      # the numerator has been changed, and thus this change results in at least
      # updating the denominator set and if necessary, the choice for 
      # the denominator (for either case, a choice for the numerator cannot be 
      # included in the denominator set)
      # 
      # (1-A) if the newly selected variable for the numerator is the one
      # previously selected for the denominator, the denominator's state 
      # must be reset and set to the first element of its set
      # 
      # (1-B) if the newly selected variable for the numerator is NOT the one
      # previously selected for the denominator, the denominator can keep the
      # currently chosen one
      # 
      # (2) A change occurred in the service, and 
      # the variable set for the numerator has been reset to 
      # the one of a new service and default choice is set to its first element,
      # and the denominator set must be reset and so is its default choice 
      # according to the default choice of the numerator
      # 
      # Are (1-A), (1-B) and (2) mutually exclusive? 
      # (1-A) can occur only if the current and previous services are the same
      # however, according the current coding, census variables are the same 
      # across the services, census_1 in service #1 cannot be distinguishable
      # from census_1 in service #2
      # So the first conditional statement should be (2) that handles the 
      # transition from service #i to service #j;
      # then within the cases of the same service, separate (1-A) and (1-B)
      
      
      print("current input$selectedVar4num=")
      print(input$selectedVar4num)
      
      print("current input$selectedVar4denom=")
      print(input$selectedVar4denom)
      
      
      
      if (rv$lastServiceWN == input$selectedService){
        print("%%%%% last service and current ones are the same %%%%%")
        print("This is within-service change")
        # handle cases of (1-A) and (1-B) 
        if (input$selectedVar4num == input$selectedVar4denom) {
          
          print("numerator is the previously selected denominator")
          print("reset the selected one for the denominator")
          
          updateSelectInput(
            inputId = "selectedVar4denom",
            choices = c(varset4denominator),
            selected = c(varset4denominator[1])
          )
          
        } else {
          
          
            print("keep the current choice of denominator")

            # print("varset4denominator:2nd[1]=")
            # print(varset4denominator[1])

            updateSelectInput(
              inputId = "selectedVar4denom",
              choices = c(varset4denominator),
              selected = c(input$selectedVar4denom)
            )
          
          
          
          
        }
        
      } else {
        print("%%%%% last service and current one are different %%%%%")
        print("This is a cross-service change")
        # handle cases of (2)

        updateSelectInput(
          inputId = "selectedVar4denom",
          choices = c(varset4denominator),
          selected = c(varset4denominator[1])
        )
        
        
      }



      
    } else {
      # While denominator-option is off, keep updating
      # the denom-var set-synced with the current state (service/numerator) 
      print("denominator is off: just update the denominator set")
      updateSelectInput(
        inputId = "selectedVar4denom",
        choices = c(varset4denominator),
        selected = c(varset4denominator[1])
      )
    }
    base::message("==== within observeEvent: input$selectedVar4num:end=",
                  as.POSIXct(Sys.time(), tz = "EST5EDT"))
  })
  
  
  
  
  
  # This block shows/hides the denominator UI pane 
  # 
  observeEvent(input$selectedUseDenominator, {
    base::message("===== within observeEvent: input$selectedUseDenominator")
    print("input$selectedUseDenominator=")
    print(input$selectedUseDenominator)
    if (input$selectedUseDenominator){
      print("use denominator case")
      updateRadioButtons(inputId = "selectMultiplier", selected = 2)
    } else {
      print("do not use denominator case")
      updateRadioButtons(inputId = "selectMultiplier", selected = 0)
    }
    
    print("within observeEvent: input$selectedUseDenominator: after multiplier")
    print("input$selectedService=")
    print(input$selectedService)
    print("input$selectedVar4num=")
    print(input$selectedVar4num)
    
    
    
    # get the current list of **all** variables for the current service
    # note: this list now includes census variables
    rawlist <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    # print("rawlist=")
    # print(rawlist)
    # the list of denominators must excluded the variable
    # that is currently selected as the numerator
    # so exclude the numerator from the above list

    varset4denominator <- rawlist[!rawlist %in% c(input$selectedVar4num)]

    # print("update the UI: selectedVar4denom ")
    # 
    freezeReactiveValue(input, "selectedVar4denom")
    updateSelectInput(
      inputId = "selectedVar4denom",
      choices = c(varset4denominator),
      selected = c(varset4denominator[1])
    )
    
    # print("input$selectedVar4denom=")
    # print(input$selectedVar4denom)
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
  # observeEvent(input$selectedUseDenominator,{
  #   if (input$selectedUseDenominator){
  #     updateRadioButtons(inputId = "selectMultiplier", selected = 2)
  #   } else {
  #     updateRadioButtons(inputId = "selectMultiplier", selected = 0)
  #   }
  # })
  
  # years UI: Must warn the no-selection case
  observeEvent(input$selectedYears,{
    
    message("===== within observeEvent: input$selectedYears")
    # print("length(input$selectedYears")
    # print(length(input$selectedYears))
    
    if (length(input$selectedYears) == 0){
      
      # issue the warning message
      validate("Please select at least one year!")

      # message("no-year-selected state: the last year will be selected")
      
      # The following updatePickerInput() is not working for some unknown reason
      updatePickerInput(session=session, 
        inputId = "selectedYears",
        choices = c(y_list),
        selected = c(y_list[length(y_list)])
      )
      
      
      
      
    } else {
      # do nothing 
      # shinyFeedback::hideFeedback("selectedYears")
    }

  })
  

  #############################################################################
  # renderText(): generating the first-time-only session-starting message
  #############################################################################
  output$initialMessage <- renderText({
    if (input$goButton == 0) {
      HTML(paste(
        "Welcome to Benchmarking 2.0.",
        paste(
          "Please click the Submit button ",
          "to start a new benchmarking session.",
          sep = "<br />"
        ),
        sep = "<br /><br />"
      ))
    } else {
      paste(c(""))
    }
    
  })
  
  # waitress <- waiter::Waitress$new("#benchmarkingDB", hide_on_render = TRUE)

  
  #############################################################################
  # benchmarking-plot-generating function 
  #############################################################################
  benchmarking_plot <- reactive({
    # waitress <- waiter::Waitress$new("#benchmarkingDB",  theme = "overlay", min = 0, max = 10)
    # 
    # for(i in 1:15){
    #   waitress$inc(10)
    #   Sys.sleep(.3)
    # }
    
    # att <- Attendant$new("progress-bar")
    # 
    # for(i in 1:10){
    #   Sys.sleep(runif(1))
    #   att$set(i * 10, text = sprintf("%s%%", i * 10))
    # }
    base::message("==== benchmarking_plot: start-time=",
    as.POSIXct(Sys.time(), tz = "EST5EDT"))


    # The following setting ensures that 
    # peer-group membership is not reactively (instantaneously) updated;
    # the current number of members seems to be too many to reactively handle
    # quick check/uncheck-box actions; consequently, 
    # it is now collectively updated by clicking the Submit button and 
    # isolate() is used here
    checkedM <- isolate(input$peerGroup)
    print("input$peerGroup: checkedM=")
    print(checkedM)
    
    # The first-time handling when the Submit button has nothing to submit
    if (input$goButton == 0){
      # waitress$close()
      return()
    }
    # -------------------------------------------------------------------------
    # input: sanity check
    # -------------------------------------------------------------------------
    # print("selectedYears=")
    # print(length(input$selectedYears))
    
    # Currently there is no convenient, ready-to-use solution about other than
    # the following primitive, warning-message solution
    # attempts to automatically set the latest year selected at least 
    # when a user mistakenly unchecked all years did not work so far. 
    if (length(input$selectedYears) == 0){
      validate("Please select at least a year")
    }

    # sanity check against key input variables: base city, service, numerator
    # variable
    print("before req block: sanity check")
    
    print("input$selectedCity=")
    print(input$selectedCity)
    print("input$selectedService=")
    print(input$selectedService)
    print("input$selectedVar4num=")
    print(input$selectedVar4num)
    req(input$selectedCity,
        input$selectedService,
        input$selectedVar4num
    )
    varset4check <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    #print("varset4check=")
    #print(varset4check)
    if (input$selectedVar4num %in% varset4check) {
      print("member")
    } else {
      print("not member")
    }
    print("before denominator-req: sanity check")
    print("input$selectedVar4denom=")
    print(input$selectedVar4denom)
    if (input$selectedVar4denom %in% varset4check) {
      print("denominator is a member")
    } else {
      print("not a member")
    }
    
    if (input$selectedUseDenominator){
      req(input$selectedVar4denom %in% varset4check)
    }
    # 
    # isolcating the base-city did not work; it seems the choice of a base city
    # must reactively update the state of selected comparison cities;
    # otherwise, isolating the choice of a base-city(= waiting for a submission
    # action to complete the choice) ended up erratic/unstable UI states.
    # baseCity = isolate(input$selectedCity) <= this did not work
    baseCity = input$selectedCity
    
    # -------------------------------------------------------------------------
    # data-prep stage: 2 steps [factory-default] or 4 steps
    # 
    # if the denominator option is not selected
    # step 1: base-city (numerator)
    # step 2: peer-group (numerator)
    # 
    # if the denominator option is selected
    # steps 1/2: base-city (numerator, denominator)
    # steps 3/4: peer-group (numerator, denominator)
    # -------------------------------------------------------------------------
    # note: data for these numerators and denominators are organized as matrix,
    # not tibble for easier division-operations
    
    #--------------------------------------------------------------------------
    # step 1: base city's numerator
    #--------------------------------------------------------------------------
    #
    # !!!!!!!!!! warning !!!!!!!!!!
    # 
    # About duplicated variable names
    # 
    # the same variable name appears in different services, i.e.,
    # the same variable name is used in more than one services and therefore
    # the service filter must be applied first before the filter of variable 
    # name


    print("selected Service=")
    print(input$selectedService)
    lastService <-input$selectedService
    print("++++++++++++++++++ lastService ++++++++++++++++++")
    print(lastService)

    
    # get full service-name for user-friendly rendering
    serviceNameFull <-
      names(srvclngToShrtRefLstWoc)[which(srvclngToShrtRefLstWoc == input$selectedService)]
    # print("full service name=")
    # print(serviceNameFull)
    
    print("currently selected numerator=")
    print(input$selectedVar4num)


    # define a coding-friendly var for the denominator-option switch
    useDenominator <- FALSE
    if (input$selectedUseDenominator) {
      useDenominator <- TRUE
      print("A case of using a denominator")
      # print(input$selectedUseDenominator)
      
      print("current denominator=")
      print(input$selectedVar4denom)
      
    } else {
      # print("This request does not use a denominator")
    }



    # dplyr's select() had some issue with a vector of numeric values
    # given by an input variable such as c(2020, 2021, 2022)
    #
    # print("selectedYears=")
    # print(input$selectedYears)
    # print(typeof(input$selectedYears))
    # print(str(input$selectedYears))
    #
    # dplyr::select() failed to evaluate input$selectedYears as a vector
    # for some reason
    # make sure the vector is character
    selectedYearsC <- as.character(input$selectedYears)
    
    # 
    print("selectedYearsC=")
    print(selectedYearsC)

    
    
    # !!!!!!!!!! warning !!!!!!!!!!
    # participating cities do not provide a complete data for a given year
    # under a certain combination of selection criteria, 
    # there would be an empty-data case
    # The following block checks this empty-data case for the base city 
    # and if so, just generates a warning message; 
    # Even if a base-city does not have data, this does not terminate 
    # the whole rendering process and tries to display a peer-group's data.
    # 
    valueAvailableCities <- bd_data %>%
      filter(Service == input$selectedService |
               Service == "census")   %>%
      filter(Variable == input$selectedVar4num)    %>%
      filter(!is.na(Value)) %>%
      distinct(Municipality) %>%
      pull()
    
    print("valueAvailableCities=")
    print(valueAvailableCities)
    print("selected city=")
    print(baseCity)

    msg_no_base_m_data <- ""
    if (is.na(match(baseCity, valueAvailableCities))) {
      msg_no_base_m_data <-  paste(
        "\nData for the selected base municipality (",
        baseCity,
        ") are not available for these years."
      )
    }




    # generating a base-city's numerator data as matrix
    # dimension: 1 x # of selected years
    data_sc_nm <- bd_data %>%
      filter(Service == input$selectedService |
               Service == "census")   %>%
      filter(Variable == input$selectedVar4num)    %>%
      filter(Municipality == baseCity) %>%
      spread(key = Year, value = Value)        %>%
      select(selectedYearsC)        %>%
      as.matrix()
    
    print("data_sc_nm: afer as.matrix()=")
    print(data_sc_nm)

    # re-add column names: the above as.matrix() removed column names
    colnames(data_sc_nm) <- selectedYearsC
    
    print("data_sc_nm: afer colnames=")
    print(data_sc_nm)

    suppressWarnings(storage.mode(data_sc_nm) <- "numeric")
    print("data_sc_nm:after changing stroage mode=")
    print(data_sc_nm)
    print(str(data_sc_nm))
    
    
    # Before the adjustment by the multiplier,
    # check whether it is an all-NA case first
    numerator_is_all_na <- FALSE
    if (all(is.na(data_sc_nm))) {
      message("Base municipality's numerator: all NA case")
      
      numerator_is_all_na <- TRUE
      
      msg_no_base_m_data <-  paste(
        "\nData for the selected base municipality (",
        baseCity,
        ") are not available for these years."
      )
      
      
      # what happens if all-NA
      #validate("Please select at least a year")
      
      
    } else {
      data_sc_nm <- 10 ^ as.integer(input$selectMultiplier) *  data_sc_nm
    }
    
    # print("data_sc_nm: after multiplier is aplied=")
    # print(data_sc_nm)
    
    # re-attach the base-city's name to the row
    rownames(data_sc_nm) <-  baseCity
    
    
    print("data_sc_nm: after adding rowname=")
    print(data_sc_nm)

    #--------------------------------------------------------------------------
    # step 2: base municipality: denominator: to be ignored if no denominator
    #--------------------------------------------------------------------------
    # base-line data for the denominator variables must be added

    if (useDenominator) {
      
      print("base m: denominator data block: ========== start ==========")
      print("current city=")
      print(baseCity)
      print("current service=")
      print(input$selectedService)
      print("current metric=")
      print(input$selectedVar4denom)
      
      
      # generating a base-city's denominator data as matrix
      # dimension: 1 x # of selected years
      data_sc_dm <- bd_data %>%
        filter(Service == input$selectedService |
                 Service == "census")   %>%
        filter(Variable == input$selectedVar4denom)  %>%
        filter(Municipality == baseCity) %>%
        spread(key = Year, value = Value)        %>%
        select(selectedYearsC)        %>%
        as.matrix()
      
      
      # print("data_sc_dm:before=")
      # print(data_sc_dm)
      
      # re-attach the base-city's name
      dimnames(data_sc_dm)[[1]] <- baseCity
      
      # print("data_sc_dm:after=")
      # print(data_sc_dm)
      
      # generating a waning message
      if (all(is.na(data_sc_dm))) {
        message("base city: denominator: all NA case")
        numerator_is_all_na <- TRUE
        msg_no_base_m_data <-  paste(
          "\nDenominator data for the selected base municipality (",
          baseCity,
          ") is not available for these years."
        )
      }
      
      
    }


    # -------------------------------------------------------------------------
    # step 3: peer-group's numerator data
    # -------------------------------------------------------------------------
    # warning: since the same variable name is used in more than one services
    # the service filter must be applied first
    # 
    # base-line data for the numerator

    # note: Even if the comparison group is empty, 
    # as long as valid data for the base municipality exist,
    # the rendering process continues

    if (!is.null(checkedM)) {
      
      
      # generating a peer-group's numerator data as matrix
      # dimension: # of selected comparison cities x # of selected years
      data_pg_nm <- bd_data %>%
        filter(Service == input$selectedService |
                 Service == "census")   %>%
        filter(Variable == input$selectedVar4num)   %>%
        filter(Municipality %in% checkedM) %>%
        arrange(Municipality, Year) %>%
        spread(key = Year, value = Value)       %>%
        select(selectedYearsC) %>%
        as.matrix()
      
      # print("data_pg_nm(s)=")
      # print(data_pg_nm)
      
      
      # Before the adjustment by the multiplier,
      # check whether it is an all-NA case first
      if (all(is.na(data_pg_nm))) {
        message("peer group's numerator: all NA case")
        
        pg_numerator_is_all_na <- TRUE
        
        msg_no_pg_m_data <-  paste(
          "\nNumerator data for the current peer-group members (",
          checkedM,
          ") are not available for these years."
        )
      } else {
        data_pg_nm <- 10 ^ as.integer(input$selectMultiplier) * data_pg_nm
      }
      
      # print("data_pg_nm(b)=")
      # print(data_pg_nm)
      
      # re-attach peer-group's name vector
      rownames(data_pg_nm) <- checkedM
      
      # print("data_pg_nm(a)=")
      # print(data_pg_nm)
      
    }
    
    # -------------------------------------------------------------------------
    # step 4: peer-group's denominator if the denominator option is selected
    # -------------------------------------------------------------------------

    if (!is.null(checkedM)) {
      if (useDenominator) {
        
        # generating a peer-group's denominator data as matrix
        # dimension: # of selected comparison cities x # of selected years
        
        data_pg_dm <- bd_data %>%
          filter(Service == input$selectedService |
                   Service == "census")   %>%
          filter(Variable == input$selectedVar4denom) %>%
          filter(Municipality %in% checkedM) %>%
          spread(key = Year, value = Value)       %>%
          select(selectedYearsC) %>%
          as.matrix()
        
        # re-attach the peer-group's name vector
        rownames(data_pg_dm) <- checkedM
        

        # print("data_pg_dm=")
        # print(data_pg_dm)
      }
    }
    
    # -------------------------------------------------------------------------
    # additional step if the denominator option is selected
    # calculating a quotient for the peer-group for selected years
    # -------------------------------------------------------------------------
    

    if (!is.null(checkedM)) {
      
      # print("selectedYears=")
      # print(selectedYearsC)
      # print("selectedYears:length=")
      # print(length(selectedYearsC))
      
      
      
      # working on the peer-group first
      # create a copy of the numerator data-matrix
      data4EachPeerCity_raw_m <- data_pg_nm
      
      # if denominoatr option is select6ed
      if (useDenominator) {
        data4EachPeerCity_raw_m <- data4EachPeerCity_raw_m / data_pg_dm
      }
      
      print("data4EachPeerCity_raw_m=")
      print(data4EachPeerCity_raw_m)
      # print(str(data4EachPeerCity_raw_m))
      # print("has_rownames: data4EachPeerCity_raw_m=")
      # print(has_rownames(data4EachPeerCity_raw_m))
      
      # attach a column of city names to the data and convert to tibble
      data4EachPeerCity_rawt <-
        rownames_to_column(as.data.frame(data4EachPeerCity_raw_m),
                           var = "catgry") %>%
        as_tibble()
      
      
      print("data4EachPeerCity_rawt=")
      print(data4EachPeerCity_rawt)
      
      # tibble: wide to long transformation
      data4EachPeerCity_rawt <- data4EachPeerCity_rawt %>%
        gather(selectedYearsC, key = "Year", value = "quotient")
      
      
      print("data4EachPeerCity_rawt=")
      print(data4EachPeerCity_rawt)
      
      # extra step if the denominator option is selected
      tmpTibblePg <- NULL
      if (useDenominator) {
        tmpTibblePg <- as_tibble(data_pg_nm / data_pg_dm)
      } else {
        tmpTibblePg <- as_tibble(data_pg_nm)
      }
      
      
      
      # re-attach column names after the above matrix-element-wise division
      # ???????? data4EachPeerCity is used later ?????
      data4EachPeerCity <- tmpTibblePg  %>%
        gather(selectedYearsC, key = "Year", value = "quotient")
      
      
      print("data4EachPeerCity=")
      print(data4EachPeerCity)
      
      
      # A convenient way to calculate year-wise mean and CI values by using 
      # Rmisc::susummarySE()
      summarizedPG <- tmpTibblePg %>%
        gather(selectedYearsC, key = "Year", value = "quotient") %>%
        Rmisc::summarySE(measurevar = "quotient",
                  groupvars = "Year",
                  na.rm = TRUE)
      
      # print("summarizedPG=")
      # print(summarizedPG)
      
    } # end of non-empty peerGroup-case

    # -------------------------------------------------------------------------
    # additional step if the denominator option is selected
    # calculating quotients for the selected city for selected years
    # -------------------------------------------------------------------------
    tmpMatrixScQ <- NULL
    if (!is.null(checkedM)) {
      # non-empty peer-group cases
      if (useDenominator) {
        tmpMatrixScQ <- data_sc_nm / data_sc_dm
      } else{
        tmpMatrixScQ <- data_sc_nm
      }
      
    } else {
      # empty peer-group case
      
      if (useDenominator) {
        tmpMatrixScQ <- data_sc_nm / data_sc_dm
      } else{
        tmpMatrixScQ <- data_sc_nm
        
        
        # print("tmpMatrixScQ=")
        # print(tmpMatrixScQ)
        # print("t(tmpMatrixScQ)[, 1]=")
        # print(t(tmpMatrixScQ)[, 1])
      }
      
    }


    # last step: selected City
    if (!is.null(checkedM)) {
      # re-organize data with a rowname
      data4plot_raw <- rbind(t(tmpMatrixScQ)[, 1], t(summarizedPG[, 3]))
      rownames(data4plot_raw) <- c(baseCity, "Average")
    } else {
      # do nothing for no-peer-group case, 
      # i.e., rendering the selected city only
      data4plot_raw <- tmpMatrixScQ
      
      
      # print("data4plot_raw=")
      # print(data4plot_raw)

    }

    print("data4plot_raw=")
    print(data4plot_raw)
    
    
    #--------------------------------------------------------------------------
    # additional steps before rendering 
    #--------------------------------------------------------------------------
    #
    # step to convert rownames to a column whose name is catgry and
    # transform it to tibble
    data4plot <-
      rownames_to_column(as.data.frame(data4plot_raw), var = "catgry") %>%
      as_tibble()
    
    
    print("data4plot:b=")
    print(data4plot)
    
    
    # step to handle the number of selected years is one, etc.
    # 
    # If # of years is 1, dynamically rename the column name 
    # from the generic "V1" to the selected year such as "2022"
    # for later plotting (labeling)
    # 
    if (ncol(data4plot) == 2) {

      yr <- selectedYearsC[1]
      # print(yr)
      if (is.null(checkedM)) {
        # do nothing
      } else {
        print("***** column is 2 *****")
        # Note about the following coding in rename() function, "!!yr :="
        # since the selected year is dynamically determined, i.e., 
        # we cannot specify a particular year's value such as "2022" here
        # and this notation is necessary
        # see the following source:
        # https://community.rstudio.com/t/pass-a-variable-to-dplyr-rename-to-change-columnname/6907
        
        data4plot <- data4plot %>% dplyr::rename(!!yr := "V1")
      }
      
    } else {
      print("***** column is not 2 *****")
      print("ncol(data4plot)=")
      print(ncol(data4plot))
    }
    print("data4plot:a=")
    print(data4plot)
    
    
    
    # the following line fails if the number of years is 1
    data4plot_sc <- data4plot %>%
      gather(selectedYearsC, key = "Year", value = "quotient") %>%
      filter(catgry == baseCity)
    
    print("data4plot_sc=")
    print(data4plot_sc)
    
    if (!is.null(checkedM)) {
      data4plot_pg <- data4plot %>%
        gather(selectedYearsC, key = "Year", value = "quotient") %>%
        filter(catgry != baseCity)
      
      
      print("data4plot_pg=")
      print(data4plot_pg)
      
      data4plot_pgx <-
        data4plot_pg %>% add_column(ci = summarizedPG[["ci"]])
      
      print("data4plot_pgx=")
      print(data4plot_pgx)
      
      # print("==================== end of data-prep for plot ==================")
      
    }
    # -------------------------------------------------------------------------
    # plotting
    # -------------------------------------------------------------------------
    # An if-block after ggplot() seems not to accept more than 1 geom_xxx
    # statements within it and the following incremental approach was used
    

    # print("==================== beginning of plot ==================== ")
    
    # setting for using a custom google-font 
    showtext_auto()
    
    # update the palette according to the current peer-group members
    fixed_f_scale <-
      scale_fill_manual(name = "Legend", values = pairedPalette[checkedM])
    fixed_c_scale <-
      scale_color_manual(name = "Legend", values = pairedPalette[checkedM])
    fixed_s_scale <-
      scale_shape_manual(name = "Legend", values = shapeNoList[checkedM])
    
    # baseline rendering
    # This block generates the bar plot for the base city
    plt1 <- data4plot_sc %>%
      ggplot() +
      geom_col(aes(x = Year, y = quotient, fill = catgry)) +
      scale_y_continuous(name = "Value", labels = comma) +
      scale_fill_manual(name = "Base", values = c("#B3B3B3"))
    
    # incremetally adding optional items
    # collective average line instead of individual lines for the peer-group 
    # also overlaying points
    if (input$selectAvg) {
      # print("extra-step for adding average")
      # add an average line
      plt1 <- plt1 +
        geom_line(
          data = data4plot_pg,
          aes(
            x = Year,
            y = quotient,
            group = catgry,
            color = catgry
          ),
          size = 1
        ) +
        geom_point(
          data = data4plot_pg,
          aes(
            x = Year,
            y = quotient,
            group = catgry,
            color = catgry
          ),
          size = 3
        )
      # handling the CI-error-bar option
      if (input$selectCI) {
        # print("data4plot_pgx=")
        # print(data4plot_pgx)
        # add CI-bands
        plt1 <- plt1 + geom_errorbar(
          data = data4plot_pgx,
          aes(
            x = Year,
            y = quotient,
            ymin = quotient - ci,
            ymax = quotient + ci
          ),
          width = .1,
          color = "#696969",
          position = position_dodge(0.1)
        )
      }
      
      
    } else {
      # each comparison municipality's line is added
      # tibble to be used
      # print("no average, etc.")
      
      if (!is.null(checkedM)) {
        if (length(selectedYearsC) > 1) {
          # multiple years => lines 
          
          # print("data4EachPeerCity_rawt=")
          # print(data4EachPeerCity_rawt)
          # print("data4EachPeerCity_rawt$catgry=")
          # print(data4EachPeerCity_rawt$catgry)
          # print("levels(data4EachPeerCity_rawt$catgry)=")
          # print(levels(data4EachPeerCity_rawt$catgry))
          
          plt1 <- plt1 +
            geom_line(
              data = data4EachPeerCity_rawt,
              aes(
                x = Year,
                y = quotient,
                group = catgry,
                color = catgry
              ),
              size = 1
            ) +
            geom_point(
              data = data4EachPeerCity_rawt,
              aes(
                x = Year,
                y = quotient,
                shape = catgry,
                color = catgry
              ),
              size = 3
            )
          
        } else {
          # single-year => points
          plt1 <- plt1 +
            geom_point(
              data = data4EachPeerCity_rawt,
              aes(
                x = Year,
                y = quotient,
                shape = catgry,
                color = catgry
              ),
              size = 3
            )
        }
        
      }
    }


    # print("numerator=")
    # print(input$selectedVar4num)
    # print("numerator's name(label) check=")
    # print("v2lallinOne[[input$selectedVar4num]]=")
    # print(v2lallinOne[[input$selectedVar4num]])


    # numerator(metric) variable name: full
    metricVarLabel <- stringr::str_wrap(v2lallinOne[[input$selectedVar4num]], 
                                        width = 80)
    # print(metricVarLabel)
    

    # print("denominator=")
    # print(input$selectedVar4denom)
    # print("name check: denominator: input$selectedVar4denom=")
    # print("current input$selectedService is=")
    # print(input$selectedService)
    
    # denominator(context) variable name: full

    # print("no denominator case: use blank")
    contextVarLabel <- ""
    if (useDenominator) {
      # print("use denominator case: variable-name")
      contextVarLabel <- paste("\n/",
        stringr::str_wrap(v2lallinOne[[input$selectedVar4denom]], width = 80))
    }
    # print("contextVarLabel=")
    # print(contextVarLabel)

    # title 
    titleText <- paste(c("" ,
                         metricVarLabel,  contextVarLabel), collapse = "")
    # print("titleText=")
    # print(titleText)
    
    
    # print("multiplier=")
    # print(input$selectMultiplier)
    multiplierValue <-
      as.character(10 ^ as.integer(input$selectMultiplier))
    
    
    peerGroupList <- ""
    if (!is.null(checkedM)) {
      peerGroupList <- stringr::str_wrap(
        paste(checkedM, collapse = ", ")
        , width = 80)
    }
    
    
    subtitleText <- paste(
      c(
        "Base Municipality: ",
        baseCity,
        "\nService: ",
        serviceNameFull,
        "\nComparison Municipalities: ",
        peerGroupList,
        "\nMultiplier: ",
        multiplierValue,
        paste("\ngenerated at: ", 
        as.character(lubridate::with_tz(Sys.time(), tzone="EST5EDT"), 
                     usetz=TRUE), sep=""),
        "\n\nData upated on: November 22, 2022"
      ),
      collapse = ""
    )
    # print("subtitleText=")
    # print(subtitleText)
    
    
    # -------------------------------------------------------------------------
    # furnish the graph with its title, etc.
    # -------------------------------------------------------------------------
    
    # adding title/caption data
    if (input$selectAvg) {
      # with the average line
      plt1 <- plt1 +  labs(title = titleText,
                           subtitle = msg_no_base_m_data,
                           caption = subtitleText)
    } else {
      # no average line
      plt1 <- plt1 +  labs(title = titleText,
                           subtitle = msg_no_base_m_data,
                           caption = subtitleText)
    }

    # The average-option-related special processing
    if (input$selectAvg) {
      # with the average line
      plt1 <- plt1 +
        fixed_c_scale +
        scale_color_discrete(
          name = "Legend",
          labels = c("average of \ncomparison \nmunicipalities")
          )
      
    } else {
      # without the average line
      plt1 <- plt1 +
        fixed_c_scale +
        fixed_s_scale
    }
    
    # common legend-related settings
    plt1 <- plt1 +
      theme_bw() +
      theme(
        text = element_text(family = "barlow"),
        plot.title =   element_text(size = 22,
                                    vjust = 5),
        plot.subtitle = element_text(size = 20,
                                     color = "red"),
        plot.margin =  margin(t = 40, l = 20),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        legend.title = element_text(size = 18),
        legend.text =  element_text(size = 14),
        axis.title.y = element_blank(),
        axis.title.x =  element_text(size = 14),
        axis.text.y =   element_text(size = 12),
        axis.text.x =   element_text(size = 12)
      ) +
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(order = 1),
        fill = guide_legend(order = 2)
      )
    
    
    
    base::message("====== benchmarking_plot: end-time:", as.POSIXct(Sys.time(),
      tz = "EST5EDT"))
    # waitress$close()
    plt1

}) # end of tab 1's plot-generation function
  
  
  #############################################################################
  # renderPlot(): generating the benchmarking plot
  #############################################################################  
  output$benchmarkingDB <- renderPlot({
    # waitress$start(h3("Loading image ..."))
    # w$show()
    waiter::waiter_show(html = waiting_screen, color = "#4B9CD380")
    
    print(benchmarking_plot())
    # waitress$close() 
    # w$hide()
    
    waiter_hide()
  })
  
  
  
  
  #############################################################################
  # downloading the current benchmarking graph as a PDF file
  #############################################################################
  output$dwonloadImage <- downloadHandler(
    filename = function(){
      pdf_file_name <-paste(tempfile(), ".pdf", sep = "")
      print("pdf_file_name=");print(pdf_file_name)
      pdf_file_name
    },
    content = function(file){
      
      
      if (input$selectPageLayout == 0) {
        # portrait case
        # print("portrait request")
        paperWidth <- 8.5
        paerHeight <- 11
      }
      
      
      # print("paperWidth=")
      # print(paperWidth)
      # print("paerHeight=")
      # print(paerHeight)
      
      ggsave(filename = file, 
             plot = benchmarking_plot(),
             device=cairo_pdf,
             width = paperWidth,
             height = paerHeight,
             units = "in"
             )
      
    }
    
    
  )
  
  waiter::waiter_hide()

} # end of server



shinyApp(ui = ui, server = server)
