library(shiny)
source("helpers.R")
library(shinyjs)
library(grDevices)
library(shinyWidgets)
library(waiter)
library(lubridate)
library(DT)
library(cowplot)
library(flextable)
library(grid)
waiting_screen <- shiny::tagList(
  waiter::spin_wave(),
  shiny::h4("loading ...")
) 


################################################################################
# ui defintion
################################################################################
ui <- shiny::fluidPage(
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
  
  
  
  # shinyFeedback::useShinyFeedback(),
  #  shinythemes::themeSelector(),
  shinyjs::useShinyjs(),
  
  # The following css setting was ignored by ggplot2;
  # however, it seems to control the font-settings of the contents/side-panel areas
  # htmltools::includeCSS("www/css/barlow-semi-condensed.css"),
  
  # The following setting was ignored by ggplot2
  # gfonts::use_font(
  #   id = "barlow-semi-condensed",
  #   css_path = file.path("www", "css/barlow-semi-condensed.css")
  # ),
  
  #
  shiny::tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
    href = "css/barlow-semi-condensed.css"),
    shiny::tags$style(
      shiny::HTML(
        "* {font-family: 'Barlow Semi Condensed', sans-serif; }
        .titleText {
        font-weight:700;
        } 
    .shiny-output-error-validation {
    color: red;
    font-size: 12px;
    }
    .metric_def_table 
      
      table {
      border-top: 1px solid black;
      border-bottom: 1px solid black;
      font-size:16px;
    }
    #goButton{
    margin-bottom: 20px;}
    "
      )
    )),
  
  
  
  shiny::titlePanel("Benchmarking 2.0"),
  
  # sidebarLayout = mainPanel + sidebarPanel
  shiny::sidebarLayout(
    ###########################################################################
    # main panel
    ###########################################################################
    shiny::mainPanel(shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Home",
        shiny::br(),
        shiny::tags$div(
          style="text-align: left",
          shiny::htmlOutput(outputId = "initialMessage",
                            container = shiny::h3, style="font-weight:800;"),
          
          shiny::plotOutput(outputId = "benchmarkingDB", height = "500px", 
                             width = "90%"),
          # 1000px
          # shiny::plotOutput(outputId = "benchmarkingDB", 
          #                   width = "90%")
          tags$div(
            class="metric_def_table",
            shiny::tableOutput("metricDefTable")
          )

        )

        
      ),
      
      shiny::tabPanel(
        "About",
        shiny::tags$h3("Benchmarking 2.0: Project Overview"),
        
        shiny::tags$p(
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
    shiny::sidebarPanel(
      style="background-color: rgba(123, 175, 212, 0.15); font-family: 'Barlow Semi Condensed', sans-serif;font-weight: 100;",
      # #7BAFD4
      # rgba(123, 175, 212, 1)
      # 
      # 
      #------------------------------------------------------------------------
      # Step 1: Base city
      #------------------------------------------------------------------------
      # select a base city
      #shiny::selectInput(
      shinyWidgets::pickerInput(

        inputId = 'selectedCity',
        label = 'Step 1: Select your base municipality',
        choices = c(citylabel),
        options = list(
          `live-search` = TRUE)
      ),
      
      #------------------------------------------------------------------------
      # Step 2: Rendering type
      #------------------------------------------------------------------------
      # 2023-new addition
      # 2024-02 revision: 3rd option was added
      # 
      # radio button 
      # The default is to individually render up to 5 peers 
      # Two extra alternatives
      # 2A: average among the selected
      # 2B: average of all cities
      # 
      shiny::radioButtons(
        inputId = "selectRenderingType",
        label = "Step 2: Select whether you would like to:",
        choices = list(
          "Individually compare with up to 5 municipalities" = 0,
          "Compare with an average of as many municipalities as you like" = 1,
          "System average of all jurisdictions" =2
        ),
        selected = 0
      ),
      
      #------------------------------------------------------------------------
      # Step 3: peers or comparison municipalities
      #------------------------------------------------------------------------
      # select comparison municipalities
      shinyWidgets::pickerInput(
        inputId = "peerGroup",
        label = "Step 3: Select comparison municipalities",
        choices = rvllabel,
        multiple = TRUE,
        options = pickerOptions(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `virtualScroll` = (length(citylabel) - 1),
          `max-options` = (sizeOfMuniucipalities -1),
          `max-options-text` = "individual rendering is limited to 5 municipalities",
          noneSelectedText="None"
        )
      ),
      shiny::textOutput("step3message"),
      
      
      #------------------------------------------------------------------------
      # Step 4: Years
      #------------------------------------------------------------------------
      # select years
      shinyWidgets::pickerInput(
        inputId = "selectedYears",
        label = "Step 4: Select years",
        choices = y_list,
        selected = y_list,
        multiple = TRUE,
        options = list(# `actions-box` = TRUE,
          `virtualScroll` = length(y_list))
      ),
      
      

      #------------------------------------------------------------------------
      # Step 5: Service
      #------------------------------------------------------------------------
      
      # select a service
      #shiny::selectInput(
      shinyWidgets::pickerInput(
        inputId = 'selectedService',
        label = 'Step 5: Select service',
        choices = srvclngToShrtRefLstWoc,
        selected = srvclngToShrtRefLstWoc[1],
        options = list(
          `live-search` = TRUE)
      ),
      
      
      
      
      #------------------------------------------------------------------------
      # Step 6: numerator 
      #------------------------------------------------------------------------
            # select a numerator(metric) variable:pulldown menu
      #shiny::selectInput(
      shinyWidgets::pickerInput(
        inputId = 'selectedVar4num',
        label = 'Step 6: Select service metric',
        choices = c(srv2varlbllst[[default_selected_service]]),
        #selected = initialNumeratorValue
        selected = c(),
        options = pickerOptions(
          `max-options` =1,
          `live-search` = TRUE)
      ),
      

      
      #------------------------------------------------------------------------
      # Step 7: denominator
      #------------------------------------------------------------------------
      # 2023-update
      # the denominator pane
      # shiny::conditionalPanel(
        # condition = "input.selectedUseDenominator == FALSE",
        
        # 2023-09-29: this widget will be replaced by shinyWidgets::pickerInput
        # select a denominator variable: pulldwon menu

        # shiny::selectInput(
        #   inputId = 'selectedVar4denom',
        #   label = 'Step 7: Select denominator variable (to normalize the service metric)',
        #   choices = c()
        # ),
        # 
      shinyWidgets::pickerInput(
        
        inputId = 'selectedVar4denom',
        label = 'Step 7: Select denominator variable (to normalize the service metric)',
        
        choices = c(0),
        selected = c(0),
        multiple = TRUE,
        options = pickerOptions(
          `actions-box` = TRUE,
          showTick=TRUE,
          maxOptions =1,
          noneSelectedText="None",
          
          `live-search` = TRUE)
      ),
      
      #------------------------------------------------------------------------
      # Step 8: multiplier
      #------------------------------------------------------------------------
      # select a multiplier
      
      shiny::radioButtons(
        inputId = "selectMultiplier",
        label = "Step 8: Select a multiplier",
        choices = list(
          "No multiplier" = 0,
          "x 100" = 2,
          "x 1,000" = 3,
          "x 10,000" = 4,
          "x 100,000" = 5
          #"x 1M" = 6
        ),
        selected = 0
      ),
      
      #------------------------------------------------------------------------
      # Step 9: Submit button
      #------------------------------------------------------------------------
      # 2023-update-item:
      shiny::tags$label("Step 9: Click below to populate the graph"),
      shiny::tags$br(),
      # 2023-update: moved at Step 9
      # Button that submit the above choice of base/comparison municipalities
      shiny::actionButton(inputId = "goButton", 
                          label = "Submit"),
      

      #------------------------------------------------------------------------
      # radio-button for page layout
      #------------------------------------------------------------------------
      shiny::radioButtons(
        inputId = "selectPageLayout",
        label = "Step 10: Select page layout and click the button below to download",
        choices = list("Portrait" = 0, "Landscape" = 1),
        selected = 1
      ),
      # 2023-update: moved below Step 10
      # Graph-Downloading button
      # downloadButton('downloadGraph'),
      shiny::downloadButton(outputId = 'dwonloadImage', 
                            label = "Download this graph")
      
      
    ) # sidePanel
    
    
    
    
  ) # sidebarLayout
  
) # fluidPage

###############################################################################
# server definition
###############################################################################

server <- function(input, output, session) {
  # the following rv is used to store the choice of a service over time
  
  
  
  
  rv <- shiny::reactiveValues(maxPeerSelected=sizeOfMuniucipalities-1,
                              denominatorUsed=FALSE,
                              selectAvg=FALSE,  # to be removed soon
                              avgRendering=FALSE,
                              sysAvgRendering=FALSE,
                              avg_case=FALSE
                              )
  
  # w <- waiter::Waiter$new()

  output$step3message <- shiny::renderText(
    {
      if ((input$selectRenderingType == 1) && (is.null(input$peerGroup))){
        validate("For average cases, choose at least one comparison municipality")
      }
    }
  )
  
  
  
  # ===========================================================================
  # observeEvent blocks
  # ===========================================================================
  #
  # ---------------------------------------------------------------------------
  # Step 1: choosing the target city 
  # ---------------------------------------------------------------------------
  # Expected update behavior: 
  # A change in Step 1 (base city) updates candidates for the Step 3 
  # (peer group) i.e, the contents of "choice" set, NOT "selected" set
  # 
  # Note: a change of the base city does not affect service and 
  # numerator/denominator
  
  shiny::observeEvent(input$selectedCity, {
    
    base::message("===== observeEvent(input$selectedCity: start ===============")
    
    
    print("observeEvent:city change: current settings=")
    print("current city=")
    print(input$selectedCity)
    
    

    # create a new peer-group set: it is the complement of the base(target) city
    updatedPeerGroup <- citylabel[!citylabel %in% c(input$selectedCity)]
    
    
    # print("updatedPeerGroup=")
    # print(updatedPeerGroup)
    # 
    # print("current peerGroup=")
    # print(input$peerGroup)
    
    
    # 2023-10 update
    # rendering option must be reset to the default 
    # whenever a base-city is changed to avoid an inconsistent state in steps 2/3
    # A change in the base city resets the set of peers => this means 
    # the previous average calculation state must be discarded
    # 
    # reset the state of Step 2 to 0
    
    
    
    # print("input$selectRenderingType=")
    # print(input$selectRenderingType)
    
    
    
    if (!is.null(rv$avgRendering) ){
      
      
      # print("This is not very first time: base-city upate")
      # print("current rv$selectAvg=")
      # print(rv$selectAvg)
      
      
      if (input$selectRenderingType == 1){
        # rever to state 0 
        updateRadioButtons(inputId = "selectRenderingType",
                                        selected = 0)
        rv$avgRendering <- FALSE
        #rv$selectAvg <- FALSE  # to be removed soon
        
        
        # print("input$selectRenderingType=")
        # print(input$selectRenderingType)
        
      }



      # this not very first time
      # print("current rv$selectAvg=")
      # print(rv$selectAvg)
      # 
      # 
      # # 
      # # if (rv$selectAvg) {
      # #   print("updating rv$avgRendering")
      # #   rv$selectAvg <- FALSE
      # # } else {
      # #   rv$selectAvg <- TRUE
      # # }
      # # print("post rv$avgRendering=")
      # # print(rv$selectAvg)
      
      
    } else {
      
      # print("very first time: base-city update")
      
    }
    
    
    if (!is.null(rv$sysAvgRendering) ){
      
      
      # print("This is not very first time: base-city upate")
      # print("current rv$sysAvgRendering=")
      # print(rv$sysAvgRendering)
      
      
      if (input$selectRenderingType == 2){
        updateRadioButtons(inputId = "selectRenderingType",
                           selected = 0)
        rv$sysAvgRendering <- FALSE
        #rv$selectAvg <- FALSE  # to be removed soon
        
        
        # print("input$selectRenderingType=")
        # print(input$selectRenderingType)
        
      }
    }
    
    

    
    # update the PickerInput with updatedPeerGroup
    shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                             choices = updatedPeerGroup,
                             selected = character(0),
                             options=pickerOptions(noneSelectedText="None"))
    
    
    # print("after upate: current peerGroup=")
    # print(input$peerGroup)
    
    
    rv$lastPeerGroup <- rv$currentPeerGroup
    
    rv$currentPeerGroup <- NULL
    
    base::message("===== observeEvent(input$selectedCity: end ===============")
  })
  
  
  # ---------------------------------------------------------------------------
  # Step 2: choosing the rendering type
  # ---------------------------------------------------------------------------
  # Expected update behavior:
  # This radio button partially affects the widget of Step 3 (peers);
  # other UI widgets are not affected.
  # 
  # there are the two possible moves:
  # (1) Individual lines (max 5) => Average line: this move is always safe 
  # (no update necessary)
  # (2) Average line => Individual lines: this one may be unsafe 
  # (updating the peer set may be necessary) 
  # because an average-line case might have been based on more than 5 cities
  # and these >5 cities must be truncated to 5 anyway.
  # 
  # The first 5 cities (from element 1 to element 5) of the current peer set
  # are kept.
  
  shiny::observeEvent(input$selectRenderingType, { 
    
    base::message("===== observeEvent(input$selectRenderingType: start ======")

    
    # print("rv$lastRendering=")
    # print(rv$lastRendering)
    # print("input$selectRenderingType=")
    # print(input$selectRenderingType)
    
    updatedPeerGroup <- citylabel[!citylabel %in% c(input$selectedCity)]
    
    if (input$selectRenderingType == 0){
      rv$lastRendering <- 0
      # Shift from A to I case
      # back to default state
      # 
      # under this situation (individual-line rendering), 
      # the size of the peerGroup must be 5 or less
      # 
      
      
      print("Intial default case or A-to-I switch case")
      
      # update the limit
      rv$maxPeerSelected <- maxPeerSelection4I
      # update the indicator variable 
      rv$avgRendering <- FALSE
      rv$sysAvgRendering <- FALSE
      
      
      # print("indivisual-case: current values")
      # print("rv$maxPeerSelected=")
      # print(rv$maxPeerSelected)
      # print("rv$avgRendering=")
      # print(rv$avgRendering)
      print("rv$currentPeerGroup=")
      print(rv$currentPeerGroup)
      print("input$peerGroup=")
      print(input$peerGroup)
      
      
      if (is.null(rv$currentPeerGroup)) {
        # a User has not yet reached to Step 3 (peer cities)
        
        
        print("peerGroup is still NULL")
        
        
        shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                                        choices = updatedPeerGroup,
                                        selected = character(0),
                                        #selected = rv$currentPeerGroup,  #character(0),
                                        options=list(`max-options` = maxPeerSelection4I))
        
      } else if ( length(rv$currentPeerGroup) <= maxPeerSelection4I){
        print("not yet reached to the max")
        
        # print("no need to truncate the peer-group set")
        # remove the base city if it is included
        updatedSelected <- rv$currentPeerGroup[!rv$currentPeerGroup %in% c(input$selectedCity)]
        print("updatedSelected=")
        print(updatedSelected)
        # no action necessary
        shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                                        choices = updatedPeerGroup,
                                        selected = rv$currentPeerGroup,  #character(0),
                                        options=list(`max-options` = maxPeerSelection4I))
        
      } else {
        # must truncate the set to <= 5
        
        
        print("peerGroup must be truncated to the length of 5")
        # remove the base city if included
        updatedSelected <-rv$currentPeerGroup[!rv$currentPeerGroup %in% c(input$selectedCity)]
        tobecut <- updatedSelected[1:5]
        
        print("tbecut:length=")
        print(length(tobecut))
        
        
        shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                                        #choices = tobecut, 
                                        choices = updatedPeerGroup,
                                        selected = tobecut,  #character(0),
                                        #stateInput = TRUE,
                                        options=list(`max-options` = maxPeerSelection4I))
        rv$currentPeerGroup <- tobecut
      }
      
      
      # print("***************** after truncation *****************")
      # print("rv$currentPeerGroup=")
      # print(rv$currentPeerGroup)
      # print("input$peerGroup=")
      # print(input$peerGroup)
      
      
      

      
    } else {
      # -----------------------------------------------------------------------
      # for cases 1 and 2
      
      if (input$selectRenderingType == 1) {
        print("state 1 case: average")
        # print("lastRendering=")
        # print(rv$lastRendering)

        

        # I to A case
        # average-line rendering
        # the size of the peerGroup is more than 5

        # print("I to A case")
        
        
        rv$maxPeerSelected <- sizeOfMuniucipalities-1
        rv$avgRendering <- TRUE
        rv$sysAvgRendering <- FALSE
        
        # print("average-case: current values")
        # print("rv$maxPeerSelected=")
        # print(rv$maxPeerSelected)
        # print("rv$avgRendering=")
        # print(rv$avgRendering)
        # print("rv$lastPeerGroup=")
        # print(rv$lastPeerGroup)
        # print("rv$currentPeerGroup=")
        # print(rv$currentPeerGroup)
        # 
        
        rv$lastPeerGroup <- rv$currentPeerGroup
        

        
        # # From-state-2 case the following set is all cities
        print("rv$currentPeerGroup=")
        print(rv$currentPeerGroup)
        # # From-state-2 case the following set is all cities
        print("input$lpeerGroup=")
        print(input$peerGroup)
        
        # for the from-state-2 case, the base city must be removed
        updatedGroup <- citylabel[!citylabel %in% c(input$selectedCity)]
        updatedSelected <- rv$currentPeerGroup[!rv$currentPeerGroup %in% c(input$selectedCity)]
        rv$currentPeerGroup <- updatedSelected
        
        print("updatedGroup=")
        print(updatedGroup)
        print("after update: rv$currentPeerGroup=")
        print(rv$currentPeerGroup)
        
        
        # change after state 2 was added in 2024
        # clear the selected set any way
        # shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
        #                                 selected = rv$currentPeerGroup, #character(0),
        #                                 options=list(`max-options` = sizeOfMuniucipalities -1))
        shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                                        choices = updatedGroup,
                                        selected = updatedSelected,
                                        #stateInput = TRUE,
                                        options=list(`max-options` = sizeOfMuniucipalities -1))
        
        rv$lastRendering <- 1
        
        if ((input$selectRenderingType == 1) && (is.null(input$peerGroup))){
          print("!!!!!!!!!!!! Warning: for average-cases, choose at least one peer city !!!!!!!!!!!!")
          
        }
        
        
      } else if (input$selectRenderingType == 2){
        # new block : this is a partial change of the above option ==1 block
        # print("state 2 case: System Average")
        # print("lastRendering=")
        # print(rv$lastRendering)
        rv$lastRendering <- 2
        # I to A case
        # average-line rendering
        # the size of the peerGroup is more than 5
        
        
        print("State 2 system average case")
        
        
        rv$maxPeerSelected <- sizeOfMuniucipalities
        rv$sysAvgRendering <- TRUE
        rv$avgRendering <- FALSE
        #rv$avg_case <- TRUE

        rv$lastPeerGroup <- rv$currentPeerGroup
        
        # this is different from the option == 1 case
        # rv$currentPeerGroup <- input$peerGroup
        # all minucipalities: citylabel
        rv$currentPeerGroup <- citylabel
        
        
        # update: all selected
        shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                                        choices = citylabel,
          selected = citylabel, #rv$currentPeerGroup, #character(0),
          #stateInput=FALSE,
          options=list(`max-options` = sizeOfMuniucipalities))
        
        
        
        
        
      }
      
      
      
    }
    
    
    
    base::message("===== observeEvent(input$selectRenderingType: end ===============")    
  })
  
  
  
  
  # ---------------------------------------------------------------------------
  # Step 3: choosing peers
  # ---------------------------------------------------------------------------
  
  # Expected update behavior
  # There is no cascading effect from this widget; however,
  # Step 2 might update the size of peerGroup for individual-rendering cases;
  #  
  # Important selection validation:
  # if the size of peer is zero with the average case (step 2), i.e., 
  # Step 2 is average (input$selectRenderingType == 1) 
  # and the current peer set is empty (length(input$peerGroup) == 0)
  # then it must be warned with a message of "choose at leaset 1 peer, etc."
  # This validation is implemented as output$step3message above
  # 
  shiny::observeEvent(input$peerGroup, { 
    base::message("===== observe:select peers: start")
    
    
    rv$lastPeerGroup <- rv$currentPeerGroup
    rv$currentPeerGroup <- input$peerGroup
    
    
    
    # print("rv$lastPeerGroup=")
    # print(rv$lastPeerGroup)
    # 
    # print("rv$currentPeerGroup=")
    # print(rv$currentPeerGroup)
    # 
    # print("input$peerGroup=")
    # print(input$peerGroup)
    # 
    # print("input$selectRenderingType=")
    # print(input$selectRenderingType)
    
    
    # warning: average is selected and empty peerGroup
    if ((input$selectRenderingType == 1) && (is.null(input$peerGroup))){
      
      
      print("!!!!!!!!!!!! Warning: for average-cases, choose at least one peer city !!!!!!!!!!!!")
      
      
    }
      
      
      
    if (input$selectRenderingType == 0) { 
      # individual-line rendering cases
        if ((length(input$peerGroup) > maxPeerSelection4I)){
          
          
          # print("individual case: need to truncate the peerGroup")
          
          
          tobecut <- rv$currentPeerGroup[1:5]
          
          
          # print("tbecut:length=")
          # print(length(tobecut))
          # print("tobecut=")
          # print(tobecut)
          
          
          shinyWidgets::updatePickerInput(session=session, inputId = "peerGroup",
                                          #choices = tobecut, 
                                          selected = tobecut,  #character(0),
                                          options=list(`max-options` = maxPeerSelection4I))
          rv$currentPeerGroup<- tobecut
          
          # print("----- after truncation -----")
          # print("rv$currentPeerGroup=")
          # print(rv$currentPeerGroup)
          # print("input$peerGroup=")
          # print(input$peerGroup)
          
          
          
        } else {
          
          
          # print("individual case: no need to truncate the peerGroup")
          
          
        }
    } else if (input$selectRenderingType == 1) {
      
      if ((is.null(input$peerGroup))){
        
        
        # print("!!!!!!!!!!!! Warning: for average-cases, choose at least one peer city !!!!!!!!!!!!")
        
        
      } else {
        
        
        # print("average case: peerGroup is not null")
        
        
      }
      
      
      
      
      
    } else if (input$selectRenderingType == 2) {
      print("System Average case: set all selected")
      # input $peerGroup must be update or 
      # 
    }

    
    base::message("===== observe:selectPeers: end")
  })
  

  
  
  # ---------------------------------------------------------------------------
  # Step 5: choosing a service
  # ---------------------------------------------------------------------------
  # 
  # Expected update behavior
  # selecting a service have two cascading effects: 
  # (1) reset the set of numerators to be chosen for the numerator UI and 
  # (2) reset the currently selected metric of the numerator UI
  

  # a-change-of-service-invoked cascading effects
  # a change in service => 
  # 1. update the current list of a set of metrics (varset4numerator)
  # 2. set the selected metric to the first of the above new list
  # 
  # a change in input$selectedVar4num does not affect its service
  # Note: the update of the denominator UI invoked by a change in the service UI
  # is handled in shiny::observeEvent(input$selectedVar4num,{}) not here

  shiny::observeEvent(input$selectedService, {
    
    base::message("===== observeEvent(input$selectedService: start =====")
    
    
    # print("observeEvent(service): current input$selectedService=")
    # print(input$selectedService)
    
    
    # the following two lines store the last selected service
    # This recording is necessary for updating the denominator;
    # if the newly chosen service is the same as before, no need to update the 
    # metric set for the denominator; however, if they differ, the metric
    # set for the denominator must be updated upon a change to the service UI
    rv$lastService <- rv$currentService
    rv$currentService <- input$selectedService;
    
    
    # print("rv$lastService=")
    # print(rv$lastService)
    
    
    # print("observeEvent(service): current metric=input$selectedVar4num=")
    # print(input$selectedVar4num)


    
    # new settings
    # change the set of metrics according to a newly selected service
    varset4numerator <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    print("step 5: contents check: varset4numerator: original")
    print(varset4numerator)
    # print("observeEvent(service): check new the changedvarset4numerator=")
    varset4numerator<- varset4numerator[order(names(varset4numerator))]
    print("after sort: varset4numerator")
    print(varset4numerator)
    print("observeEvent(service): to-be-assgined value for input$selectedVar4num=")
    print(varset4numerator[1])
    # 
    # print("freezeReactiveValue: start")
    
    shiny::freezeReactiveValue(input, "selectedVar4num")
    
    # print("freezeReactiveValue: end")
    
    # default selection of a numerator is the first element (metric)
    #shiny::updateSelectInput(
    shinyWidgets::updatePickerInput(session=session,
      inputId = "selectedVar4num",
      choices=c(varset4numerator),
      selected = c(varset4numerator[1])
    )
    
    
    # print("observeEvent(service): post-update-check")
    # print("observeEvent(service): updated metric=input$selectedVar4num=")
    # print(input$selectedVar4num)
    
    
    base::message("===== observeEvent(input$selectedService: end ===============")
    })
  
  
  
  
  
  # ---------------------------------------------------------------------------
  # Step 6: choosing a numerator (service metric)
  # ---------------------------------------------------------------------------
  
  
  
  # Dealing with a change in the numerator selector
  # 
  # There are two changes relevant to the numerator widget: 
  # 
  # (1) Service-induced change (side-effect): a new service is chosen => new 
  # set of variables for the numerator widget and its default choice
  # This change is handled by shiny::observeEvent(input$selectedService,{})
  # 
  # (2) User-induced change (own): when a user chooses a new metric, 
  # this change have side-effects on another widget (denominator)
  # This function must update the denominator set whose contents are
  # conditioned by the currently selected numerator metric
  # 
  # Possible events that invoke this update functions are:
  # 
  # (2-A): When a new numerator metric has been chosen, the denominator widget
  # might be updated as follows:
  # the previous metric is now added back to the denominator set;
  # the new numerator metric must be removed from the denominator set
  # 
  # (2-B): When a new service has been chosen => 
  # the numerator set has to be updated;
  # so does the denominator set (cascading effect)
  # 
  # Here a numerator-denominator change always starts from the numerator side, 
  # not the other way around 
  # 
  # for the use case where a user fixates a particular denominator and 
  # tries various numerators, the "selected" of the denominator widget should be
  # updated by a change in the numerator widget; is this memory function usable?
  # 
  # more frequent use cases would be that the numerator is fixed and 
  # various denominators are tried to derive a better adjusted metric
  # 
  # any change to this UI modifies the set of denominator variables
  # and the currently selected denominator, 
  # either the top of the updated list because the previous one has
  # been selected as the new numerator
  # or keeping the current one if it was not chosen as
  # the newly selected numerator.
  shiny::observeEvent(input$selectedVar4num,{

    base::message("==== within observeEvent: input$selectedVar4num:start=",
                  as.POSIXct(Sys.time(), tz = "EST5EDT"))
    
    # print("input$selectedVar4num=")
    # print(input$selectedVar4num)
    # # print("input$selectedUseDenominator=")
    # # print(input$selectedUseDenominator)
    # print("rv$denominatorUsed=")
    # print(rv$denominatorUsed)
    
    # the following two lines keep the last state of the numerator selector
    # this information will be used to differentiate within-service changes 
    # from cross-service changes
    rv$lastServiceWN <- rv$currentServiceWN
    rv$currentServiceWN <- input$selectedService;
    
    # print("rv$lastServiceWN=")
    # print(rv$lastServiceWN)
    # print("rv$currentServiceWN=")
    # print(rv$currentServiceWN)
    # 
    # print("rv$lastService=")
    # print(rv$lastService)
    # print("rv$currentService=")
    # print(rv$currentService)
    
    # get the current list of **all** variables for the current service
    # note: this list now includes census variables
    rawlist <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    # print("rawlist=")
    # print(rawlist)
    # 
    # the list of denominators must excluded the variable
    # that is currently selected as the numerator
    # so exclude the numerator from the above raw list
    
    varset4denominator <- rawlist[!rawlist %in% c(input$selectedVar4num)]
    print("before: current varset4denominator[1]=")
    print(varset4denominator[1])
    varset4denominator <- varset4denominator[order(names(varset4denominator))]
    print("after sort: current varset4denominator[1]=")
    print(varset4denominator[1])
    #if (input$selectedUseDenominator) {
    # if (TRUE) {
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
      
      # checking the current values of key variables
      # numerator
      # print("current input$selectedVar4num=")
      # print(input$selectedVar4num)
      
      # denominator
      # print("current input$selectedVar4denom=")
      # print(input$selectedVar4denom)
      
      # saved selected service
      # print("rv$lastServiceWN=")
      # print(rv$lastServiceWN)
      
      # currently selected service
      # print("input$selectedService=")
      # print(input$selectedService)
      # print("before line 715")
      
    if (is.null(rv$lastServiceWN)) {
      # this is the very first load case
      #
      # print("denominator is on but no previous service data: just update the denominator set")
      
      #shiny::updateSelectInput(
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "selectedVar4denom",
        choices = c(varset4denominator),
        selected = character(0) #selected = c(varset4denominator[1])
      )
      
      
    } else {
      # non-1st cases
      # print("================= the same service case =================")
      
      
      if (rv$lastServiceWN == input$selectedService) {
        # print("================== 1-A/1-B case=========================")
        # print("%%%%% last service and current one are the same %%%%%")
        # print("This is within-service change")
        # handle cases of (1-A) and (1-B)
        #
        # two possibilities here
        # denominator is still NULL or selected
        
        if (is.null(input$selectedVar4denom)) {
          # print("------------ denominator is still NULL: not yet selected -----")
          # handle denominator-null cases
          # keep NULL for the denomnator
          #
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "selectedVar4denom",
            choices = c(varset4denominator),
            selected = character(0)
          )
          
        } else {
          # handle denominator-selected cases
          # print("---------- denominator is selected ----------------")
          if (input$selectedVar4num == input$selectedVar4denom) {
            # print("numerator is the previously selected denominator")
            # print("reset the selected one for the denominator")
            
            
            #shiny::updateSelectInput(
            shinyWidgets::updatePickerInput(
              session = session,
              inputId = "selectedVar4denom",
              choices = c(varset4denominator),
              selected = c(varset4denominator[1]),
              option = pickerOptions(noneSelectedText = "None")
            )
            
          } else {
            # is this block necessary?????????????????
            # print("keep the current choice of denominator")
            
            # print("varset4denominator:2nd[1]=")
            # print(varset4denominator[1])
            
            #shiny::updateSelectInput(
            shinyWidgets::updatePickerInput(
              session = session,
              inputId = "selectedVar4denom",
              choices = c(varset4denominator),
              selected = c(input$selectedVar4denom),
              option = pickerOptions(noneSelectedText = "None")
            )
            
          }
          
        }
        
        
      } else {
        
        
        # print("%%%%% last service and current one are different %%%%%")
        # print("This is a cross-service change")
        
        
        # handle cases of (2)
        # 2023-10: default stat is that a denominator is not selected, i.e.,
        # NULL, so assign character(0) instead of
        # the first element of the denominator-variable set
        # this configuration may need to change if we want to have
        # a particular, fixed denominator such as population across
        # numerators
        #shiny::updateSelectInput(
        
        shinyWidgets::updatePickerInput(
          session = session,
          
          inputId = "selectedVar4denom",
          choices = c(varset4denominator),
          selected = character(0) # #c(varset4denominator[1])
          
        )
        
      }
      
    }
    
    
    base::message("==== within observeEvent: input$selectedVar4num:end=",
                  as.POSIXct(Sys.time(), tz = "EST5EDT"))
  })
  
  
  
  # ---------------------------------------------------------------------------
  # Step 7: choosing a denominator
  # ---------------------------------------------------------------------------
  
  shiny::observeEvent(input$selectedVar4denom, {
    base::message("===== within observeEvent: input$selectedVar4denom: start")
    print("rv$denominatorUsed=")
    print(rv$denominatorUsed)
    print("current input$selectedVar4denom=")
    print(input$selectedVar4denom)
  #   
  #   if (rv$denominatorUsed){
  #     print("use denominator case")
  #     shiny::updateRadioButtons(inputId = "selectMultiplier", selected = 2)
  #     
  #     # get the current list of **all** variables for the current service
  #     # note: this list now includes census variables
  #     rawlist <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
  #     # print("rawlist=")
  #     # print(rawlist)
  #     # the list of denominators must excluded the variable
  #     # that is currently selected as the numerator
  #     # so exclude the numerator from the above list
  #     
  #     varset4denominator <- rawlist[!rawlist %in% c(input$selectedVar4num)]
  #     
  #     print("update the selectedVar4denom and varset4denominator")
  #     # 
  #     shiny::freezeReactiveValue(input, "selectedVar4denom")
  #     shiny::updateSelectInput(
  #       inputId = "selectedVar4denom",
  #       choices = c(varset4denominator),
  #       selected = input$selectedVar4denom #c(varset4denominator[1])
  #     )
  #     
  #     
  #   } else {
  #     print("do not use denominator case")
  #     shiny::updateRadioButtons(inputId = "selectMultiplier", selected = 0)
  #   }
  #   
  # 
  
    
    # if the length of the selected is more than 1, reset the choice

    if (length(input$selectedVar4denom) > 1){
      
      
          shiny::updateSelectInput(
            inputId = "selectedVar4denom",
            selected = character(0) #c(varset4denominator[1])
          )
      
      
    }

    base::message("===== within observeEvent: input$selectedVar4denom: end")
  },
  ignoreNULL = FALSE
  ) 

  

  
  
  
  
  # observeEvent(input$selectAvg, {
  #   if (!input$selectAvg){
  #     updateCheckboxInput(inputId = "selectCI", 
  #       label = "Add Confidence Interval (95%)", value = FALSE )
  #   }
  #   
  # })
  

  
  # years UI: Must warn the no-selection case
  shiny::observeEvent(input$selectedYears,{
    
    message("===== within observeEvent: input$selectedYears")
    # print("length(input$selectedYears")
    # print(length(input$selectedYears))
    
    if (length(input$selectedYears) == 0){
      
      # issue the warning message
      shiny::validate("Please select at least one year!")

      # message("no-year-selected state: the last year will be selected")
      
      # The following updatePickerInput() is not working for some unknown reason
      shinyWidgets::updatePickerInput(session=session, 
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
  oldWelcomText <- c("Please click the Submit button ", 
                     "to start a new benchmarking session.")
  welcomeText <-c("Please follow the steps on the right and",
                  "click the Submit button ",
                  "to start a new benchmarking session.")
  output$initialMessage <- shiny::renderText({
    if (input$goButton == 0) {
      shiny::HTML(paste(
        "Welcome to Municipalities Benchmarking 2.0.",
        paste("Please follow the steps on the right and",
              "click the Submit button to start a new benchmarking session.",
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
  benchmarking_plot <- shiny::reactive({
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

    
    # new flag variable for average/System_average cases
    rv$avg_case <- if (rv$avgRendering || rv$sysAvgRendering) TRUE else FALSE
    print("rv$avgRendering=")
    print(rv$avgRendering)
    print("rv$sysAvgRendering=")
    print(rv$sysAvgRendering)
    print("rv$avg_case=")
    print(rv$avg_case)
    
    
    # The following setting ensures that 
    # peer-group membership is not reactively (instantaneously) updated;
    # the current number of members seems to be too many to reactively handle
    # quick check/uncheck-box actions; consequently, 
    # it is now collectively updated by clicking the Submit button and 
    # isolate() is used here
    # 
    # 2023-09-28; temporarily changed to set checkedM to rv$currentPeerGroup
    
    #checkedM <- shiny::isolate(input$peerGroup)
    # input$peerGroup does not immediately update its value after a UI change
    # therefore use rv$currentPeerGroup instead
    
    # system average case, currentPeerGroup might be empty
    checkedM <- rv$currentPeerGroup
    checkedM <- if (rv$sysAvgRendering) citylabel else checkedM
    
    
    
    
    
    
    
    print("input$peerGroup: checkedM=")
    print(checkedM)
    print("rv$currentPeerGroup=")
    print(rv$currentPeerGroup)
    print("input$peerGroup=")
    print(input$peerGroup)
    
    
    if (is.null(input$peerGroup) && length(checkedM) >0) {
      print("peerGroup is null but currentPeerGroup is not set to NULL")
      rv$currentPeerGroup<-c()
      checkedM<-c()
    } else {
      print("this would not be after disselect-all in peer")
      print("input$peerGroup: current state=")
      print(input$peerGroup)
      print("checkedM state=")
      print(checkedM)
    }

     
    # The first-time handling when the Submit button has nothing to submit
    if (input$goButton == 0){
      # waitress$close()
      return()
    }
    
    
    # -------------------------------------------------------------------------
    # input: sanity check
    # -------------------------------------------------------------------------
    
    
    # print("running validation test")
    
    
    
    # 
    # print("selectedYears=")
    # print(length(input$selectedYears))
    
    # Currently there is no convenient, ready-to-use solution about other than
    # the following primitive, warning-message solution
    # attempts to automatically set the latest year selected at least 
    # when a user mistakenly unchecked all years did not work so far. 
    
    
    if (length(input$selectedYears) == 0){
      shiny::validate("Please select at least a year")
    }
    
    
    # sanity check against key input variables: base city, service, numerator
    # variable
    
    
    # print("before req block: sanity check")
    # print("input$selectedCity=")
    # print(input$selectedCity)
    # print("input$selectedService=")
    # print(input$selectedService)
    # print("input$selectedVar4num=")
    # print(input$selectedVar4num)
    
    
    
    shiny::req(input$selectedCity,
        input$selectedService,
        input$selectedVar4num
    )
    varset4check <- c(srv2varlbllst[[input$selectedService]], srv2varlbllst[["census"]])
    
    
    #print("varset4check=")
    #print(varset4check)
    
    
    if (input$selectedVar4num %in% varset4check) {
      print("selected metric is a member")
    } else {
      print("selected metric is not a member")
    }
    
    
    print("before denominator-req: sanity check")
    print("input$selectedVar4denom=")
    if (!is.null(input$selectedVar4denom)){
      
        
        print(input$selectedVar4denom)
        if (input$selectedVar4denom %in% varset4check) {
          print("denominator is a member")
        } else {
          print("denominator is not a member")
        }
        

        if (input$selectedVar4denom != ""){
          shiny::req(input$selectedVar4denom %in% varset4check)
        } else {
          print("denominator is not selected")
        }
      
    } else {
      print("denominator is not selected")
    }
    
    
    # isolcating the base-city did not work; it seems the choice of a base city
    # must reactively update the state of selected comparison cities;
    # otherwise, isolating the choice of a base-city(= waiting for a submission
    # action to complete the choice) ended up erratic/unstable UI states.
    # baseCity = isolate(input$selectedCity) <= this did not work
    
    
    baseCity = input$selectedCity
    
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2023 addition : to be removed in2024
    # if (input$selectRenderingType == 1) {
    #   # default case
    #   # individual rendering
    #   rv$selectAvg<- TRUE
    # } else {
    #   rv$selectAvg<- FALSE
    # 
    # }

    
    # print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    # 
    # 
    # print("current state of selectRenderingType(0 or 1 or 2)=")
    # print(input$selectRenderingType)
    # print("rv$avgRendering=")
    # print(rv$avgRendering)
    # print("rv$sysAvgRendering=")
    # print(rv$sysAvgRendering)
    
    
    
    # print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #if (rv$selectAvg && length(input$peerGroup) == 0){
      #shiny::validate("Please select at least one comaprison municipality")
      #waiter::waiter_hide()
      #return();
    #}
    
    
    
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

    if (!is.null(input$selectedVar4denom)){
      rv$denominatorUsed <- TRUE
      useDenominator <- TRUE
      
      
      
      print("A case of using a denominator")
      print("current denominator=")
      print(input$selectedVar4denom)
      
      
      
    } else {
      print("This request does not use a denominator")
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
    
    
    # print("selectedYearsC=")
    # print(selectedYearsC)

    
    
    # !!!!!!!!!! warning !!!!!!!!!!
    # participating cities do not provide a complete data for a given year
    # under a certain combination of selection criteria, 
    # there would be an empty-data case
    # The following block checks this empty-data case for the base city 
    # and if so, just generates a warning message; 
    # Even if a base-city does not have data, this does not terminate 
    # the whole rendering process and tries to display a peer-group's data.
    
    
    # print("input$selectedVar4num=")
    # print(input$selectedVar4num)
    
    
    valueAvailableCities <- bd_data %>%
      dplyr::filter(Service == input$selectedService |
               Service == "census") %>% 
      dplyr::filter(Variable == input$selectedVar4num) %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::distinct(Municipality) %>%
      dplyr::pull()
    
    
    
    # print("valueAvailableCities=")
    # print(valueAvailableCities)
    # print("selected city=")
    # print(baseCity)
    
    
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
    # data_sc_nm <- bd_data %>%
    #   dplyr::filter(Service == input$selectedService |
    #            Service == "census")   %>%
    #   dplyr::filter(Variable == input$selectedVar4num)    %>%
    #   dplyr::filter(Municipality == baseCity) %>%
    #   tidyr::spread(key = Year, value = Value)        %>%
    #   dplyr::select(selectedYearsC)        %>%
    #   as.matrix()
    # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    # print("data_sc_nm: afer as.matrix()=")
    # print(data_sc_nm)
    
    
    #--------------------------------------------------------------------------
    # test with a common method
    # dt, selectedService, selectedVar4num, group, selectedYears
    #--------------------------------------------------------------------------
    data_sc_nm <-get_bd_matrix_data(bd_data, input$selectedService, 
                 input$selectedVar4num, c(baseCity), selectedYearsC)
    
    
    # print("using a common method")
    # print("data_sc_nm=")
    # print(data_sc_nm)
    # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    
    
    
    
    
    

    # re-add column names: the above as.matrix() removed column names
    colnames(data_sc_nm) <- selectedYearsC
    
    
    # print("data_sc_nm: afer colnames=")
    # print(data_sc_nm)
    
    
    suppressWarnings(storage.mode(data_sc_nm) <- "numeric")
    
    
    # print("data_sc_nm:after changing stroage mode=")
    # print(data_sc_nm)
    # print(str(data_sc_nm))
    
    
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
    
    
    # print("data_sc_nm: after adding rowname=")
    # print(data_sc_nm)
    
    
    #--------------------------------------------------------------------------
    # step 2: base municipality: denominator: to be ignored if no denominator
    #--------------------------------------------------------------------------
    # base-line data for the denominator variables must be added

    if (useDenominator) {
      
      print("base m: denominator data block: ========== start ==========")
      
      
      # print("current city=")
      # print(baseCity)
      # print("current service=")
      # print(input$selectedService)
      # print("current metric=")
      # print(input$selectedVar4denom)
      
      
      # generating a base-city's denominator data as matrix
      # dimension: 1 x # of selected years
      # data_sc_dm <- bd_data %>%
      #   dplyr::filter(Service == input$selectedService |
      #            Service == "census")   %>%
      #   dplyr::filter(Variable == input$selectedVar4denom)  %>%
      #   dplyr::filter(Municipality == baseCity) %>%
      #   tidyr::spread(key = Year, value = Value)        %>%
      #   dplyr::select(selectedYearsC)        %>%
      #   as.matrix()
      # 
      # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      # print("data_sc_dm:before=")
      # print(data_sc_dm)
      
      
      data_sc_dm <-get_bd_matrix_data(bd_data, input$selectedService, 
                   input$selectedVar4denom, c(baseCity), selectedYearsC)
      
      
      # print("using a common method")
      # print(data_sc_dm)
      # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      
      
      
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
          ")\n is not available for these years."
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
      # data_pg_nm <- bd_data %>%
      #   dplyr::filter(Service == input$selectedService |
      #            Service == "census")   %>%
      #   dplyr::filter(Variable == input$selectedVar4num)   %>%
      #   dplyr::filter(Municipality %in% checkedM) %>%
      #   dplyr::arrange(Municipality, Year) %>%
      #   tidyr::spread(key = Year, value = Value)       %>%
      #   dplyr::select(selectedYearsC) %>%
      #   as.matrix()
      # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      # print("data_pg_nm(s)=")
      # print(data_pg_nm)
      
      
      #--------------------------------------------------------------------------
      # test with a common method
      # dt, selectedService, selectedVar4num, group, selectedYears
      #--------------------------------------------------------------------------
      #

      data_pg_nm <-get_bd_matrix_data(bd_data, input$selectedService,
                   input$selectedVar4num, checkedM, selectedYearsC)

      
      # print("using a common method")
      # print("data_pg_nm=")
      # print(data_pg_nm)
      # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      
      
      
      
      
      
      
      
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
        
        # data_pg_dm <- bd_data %>%
        #   dplyr::filter(Service == input$selectedService |
        #            Service == "census")   %>%
        #   dplyr::filter(Variable == input$selectedVar4denom) %>%
        #   dplyr::filter(Municipality %in% checkedM) %>%
        #   tidyr::spread(key = Year, value = Value)       %>%
        #   dplyr::select(selectedYearsC) %>%
        #   as.matrix()
        # 
        # 
        # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        # print("data_pg_dm=")
        # print(data_pg_dm)
        

        data_pg_dm <-get_bd_matrix_data(bd_data, input$selectedService,
                     input$selectedVar4denom, checkedM, selectedYearsC)
        
        # print("using a common method")
        # print(data_pg_dm)
        # print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        
        
        
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
      
      
      # print("data4EachPeerCity_raw_m=")
      # print(data4EachPeerCity_raw_m)
      
      
      # print(str(data4EachPeerCity_raw_m))
      # print("has_rownames: data4EachPeerCity_raw_m=")
      # print(has_rownames(data4EachPeerCity_raw_m))
      
      # attach a column of city names to the data and convert to tibble
      data4EachPeerCity_rawt <-
        tibble::rownames_to_column(as.data.frame(data4EachPeerCity_raw_m),
                           var = "catgry") %>%
        tibble::as_tibble()
      
      
      # print("data4EachPeerCity_rawt=")
      # print(data4EachPeerCity_rawt)
      
      
      # tibble: wide to long transformation
      data4EachPeerCity_rawt <- data4EachPeerCity_rawt %>%
        tidyr::gather(selectedYearsC, key = "Year", value = "quotient")
      
      
      # print("data4EachPeerCity_rawt=")
      # print(data4EachPeerCity_rawt)
      
      
      # extra step if the denominator option is selected
      tmpTibblePg <- NULL
      if (useDenominator) {
        tmpTibblePg <- tibble::as_tibble(data_pg_nm / data_pg_dm)
      } else {
        tmpTibblePg <- tibble::as_tibble(data_pg_nm)
      }
      
      
      
      # re-attach column names after the above matrix-element-wise division
      # ???????? data4EachPeerCity is used later ?????
      data4EachPeerCity <- tmpTibblePg  %>%
        tidyr::gather(selectedYearsC, key = "Year", value = "quotient")
      
      
      # print("data4EachPeerCity=")
      # print(data4EachPeerCity)
      
      
      # A convenient way to calculate year-wise mean and CI values by using 
      # Rmisc::susummarySE()
      summarizedPG <- tmpTibblePg %>%
        tidyr::gather(selectedYearsC, key = "Year", value = "quotient") %>%
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
      
      rownames(data4plot_raw) <- if (rv$sysAvgRendering) c(baseCity, "System Average") else c(baseCity, "Average")
      
      
      
      
    } else {
      # do nothing for no-peer-group case, 
      # i.e., rendering the selected city only
      data4plot_raw <- tmpMatrixScQ
      
      
      # print("data4plot_raw=")
      # print(data4plot_raw)

    }

    
    # print("data4plot_raw=")
    # print(data4plot_raw)
    
    
    #--------------------------------------------------------------------------
    # additional steps before rendering 
    #--------------------------------------------------------------------------
    #
    # step to convert rownames to a column whose name is catgry and
    # transform it to tibble
    data4plot <-
      tibble::rownames_to_column(as.data.frame(data4plot_raw), var = "catgry") %>%
      tibble::as_tibble()
    
    
    # print("data4plot:b=")
    # print(data4plot)
    
    
    # step to handle the number of selected years is one, etc.
    # 
    # If # of years is 1, dynamically rename the column name 
    # from the generic "V1" to the selected year such as "2022"
    # for later plotting (labeling)
    # 
    if (ncol(data4plot) == 2) {
      print("***** data4plot: # of columns is 2 *****")
      yr <- selectedYearsC[1]
      # print(yr)
      if (is.null(checkedM)) {
        print("checkedM is null: do nothing")
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
      print("***** data4plot: # of columns is not 2 *****")
      print("ncol(data4plot)=")
      print(ncol(data4plot))
    }
    
    
    # print("data4plot:a=")
    # print(data4plot)
    
    
    
    # the following line fails if the number of years is 1
    data4plot_sc <- data4plot %>%
      tidyr::gather(selectedYearsC, key = "Year", value = "quotient") %>%
      dplyr::filter(catgry == baseCity)
    
    
    
    # print("data4plot_sc=")
    # print(data4plot_sc)
    
    
    
    if (!is.null(checkedM)) {
      data4plot_pg <- data4plot %>%
        tidyr::gather(selectedYearsC, key = "Year", value = "quotient") %>%
        dplyr::filter(catgry != baseCity)
      
      
      # print("data4plot_pg=")
      # print(data4plot_pg)
      
      
      
      data4plot_pgx <-
        data4plot_pg %>% tibble::add_column(ci = summarizedPG[["ci"]])
      
      
      
      # print("data4plot_pgx=")
      # print(data4plot_pgx)
      
      
      
      # print("==================== end of data-prep for plot ==================")
      
    }
    
    
    
    
    
    
    # -------------------------------------------------------------------------
    # plotting
    # -------------------------------------------------------------------------
    # An if-block after ggplot() seems not to accept more than 1 geom_xxx
    # statements within it and the following incremental approach was used
    

    print("==================== beginning of plot ==================== ")
    
    # setting for using a custom google-font 
    showtext::showtext_auto()
    
    # checking the current value of rv$avgRendering
    # print("rv$avgRendering=")
    # print(rv$avgRendering)
    
    # checking the current selected comp. cities
    print("checkedM=")
    print(checkedM)
    
    # setting up "checkedMx" that is "checkedM" customized for average cases
    checkedMx <-c()
    if (rv$avgRendering){
      checkedMx <- c(input$selectedCity,"Average")
    } else if (rv$sysAvgRendering){
      # to be checked later
      checkedMx <- c(input$selectedCity,"System Average")
    } else {
      checkedMx <-c(input$selectedCity, checkedM)
    }
    
    print("checkedMx=")
    print(checkedMx)
    
    # preparing the palette for color

    # indv_palette <- color_palette_mpty_indv(input$selectedCity, checkedM, 
    #                                         rv$avgRendering)
    indv_palette <- color_palette_mpty_indv(input$selectedCity, checkedM, 
                                            rv$avg_case)    
    
    
    # print("indv_palette=")
    # print(indv_palette)
    # # print(str(indv_palette))
    # print("indv_palette[checkedMx]=")
    # print(indv_palette[checkedMx])
    
    # preparing the palette for shape
    # indv_shape_list <- shape_no_list(input$selectedCity, checkedM, 
    #                                  rv$avgRendering)
    indv_shape_list <- shape_no_list(input$selectedCity, checkedM, 
                                     rv$avg_case)
    
    
    # print("new shape list=")
    # print(indv_shape_list[checkedMx])
    
    # shape list
    # base + average c(19, 19)
    

    #              | base only   | base+peers | base+ average
    # --------------------------------------------------------
    # numeric only |  case 1     | case 2     | case 2A
    # numeric+denom|  case 3     | case 4     | case 4A
    # --------------------------------------------------------
    # one year-only| point       | points     | points
    
    # baseline rendering
    # This block generates the bar plot for the base city
    
    print("=========== preparing the base-city's line plot")
    
    # print(str(data4plot_sc))


    data4plot_sc <- data4plot_sc |> dplyr::mutate_at(c('Year'), as.integer)
    
    # print("data4plot_sc=: after integer conversion")
    # print(str(data4plot_sc))
    
    yearset <- data4plot_sc$Year
    
    # print("yearset=")
    # print(yearset)
    
    key_space <- 30
    plt1 <- data4plot_sc %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = Year,
          y = quotient,
 #         group = catgry, # without this spec, dots are not connected <= this is for x is discrete case
          color = catgry
        ) 
      ) +
      
      ggplot2::scale_y_continuous(name = "Value", labels = scales::comma)
    
    if (!rv$sysAvgRendering){
    
    plt1 <- plt1 +
      # new way 
      ggplot2::geom_line(
        linejoin ="round",
        linewidth = 2,
        )+
      
      
      ggplot2::scale_x_continuous(breaks= yearset) +
      
      ggplot2::geom_point(
        ggplot2::aes(
          shape = catgry, # this config is necessary to unified legend
          # color = catgry
        ),
        size = 4,
      ) 

    }
    
    
    # end of the base-line plot: this is the case 1( base/numeric only)
    
    
    
    
    #######################################
    # incrementally adding optional items
    # collective average line instead of individual lines for the peer-group 
    # also overlaying points
    # 2024 change
    if (rv$avg_case) {
    #if (rv$avgRendering) {# if (input$selectAvg) {
      
#######################################
      # average-case (two-line case: 2nd)
#######################################
      
      if (!is.null(rv$currentPeerGroup)){
        # no empty peer group
        # case 2A
        print("========== average case ")
        
        data4plot_pgx <- data4plot_pgx |>
          dplyr::mutate_at(c('Year'), as.integer)
        yearsetAvg <- data4plot_pgx$Year
        
        
        legend_label_set <-
          c("Base\nmunicipality",
            "Average of\ncomparison\nmunicipalities")
        breakset_c <- names(indv_palette[checkedMx])
        breakset_s <- names(indv_shape_list[checkedMx])
        
        # print("breakset+")
        # print(breakset_c)
        # print(breakset_s)
        
        # add an average line
        plt1 <- plt1 +
          ggplot2::geom_ribbon(
            data = data4plot_pgx,
            
            alpha = 0.15,
            color = NA,
            # this erases boundary lines
            ggplot2::aes(
              x = Year,
              y = quotient,
              ymin = quotient - ci,
              ymax = quotient + ci,
              fill = "95%\nConfidence\nInterval"
            )
          ) +
          ggplot2::geom_line(data = data4plot_pgx,
                             #color="#7BAFD4",
                             linewidth = 2,
                             ggplot2::aes()) +
          
          ggplot2::scale_x_continuous(breaks = yearsetAvg) +
          
          
          ggplot2::geom_point(data = data4plot_pgx,
                              #color="#7BAFD4",
                              ggplot2::aes(shape = catgry,),
                              size = 4) +
          ggplot2::scale_color_manual(name = "",
                                      values = indv_palette[checkedMx],
                                      breaks = breakset_c) +
          ggplot2::scale_shape_manual(name = "",
                                      values = indv_shape_list[checkedMx],
                                      breaks = breakset_s) +
          ggplot2::scale_fill_manual(name = "    ",
                                     values = c("95%\nConfidence\nInterval" = "#7BAFD4"))
        # wider key space for a CI case
        key_space <- 45
      
      } else if (is.null(rv$currentPeerGroup)) {
        # empty peer => degenerated into base-only case (case 1)
        # average is selected, but no peer is selected
        # just base-city only rendering
        print("========== average is selected but no peer has been selected")
        
        legend_label_set<- c("Base\nmunicipality","Average of\ncomparison\nmunicipalities")
        
        plt1 <- plt1 + 
          ggplot2::scale_color_manual(name = "", values = indv_palette[checkedMx]) + 
          ggplot2::scale_shape_manual(name = "", values = indv_shape_list[checkedMx])
      }
        

      
      
    } else {
###########################
# non-average case: A: individual lines, or  B: no peer
###########################
      # each comparison municipality's line is added: case 2
      # tibble to be used
      print("no average case: individual-peer or base only")
      
      if (!is.null(checkedM)) {
        # no empty peers: case 2
        
        # the order of keys in legend for color and shape
        breakset_c <- names(indv_palette[checkedMx])
        breakset_s <- names(indv_shape_list[checkedMx])
        
        data4EachPeerCity_rawt <- data4EachPeerCity_rawt |> 
          dplyr::mutate_at(c('Year'), as.integer)
        
        
        if (length(selectedYearsC) > 1) {
          # multiple years => lines 
          print("========== individual, multiple year case")
          # print("data4EachPeerCity_rawt=")
          # print(data4EachPeerCity_rawt)
          # print("data4EachPeerCity_rawt$catgry=")
          # print(data4EachPeerCity_rawt$catgry)
          # print("levels(data4EachPeerCity_rawt$catgry)=")
          # print(levels(data4EachPeerCity_rawt$catgry))
          

          
          yearsetInd <- data4EachPeerCity_rawt$Year
          

          
          
          
          plt1 <- plt1 +
            ggplot2::geom_line(
              data = data4EachPeerCity_rawt,
              ggplot2::aes(
                x = Year,
                y = quotient,
                #group = catgry,
                color = catgry
              ),
              linewidth = 2
            ) +
            
            ggplot2::scale_x_continuous(breaks= yearsetInd) +
            
            
            ggplot2::geom_point(
              data = data4EachPeerCity_rawt,
              ggplot2::aes(
                x = Year,
                y = quotient,
                shape = catgry,
                color = catgry
              ),
              size = 4
            ) +
            
            #fixed_c_scale + fixed_s_scale
            
          ggplot2::scale_color_manual(name = "", values = indv_palette[checkedMx],
             breaks = breakset_c) +
          ggplot2::scale_shape_manual(name = "", values = indv_shape_list[checkedMx],
            breaks = breakset_s)
          
          
        } else {
          # single-year => points
          print("========== !!! individual, single year rendering case: may fail due to a ggplot2 setting!!!")
          # " Discrete value supplied to continuous scale"
          plt1 <- plt1 +
            ggplot2::geom_point(
              data = data4EachPeerCity_rawt,
              ggplot2::aes(
                x = Year,
                y = quotient,
                shape = catgry,
                color = catgry
              ),
              size = 4
            ) +
            
            ggplot2::scale_color_manual(name = "", values = indv_palette[checkedMx],
              breaks = breakset_c) +  
            
            ggplot2::scale_shape_manual(name = "", values = indv_shape_list[checkedMx],
              breaks = breakset_s)
          
        }
        
      } else {
        # no peer: just base city only: case 1
        print("======= base-city only case =========")
        plt1 <- plt1 + 
          ggplot2::scale_color_manual(name = "", values = indv_palette[checkedMx]) + 
          ggplot2::scale_shape_manual(name = "", values = indv_shape_list[checkedMx])
      }
      
      
      
    }
# end of additional plot
    # -------------------------------------------------------------------------
    # metric-definition table
    # -------------------------------------------------------------------------
    # print("numerator=")
    # print(input$selectedVar4num)
    # print("numerator's name(label) check=")
    # print("v2lallinOne[[input$selectedVar4num]]=")
    # print(v2lallinOne[[input$selectedVar4num]])
    # 
    # print("data for the metric defintion table: numeric data")
    # numerator(metric) variable name: full
    metricVarLabel <- stringr::str_wrap(v2lallinOne[[input$selectedVar4num]], 
                                        width = 80)
    # print("metricVarLabel=")
    # print(metricVarLabel)
    # print("**** var-defintion for this N var **** this is new")
    # print(all_vname2def[[input$selectedVar4num]])
    
    

    
    df4DefTable <- tibble(

      Metric=c(
        paste(stringr::str_wrap(v2lallinOne[[input$selectedVar4num]], width = 25))),
      Definition=c(
        paste(stringr::str_wrap(all_vname2def[[input$selectedVar4num]], width = 80)))
    )

    # print("df4DefTable without denominator=")
    # print(df4DefTable)
    

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
      
      # print("denominator=")
      # print(input$selectedVar4denom)
      # print("denominator var_label=")
      # print(v2lallinOne[[input$selectedVar4denom]])
      # print("**** var-defintion for this D var **** this is new")
      # print(all_vname2def[[input$selectedVar4denom]])
      

      
      df4DefTable <- tibble(
        Metrics=c(
          # v2lallinOne[[input$selectedVar4num]],
          # v2lallinOne[[input$selectedVar4denom]]
        
          paste(stringr::str_wrap(v2lallinOne[[input$selectedVar4num]], width = 25)),
          # vector("character", length(n_def)-1),
          paste(stringr::str_wrap(v2lallinOne[[input$selectedVar4denom]], width = 25))
          # vector("character", length(d_def)-1)
        ),
        Definitions=c(
          
          # all_vname2def[[input$selectedVar4num]],
          # all_vname2def[[input$selectedVar4denom]]
          # stringr::str_split_1(
          # stringr::str_wrap(all_vname2def[[input$selectedVar4num]], width = 60), "\\n"),
          # 
          # stringr::str_split_1(
          #   stringr::str_wrap(all_vname2def[[input$selectedVar4denom]], width = 60), "\\n")
          paste(stringr::str_wrap(all_vname2def[[input$selectedVar4num]], width = 80)),
          paste(stringr::str_wrap(all_vname2def[[input$selectedVar4denom]], width = 80))
          
          )
      )

      # print("df4DefTable with denominator=")
      # print(df4DefTable)
      # print("df4DefTable: strcuture=")
      # print(str(df4DefTable))
      
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
      peerGroupList <- if (rv$sysAvgRendering) "All participating municipalities" else stringr::str_wrap(
        paste(checkedM, collapse = ", ")
        , width = 80)
    }
    
    baseCityline <- if (!rv$sysAvgRendering) paste0("Base Municipality: ",
                                                     baseCity) else ""
    
    
    subtitleText <- paste(
      c(
        # "Base Municipality: ",
        # baseCity,
        baseCityline,
        "\nService: ",
        serviceNameFull,
        "\nComparison Municipalities: ",
        peerGroupList,
        "\nMultiplier: ",
        multiplierValue,
        paste("\ngenerated at: ", 
        as.character(lubridate::with_tz(Sys.time(), tzone="EST5EDT"), 
                     usetz=TRUE), sep=""),
        "\n\nData updated on: February 2, 2024\n\n\n"
      ),
      collapse = ""
    )
    # print("subtitleText=")
    # print(subtitleText)
    
    
    # -------------------------------------------------------------------------
    # furnish the graph with its title, etc.
    # -------------------------------------------------------------------------
    
    # adding title/caption data
    # print("adding a Title/SubTitle")
    if (rv$avg_case) {    
    #if (rv$avgRendering) {#if (input$selectAvg) {
      # with the average line
      plt1 <- plt1 +  ggplot2::labs(title = titleText,
                           subtitle = msg_no_base_m_data,
                           caption = subtitleText)
    } else {
      # no average line
      plt1 <- plt1 +  ggplot2::labs(title = titleText,
                           subtitle = msg_no_base_m_data,
                           caption = subtitleText)
    }


    
    # common legend-related settings
    plt1 <- plt1 +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.key = ggplot2::element_rect(fill="white", color="white"),
        text = ggplot2::element_text(family = "barlow"),
        plot.title =   ggplot2::element_text(size = 22,
                                    vjust = 5),
        plot.subtitle = ggplot2::element_text(size = 18,
                                     color = "red"),
        plot.margin =  ggplot2::margin(t = 40, l = 20),
        plot.caption = ggplot2::element_text(size = 12,
                                    hjust = 0),
        legend.title = ggplot2::element_text(size = 14),
        legend.key.height = grid::unit(key_space, "points"),
        legend.text =  ggplot2::element_text(size = 14),
        legend.spacing.y = grid::unit(10, "points"),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x =  ggplot2::element_text(size = 14),
        axis.text.y =   ggplot2::element_text(size = 12),
        axis.text.x =   ggplot2::element_text(size = 12)
      )
    # +
    #   ggplot2::guides(
    #     color = ggplot2::guide_legend(order = 1)
    #     # shape = ggplot2::guide_legend(order = 1),
    #     # fill = ggplot2::guide_legend(order = 2)
    #   )
    
    
    # table 
    # pDefTable <- createTextualTable(df4DefTable, useDenominator)
    
    
    rv$defTable <- df4DefTable
    
    
    base::message("====== benchmarking_plot: end-time:", as.POSIXct(Sys.time(),
      tz = "EST5EDT"))
    # waitress$close()
    

    plt1

}) # end of tab 1's plot-generation function
  
  
  #############################################################################
  # renderPlot(): generating the benchmarking plot
  #############################################################################  
  output$benchmarkingDB <- shiny::renderPlot({
    # waitress$start(h3("Loading image ..."))
    # w$show()
    waiter::waiter_show(html = waiting_screen, color = "#4B9CD380")
    
    print(benchmarking_plot())
    # waitress$close() 
    # w$hide()
    
    waiter::waiter_hide()
  })
  
  #############################################################################
  output$metricDefTable <-  shiny::renderTable(rv$defTable)



  
  #############################################################################
  
  
  #############################################################################
  # downloading the current benchmarking graph as a PDF file
  #############################################################################
  output$dwonloadImage <- shiny::downloadHandler(
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
      
      ggplot2::ggsave(filename = file, 
             plot = benchmarking_plot(),
             device=grDevices::cairo_pdf,
             width = paperWidth,
             height = paerHeight,
             units = "in"
             )
      
    }
    
    
  )
  
  waiter::waiter_hide()

} # end of server


# shinyApp(ui = ui, server = server, options=c(launch.browser = .rs.invokeShinyPaneViewer))
shiny::shinyApp(ui = ui, server = server)
