library(shiny)
library(shinythemes)
library(plotly)
source("helpers.R")


################################################################################
# ui defintion
################################################################################
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("Benchmarking 2.0 Project"),
  
  
  sidebarLayout(
    
    
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Home",
                 
                 
                 h3("Your selections"),
                 textOutput("selectedCity", container = p),
                 textOutput("peerGroup", container = p),
                 textOutput("selectedService", container = p),
                 textOutput("selectedYears", container=p),
                 textOutput("selectedVar4num", container = p),
                 textOutput("selectedVar4denom", container = p),
                 textOutput("selectAvg", container = p),  
                 textOutput("selectCI", container = p),    
                 hr(),
                 h3("Graph"),
                 
                 textOutput("selectedQuotient", container = h4),
                 br(),
                 plotlyOutput(outputId = "scatterplot")
                 
                 
                 ),
        tabPanel("Service",
                 # across cities comparison for variable, for year
                 h3("Service in Focus"),
                 plotlyOutput(outputId = "plotOnTab2")
                 ),
        tabPanel("Tab 3", 
                 h3("Tab 3 title here"),
                 DT::dataTableOutput("bd_data_x")
                 
                 
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

 selectInput(inputId='selectedCity', 
            labe='Select Base Municipality', 
            choices=citylabel, selected="Apex"),

checkboxInput(inputId ="selectAllpeer", 
label= "select all comparison municipalities",
value=TRUE
),

conditionalPanel(
  condition = "input.selectAllpeer == false",
checkboxGroupInput(inputId = "peerGroup", 
                   label = "Peer municipalities", 
    choices = rvllabel, selected=rvllabel)
)
,
          


selectInput(inputId='selectedService', 
            label='Service', 
            choices=s_list,
            selected=c("Residential Refuse Collection")
            ), 


checkboxGroupInput(inputId="selectedYears", 
                   label = "Years", 
                   choices = y_list,
                   selected = y_list),


selectInput(
  inputId='selectedVar4num', 
  label = 'numerator choice', 
  choices = c()),
selectInput(
  inputId='selectedVar4denom', 
  label = 'Normalization choice', 
  choices = c()),


checkboxInput(inputId ="selectAvg", 
              label= "Add Average",
              value=TRUE
),
conditionalPanel(
  condition = "input.selectAvg == true",
  checkboxInput(inputId ="selectCI", 
                label= "Add Confidence Interval (95%)",
                value=TRUE
  )
)

)




) # sidebarLayout

) # fluidPage

################################################################################
# server definition
################################################################################

server <- function(input, output) {

  
  
  observe({
    if (input$selectAllpeer){
      updateCheckboxGroupInput(inputId ="peerGroup",
                               choices=citylabel[!citylabel %in% c(input$selectedCity)], 
                               selected=citylabel[!citylabel %in% c(input$selectedCity)]
      )}
    
  })
  
  
  observe({ 
    #s1numlist   <- v_list %>% filter(Service == input$selectedService ) %>% pull()

    updateCheckboxGroupInput(
                             inputId="peerGroup",
                             choices=citylabel[!citylabel %in% c(input$selectedCity)], 
                             selected=citylabel[!citylabel %in% c(input$selectedCity)])
  })
  
  observe({
    varset4numerator <- service2var[input$selectedService]
    updateSelectInput(inputId = "selectedVar4num",
                      choices=varset4numerator)
    
  })
  
  observe({
    rawlist <-  service2var[[input$selectedService]]
    netlist <-rawlist[! rawlist %in% c(input$selectedVar4num) ]
    varset4denominator<-c(netlist, service2var[["Census"]])
    updateSelectInput(inputId ="selectedVar4denom", choices=varset4denominator )
  })
  
  observe({
    if (!input$selectAvg){
      updateCheckboxInput(inputId = "selectCI", label = "Add Confidence Interval (95%)", value = FALSE )
    }
    
  })
  

  
  
  output$selectedCity <- renderText({
    paste(c("selected Municpality=", input$selectedCity))
  })
  output$peerGroup <- renderText({
    paste(c("selected peerGroup=", input$peerGroup))
  })
  
  
  output$selectedService <- renderText({
    paste(c("selected Service=", input$selectedService))
  })
  
  output$selectedYears <- renderText({
    paste(c("selected Years=", input$selectedYears))
  })
  
  output$selectedVar4num <- renderText({
    paste(c("selected variable=", input$selectedVar4num))
  })
  
  output$selectedVar4denom <- renderText({
    paste(c("seclected denominator variable=",input$selectedVar4denom))
  })
  
  output$selectedQuotient <- renderText({
    paste(c(input$selectedVar4num, "/", input$selectedVar4denom))
  })
  
  output$selectAvg <- renderText({
    paste(c("Average seclected ?=",input$selectAvg))
  })  
  
  output$selectCI <- renderText({
    paste(c("CI seclected ?=",input$selectCI))
  })
  
  output$scatterplot <- renderPlotly({

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
print("numerator=")
print(input$selectedVar4num)
print("denominator=")
print(input$selectedVar4denom)
print("selectedYears=")
print(input$selectedYears)
data_sc_nm <- bd_data %>% 
  filter(Service == input$selectedService)   %>%
  filter(Variable==input$selectedVar4num)    %>%
  filter(Municipality == input$selectedCity) %>%
  spread(key=Year, value=Value)        %>%
  select(input$selectedYears)        %>% 
  as.matrix()
print("data_sc_nm=1")
print(str(data_sc_nm))
#dimnames(data_sc_nm)[[1]] <- input$selectedCity
rownames(data_sc_nm)<-  input$selectedCity
print("data_sc_nm=2")
print(data_sc_nm)

# base-line data for the denominator
# Census variables must be added
data_sc_dm <- bd_data %>% 
  filter(Service == input$selectedService | Service =="Census")   %>%  
  filter(Variable==input$selectedVar4denom)  %>%
  filter(Municipality == input$selectedCity) %>%
  spread(key=Year, value=Value)        %>%
  select(input$selectedYears)        %>% 
  as.matrix()
print("data_sc_dm=1")
print(data_sc_dm)
dimnames(data_sc_dm)[[1]] <- input$selectedCity
print("data_sc_dm=2")
print(data_sc_dm)
# ------------------------------------------------------------------------------
# step 2: peer group
# warning: since the same variable name is used in more than one services
# the service filter must be applied first
# base-line data for the numerator
data_pg_nm <- bd_data %>% 
  filter(Service == input$selectedService)   %>%
  filter(Variable==input$selectedVar4num)   %>%
  filter(Municipality %in% input$peerGroup) %>% 
  spread(key=Year, value=Value)       %>%
  select(input$selectedYears) %>% 
  as.matrix()
rownames(data_pg_nm) <- input$peerGroup
print("data_pg_nm=")
print(data_pg_nm)
# base-line data for the denominator
# Census variables must be added
data_pg_dm <- bd_data %>% 
  filter(Service == input$selectedService | Service =="Census")   %>%
  filter(Variable==input$selectedVar4denom) %>%
  filter(Municipality %in% input$peerGroup) %>% 
  spread(key=Year, value=Value)       %>%
  select(input$selectedYears) %>% 
  as.matrix()
rownames(data_pg_dm) <- input$peerGroup
print("data_pg_dm=")
print(data_pg_dm)

# ------------------------------------------------------------------------------
# working on peer group
print(input$selectedYears)
summarizedPG <-as_tibble(data_pg_nm / data_pg_dm) %>% 
  gather(input$selectedYears, key="Year", value = "quotient") %>%
  summarySE(measurevar = "quotient", groupvars = "Year")
print("summarizedPG=")
print(summarizedPG)
# last step 
data4plot_raw <- rbind(t(data_sc_nm / data_sc_dm)[, 1], t(summarizedPG[, 3]))
rownames(data4plot_raw ) <- c(input$selectedCity, "peer group")
print("data4plot_raw=")
print(data4plot_raw)
#-------------------------------------------------------------------------------

data4plot <- rownames_to_column(as.data.frame(data4plot_raw), var="catgry") %>% as_tibble()
print("data4plot=")
print(data4plot)

data4plot_sc <- data4plot %>% 
  gather(input$selectedYears, key = "Year", value = "quotient") %>%
  filter(catgry== input$selectedCity)
print("data4plot_sc=")
print(data4plot_sc)

data4plot_pg <- data4plot %>% 
  gather(input$selectedYears, key = "Year", value = "quotient") %>%
  filter(catgry != input$selectedCity)
print("data4plot_pg=")
print(data4plot_pg)

data4plot_pgx <- data4plot_pg %>% add_column(ci = summarizedPG[["ci"]])

# -----------------------------------------------------------------------------
# plot
# -----------------------------------------------------------------------------
# An if-block after ggplot() seems not to accept more than 1 geom_xxx 
# statements within it and the following incremental approach was used

plt1 <- data4plot_sc %>%
  ggplot(aes(x=Year, y=quotient)) +
       geom_bar(stat = 'identity', position = 'dodge', fill="#A9A9A9",colour="#A9A9A9") 
if (input$selectAvg){
  plt1 <- plt1 + geom_line(data = data4plot_pg, aes(group= catgry, color=catgry), size=1.0) 
} 
if (input$selectCI) {
  plt1 <- plt1 +  geom_errorbar(data = data4plot_pgx, aes(ymin=quotient-ci, ymax=quotient+ci), width=.1, color = "#696969", position = position_dodge(0.1)) 
}

plt1
}) # end of tab 1's plot
  
output$plotOnTab2 <- renderPlotly({
# plot for tab 2: start =======================================================
# plot for tab 2: end =========================================================
})



} # end of server



shinyApp(ui = ui, server = server)