library(shiny)
library(shinythemes)
library(plotly)
source("helpers.R")


################################################################################
# ui defintion
################################################################################
ui <- fluidPage(
#  shinythemes::themeSelector(),
  titlePanel("Benchmarking 2.0"),

  
  sidebarLayout(
    
    
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Home",
                 
                 
    

                # h3("UNC SoG Benchmark 2.0 Dashboard"),
                 
                 textOutput("selectedQuotient", container = h4),
                 br(),
                 plotlyOutput(outputId = "scatterplot"),
                 hr(),
                 h3("Your selections"),
                 textOutput("selectedCity", container = p),
                 textOutput("peerGroup", container = p),
                 textOutput("selectedService", container = p),
                 textOutput("selectedYears", container=p),
                 textOutput("selectedVar4num", container = p),
                 textOutput("selectedVar4denom", container = p),
                 textOutput("selectAvg", container = p),  
                 textOutput("selectCI", container = p)
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
              choices=c(citylabel), 
              selectize = TRUE),

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

selectInput(inputId='selectedService', 
            label='Service', 
            choices=s_list), 

# select years: checkboxes 
checkboxGroupInput(inputId="selectedYears", 
                   label = "Years", 
                   choices = y_list,
                   selected = y_list),

# select a numerator variable:pulldown menu

selectInput(inputId='selectedVar4num', 
            label = 'Select Service Metric', 
            choices = c()),

# select a denominator variable: pulldwon menu
selectInput(inputId='selectedVar4denom', 
            label = 'Select Denominator/Context Variable', 
            choices = c()),

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
downloadButton('downloadGraph')

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
    if (!input$selectAllpeer){
      updateCheckboxGroupInput(inputId ="peerGroup",
                               choices=citylabel[!citylabel %in% c(input$selectedCity)], 
                               selected=character(0)
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
    
    
    if (identical(input$peerGroup, citylabel[!citylabel %in% c(input$selectedCity)])){
      print("select all state")
      print(input$peerGroup)
      updateCheckboxInput(inputId = "selectAllpeer", value = TRUE)
    } else {
      print("not select all state")
      print(input$peerGroup)
    }
  })
  
  
  
  
  
# updating the set of numerator variables
  
  observe({
    
    varset4numerator <- service2var[input$selectedService]
    
    updateSelectInput(inputId = "selectedVar4num",
                      choices=c("", varset4numerator))
    
  })
  
  observe({
    
    rawlist <-  service2var[[input$selectedService]]
    netlist <-rawlist[! rawlist %in% c(input$selectedVar4num) ]
    varset4denominator<-c(netlist, service2var[["Census"]])
    
    updateSelectInput(inputId ="selectedVar4denom", 
                      choices=c("", varset4denominator) )
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
    paste(c("selected Comparison Municipalities=", input$peerGroup))
  })
  
  
  output$selectedService <- renderText({
    paste(c("selected Service =", input$selectedService))
  })
  
  output$selectedYears <- renderText({
    paste(c("selected Years =", input$selectedYears))
  })
  
  output$selectedVar4num <- renderText({
    paste(c("selected Service Metric =", input$selectedVar4num))
  })
  
  output$selectedVar4denom <- renderText({
    paste(c("seclected Denominator Variable =",input$selectedVar4denom))
  })
  
  output$selectedQuotient <- renderText({
    paste(c("", input$selectedVar4num, "/", input$selectedVar4denom), collapse = "")
  })
  output$selectMultiplier <- renderText({ 
    paste(c("Multiplier=", input$selectMultiplier)) })
  
  output$selectAvg <- renderText({
    paste(c("Average seclected =",input$selectAvg))
  })  
  
  output$selectCI <- renderText({
    paste(c("CI seclected =",input$selectCI))
  })
  
  output$scatterplot <- renderPlotly({
    
    
    
    # input: sanity check
    req(input$selectedCity, 
        input$selectedService,
        input$selectedYears,
        input$selectedVar4denom, 
        input$selectedVar4denom)

    
    
    
    
    
    
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
data_sc_nm <- 10^as.integer(input$selectMultiplier) *  data_sc_nm

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
data_pg_nm <- 10^as.integer(input$selectMultiplier) * data_pg_nm

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

print("selectedYears=")
print(input$selectedYears)
print("selectedYears:length=")
print(length(input$selectedYears))

data4EachPeerCity_raw_m <- data_pg_nm / data_pg_dm
print("data4EachPeerCity_raw_m=")
print(data4EachPeerCity_raw_m)
print(str(data4EachPeerCity_raw_m))
print("has_rownames: data4EachPeerCity_raw_m=")
print(has_rownames(data4EachPeerCity_raw_m))

# data4plot <- rownames_to_column(as.data.frame(data4plot_raw), var="catgry") %>% as_tibble()
data4EachPeerCity_rawt <- rownames_to_column(as.data.frame(data4EachPeerCity_raw_m), var="catgry") %>%
  as_tibble()

print("data4EachPeerCity_rawt=")
print(data4EachPeerCity_rawt)

data4EachPeerCity_rawt <- data4EachPeerCity_rawt %>% gather(input$selectedYears,
                                  key="Year", 
                                  value = "quotient")
print("data4EachPeerCity_rawt=")
print(data4EachPeerCity_rawt)



data4EachPeerCity <- as_tibble(data_pg_nm / data_pg_dm) %>% 
  gather(input$selectedYears, key="Year", value = "quotient")
print("data4EachPeerCity=")
print(data4EachPeerCity)




summarizedPG <-as_tibble(data_pg_nm / data_pg_dm) %>% 
  gather(input$selectedYears, key="Year", value = "quotient") %>%
  summarySE(measurevar = "quotient", groupvars = "Year")
print("summarizedPG=")
print(summarizedPG)
# last step 
data4plot_raw <- rbind(t(data_sc_nm / data_sc_dm)[, 1], t(summarizedPG[, 3]))
rownames(data4plot_raw ) <- c(input$selectedCity, "Average")
print("data4plot_raw=")
print(data4plot_raw)
#-------------------------------------------------------------------------------

data4plot <- rownames_to_column(as.data.frame(data4plot_raw), var="catgry") %>% 
  as_tibble()
if (ncol(data4plot) == 2){
  print("***** column is 3 *****")
  yr <- input$selectedYears[1]
  print(yr)
  data4plot <- data4plot %>% dplyr::rename(!!yr:= "V1")
} else {
  print("***** column is not 2 *****")
  print(ncol(data4plot))
}
print("data4plot=")
print(data4plot)



# the following line fails if the number of years is 1
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

# baseline rendering 
plt1 <- data4plot_sc %>%
  ggplot(aes(x=Year, y=quotient)) +
       geom_bar(stat = 'identity', position = 'dodge', fill="#A9A9A9", 
                color="#A9A9A9") + scale_y_continuous(name="Value")

if (input$selectAvg){
  # add an average line
  plt1 <- plt1 + 
            geom_line(data = data4plot_pg, 
            aes(x = Year, y = quotient, group = catgry, color = catgry)) + 
            scale_color_discrete(name = "Legend", labels = c("average"))
  
  if (input$selectCI) {
    # add CI-bands
    plt1 <- plt1 + geom_errorbar(data = data4plot_pgx,
      aes(ymin = quotient - ci, ymax = quotient + ci), 
      width = .1, color = "#696969", position = position_dodge(0.1)) 
  }
  
} else {
  # each municipality's line is added 
  # tibble to be used
  
  plt1 <- plt1 + 
    geom_line(data = data4EachPeerCity_rawt, 
              aes(x = Year, y = quotient, group = catgry, color = catgry)) +
    scale_color_discrete(name = "Legend")

}
# furnish the graph with its title, etc.

print("numerator=")
print(input$selectedVar4num)
print("denominator=")
print(input$selectedVar4denom)
titleText <- paste(c("" , 
                     input$selectedVar4num, " ", 
                     input$selectedVar4denom), collapse = "")

peerGroupList <- paste(input$peerGroup,collapse=",")

subtitleText <-paste(c("Base Municipality: ",input$selectedCity,
                      "\nService: ", input$selectedService,
                      "\nComparison Municipalities: ", peerGroupList
                      ), 
                      collapse = "")
print("subtitleText=")
print(subtitleText)

plt1 <- plt1 +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(axis.title.y = element_blank()  )
  labs(title = titleText,
       caption = subtitleText)


ggsave(filename = "graph.pdf",plot =  plt1, device = "pdf", width = 8.5, 
       height = 10, units = "in")

#plot_ly() %>% config(displayModeBar = FALSE)
ggplotly(plt1) %>% config(displayModeBar = FALSE)
}) # end of tab 1's plot
  
output$plotOnTab2 <- renderPlotly({
# plot for tab 2: start =======================================================
# plot for tab 2: end =========================================================
})
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