# Load Libaries
library(shiny)
library("highcharter")
library(tidyr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(latticeExtra)

# Read CSVs
scenario_base <- read.csv('base_scenario_aggregated', header=TRUE, sep=",")
scenario_25 <- read.csv('scenario_25_aggregated', header=TRUE, sep=",")
scenario_50 <- read.csv('scenario_50_aggregated', header=TRUE, sep=",")

# Remove extra columns
scenario_25 <- scenario_25[,-7]
scenario_base <- scenario_base[,-7]

# Merge data frames
data <- do.call(rbind, Map(cbind, Scenario = c("Scenario_base","Scenario_25","Scenario_50"), 
                           mget(paste0("scenario", c("_base", "_25","_50")))))

# Reset row.names and remove more columns
rownames(data) <- NULL
data<-data[,(-2:-3)]

# Reshape dataframe from wide to long
data_long <- data %>% 
  gather(var_year, Value, starts_with("Income"), starts_with("Electricity")) %>% 
  separate(var_year, c("Variable", "Year"), -5)

# Add Column for country, and reorganize data frame
data_long["Country"] <- as.factor("Mauritius")
data_long <- data_long[c('Variable', 'Country', 'Scenario', 'Year', 'Quantile', 'Value', 
                         'PopulationWeight', 'ElasticityIncomeDemand')]

# Convert from character to integer
data_long$Year <- as.integer(data_long$Year)

# Write to csv
write.csv(x = data_long, file = "Mauritius.csv")



ui <- fluidPage(
  h1("Economic Forecasts in several countries"),
  fluidRow(
    column(width = 1.5, class = "panel",
           selectInput("Country", label = "Country", width = "100%",
                       choices = c("Mauritius", "Uganda", "Mexico", "Bolivia")), 
           selectInput("Variable", label = "Variable",  width = "100%",
                       choices = c("Income", "Electricity Consumption"))
    ),
    column(width = 11, "Main Plot",
           highchartOutput("hcontainer", height = "500px"),
           fixedRow(
             column(3,
                    "Base Scenario",
                    highchartOutput("hcontainer",height = "500px")
                  ),
             column(3,
                    "Pro-Poor",
                    highchartOutput("hcontainer",height = "500px")
                    ),
             column(3,
                    "Pro-Poor & Middle Class",
                    highchartOutput("hcontainer",height = "500px")
                    )
           )
    )
  )
)

#server = function(input, output) {
  
#  output$hcontainer <- renderHighchart({
    
#    hc <- hc(data_long, "line", hcaes(x = Year, y = Value, group = Scenario),
#             color = c("#e5b13a", "#4bd5ee", "#4AA942")) %>%
#      hc_title(
#        text = "Differences Across Scenarios", 
#        useHTML = TRUE) %>%
#      hc_tooltip(table = TRUE, sort = TRUE) %>%
#      hc_credits(
#        enabled = TRUE,
#        text = "Source: United Nations Data") %>%
#      hc_add_theme(hc_theme_538()) %>%
#      hc_exporting(
#        enabled = TRUE
#      )
#    
#  })
  
#}

server <- function(input, output){
  # Define variable tab and filter out data according to selection
  output$variableSelectorOutput <- renderUI({
    selectInput("variableInput", "Variable",
                sort(unique(data_long$Variable)),
                selected = "Income")
  })
  # Define country tab and filter out data according to selection
  output$countrySelectorOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(data_long$Country)),
                selected = "Mauritius")
  })
  
  Value <- reactive({
    Value <- data_long
    
    if(is.null(input$countryInput)){
      return(NULL)
      
    }
    
    Value <- dplyr::filter(Value, Variable %in% input$variableInput)
    #if(input$filterVariable){
    #  Value <- dplyr::filter(Value, Variable == input$variableInput)
    #}
    
    Value <- dplyr::filter(Value, Country %in% input$countryInput)
    
    
    if(nrow(Value) == 0){
      return(NULL)
    }
    
    Value
    
  })
  
  
  output$hcontainer <- renderHighchart({
    
      hc <- hc(data_long, "line", hcaes(x = Year, y = Value, group = Scenario),
               color = c("#e5b13a", "#4bd5ee", "#4AA942")) %>%
        hc_title(
          text = "Differences Across Scenarios", 
          useHTML = TRUE) %>%
        hc_tooltip(table = TRUE, sort = TRUE) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: United Nations Data") %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_exporting(
          enabled = TRUE
        )
    
    
  })
  
}


shinyApp(ui = ui, server = server)


