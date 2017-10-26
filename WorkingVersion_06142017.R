# Load Libarries
library(shiny)
library("highcharter")
library(tidyr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(latticeExtra)

df <- data_long %>%
  group_by(Variable, Country, Scenario, Year) %>%
  summarise_each(funs(mean))


ui <- fluidPage(
  titlePanel("Economic Forecasts"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("variableInput", "Variable",
                   choices = c("Income", "Electricity"),
                   selected = "Income"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(df$Country)),
                selected = "Mauritius")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    df %>%
      filter(Variable == input$variableInput,
             Country == input$countryInput
      )
    
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    p1 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white")
    p2 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white")
    p3 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white")
    p4 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white")
    grid.arrange(p1,p2,p3,p4, layout_matrix = rbind(c(1,1,1),c(2,3,4)))
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)
