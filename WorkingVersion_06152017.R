# Load Libaries
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
      selectInput("variableInput", "Variable",
                  choices = c("Income", "Electricity"),
                  selected = "Income"),
      uiOutput("countryOutput"),
      width = 2),
    mainPanel(
      plotOutput("mainplot"),
      plotOutput("subplots")
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
  
  output$mainplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white") +
      labs(title="Let's Compare Scenarios")
    
  })
  
  output$subplots <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    p1 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white") +
      labs(title="Comparing Economic Classes (Base Scenario")
    p2 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white") +
      labs(title="Comparing Economic Classes (Pro-Poor Scenario")
    p3 <- ggplot(filtered(), aes(x = Year, y = Value, group = Scenario, colour = Scenario)) +
      geom_line() +
      geom_point( size=4, shape=21, fill="white") +
      labs(title="Comparing Economic Classes (Pro-Poor & Middle-Class Scenario")
    grid.arrange(p1,p2,p3, ncol=3)
    
    #extract legend
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
    
    mylegend<-g_legend(p1)
    
    grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                             p2 + theme(legend.position="none"),
                             p3 + theme(legend.position="none"),
                             nrow=1),
                 mylegend, nrow=2,heights=c(10, 3))
    
  })
  
}

shinyApp(ui = ui, server = server)