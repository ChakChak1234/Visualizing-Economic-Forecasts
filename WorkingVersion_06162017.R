# Load Libaries
library(shiny)
library("highcharter")
library(tidyr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(latticeExtra)

# Pre-process dataframe for mainplot
df <- data_long %>%
  group_by(Variable, Country, Scenario, Year) %>%
  summarise_each(funs(mean))

# Pre-process dataframe for subplots
data_long$Dummy1 <- ifelse(data_long$Quantile >= 900, "Top 10%", 
                           ifelse(data_long$Quantile <= 499, "Bottom 50%","Middle Class"))

df_subplot <- data_long %>% # Include variable to split quantiles into groups
  group_by(Variable, Country, Scenario, Year, Dummy1) %>%
  summarise_each(funs(mean))

# Split into smaller data frames
df_subplot_base <- df_subplot[df_subplot[,'Scenario'] == 'Scenario_base',]
df_subplot_25 <- df_subplot[df_subplot[,'Scenario'] == 'Scenario_25',]
df_subplot_50 <- df_subplot[df_subplot[,'Scenario'] == 'Scenario_50',]


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
      geom_point(size=4, shape=21, fill="white") +
      labs(title="Let's Compare Scenarios")
    
  })
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(df_subplot$Country)),
                selected = "Mauritius")
  })  
  
  filtered_subplot <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    df_subplot %>%
      filter(Variable == input$variableInput,
             Country == input$countryInput
      )
    
  })
  
  output$subplots <- renderPlot({
    if (is.null(filtered_subplot())) {
      return()
    }
    
    
    p1 <- ggplot(filtered_subplot(), aes(x = Year, y = Value, group = Dummy1, colour = Dummy1)) +
      geom_line() +
      geom_point(size=4, shape=21, fill="white") +
      labs(title="Base Scenario")
    p2 <- ggplot(filtered_subplot(), aes(x = Year, y = Value, group = Dummy1, colour = Dummy1)) +
      geom_line() +
      geom_point(size=4, shape=21, fill="white") +
      labs(title="Pro-Poor Scenario")
    p3 <- ggplot(filtered_subplot(), aes(x = Year, y = Value, group = Dummy1, colour = Dummy1)) +
      geom_line() +
      geom_point(size=4, shape=21, fill="white") +
      labs(title="Pro-Poor & Middle-Class Scenario")
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

