# Install Packages
install.packages(c("shiny", "ggplot2", "tidyr", "dplyr", "tidyverse",
                   "gridExtra", "latticeExtra"))

# Load Libaries
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(latticeExtra)

data_long <- read.csv("Mauritius.csv", header=TRUE, sep=",")

# Drop columns
data_long <- select(data_long, -c(X, PopulationWeight, ElasticityIncomeDemand))

# Rename Scenario factor levels
data_long <- data_long %>%
  mutate(Scenario = recode(Scenario, 
                           Scenario_base = "Base",
                           Scenario_25 = "Pro-Poor",
                           Scenario_50 = "Pro-Poor & Middle Class")
  )

# Pre-process dataframe for mainplot by taking 
# the mean of each variable grouping
df <- data_long %>%
  group_by(Variable, Country, Scenario, Year) %>%
  summarise_each(funs(mean))

# Pre-process dataframe for subplots
data_long$Dummy1 <- ifelse(data_long$Quantile >= 900, "Top 10%", 
                           ifelse(data_long$Quantile <= 499, "Bottom 50%","Middle Class"))

df_subplot <- select(data_long, -Quantile) %>% # Include variable to split quantiles into groups
  group_by(Variable, Country, Scenario, Year, Dummy1) %>%
  summarise_each(funs(mean))

# Split into smaller data frames
df_subplot_base <- df_subplot[df_subplot[,'Scenario'] == 'Base',]
df_subplot_25 <- df_subplot[df_subplot[,'Scenario'] == 'Pro-Poor',]
df_subplot_50 <- df_subplot[df_subplot[,'Scenario'] == 'Pro-Poor & Middle Class',]


ui <- fluidPage(
  titlePanel("Economic Forecasts"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variableInput", "Variable",
                  choices = c("Income", "Electricity"),
                  selected = "Income"),
      uiOutput("countryOutput"),
      h4("Base Scenario"),
      h6("Using an income growth rate of 3.0 percent as forecasted in 2008"),
      h4("Pro-Poor Scenario"),
      h6("In this scenario, the bottom 25 percent receive an amount of 6.909 dollars
         corresponding to the cost of mean electricity consumption (53.146KW)
         observed in 2014"),
      h4("Pro-Poor & Middle-Class"),
      h6("This scenario stimulate the economy by providing fund to the bottom 25 percent
         as in the pro-poor scenario, and in addition, it distributes half of the cost of the
         mean electricity annually consumed to the households belonging to the income class 25-50 percent"),
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
  
  filtered_subplot_base <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    df_subplot_base %>%
      filter(Variable == input$variableInput,
             Country == input$countryInput
      )
    
  })
  
  filtered_subplot_25 <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    df_subplot_25 %>%
      filter(Variable == input$variableInput,
             Country == input$countryInput
      )
    
  })
  
  filtered_subplot_50 <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    df_subplot_50 %>%
      filter(Variable == input$variableInput,
             Country == input$countryInput
      )
    
  })
  
  output$subplots <- renderPlot({
    if (is.null(filtered_subplot_base())) {
      return()
    }
    
    
    p1 <- ggplot(filtered_subplot_base(), aes(x = Year, y = Value, group = Dummy1, colour = Dummy1)) +
      geom_line() +
      geom_point(size=4, shape=21, fill="white") +
      labs(title="Base Scenario")
    p2 <- ggplot(filtered_subplot_25(), aes(x = Year, y = Value, group = Dummy1, colour = Dummy1)) +
      geom_line() +
      geom_point(size=4, shape=21, fill="white") +
      labs(title="Pro-Poor Scenario")
    p3 <- ggplot(filtered_subplot_50(), aes(x = Year, y = Value, group = Dummy1, colour = Dummy1)) +
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

