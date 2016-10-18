#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Years etc whatnot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("range_years",
                     "Years:",
                     min = 1450,
                     max = 1850,
                     value = c(1600, 1750)),
         textInput("keyword_search",
                   "Keyword:",
                   value = "garden",
                   width = NULL,
                   placeholder = "keyword"),
         submitButton(text = "Apply Changes",
                      icon = icon("blind"),
                      width = NULL)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("text_debug"),
         plotOutput("year_plot"),
         plotOutput("hits_plot"),
         plotOutput("hits_averages_plot")
      )
   )
))

# Define server logic required to draw a histogram

# server data etc
source("helper_shinytesting.R")
catalog_data <- readRDS("shinytest2.Rds")
publications_yearly <- get_publications_yearly(catalog_data)
library(ggplot2)

server <- shinyServer(function(input, output) {
  
  output$text_debug <- renderText({
    sprintf("DEBUG: years low: %s, years high: %s, keyword: %s",
           input$range_years[1],
           input$range_years[2],
           input$keyword_search)
  })
   
  output$year_plot <- renderPlot({
    year_min <- input$range_years[1]
    year_max <- input$range_years[2]
    years  <- publications_yearly$years
    titles <- publications_yearly$titles
    year_min_index <- which(years == year_min)[[1]]
    year_max_index <- which(years == year_max)[[1]]
    years_subset  <- years[year_min_index:year_max_index]
    titles_subset <- titles[year_min_index:year_max_index]
    estc_subset <- data.frame(years = years_subset,
                             titles = titles_subset)
    qplot(years_subset, titles_subset, data = estc_subset,
          geom = c("point", "smooth"))
  })

  output$hits_plot <- renderPlot({
    input_years <- c(input$range_years[1], input$range_years[2])
    keyword <- input$keyword_search
    yearly_hits <- get_hits_yearly(catalog_data, input_years, keyword)   
    years <- yearly_hits$years
    hits  <- yearly_hits$hits
    qplot(years, hits, data = yearly_hits,
          geom = c("point", "smooth"))
  })
  
  output$hits_averages_plot <- renderPlot({

    input_years <- c(input$range_years[1], input$range_years[2])
    publications_yearly_subset <-
      subset(publications_yearly, years >= input_years[1] &
             years <= input_years[2])
    keyword <- input$keyword_search
    yearly_hits <- get_hits_yearly(catalog_data, input_years, keyword)   
    yearly_hits["total_publications"] <- publications_yearly_subset[2]
    yearly_hits["averages"] <-
      yearly_hits["hits"] / yearly_hits["total_publications"]
    years <- yearly_hits$years
    averages  <- yearly_hits$averages
    qplot(years, averages, data = yearly_hits,
          geom = c("point", "smooth"))
  })
  
  
       
})

# Run the application 
shinyApp(ui = ui, server = server)

