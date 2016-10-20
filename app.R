#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("R/generate_output.R")

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
                     value = c(1500, 1800)),
         textInput("keyword_search",
                   "Keyword in title:",
                   value = "garden",
                   width = NULL,
                   placeholder = "keyword"),
         submitButton(text = "Apply Changes",
                      icon = icon("blind"),
                      width = NULL)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("All titles",
                   plotOutput("year_plot")),
          tabPanel("Hits / Year",
                   plotOutput("hits_plot")),
          tabPanel("Freq. hits / Year",
                   plotOutput("hits_averages_plot")),
          tabPanel("Top 10 authors",
                   plotOutput("hits_authors_plot"))
        )
      )
   )
))

# Define server logic required to draw a histogram

# server data etc
catalog_data <- readRDS("data/estc-shinytest2.Rds")
publications_yearly <- get_publications_yearly(catalog_data,
                                               years = c(1450, 1850))
library(ggplot2)

server <- shinyServer(function(input, output) {
  
  output$year_plot <- renderPlot({
    year_min <- input$range_years[1]
    year_max <- input$range_years[2]
    years  <- publications_yearly$year
    titles <- publications_yearly$titles
    year_min_index <- which(years == year_min)[[1]]
    year_max_index <- which(years == year_max)[[1]]
    years_subset  <- years[year_min_index:year_max_index]
    titles_subset <- titles[year_min_index:year_max_index]
    estc_subset <- data.frame(year = years_subset,
                             titles = titles_subset)
    qplot(years_subset, titles_subset, data = estc_subset,
          geom = c("point", "smooth")) +
      labs(x = "Year", y = "Titles", title = "All titles per year")
  })

  output$hits_plot <- renderPlot({
    input_years <- c(input$range_years[1], input$range_years[2])
    keyword <- input$keyword_search
    
    id <- showNotification(paste("Calculating results ..."), duration = 0)
    # withProgress(message = 'calculating stuff', value = 0)
    
    yearly_hits <- get_hits_yearly(catalog_data, input_years, keyword)   
    
    years <- yearly_hits$year
    hits  <- yearly_hits$titles
    
    removeNotification(id)
    id <- NULL
    
    qplot(years, hits, data = yearly_hits,
          geom = c("point", "smooth")) +
      labs(x = "Year", y = "Titles",
           title = "Titles with keyword per year")
    
  })
  
  output$hits_averages_plot <- renderPlot({

    input_years <- c(input$range_years[1], input$range_years[2])
    keyword <- input$keyword_search

    id <- showNotification(paste("Calculating results ..."), duration = 0)
        
    title_hits_average <- get_title_hits_average(catalog_data,
                                                 input_years,
                                                 keyword,
                                                 publications_yearly)

    removeNotification(id)
    id <- NULL

    years <- title_hits_average$year
    averages  <- title_hits_average$averages
    qplot(years, averages, data = title_hits_average,
          geom = c("point", "smooth")) +
      labs(x = "Year", y = "Titles",
           title = "Titles with keyword per year, proportional")
  })
  
  output$hits_authors_plot <- renderPlot({
    input_years <- c(input$range_years[1], input$range_years[2])
    keyword <- input$keyword_search

    id <- showNotification(paste("Calculating results ..."), duration = 0)
    
    top_10_authors <- get_top10_authors(catalog_data,
                                        input_years,
                                        keyword)
    
    removeNotification(id)
    id <- NULL

    ggplot(top_10_authors, aes(x = reorder(author, hits), y = hits)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Author", y = "Titles",
           title = "Top 10 authors of titles with keyword")
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

