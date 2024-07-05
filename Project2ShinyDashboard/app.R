#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(shinyFeedback)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    navbarPage("Country Data Explorer",
    
    tabPanel("About",
             sidebarLayout(
               sidebarPanel(
                 img(src = "world.jpg", height = 200, width = 200),
                 h3("About this application"),
                 p("The purpose of this application is to analyze different data points by region, country name. "),
                 p("The API that was utilized for this application:", a("REST Countries API", href = "https://restcountries.com/")),
                 h4("Tabs"),
                 p("About: Shows information about the application."),
                 p("Data Download: Data can be downloaded."),
                 p("Data Exploration: Data can be explored and visualized/summarized.")
               ),
               mainPanel(
                 h2("World Data Explorer"),
                 p("This allows you to see the data by region. In this application, the data can be downloaded, visualized, and subset.")
               )
             )
    ),
             
    tabPanel("Data Download",
             sidebarLayout(
               sidebarPanel(
                 useShinyFeedback(),
                 textInput("region", "Enter the Region:", value = "Europe"),
                 textInput("field", "Enter the Field", value = "name,population,area"),
                 actionButton("get_data", "Get Data"),
                 hr(),
                 checkboxGroupInput("columns", "Select Columns", choices = NULL, selected = NULL),
                 downloadButton("downloadData", "Download Data")
               ),
               mainPanel(
                 dataTableOutput("data_table")
               )
             )
    ),
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 selectInput("xvar", "X-axis variable", choices = NULL),
                 selectInput("yvar", "Y-axis variable", choices = NULL),
                 selectInput("colorvar", "Color variable", choices = NULL),
                 selectInput("plot_type", "Type of plot", choices = c("Bar Plot", "Scatter Plot")),
                 actionButton("plot_data", "Plot the data")
               ),
               mainPanel(
                 plotOutput("data_plot"),
                 verbatimTextOutput("summary")
               )
             )
    )
    )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  get_countries_by_region2 <- function(region, fields = NULL) {
    base_url <- "https://restcountries.com/v3.1/region/"
    url <- paste0(base_url, region)
    if (!is.null(fields)) {
      url <- paste0(url, "?fields=", fields)
    }
    
    response <- GET(url)
    
    if (status_code(response) != 200) {
      stop("Failed to retrieve data")
    }
    content <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    return(as_tibble(content))
  }
  
  data <- reactiveVal(NULL)
  
  #Code for program to fetch the data based upon input from user.  
  observeEvent(input$get_data, {
    req(input$region, input$fields)
    fields <- gsub("\\s+", "", input$fields)
    data_fetched <- get_countries_by_region2(input$region, fields)
    data(data_fetched)  
    
    updateSelectInput(session, "x_var", choices = names(data_fetched))
    updateSelectInput(session, "y_var", choices = names(data_fetched))
    
  
    output$data_table <- renderDataTable({
      datatable(data_fetched)
    })
  })
  
#This has to subset data
  output$subset_data <- renderUI({
    req(data())
    tagList(
      selectInput("subset_cols", "Columns to display", choices = names(data()), multiple = TRUE),
      numericInput("subset_rows", "Number of rows to display", value = nrow(data()), min = 1, max = nrow(data()))
    )
  })
  
  observeEvent(input$subset_rows, {
    output$data_table <- renderDataTable({
      req(input$subset_rows)
      datatable(data()[1:input$subset_rows, , drop = FALSE])
    })
  })
  
#This has to be able to download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
#Get the plot graphs
  output$plot <- renderPlot({
    req(input$x_var, input$y_var, input$plot_type)
    plot_data <- data()
    
    if (input$plot_type == "Bar Plot") {
      ggplot(plot_data, aes_string(x = input$x_var)) +
        geom_bar() +
        theme_minimal() +
        labs(title = "Bar Plot", x = input$x_var, y = "Count")
    } else if (input$plot_type == "Scatter Plot") {
      ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        theme_minimal() +
        labs(title = "Scatter Plot", x = input$x_var, y = input$y_var)
    }
  })
}

  

# Run the application 
shinyApp(ui = ui, server = server)
  