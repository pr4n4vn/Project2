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
ui <-
    
    # Application title
    navbarPage("Country Data Explorer",
    
    tabPanel("About",
             sidebarLayout(
               sidebarPanel(
                 h3("About this application"),
                 p("The purpose of this application is to analyze different data points by region, country name. "),
                 p("The API that was utilized for this application:", a("REST Countries API", href = "https://restcountries.com/")),
                 h4("Tabs"),
                 p("About: Shows information about the application."),
                 p("Data Download: Data can be downloaded."),
                 p("Data Exploration: Data can be explored and visualized/summarized.")
               ),
               mainPanel()
               )
             ),
             
    tabPanel("Data Download",
             sidebarLayout(
               sidebarPanel(
                 useShinyFeedback(),
                 textInput("region", "Enter the Region:", value = "Europe"),
                 textInput("field", "Enter the Field", value = "name,population,area"),
                 actionButton("get_data_region", "Get Data by Region"),
                 hr(),
                 textInput("country_name", "Country Name", "United States"),
                 checkboxInput("fullText","Full Text Match", FALSE),
                 actionButton("get_data_country", "Get Data by Country"),
                 hr(),
                 downloadButton("downloadData", "Download Data")
               ),
               mainPanel(
                 DTOutput("data_table")
               )
             )
    ),
    
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Variable to Summarize:", choices = c("population", "name")),
                 selectInput("plot_type", "Select Plot Type:", choices = c("Bar Plot", "Histogram")),
                 actionButton("plot_data", "Plot Data")
               ),
               mainPanel(
                 plotOutput("data_plot")
               )
             ))
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
  
  get_country_by_name <- function(country_name, fullText = NULL) {
    url_base <- paste0("https://restcountries.com/v3.1/name/", country_name)
    
    query_params <- list(fullText = fullText)
    
    result <- GET(url_base, query = query_params)
    
    if (status_code(result) == 200) {
      content <- content(result, "text", encoding = "UTF-8")
      content_json <- fromJSON(content, simplifyDataFrame = TRUE)
      country_data <- as_tibble(content_json)
      return(country_data)
    } else {
      stop("Country data retrieval failed.")
    }
  }
  
  data <- reactiveVal(NULL)
  
  #Code for program to fetch the data based upon input from user.  
  observeEvent(input$get_data_region, {
    req(input$region, input$fields)
    fields <- gsub("\\s+", "", input$fields)
    data <- get_countries_by_region2(input$region, fields)
    output$data_table <- renderDT(data, options = list(pageLength = 10))
    data(data_fetched)  
    
    updateSelectInput(session, "variable", choices = names(data))
  })
  
  
  observeEvent(input$get_data_country, {
    req(input$country_name)
    data <- get_country_by_name(input$country_name, input$fullText)
    output$data_table <- renderDT(data, options = list(pageLength = 10))
    
    updateSelectInput(session, "variable", choices = names(data))
  })
  
  
#This has to subset data
  output$subset_data <- renderUI({
    req(data())
    tagList(
      selectInput("subset_cols", "Columns to display", choices = names(data()), multiple = TRUE),
      numericInput("subset_rows", "Number of rows to display", value = nrow(data()), min = 1, max = nrow(data()))
    )
  })
  
#This has to be able to subset the data accordingly  
  observeEvent(input$subset_rows, {
    output$data_table <- renderDataTable({
      req(input$subset_rows)
      datatable(data()[1:input$subset_rows, , drop = FALSE])
    })
  })
  
#This has to be able to download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("country_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
#Get the plot graphs
  observeEvent(input$plot_data, {
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
  