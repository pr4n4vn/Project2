#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


#In this first ui code, I am setting the visual aspect of the Shiny Dashboard web app.  
ui <-
  navbarPage("Country Data Explorer",
             tabPanel("About",
                      sidebarLayout(
                        sidebarPanel(
                          tags$img(src = "https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/styles/medium/public/vhp_img5720.jpg?itok=ORSHFM9B"),
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
                          textInput("country_name", "Country Name"),
                          checkboxInput("fullText","Full Text Match", FALSE),
                          actionButton("get_data_country", "Get Data by Country"),
                          hr(),
                          downloadButton("download_data", "Download Data", class = NULL)
                        ),
                        mainPanel(
                          dataTableOutput("data_table"),
                          dataTableOutput("country_data_table")
                        )
                      )
             ),
             
             tabPanel("Data Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("variable", "Select Variable to Summarize:", choices = c("population", "name")),
                          selectInput("plot_type", "Select Plot Type:", choices = c("Bar Plot", "Scatter Plot")),
                          actionButton("plot_data", "Plot Data")
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      ))
  )

  


library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(DT)
# Define server logic required.
server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)

  #Function to get the data by region
  get_countries_by_region2 <- function(region, fields = NULL) {
    base_url <- "https://restcountries.com/v3.1/region/"
    url <- paste0(base_url, region)
    if (!is.null(fields)) {
      url <- paste0(url, "?fields=", fields)
    }
    response <- GET(url)
    
    if (status_code(response) == 200) {
      content <- content(response, "text", encoding = "UTF-8")
      content_json <- fromJSON(content, flatten = TRUE)
      country_data <- as_tibble(content_json)
      return(country_data)
    } else {
      stop("Failed to retrieve data")
    }
  }

 
  #here is the function to get the country data by name. Somehow, it works in my project2 file, but in the shiny app dashboard, it comes up as a "Malformed input to a URL function" error. 

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
  
  
  #Code for program to fetch the data based upon input from user.  
  observeEvent(input$get_data_region, {
    req(input$region, input$field)
    fields <- gsub("\\s+", "", input$field)
    fetched_data <- get_countries_by_region2(input$region, fields)
    data(fetched_data)
    output$data_table <- renderDataTable({
      data()
    })
    
    updateSelectInput(session, "x_var", choices = names(data))
    updateSelectInput(session, "y_var", choices = names(data))
  })
  
  
  #Code to fetch data by country. 
  observeEvent(input$get_data_country, {
    req(input$country_name)
    country_data <- get_country_by_name(input$country_name, input$fullText)
    output$country_data_table <- renderDataTable({
      country_data
    })
  })
  
  
  
  #This is to show that the output of the subsetted data can display on the UI
  output$subset_data <- renderUI({
    req(data())
    tagList(
      selectInput("subset_cols", "Columns to display", choices = names(data()), multiple = TRUE),
      numericInput("subset_rows", "Number of rows to display", value = nrow(data()), min = 1, max = nrow(data()))
    )
  })
  
  #This has to be able to subset the data accordingly  
  observeEvent(input$subset_rows, {
    req(input$subset_rows, data())
    
    output$data_table <- renderDataTable({
      req(input$subset_rows)
      datatable(data()[1:input$subset_rows, input$subset_cols, drop = FALSE])
    })
  })
  
  #This has to be able to download data.   
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(as_data_frame(data()), file)
    }
  )

  #Get the plot graphs, but here the button does not work properly, and I'm not sure where I went wrong here
  plot_data <- reactiveVal(NULL)
  
  observeEvent(input$plot_data, {
    req(input$x_var, input$y_var, input$plot_type)
    
      if (input$plot_type == "Bar Plot") {
        plot <- ggplot(plot_data(), aes_string(x = input$x_var, fill = input$y_var)) +
          geom_bar() +
          theme_minimal() +
          labs(title = paste("Bar Plot", input$x_var, "by", input$y_var),
               x = input$x_var,
               y = "Count")
      } else if (input$plot_type == "Scatter Plot") {
        plot <- ggplot(plot_data(), aes_string(x = input$x_var, y = input$y_var)) +
          geom_point() +
          theme_minimal() +
          labs(title = paste("Scatter Plot", input$x_var, "by", input$y_var),
               x = input$x_var,
               y = input$y_var)
      }
    
      output$plot <- renderPlot({
        plot
      })
    })
}



shinyApp(ui, server)
