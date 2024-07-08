#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
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
    output$data_table <- renderDataTable({
      data
    })
    
    updateSelectInput(session, "x_var", choices = names(data))
    updateSelectInput(session, "y_var", choices = names(data))
  })
  
  
  
  observeEvent(input$get_data_country, {
    req(input$country_name)
    country_data <- get_country_by_name(input$country_name, input$fullText)
    output$country_data_table <- renderDataTable({
      country_data
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
      paste("country_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(as.data.frame(data()), file, row.names = FALSE)
    }
  )
  
  #Get the plot graphs
  observeEvent(input$plot_data, {
    req(input$x_var, input$y_var, input$plot_type)
    
    output$plot <- renderPlot({
     if (input$plot_type == "Bar Plot") {
      ggplot(plot_data, aes_string(x = input$x_var, fill = input$y_var)) +
        geom_bar() +
        theme_minimal() +
        labs(title = paste("Bar Plot", input$x_var, "by", input$y_var),
          x = input$x_var,
          y = "Count")
    } else if (input$plot_type == "Scatter Plot") {
      ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        theme_minimal() +
        labs(title = paste("Scatter Plot", input$x_var, "by", input$y_var),
             x = input$x_var,
             y = input$y_var)
    }
  })
})
}

shinyApp(ui = ui, server = server)

