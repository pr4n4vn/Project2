#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
