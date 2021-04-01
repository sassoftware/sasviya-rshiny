# Copyright © 2020, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Import custom functions we developed for the app
source("R/get_data.R")
source("R/generate_analysis.R")

library(dplyr)
library(swat)
library(ggplot2)
library(reshape2)
library(shiny)
library(plotly)
options(cas.print.messages = FALSE)

# What is the largest file size (in MB) that a user should be able to upload to
# the app?
max_mb <- 30   # we're setting it to 30MB

# Restrict file uploads to 30MB max
options(shiny.maxRequestSize = (max_mb * 1024^2))

conn <- 0
miss <- 0


########################################
#             Defining the UI 
#    Creating the panels and tabs 
########################################

if (interactive()) {
  ui <- fluidPage(
    titlePanel("Viya App"),
    tabsetPanel(     
      tabPanel(title = "Login",
               sidebarLayout(
                 sidebarPanel(
                   tags$head(tags$style(type="text/css", "
                       #loadmessage {
                         position: fixed;
                          top: 50%;
                          left: 50%;
                          transform: translate(-50%, -50%);
                          width: 100%;
                          padding: 5px 0px 5px 0px;
                          text-align: center;
                          font-weight: bold;
                          font-size: 100%;
                          color: #000000;
                          background-color: rgba(0,255,0,0.5);
                       }")
                   ),
                   textInput(label = "What is your username?", inputId = "username"),
                   passwordInput(label= "What is your password?", inputId = "pwd"),
                   submitButton("Login", icon("sign-in-alt")),
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    tags$div("Loading...",id="loadmessage"))
                 ),
                 mainPanel(
                   tableOutput("connection"),
                 )
               )
      ),
      tabPanel(title = "Import Data",
               sidebarLayout(
                 sidebarPanel(
                   fileInput(inputId = "csv_file", label="File", multiple = FALSE, accept = NULL, width = NULL)
                 ),
                 mainPanel(
                   tableOutput("tbl_import"),
                 )
               )
      ),
      tabPanel(title = "Verify Data",
               sidebarLayout(
                 sidebarPanel(
                   textInput(label = "Library Name", inputId = "library_to_explore"),
                   submitButton("List Tables", icon("wpexplorer"))
                 ),
                 mainPanel(
                   tableOutput("tbl_list"),
                 )
               ),
               sidebarLayout(
                 sidebarPanel(
                   textInput(label = "Table Name", inputId = "tbl_to_profile"),
                   submitButton("Profile Data", icon("wpexplorer"))
                 ),
                 mainPanel(
                   tableOutput("tbl_profile"),
                 )
               )
      ),
      tabPanel(title = "Explore Data",
               sidebarLayout(
                 sidebarPanel(
                   tags$head(tags$style(type="text/css", "
                       #loading {
                         position: fixed;
                        top: 50%;
                        left: 50%;
                        transform: translate(-50%, -50%);
                        width: 100%;
                        padding: 5px 0px 5px 0px;
                        text-align: center;
                        font-weight: bold;
                        font-size: 100%;
                        color: #000000;
                        background-color: rgba(0,255,0,0.5);
                       }")
                   ),
                   textInput(label = "Table Name", inputId = "tbl_to_explore"),
                   submitButton("Explore Data", icon("wpexplorer")),
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    tags$div("Loading...",id="loadmessage"))
                 ),
                 mainPanel(
                   plotlyOutput("tbl_explore"),
                   plotlyOutput("tbl_cardinality")
                 )
               )
      ),
      tabPanel(title = "Feature Enginering",
               sidebarLayout(
                 sidebarPanel(
                   tags$head(tags$style(type="text/css", "
                       #loading {
                         position: fixed;
                          top: 50%;
                          left: 50%;
                          transform: translate(-50%, -50%);
                          width: 100%;
                          padding: 5px 0px 5px 0px;
                          text-align: center;
                          font-weight: bold;
                          font-size: 100%;
                          color: #000000;
                          background-color: rgba(0,255,0,0.5);
                       }")
                   ),
                   textInput(label = "Table Name", inputId = "tbl_to_model"),
                   textInput(label = "Target Variable", inputId = "target_var"),
                   checkboxInput(inputId="a", label="Analyze Missing", value = FALSE, width = NULL),
                   checkboxInput(inputId="b", label="Analyze Cardinality", value = FALSE, width = NULL),
                   checkboxInput(inputId="c", label="Analyze Entropy", value = FALSE, width = NULL),
                   checkboxInput(inputId="d", label="Analyze IQV", value = FALSE, width = NULL),
                   checkboxInput(inputId="e", label="Analyze Skewness", value = FALSE, width = NULL),
                   checkboxInput(inputId="f", label="Analyze Kurtosis", value = FALSE, width = NULL),
                   checkboxInput(inputId="g", label="Analyze Outlier", value = FALSE, width = NULL),
                   
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    tags$div("Loading...",id="loading")),
                   submitButton("Generate Features", icon("wpexplorer"))
                 ),
                 mainPanel(
                   tableOutput("transformed")
                 )
               )
      ),
      tabPanel(title = "Auto ML",
               sidebarLayout(
                 sidebarPanel(
                   tags$head(tags$style(type="text/css", "
                       #loading {
                         position: fixed;
                        top: 50%;
                        left: 50%;
                        transform: translate(-50%, -50%);
                        width: 100%;
                        padding: 5px 0px 5px 0px;
                        text-align: center;
                        font-weight: bold;
                        font-size: 100%;
                        color: #000000;
                        background-color: rgba(0,255,0,0.5);
                       }")
                   ),
                   checkboxInput(inputId="dt", label="Decision Tree", value = FALSE, width = NULL),
                   checkboxInput(inputId="rf", label="Random Forest", value = FALSE, width = NULL),
                   checkboxInput(inputId="gb", label="Gradient Boosting", value = FALSE, width = NULL),
                   checkboxInput(inputId="nn", label="Neural Network", value = FALSE, width = NULL),
                   
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    tags$div("Loading...",id="loading")),
                   submitButton("Train Models", icon("wpexplorer"))
                 ),
                 mainPanel(
                   plotlyOutput("model_perf"),
                   tableOutput("missclass")
                 )
               )
      )
    )
  )
  
########################################
#         Defining R Shiny Server
#    Creating calls to R code and 
#    rendering it on the panels
#    defined above on the UI
########################################
  
  
  server <- function(input, output, session) {
    
    output$connection <- renderTable ({
      validate(need(input$username, ''))
      validate(need(input$pwd, ''))
      connect(input$username, input$pwd, 'CASUSER')
    })
    
    output$tbl_import <- renderTable({
      validate(need(input$csv_file, ''))
      upload_tbl(input$csv_file)
    })
    
    output$tbl_profile <- renderTable({
      validate(need(input$tbl_to_profile, ''))
      explore_tbl(input$tbl_to_profile)
    })
    
    output$tbl_list <- renderTable({
      validate(need(input$library_to_explore, ''))
      list_tables(input$library_to_explore)})
    
    output$tbl_explore <- renderPlotly({
      validate(need(input$tbl_to_explore, ''))
      analyze_miss(input$tbl_to_explore)
    })
    
    output$tbl_cardinality <- renderPlotly({
      validate(need(input$tbl_to_explore, ''))
      analyze_cardinality(input$tbl_to_explore)})
    
    output$transformed <- renderTable({
      validate(need(input$tbl_to_model, ''))
      validate(need(input$target_var, ''))
      feature_engineering(input$tbl_to_model,input$target_var, input$a, input$b, input$c, input$d, input$e, input$f, input$g)})
    
    output$model_perf <- renderPlotly({
      validate(need(input$gb, ''))
      auto_ml(input$dt,input$rf,input$gb,input$nn)
    })
    
    output$missclass <- renderTable({
      validate(need(input$gb, ''))
      get_miss()
    })
    
  }
}

shinyApp(ui, server)