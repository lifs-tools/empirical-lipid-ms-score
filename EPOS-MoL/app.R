#
# Empirical Point Score Model for MS-based Lipidomics
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(rgoslin)

source("shinyio.R")
scoringTable <- loadScoringTable()
lipidClassifications <- sort(unique(scoringTable$lipidClassification))
primaryClassifications <- sort(unique(scoringTable$Primary))
secondaryClassifications <- sort(unique(scoringTable$Secondary))
fragmentClassifications <- sort(unique(scoringTable$Fragment))
evidenceClassifications <- sort(unique(scoringTable$Evidence))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("lipidName", "Lipid Name"),
            selectInput("lipidCategoryOrClass", 
                label= "Select lipid category or class",
                choices = lipidClassifications
            ),
            selectInput("primaryClassification",
                label="Primary Classification",
                choices = primaryClassifications
            ),
            selectInput("secondaryClassification",
                label="Secondary Classification",
                choices = secondaryClassifications
            ),
            selectInput("evidenceClassification",
                label="Evidence Stream",
                choices = evidenceClassifications
            ),
            textInput("evidenceScore",
                label="Lipid Score",
                value=NA
            ),
            actionButton("addScore", "Add score")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("lipidCategoryOrClass"),
           fluidRow(
             column(12,
                    dataTableOutput('lipidEvidenceScoreTable')
             )
           )
           # plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # TODO add lists to attach individual score contributions by name of lipid
    values <- reactiveValues()
    values$lipidScoresTableData <- tibble::tibble(
      Lipid = character(),
      LipidCategoryOrClass = character(),
      Primary = character(),
      Secondary = character(),
      EvidenceClassification = character(),
      Score = numeric()
    )
    output$lipidCategoryOrClass <- renderPrint({ input$lipidCategoryOrClass })
  
    primaryClassificationChoices <- reactive({
      unique(
        scoringTable %>% 
          pull(Primary))
    })
    
    secondaryClassificationChoices <- reactive({
      unique(
        scoringTable %>% 
          filter(
            lipidClassification==input$lipidCategoryOrClass &
            Primary==input$primaryClassification  
          ) %>% 
          pull(Secondary))
    })
    
    evidenceClassificationChoices <- reactive({
      unique(
        scoringTable %>% 
          filter(
            lipidClassification==input$lipidCategoryOrClass &
              Primary==input$primaryClassification &
              Secondary==input$secondaryClassification
          ) %>% 
          pull(Evidence))
    })
    
    evidenceStreamScore <- reactive({
      scoringTable %>% 
        filter(
          lipidClassification==input$lipidCategoryOrClass &
            Primary==input$primaryClassification &
            Secondary==input$secondaryClassification &
            Evidence==input$evidenceClassification
        ) %>% 
        pull(value)
    })
    observe({
      updateSelectInput(
        session, "primaryClassification",
        choices = primaryClassificationChoices()
      )
    })
    observe({
      updateSelectInput(
        session, "secondaryClassification",
        choices = secondaryClassificationChoices()
      )
    })
    observe({
      updateSelectInput(
        session, "evidenceClassification",
        choices = evidenceClassificationChoices()
      )
    })
    observe({
      updateTextInput(
        session, "evidenceScore",
        value = evidenceStreamScore()
      )
    })
    
    observeEvent(input$addScore, {
      values$lipidScoresTableData <- values$lipidScoresTableData %>% 
      add_row(
        Lipid=input$lipidName, 
        LipidCategoryOrClass=input$lipidCategoryOrClass, 
        Primary=input$primaryClassification,
        Secondary=input$secondaryClassification,
        EvidenceClassification=input$evidenceClassification,
        Score=as.numeric(input$evidenceScore)
      )
      
    })
  
    output$lipidEvidenceScoreTable <- renderDataTable(
      values$lipidScoresTableData,
      options = list(
        select = 'single'
      )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
