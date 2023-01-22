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
library(shinydashboard)
library(openxlsx)

source("shinyio.R")
scoringTable <- loadScoringTable()
lipidClassifications <- sort(unique(scoringTable$lipidClassification))
primaryClassifications <- sort(unique(scoringTable$Primary))
secondaryClassifications <- sort(unique(scoringTable$Secondary))
fragmentClassifications <- sort(unique(scoringTable$Fragment))
evidenceClassifications <- sort(unique(scoringTable$Evidence))

lipidScoresTableDataEmpty <- tibble::tibble(
  Name = character(),
  LipidCategoryOrClass = character(),
  IonMode = character(),
  Feature = character(),
  Value = character(),
  Score = numeric()
)

# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
    
    # Application title
    titlePanel("EPoS-ML Calculation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Upload",
               fileInput("tableFile", "Choose EXCEL File",
                         multiple = FALSE,
                         accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                    "application/msexcel",
                                    ".xlsx","xls")),
               
               # Horizontal line ----
               tags$hr(),
               selectInput("tableSheet", 
                   label="Select the sheet to load",
                   choices = c()
               ),
               selectInput("tableFormat", 
                   label="Select the table format",
                   choices = c("wide","long")
               ),
               tags$hr(),
               actionButton("loadExcel", "Load table", class = "btn-primary"),
               actionButton("reset1", "Reset", class="btn-danger")
            ),
            tabPanel("Manual Input",
              textInput("lipidName", "Lipid Name"),
              selectInput("lipidCategoryOrClass", 
                  label= "Select lipid category or class",
                  choices = lipidClassifications
              ),
              selectInput("ionMode",
                  label="Select ionization mode",
                  choices = c("+","-")
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
              actionButton("addScore", "Add score", class = "btn-primary"),
              actionButton("reset2", "Reset", class="btn-danger")
            )
          ),
          tags$hr(),
          bookmarkButton()
        ),

        mainPanel(
           fluidRow(
             column(12,
                    dataTableOutput('lipidEvidenceScoreTable')
             )
           ),
           fluidRow(
             column(12,
                    dataTableOutput('totalLipidEvidenceScoreTable'),
                    downloadButton("download", "Download .xlsx", class="btn-success")
             )
           )
        )
    )
  )
}
server <- function(input, output, session) {
    values <- reactiveValues()
    values$lipidScoresTableData <- lipidScoresTableDataEmpty
    totalLipidScoresTableData <- tibble::tibble(
      Name = character(),
      LipidCategoryOrClass = character(),
      TotalScore = numeric()
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
    
    observeEvent(input$reset1, {
      values$tble <- NA
      values$lipidScoresTableData <- NA
      values$totalLipidScoresTableData <- NA
    })
    
    observeEvent(input$reset2, {
      values$tble <- NA
      values$lipidScoresTableData <- NA
      values$totalLipidScoresTableData <- NA
    })
    
    observeEvent(input$tableFile, {
      updateSelectInput(session, "tableSheet", choices = readxl::excel_sheets(input$tableFile$datapath))
    })
    
    observeEvent(input$loadExcel, {
      tryCatch({
        values$tble <- readxl::read_excel(input$tableFile$datapath, sheet = input$tableSheet, col_names = TRUE)
        if (input$tableFormat == "long") {
          values$lipidScoresTableData <- values$tble |> drop_na() |>
            left_join(scoringTable, by=c("LipidCategoryOrClass"="lipidClassification","Feature"="Evidence")) |>
            rename(Score=value)
          naScores <- c(is.na(values$lipidScoresTableData$Score))
          if (sum(naScores, na.rm = TRUE)>0) {
            # TODO: notify user!
            which(naScores==TRUE)
          }
          values$totalLipidScoresTableData <- values$lipidScoresTableData |> 
            group_by(Name,LipidCategoryOrClass) |> 
            summarise(TotalScore=sum(Score))
        } else {
          values$lipidScoresTableData <- values$tble |> 
            group_by(Name, LipidCategoryOrClass, IonMode) |>
            pivot_longer(
              4:last_col(), 
              names_to="Feature",
              values_to = "Value",
              values_ptypes = list(Value=character()),
              values_transform = as.character
            ) |> 
            drop_na() |> 
            left_join(scoringTable, by=c("LipidCategoryOrClass"="lipidClassification","Feature"="Evidence")) |>
            rename(Score=value)
          naScores <- c(is.na(values$lipidScoresTableData$Score))
          if (sum(naScores, na.rm = TRUE)>0) {
            # TODO: notify user!
            which(naScores==TRUE)
          }
          values$totalLipidScoresTableData <- values$lipidScoresTableData |> 
            group_by(Name,LipidCategoryOrClass) |> 
            summarise(TotalScore=sum(Score))
        }
      },
      error=function(cond) {
        return(NA)
      },
      warning=function(cond) {
        return(NA)
      })
    })
    
    observeEvent(input$textInput, {
      tryCatch(
        {
          tble <- rgoslin::parseLipidNames(input$textInput)
          selectedLipidCategoryOrClass <- unlist(tble$Lipid.Maps.Main.Class)
          updatedLipidClassifications <- lipidClassifications
          if(!rv$selectedLipidCategoryOrClass %in% lipidClassifications) {
            updatedLipidClassifications <- c(lipidClassifications, selectedLipidCategoryOrClass)
            selectedLipidCategoryOrClass <- selectedLipidCategoryOrClass
          }
          updateSelectInput(
            session, "lipidCategoryOrClass",
            choices = updatedLipidClassifications,
            selected = selectedLipidCategoryOrClass
          )
        },
        error=function(cond) {
          return(NA)
        },
        warning=function(cond) {
          return(NA)
        }
      )
    })
    
    observeEvent(input$addScore, {
      values$lipidScoresTableData <- values$lipidScoresTableData |>
      add_row(
        Name=input$lipidName, 
        LipidCategoryOrClass=input$lipidCategoryOrClass,
        IonMode=input$ionMode,
        Feature=input$evidenceClassification,
        Score=as.numeric(input$evidenceScore)
      )
      
      values$totalLipidScoresTableData <- values$lipidScoresTableData |>
        group_by(Name, LipidCategoryOrClass) |>
        summarise(TotalScore=sum(Score))
    })
  
    output$lipidEvidenceScoreTable <- renderDataTable(
      values$lipidScoresTableData,
      options = list(
        select = 'single'
      )
    )
    
    output$totalLipidEvidenceScoreTable <- renderDataTable(
      values$totalLipidScoresTableData,
      options = list(
        select = 'single'
      )
    )
    
    output$download <- downloadHandler(
      filename = function() {
        paste0("EPOS-Mol-Total-Scores", ".xlsx")
      },
      content = function(file) {
        write.xlsx(values$totalLipidScoresTableData, file)
      }
    )
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
      state$values$totalLipidScoresTableData <- values$totalLipidScoresTableData
      state$values$lipidScoresTableData <- values$lipidScoresTableData
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
      values$totalLipidScoresTableData <- state$values$totalLipidScoresTableData
      values$lipidScoresTableData <- state$values$lipidScoresTableData
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")
