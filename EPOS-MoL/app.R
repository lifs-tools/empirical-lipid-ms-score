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
library(openxlsx)

source("shinyio.R")
scoringTable <- loadScoringTable()
lipidClassifications <-
  sort(unique(scoringTable$LipidCategoryOrClass))
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

requiredColumnsCommon <- c("Name", "LipidCategoryOrClass", "IonMode")
requiredColumnsLong <- c(requiredColumnsCommon, "Feature", "Value")
requiredColumnsWide <-
  c(requiredColumnsCommon, unique(sort(scoringTable$Evidence)))

# Define UI for application that draws a histogram
ui <- function(request) {
  

  fluidPage(# Application title
    titlePanel("EPoS-ML Calculation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        bookmarkButton(),
        tags$hr(),
        tabsetPanel(
          id = "sidebarPanels",
          tabPanel(
            "Upload",
            fileInput(
              "tableFile",
              "Choose EXCEL File",
              multiple = FALSE,
              accept = c(
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                "application/msexcel",
                ".xlsx",
                "xls"
              )
            ),

            # Horizontal line ----
            tags$hr(),
            selectInput("tableSheet",
                        label = "Select the sheet to load",
                        choices = c()),
            selectInput(
              "tableFormat",
              label = "Select the table format",
              choices = c("wide", "long")
            ),
            tags$hr(),
            actionButton("loadExcel", "Load table", class = "btn-primary"),
            actionButton("reset1", "Reset", class = "btn-danger")
          ),
          tabPanel(
            "Manual Input",
            textInput("lipidName", "Lipid Name"),
            selectInput(
              "lipidCategoryOrClass",
              label = "Select lipid category or class",
              choices = lipidClassifications
            ),
            selectInput("ionMode",
                        label = "Select ionization mode",
                        choices = c("+", "-")),
            selectInput(
              "primaryClassification",
              label = "Primary Classification",
              choices = primaryClassifications
            ),
            selectInput(
              "secondaryClassification",
              label = "Secondary Classification",
              choices = secondaryClassifications
            ),
            selectInput(
              "evidenceClassification",
              label = "Evidence Stream",
              choices = evidenceClassifications
            ),
            textInput("evidenceScore",
                      label = "Lipid Score",
                      value = NA),
            actionButton("addScore", "Add score", class = "btn-primary"),
            actionButton("reset2", "Reset", class = "btn-danger")
          )
        ),
        tags$hr(),
        downloadButton(
          "sampleFile",
          "Download Example",
          class = "btn-info",
          icon = icon("download")
        )
      ),

      mainPanel(
         tabsetPanel(
           id = "mainPanels",
           tabPanel("Getting Started",
                    uiOutput("gettingStarted")
           ),
           tabPanel("Total Scores",
                    fluidRow(
                      column(
                        12,
                        dataTableOutput('totalLipidEvidenceScoreTable'),
                        actionButton(
                          "checkNames",
                          "Check Lipid Names",
                          class = "btn-success",
                          icon = icon("check")
                        ),
                        downloadButton("download", "Download Total Score Table", class =
                                         "btn-success")
                      )
                    )),
           tabPanel("Individual Scores",
                    fluidRow(column(
                      12,
                      dataTableOutput('lipidEvidenceScoreTable')
                    ))),
           tabPanel("Original Table",
                    fluidRow(column(
                      12,
                      dataTableOutput('originalTable')
                    ))),
           tabPanel("Reference Score Table",
                    fluidRow(column(
                      12,
                      dataTableOutput('referenceScoreTable')
                    )))
         ),
      )
    ))
}
server <- function(input, output, session) {
  values <- reactiveValues()
  values$lipidScoresTableData <- lipidScoresTableDataEmpty
  totalLipidScoresTableData <- tibble::tibble(
    Name = character(),
    LipidCategoryOrClass = character(),
    TotalScore = numeric()
  )
  output$lipidCategoryOrClass <-
    renderPrint({
      input$lipidCategoryOrClass
    })
  
  primaryClassificationChoices <- reactive({
    unique(scoringTable %>%
             pull(Primary))
  })
  
  secondaryClassificationChoices <- reactive({
    unique(
      scoringTable %>%
        filter(
          LipidCategoryOrClass == input$lipidCategoryOrClass &
            Primary == input$primaryClassification
        ) %>%
        pull(Secondary)
    )
  })
  
  evidenceClassificationChoices <- reactive({
    unique(
      scoringTable %>%
        filter(
          LipidCategoryOrClass == input$lipidCategoryOrClass &
            Primary == input$primaryClassification &
            Secondary == input$secondaryClassification
        ) %>%
        pull(Evidence)
    )
  })
  
  evidenceStreamScore <- reactive({
    scoringTable %>%
      filter(
        LipidCategoryOrClass == input$lipidCategoryOrClass &
          Primary == input$primaryClassification &
          Secondary == input$secondaryClassification &
          Evidence == input$evidenceClassification
      ) %>%
      pull(value)
  })
  observe({
    updateSelectInput(session, "primaryClassification",
                      choices = primaryClassificationChoices())
  })
  observe({
    updateSelectInput(session, "secondaryClassification",
                      choices = secondaryClassificationChoices())
  })
  observe({
    updateSelectInput(session, "evidenceClassification",
                      choices = evidenceClassificationChoices())
  })
  observe({
    updateTextInput(session, "evidenceScore",
                    value = evidenceStreamScore())
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
    updateSelectInput(session,
                      "tableSheet",
                      choices = readxl::excel_sheets(input$tableFile$datapath))
  })
  
  observeEvent(input$loadExcel, {
    tryCatch({
      values$tble <- NA
      values$lipidScoresTableData <- NA
      values$totalLipidScoresTableData <- NA
      values$tble <-
        readxl::read_excel(
          input$tableFile$datapath,
          sheet = input$tableSheet,
          col_names = TRUE
        )
      if (input$tableFormat == "long") {
        values$lipidScoresTableData <- values$tble |> drop_na() |>
          left_join(
            scoringTable,
            by = c(
              "LipidCategoryOrClass" = "LipidCategoryOrClass",
              "Feature" = "Evidence"
            )
          ) |>
          rename(Score = value) |> 
          distinct(Name, LipidCategoryOrClass, IonMode, Feature, .keep_all = TRUE) |>
          distinct(.keep_all = TRUE) |>
          arrange(.by_group = TRUE)
        naScores <- c(is.na(values$lipidScoresTableData$Score))
        # validate(
        #   need(try(sum(naScores, na.rm = TRUE)>0), which(naScores==TRUE))
        # )
        values$totalLipidScoresTableData <-
          calculateTotalLipidScoresTableData(values$lipidScoresTableData)
      } else {
        # validate(
        #   need(!all(requiredColumnsWide %in% names(values$tble)), paste0("Input table must contain column names (case-sensitive): ", paste(requiredColumnsWide, ", "))),
        # )
        values$lipidScoresTableData <- values$tble |>
          group_by(Name, LipidCategoryOrClass, IonMode) |>
          pivot_longer(
            4:last_col(),
            names_to = "Feature",
            values_to = "Value",
            values_ptypes = list(Value = character()),
            values_transform = as.character
          ) |>
          drop_na() |>
          left_join(
            scoringTable,
            by = c(
              "LipidCategoryOrClass" = "LipidCategoryOrClass",
              "Feature" = "Evidence"
            )
          ) |>
          rename(Score = value) |> 
          group_by(Name, LipidCategoryOrClass, IonMode) |> 
          distinct(Name, LipidCategoryOrClass, IonMode, Feature, .keep_all = TRUE) |>
          arrange(.by_group = TRUE)
        naScores <- c(is.na(values$lipidScoresTableData$Score))
        # validate(
        # need(sum(naScores, na.rm = TRUE)>0, which(naScores==TRUE))
        # )
        values$totalLipidScoresTableData <-
          calculateTotalLipidScoresTableData(values$lipidScoresTableData)
      }
      updateTabsetPanel(session, "mainPanels",
                        selected = "Total Scores"
      )
    },
    error = function(cond) {
      return(NA)
    },
    warning = function(cond) {
      return(NA)
    })
  })
  
  observeEvent(input$addScore, {
    values$lipidScoresTableData <- values$lipidScoresTableData |>
      ungroup() |>
      add_row(
        Name = input$lipidName,
        LipidCategoryOrClass = input$lipidCategoryOrClass,
        IonMode = input$ionMode,
        Feature = input$evidenceClassification,
        Primary = input$primaryClassification,
        Secondary = input$secondaryClassification,
        # Fragment=input$,
        Score = as.numeric(input$evidenceScore)
      ) |>
      group_by(Name, LipidCategoryOrClass, IonMode) |> 
      distinct(Name, LipidCategoryOrClass, IonMode, Feature, .keep_all = TRUE) |>
      arrange(.by_group = TRUE)
    
    values$totalLipidScoresTableData <-
      calculateTotalLipidScoresTableData(values$lipidScoresTableData)
  })
  
  observeEvent(input$checkNames, {
    req(values$totalLipidScoresTableData)
    tryCatch({
      goslinResm <-
        rgoslin::parseLipidNames(values$totalLipidScoresTableData$Name)
      messages <- goslinResm$Message
      validNames <- goslinResm$Message %in% c("NA")
      messages[validNames] <- ""
      messages[!validNames] <- "Unrecognised shorthand name"
      values$totalLipidScoresTableData$Message <- messages
      values$totalLipidScoresTableData$Lipid.Maps.Category <-
        goslinResm$Lipid.Maps.Category
      values$totalLipidScoresTableData$Lipid.Maps.Main.Class <-
        goslinResm$Lipid.Maps.Main.Class
      values$totalLipidScoresTableData$Normalized.Name <-
        goslinResm$Normalized.Name
    }, error = function(cond) {
      return(NA)
    },
    warning = function(cond) {
      return(NA)
    })
  })
  
  output$lipidEvidenceScoreTable <- renderDataTable(values$lipidScoresTableData,
                                                    options = list(select = 'single',
                                                                   scrollX = TRUE))
  
  output$totalLipidEvidenceScoreTable <- renderDataTable(values$totalLipidScoresTableData,
                                                         options = list(select = 'single',
                                                                        scrollX = TRUE))
  
  output$originalTable <- renderDataTable(values$tble,
                                          options = list(select = 'single',
                                                         scrollX = TRUE))
  
  output$referenceScoreTable <- renderDataTable(scoringTable,
                                                options = list(select = 'single',
                                                               scrollX = TRUE))
  
  output$sampleFile <- downloadHandler(
    filename = function() {
      paste0("EPOS-Mol-Examples", ".xlsx")
    },
    content = function(file) {
      file.copy("../data/Table S2.xlsx", file)
    },
    contentType = "application/msexcel"
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("EPOS-Mol-Total-Scores", ".xlsx")
    },
    content = function(file) {
      write.xlsx(values$totalLipidScoresTableData, file)
    }
  )
  
  output$gettingStarted <- renderUI({
    HTML(markdown::markdownToHTML('gettingStarted.md'))
  })
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$tble <- values$tble
    state$values$totalLipidScoresTableData <-
      values$totalLipidScoresTableData
    state$values$lipidScoresTableData <- values$lipidScoresTableData
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    values$tble <- state$values$tble
    values$totalLipidScoresTableData <-
      state$values$totalLipidScoresTableData
    values$lipidScoresTableData <- state$values$lipidScoresTableData
  })
}

# Run the application
shinyApp(ui = ui,
         server = server,
         enableBookmarking = "server")
