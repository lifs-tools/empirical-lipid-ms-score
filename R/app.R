#
# Empirical Point Score Model for MS-based Lipidomics
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio.
#

library(shiny)
library(shinyjs)
library(readxl)
library(rgoslin)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(htmltools)

source("eposmol.R")

appInfo <- list(
  "application.name"="EPoS-MoL",
  "application.version"="0.0.1",
  "application.date"=date(),
  "application.authors"="Nils Hoffmann",
  "application.license"="MIT",
  "application.url"="https://github.com/lifs-tools/eposmol",
  "application.imprintAndPrivacyPolicy"="https://lifs-tools.org/imprint-privacy-policy.html",
  "application.issues"="https://github.com/lifs-tools/eposmol/issues"
)

scoringTable <- loadScoringTable()
lipidCategoryAndClassMapTable <- loadCategoryAndClassMapTable()
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

totalLipidScoresTableDataEmpty <- tibble::tibble(
  Name = character(),
  LipidCategoryOrClass = character(),
  TotalScore = numeric()
)

lipidTableDataEmpty <- tibble::tibble(
  Name = character(),
  LipidCategoryOrClass = character(),
  IonMode = character(),
  Feature = character(),
  Value = character()
)

requiredColumnsCommon <- c("Name", "LipidCategoryOrClass", "IonMode")
requiredColumnsLong <- c(requiredColumnsCommon, "Feature", "Value")
requiredColumnsWide <-
  c(requiredColumnsCommon, unique(sort(scoringTable$Evidence)))

# User Interface
ui <- function(request) {
  fluidPage(
    includeCSS("custom.css"),
    useShinyjs(),
    titlePanel("EPoS-ML Calculation"),
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
            tags$hr(),
            selectInput("tableSheet",
              label = "Select the sheet to load",
              choices = c()
            ),
            selectInput(
              "tableFormat",
              label = "Select the table format",
              choices = c("wide", "long")
            ),
            tags$hr(),
            shinyjs::disabled(
              actionButton("loadExcel", "Load table", class = "btn-primary")
            ),
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
              choices = c("+", "-")
            ),
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
            textInput("manualValue",
                      label = "Value (optional)",
                      value = ""
            ),
            shinyjs::disabled(
              textInput("evidenceScore",
                label = "Lipid Score",
                value = NA
              )
            ),
            shinyjs::disabled(
              actionButton("addScore", "Add score", class = "btn-primary")
            ),
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
          tabPanel(
            "Getting Started",
            uiOutput("gettingStarted")
          ),
          tabPanel(
            "Total Scores",
            fluidRow(
              column(
                12,
                br(),
                dataTableOutput("totalLipidEvidenceScoreTable"),
                disabled(actionButton(
                  "checkNames",
                  "Check Lipid Names",
                  class = "btn-success",
                  icon = icon("check")
                )),
                disabled(downloadButton("download", "Download Score Tables",
                  class =
                    "btn-success"
                ))
              )
            )
          ),
          tabPanel(
            "Individual Scores",
            fluidRow(column(
              12,
              br(),
              dataTableOutput("lipidEvidenceScoreTable")
            ))
          ),
          tabPanel(
            "Original Table",
            fluidRow(column(
              12,
              br(),
              dataTableOutput("originalTable")
            ))
          ),
          tabPanel(
            "Reference Score Table",
            fluidRow(column(
              12,
              br(),
              dataTableOutput("referenceScoreTable")
            ))
          ),
          tabPanel(
            "Lipid Category & Class Map",
            fluidRow(column(
              12,
              br(),
              dataTableOutput("lipidCategoryAndClassMapTable")
            ))
          ),
          tabPanel(
            "About EPoS-MoL",
            fluidRow(
              column(
                width = 12,
                h2("Application Information"),
                column(
                  width = 12,
                  tags$label("Name:"),
                  textOutput(
                    "applicationName", inline = TRUE
                  ),
                  tags$br(),
                  tags$label("Version:"),
                  textOutput(
                    "applicationVersion", inline = TRUE
                  ),
                  tags$br(),
                  tags$label("Build Date:"),
                  textOutput(
                    "applicationDate", inline = TRUE
                  ),
                  tags$br(),
                  tags$label("Authors:"),
                  textOutput(
                    "applicationAuthors", inline = TRUE
                  ),
                  tags$br(),
                  tags$label("License:"),
                  textOutput(
                    "applicationLicense", inline = TRUE
                  ),
                  tags$br(),
                  tags$label("Homepage:"),
                  uiOutput(
                    "applicationHomepage", inline = TRUE
                  ),
                  tags$br(),
                  tags$label("Imprint & Privacy Policy:"),
                  uiOutput(
                    "imprintAndPrivacyPolicy", inline = TRUE
                  ),
                  tags$br(),
                  uiOutput(
                    "applicationIssues"
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                h2("Libraries used by EPoS-MoL"),
                # collapsible = TRUE,
                # collapsed = TRUE,
                dataTableOutput("appLibraries")
              )
            )
          )
        )
      )
    )
  )
}
server <- function(input, output, session) {
  values <- reactiveValues()
  values$lipidScoresTableData <- lipidScoresTableDataEmpty
  values$tble <- lipidTableDataEmpty
  values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
  output$lipidCategoryOrClass <-
    renderPrint({
      input$lipidCategoryOrClass
    })

  primaryClassificationChoices <- reactive({
    unique(scoringTable |>
      dplyr::pull(Primary))
  })

  secondaryClassificationChoices <- reactive({
    unique(
      scoringTable |>
        dplyr::filter(
          LipidCategoryOrClass == input$lipidCategoryOrClass &
            Primary == input$primaryClassification
        ) |>
        dplyr::pull(Secondary)
    )
  })

  evidenceClassificationChoices <- reactive({
    unique(
      scoringTable |>
        dplyr::filter(
          LipidCategoryOrClass == input$lipidCategoryOrClass &
            Primary == input$primaryClassification &
            Secondary == input$secondaryClassification
        ) |>
        dplyr::pull(Evidence)
    )
  })

  evidenceStreamScore <- reactive({
    scoringTable |>
      dplyr::filter(
        LipidCategoryOrClass == input$lipidCategoryOrClass &
          Primary == input$primaryClassification &
          Secondary == input$secondaryClassification &
          Evidence == input$evidenceClassification
      ) |>
      dplyr::pull(value)
  })
  observe({
    updateSelectInput(session, "primaryClassification",
      choices = primaryClassificationChoices()
    )
  })
  observe({
    updateSelectInput(session, "secondaryClassification",
      choices = secondaryClassificationChoices()
    )
  })
  observe({
    updateSelectInput(session, "evidenceClassification",
      choices = evidenceClassificationChoices()
    )
  })
  observe({
    updateTextInput(session, "evidenceScore",
      value = evidenceStreamScore()
    )
  })

  observeEvent(input$reset1, {
    shinyjs::reset("tableFile")
    disable("checkNames")
    disable("download")
    disable("loadExcel")
    values$tble <- lipidTableDataEmpty
    values$lipidScoresTableData <- lipidScoresTableDataEmpty
    values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
  })

  observeEvent(input$reset2, {
    shinyjs::reset("tableFile")
    disable("checkNames")
    disable("download")
    disable("loadExcel")
    values$tble <- lipidTableDataEmpty
    values$lipidScoresTableData <- lipidScoresTableDataEmpty
    values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
  })

  observeEvent(input$lipidName, {
    if (stringr::str_length(input$lipidName)>0) {
      shinyjs::enable("addScore")
    } else {
      shinyjs::disable("addScore")
    }
  })

  observeEvent(input$tableFile, {
    enable("loadExcel")
    updateSelectInput(session,
      "tableSheet",
      choices = readxl::excel_sheets(input$tableFile$datapath)
    )
  })

  observeEvent(input$loadExcel, {
    tryCatch(
      {
        values$lipidScoresTableData <- NA
        values$totalLipidScoresTableData <- NA
        tble <-
          readxl::read_excel(
            input$tableFile$datapath,
            sheet = input$tableSheet,
            col_names = TRUE
          )
        lipidScoresTableData <- lipidScoresTableDataEmpty
        if (input$tableFormat == "long") {
          lipidScoresTableData <- readLongTable(tble, scoringTable)
          # validate(
          #   need(try(sum(naScores, na.rm = TRUE)>0), which(naScores==TRUE))
          # )
        } else {
          # validate(
          #   need(!all(requiredColumnsWide %in% names(values$tble)), paste0("Input table must contain column names (case-sensitive): ", paste(requiredColumnsWide, ", "))),
          # )
          lipidScoresTableData <- readWideTable(tble, scoringTable)
        }
        values$tble <- tble
        values$lipidScoresTableData <- lipidScoresTableData
        values$totalLipidScoresTableData <-
          calculateTotalLipidScoresTableData(lipidScoresTableData)
        updateTabsetPanel(session, "mainPanels",
          selected = "Total Scores"
        )
        shinyjs::enable("checkNames")
        shinyjs::enable("download")
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  })

  observeEvent(input$addScore, {
    if (is.null(values$lipidScoresTableData)) {
      values$lipidScoresTableData <- lipidScoresTableDataEmpty
    }
    if (is.null(values$manualLipidScoresTableData)) {
      values$manualLipidScoresTableData <- values$lipidScoresTableData
    }
    manualLipidScoresTableData <- addRowManually(
      values$manualLipidScoresTableData,
      lipidName=input$lipidName,
      lipidCategoryOrClass=input$lipidCategoryOrClass,
      ionMode=input$ionMode,
      evidenceClassification=input$evidenceClassification,
      evidenceScore=input$evidenceScore
    )
    values$manualLipidScoresTableData <- manualLipidScoresTableData
    values$lipidScoresTableData <- readManualTable(manualLipidScoresTableData, scoringTable)
    values$totalLipidScoresTableData <-
      calculateTotalLipidScoresTableData(values$lipidScoresTableData)
    updateTabsetPanel(session, "mainPanels",
                      selected = "Individual Scores"
    )
    shinyjs::enable("checkNames")
    shinyjs::enable("download")
  })

  observeEvent(input$checkNames, {
    req(values$totalLipidScoresTableData)
    values$totalLipidScoresTableData <- checkNames(values$totalLipidScoresTableData)
  })

  output$lipidEvidenceScoreTable <- renderDataTable(values$lipidScoresTableData,
    options = list(
      select = "single",
      scrollX = TRUE
    )
  )

  output$totalLipidEvidenceScoreTable <- renderDataTable(values$totalLipidScoresTableData,
    options = list(
      select = "single",
      scrollX = TRUE
    )
  )

  output$originalTable <- renderDataTable(values$tble,
    options = list(
      select = "single",
      scrollX = TRUE
    )
  )

  output$referenceScoreTable <- renderDataTable(scoringTable,
    options = list(
      select = "single",
      scrollX = TRUE
    )
  )

  output$lipidCategoryAndClassMapTable <- renderDataTable(lipidCategoryAndClassMapTable,
    options = list(
     select = "single",
     scrollX = TRUE
    )
  )

  output$sampleFile <- downloadHandler(
    filename = function() {
      paste0("EPOS-Mol-Examples", ".xlsx")
    },
    content = function(file) {
      file.copy(system.file("extdata", "Table S2.xlsx", package = "eposmol"), file)
    },
    contentType = "application/msexcel"
  )

  output$download <- downloadHandler(
    filename = function() {
      paste0("EPOS-Mol-Scores", ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(
        list(
          "Total Scores"=values$totalLipidScoresTableData,
          "Individual Scores"=values$lipidScoresTableData
        ),
        file
      )
    }
  )

  output$gettingStarted <- renderUI({
    HTML(htmltools::includeMarkdown("gettingStarted.md"))
  })

  output$applicationName <- shiny::renderText({appInfo$application.name})
  output$applicationVersion <- shiny::renderText({appInfo$application.version})
  output$applicationDate <- shiny::renderText({appInfo$application.date})
  output$applicationAuthors <- shiny::renderText({appInfo$application.authors})
  output$applicationLicense <- shiny::renderText({appInfo$application.license})
  output$applicationHomepage <- shiny::renderUI({tags$a(href=appInfo$application.url, appInfo$application.url)})
  output$imprintAndPrivacyPolicy <- shiny::renderUI({tags$a(href=appInfo$application.imprintAndPrivacyPolicy, appInfo$application.imprintAndPrivacyPolicy)})
  output$applicationIssues <- shiny::renderUI({tags$a(href=appInfo$application.issues, "Report an issue", class="btn btn-danger")})

  loadedLibraries <- reactive({
    assemblePackageDescriptions((.packages()))
  })

  output$appLibraries <- renderDataTable(
    loadedLibraries(),
    options = list(
      dom = 'Bfrtlip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
      pageLength = 25
    ),
    escape = FALSE
  )

  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$tble <- values$tble
    state$values$totalLipidScoresTableData <-
      values$totalLipidScoresTableData
    state$values$lipidScoresTableData <- values$lipidScoresTableData
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    values <- reactiveValues()
    values$tble <- state$values$tble
    values$totalLipidScoresTableData <-
      state$values$totalLipidScoresTableData
    values$lipidScoresTableData <- state$values$lipidScoresTableData
  })
}

devOptions <- options(
  shiny.reactlog = TRUE,
  shiny.error = browser,
  shiny.autoload.r = TRUE
)
prodOptions <- options(
  shiny.autoload.r = TRUE
)

if (file.exists(".dev")) {
  shinyOptions <- devOptions
  cat("Using development options!")
} else {
  shinyOptions <- prodOptions
  cat("Using production options!")
}

# Run the application
shinyApp(
  ui = ui,
  server = server,
  enableBookmarking = "server",
  options = shinyOptions
)
