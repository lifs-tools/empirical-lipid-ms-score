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
library(rintrojs)
library(DT)
library(here)
library(tibble)
#library(knitr)

#' Start the EPoS-MoL Shiny Webapplication.
#' @param test.mode enable or disable test.mode
#' @returns A ShinyApp object.
#' @export
run_eposmol_app <- function(test.mode=FALSE) {

  appInfo <- list(
    "application.name"="EPoS-MoL",
    "application.version"="1.0.0",
    "application.date"=date(),
    "application.authors"="Nils Hoffmann, Harald Koefeler",
    "application.license"="MIT",
    "application.url"="https://github.com/lifs-tools/empirical-lipid-ms-score",
    "application.imprintAndPrivacyPolicy"="https://lifs-tools.org/imprint-privacy-policy.html",
    "application.issues"="https://github.com/lifs-tools/empirical-lipid-ms-score/issues"
  )

  cvMapTable <- eposmol::loadCvMapTable(path = system.file("extdata", "shorthand_cv_map.xlsx", package="eposmol"))
  scoringTable <- eposmol::loadScoringTable(path = system.file("extdata", "Table_1.xlsx", package="eposmol"))
  lipidCategoryAndClassMapTable <- eposmol::loadCategoryAndClassMapTable(path = system.file("extdata", "class_map.xlsx", package="eposmol"))
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
    shiny::fluidPage(
      shiny::includeCSS(system.file("app/www/custom.css")),
      shinyjs::useShinyjs(),
      shiny::titlePanel("EPoS-MoL Calculation"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # actionButton("help", icon=shiny::icon("question"), class="btn-info", label="Start Tour"),
          shiny::tags$br(),
          shiny::tabsetPanel(
            id = "sidebarPanels",
            shiny::tabPanel(
              "Upload",
              shiny::tags$br(),
              rintrojs::introBox(
                shiny::fileInput(
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
                data.step = 1,
                data.intro = "Upload your lipid annotation data in EXCEL format here."
              ),
              shiny::tags$hr(),
              rintrojs::introBox(
                shiny::selectInput("tableSheet",
                  label = "Select the sheet to load",
                  choices = c()
                ),
                data.step = 2,
                data.intro = "Select the sheet to load your data from."
              ),
              rintrojs::introBox(
                shiny::selectInput(
                  "tableFormat",
                  label = "Select the table format",
                  choices = c("wide", "long")
                ),
                data.step = 3,
                data.intro = "Choose either the wide or long table format. You can download an example file demonstrating both formats further down."
              ),
              shiny::tags$hr(),
              shiny::fluidRow(
                shiny::column(12,
                  align="left",
                  rintrojs::introBox(
                    style="display:inline-block;",
                    shinyjs::disabled(
                      shiny::actionButton("loadExcel", "Load table", class = "btn-primary")
                    ),
                    data.step = 4,
                    data.intro = "This button will be enabled as soon as you have uploaded a table and have chosen the sheet and table format. Click on it to load the data."
                  ),
                  rintrojs::introBox(
                    style="display:inline-block;",
                    shiny::actionButton("reset1", "Reset", class = "btn-danger"),
                    data.step = 5,
                    data.intro = "Click on this button to reset any inputs. This will also remove any intermediate tables generated from your data, so please be careful!"
                  ),
                  shiny::actionButton("loadExample", "Load example data", class = "btn-default", style="float:right"),
                )
              ),
            ),
            shiny::tabPanel(
              "Manual Input",
              shiny::tags$br(),
              shiny::textInput("lipidName", "Lipid Name"),
              shiny::selectInput(
                "lipidCategoryOrClass",
                label = "Select lipid category or class",
                choices = lipidClassifications
              ),
              shiny::selectInput("ionMode",
                label = "Select ionization mode",
                choices = c("+", "-")
              ),
              shiny::selectInput(
                "primaryClassification",
                label = "Primary Classification",
                choices = primaryClassifications
              ),
              shiny::selectInput(
                "secondaryClassification",
                label = "Secondary Classification",
                choices = secondaryClassifications
              ),
              shiny::selectInput(
                "evidenceClassification",
                label = "Evidence Stream",
                choices = evidenceClassifications
              ),
              shiny::textInput("manualValue",
                        label = "Value (optional)",
                        value = ""
              ),
              shinyjs::disabled(
                shiny::textInput("evidenceScore",
                  label = "Lipid Score",
                  value = NA
                )
              ),
              shinyjs::disabled(
                shiny::actionButton("addScore", "Add score", class = "btn-primary")
              ),
              shiny::actionButton("reset2", "Reset", class = "btn-danger")
            )
          ),
          shiny::tags$hr(),
          rintrojs::introBox(
            style="display:inline-block;",
            shiny::downloadButton(
              "sampleFile",
              "Download Example",
              class = "btn-info",
              style="display:inline-block;",
              icon = shiny::icon("download")
            ),
            data.step = 6,
            data.intro = "Click here to download an example lipid annotation dataset. The file contains to sheets for the wide and long format."
          ),
          # bookmarkButton(style="display:inline-block;float: right"),
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "mainPanels",
            shiny::tabPanel(
              "Getting Started",
              rintrojs::introBox(
                shiny::uiOutput("gettingStarted"),
                data.step = 7,
                data.intro = "This tab provides an overview of the EPoS-MOL application workflow and the data formats used."
              )
            ),
            shiny::tabPanel(
              "Total Scores",
              rintrojs::introBox(
                shiny::fluidRow(
                  shiny::column(
                    12,
                    shiny::tags$br(),
                    DT::dataTableOutput("totalLipidEvidenceScoreTable"),
                    rintrojs::introBox(
                      style="display: inline-block;",
                      shinyjs::disabled(shiny::actionButton(
                        "checkNames",
                        "Parse & Convert Lipid Names",
                        class = "btn-success",
                        icon = shiny::icon("check")
                      )),
                      data.step = 9,
                      data.intro = "Click here to check and automatically normalize supported lipid names into the updated shorthand nomenclature."
                    ),
                    rintrojs::introBox(
                      style="display: inline-block;",
                      shinyjs::disabled(shiny::downloadButton("download", "Download Score Tables",
                        class =
                          "btn-success"
                      )),
                      data.step = 10,
                      data.intro = "Click here to download the final total scores table and the individual scores table within a single excel file."
                    )
                  )
                ),
                data.step = 8,
                data.intro = "This tab provides the total scores for the user provided lipid evidence."
              )
            ),
            shiny::tabPanel(
              "Individual Scores",
              rintrojs::introBox(
                shiny::fluidRow(shiny::column(
                  12,
                  shiny::tags$br(),
                  DT::dataTableOutput("lipidEvidenceScoreTable")
                )),
                data.step = 11,
                data.intro = "This tab provides the table of individual scores assigned to the user provided lipid evidence."
              )
            ),
            shiny::tabPanel(
              "Original Table",
              rintrojs::introBox(
                shiny::fluidRow(shiny::column(
                  12,
                  shiny::tags$br(),
                  DT::dataTableOutput("originalTable")
                )),
                data.step = 12,
                data.intro = "This tab shows the original table as provided or manually added to by the user."
              )
            ),
            shiny::tabPanel(
              "Reference Score Table",
              rintrojs::introBox(
                shiny::fluidRow(shiny::column(
                  12,
                  shiny::tags$br(),
                  DT::dataTableOutput("referenceScoreTable")
                )),
                data.step = 13,
                data.intro = "This tab provides the reference score table of EPoS-MOL for the different lipid classes and evidence levels."
              )
            ),
            shiny::tabPanel(
              "Lipid Category & Class Map",
              rintrojs::introBox(
                shiny::fluidRow(shiny::column(
                  12,
                  shiny::tags$br(),
                  DT::dataTableOutput("lipidCategoryAndClassMapTable")
                )),
                data.step = 14,
                data.intro = "This tab provides a mapping from LIPID MAPS category and classes to the (super-)classes used by EPoS-MOL."
              )
            ),
            shiny::tabPanel(
              "Using EPoS-MoL",
              rintrojs::introBox(
                shiny::uiOutput("usingEposmol"),
                data.step = 15,
                data.intro = "This tab provides an overview of how to use the EPoS-MOL application and the data formats used."
              )
            ),
            shiny::tabPanel(
              "About EPoS-MoL",
              rintrojs::introBox(
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    shiny::tags$h2("Application Information"),
                    shiny::column(
                      width = 12,
                      shiny::tags$label("Name:"),
                      shiny::textOutput(
                        "applicationName", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::tags$label("Version:"),
                      shiny::textOutput(
                        "applicationVersion", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::tags$label("Build Date:"),
                      shiny::textOutput(
                        "applicationDate", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::tags$label("Authors:"),
                      shiny::textOutput(
                        "applicationAuthors", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::tags$label("License:"),
                      shiny::textOutput(
                        "applicationLicense", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::tags$label("Homepage:"),
                      shiny::uiOutput(
                        "applicationHomepage", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::tags$label("Imprint & Privacy Policy:"),
                      shiny::uiOutput(
                        "imprintAndPrivacyPolicy", inline = TRUE
                      ),
                      shiny::tags$br(),
                      shiny::uiOutput(
                        "applicationIssues"
                      )
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    shiny::tags$h2("Libraries used by EPoS-MoL"),
                    # collapsible = TRUE,
                    # collapsed = TRUE,
                    DT::dataTableOutput("appLibraries")
                  )
                ),
                data.step = 15,
                data.intro = "This tab provides application related information, where to find the source code and where to report issues."
              )
            )
          )
        )
      )
    )
  }
  server <- function(input, output, session) {
    values <- shiny::reactiveValues()
    values$tableFilePath <- NULL
    values$lipidScoresTableData <- lipidScoresTableDataEmpty
    values$tble <- lipidTableDataEmpty
    values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
    output$lipidCategoryOrClass <-
      shiny::renderPrint({
        input$lipidCategoryOrClass
      })

    primaryClassificationChoices <- shiny::reactive({
      unique(scoringTable |>
        dplyr::pull(Primary))
    })

    secondaryClassificationChoices <- shiny::reactive({
      unique(
        scoringTable |>
          dplyr::filter(
            LipidCategoryOrClass == input$lipidCategoryOrClass &
              Primary == input$primaryClassification
          ) |>
          dplyr::pull(Secondary)
      )
    })

    evidenceClassificationChoices <- shiny::reactive({
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

    evidenceStreamScore <- shiny::reactive({
      scoringTable |>
        dplyr::filter(
          LipidCategoryOrClass == input$lipidCategoryOrClass &
            Primary == input$primaryClassification &
            Secondary == input$secondaryClassification &
            Evidence == input$evidenceClassification
        ) |>
        dplyr::pull(value)
    })
    shiny::observe({
      shiny::updateSelectInput(session, "primaryClassification",
        choices = primaryClassificationChoices()
      )
    })
    shiny::observe({
      shiny::updateSelectInput(session, "secondaryClassification",
        choices = secondaryClassificationChoices()
      )
    })
    shiny::observe({
      shiny::updateSelectInput(session, "evidenceClassification",
        choices = evidenceClassificationChoices()
      )
    })
    shiny::observe({
      shiny::updateTextInput(session, "evidenceScore",
        value = evidenceStreamScore()
      )
    })

    shiny::observeEvent(input$reset1, {
      shinyjs::reset("tableFile")
      shinyjs::disable("checkNames")
      shinyjs::disable("download")
      shinyjs::disable("loadExcel")
      values$tableFilePath <- NULL
      values$tble <- lipidTableDataEmpty
      values$lipidScoresTableData <- lipidScoresTableDataEmpty
      values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
    })

    shiny::observeEvent(input$reset2, {
      shinyjs::reset("tableFile")
      shinyjs::disable("checkNames")
      shinyjs::disable("download")
      shinyjs::disable("loadExcel")
      values$tableFilePath <- NULL
      values$tble <- lipidTableDataEmpty
      values$lipidScoresTableData <- lipidScoresTableDataEmpty
      values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
    })

    shiny::observeEvent(input$lipidName, {
      if (stringr::str_length(input$lipidName)>0) {
        shinyjs::enable("addScore")
      } else {
        shinyjs::disable("addScore")
      }
    })

    shiny::observeEvent(input$tableFile, {
      shinyjs::enable("loadExcel")
      values$tableFilePath <- input$tableFile$datapath
      shiny::updateSelectInput(session,
        "tableSheet",
        choices = readxl::excel_sheets(values$tableFilePath)
      )
    })

    shiny::observeEvent(input$loadExample, {
      shinyjs::enable("loadExcel")
      values$tableFilePath <- system.file("extdata","Table_S2.xlsx", package="eposmol")
      shiny::updateSelectInput(session,
        "tableSheet",
        choices = readxl::excel_sheets(values$tableFilePath)
      )
    })

    shiny::observeEvent(input$loadExcel, {
      tryCatch(
        {
          values$lipidScoresTableData <- NA
          values$totalLipidScoresTableData <- NA
          tble <-
            readxl::read_excel(
              values$tableFilePath,
              sheet = input$tableSheet,
              col_names = TRUE
            )
          lipidScoresTableData <- lipidScoresTableDataEmpty
          if (input$tableFormat == "long") {
            lipidScoresTableData <- eposmol::readLongTable(tble, scoringTable)
          } else {
            lipidScoresTableData <- eposmol::readWideTable(tble, scoringTable)
          }
          values$tble <- tble
          values$lipidScoresTableData <- lipidScoresTableData
          values$totalLipidScoresTableData <-
            eposmol::calculateTotalLipidScoresTableData(lipidScoresTableData)
          shiny::updateTabsetPanel(session, "mainPanels",
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

    shiny::observeEvent(input$addScore, {
      if (is.null(values$lipidScoresTableData)) {
        values$lipidScoresTableData <- lipidScoresTableDataEmpty
      }
      if (is.null(values$manualLipidScoresTableData)) {
        values$manualLipidScoresTableData <- values$lipidScoresTableData
      }
      manualLipidScoresTableData <- eposmol::addRowManually(
        values$manualLipidScoresTableData,
        lipidName=input$lipidName,
        lipidCategoryOrClass=input$lipidCategoryOrClass,
        ionMode=input$ionMode,
        evidenceClassification=input$evidenceClassification,
        evidenceScore=input$evidenceScore
      )
      values$manualLipidScoresTableData <- manualLipidScoresTableData
      values$lipidScoresTableData <- eposmol::readManualTable(manualLipidScoresTableData, scoringTable)
      values$totalLipidScoresTableData <-
        eposmol::calculateTotalLipidScoresTableData(values$lipidScoresTableData)
      shiny::updateTabsetPanel(session, "mainPanels",
                        selected = "Individual Scores"
      )
      shinyjs::enable("checkNames")
      shinyjs::enable("download")
    })

    shiny::observeEvent(input$checkNames, {
      shiny::req(values$totalLipidScoresTableData)
      values$totalLipidScoresTableData <- eposmol::checkNames(values$totalLipidScoresTableData, cvMapTable)
    })

    output$lipidEvidenceScoreTable <- DT::renderDataTable(values$lipidScoresTableData,
      options = list(
        select = "single",
        scrollX = TRUE
      )
    )

    output$totalLipidEvidenceScoreTable <- DT::renderDataTable(values$totalLipidScoresTableData,
      options = list(
        select = "single",
        scrollX = TRUE
      )
    )

    output$originalTable <- DT::renderDataTable(values$tble,
      options = list(
        select = "single",
        scrollX = TRUE
      )
    )

    output$referenceScoreTable <- DT::renderDataTable({
      DT::datatable(arrange(scoringTable, LipidCategoryOrClass, ID),
          extensions = "RowGroup",
          options = list(
            select = "single",
            scrollX = TRUE,
            rowGroup = list(dataSrc = c(6))
          )
      )
    })

    output$lipidCategoryAndClassMapTable <- DT::renderDataTable(lipidCategoryAndClassMapTable,
      options = list(
       select = "single",
       scrollX = TRUE
      )
    )

    output$sampleFile <- shiny::downloadHandler(
      filename = function() {
        paste0("EPOS-Mol-Examples", ".xlsx")
      },
      content = function(file) {
        file.copy(system.file("extdata", "Table_S2.xlsx", package="eposmol"), file)
      },
      contentType = "application/msexcel"
    )

    output$download <- shiny::downloadHandler(
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

    output$gettingStarted <- shiny::renderUI({
      shiny::HTML(htmltools::includeMarkdown(system.file("app/www/gettingStarted.md", package = "eposmol")))
    })

    output$usingEposmol <-  shiny::renderUI({
      shiny::HTML(htmltools::includeMarkdown(system.file("app/www/usingEposmol.md", package = "eposmol")))
    })

    output$applicationName <- shiny::renderText({appInfo$application.name})
    output$applicationVersion <- shiny::renderText({appInfo$application.version})
    output$applicationDate <- shiny::renderText({appInfo$application.date})
    output$applicationAuthors <- shiny::renderText({appInfo$application.authors})
    output$applicationLicense <- shiny::renderText({appInfo$application.license})
    output$applicationHomepage <- shiny::renderUI({shiny::tags$a(href=appInfo$application.url, target="_blank", appInfo$application.url)})
    output$imprintAndPrivacyPolicy <- shiny::renderUI({shiny::tags$a(href=appInfo$application.imprintAndPrivacyPolicy, target="_blank", appInfo$application.imprintAndPrivacyPolicy)})
    output$applicationIssues <- shiny::renderUI({shiny::tags$a(href=appInfo$application.issues, target="_blank", "Report an issue", class="btn btn-danger")})

    loadedLibraries <- shiny::reactive({
      assemblePackageDescriptions((.packages()))
    })

    output$appLibraries <- DT::renderDataTable(
      loadedLibraries(),
      options = list(
        dom = 'Bfrtlip',
        buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
        pageLength = 25
      ),
      escape = FALSE
    )

  }

  devOptions <- options(
    shiny.reactlog = TRUE,
    shiny.error = browser,
    shiny.autoload.r = TRUE,
    shiny.testmode = test.mode
  )
  prodOptions <- options(
    shiny.autoload.r = TRUE,
    shiny.testmode = test.mode
  )

  if (file.exists(here::here(".dev"))) {
    shinyOptions <- devOptions
    cat("Using development options!")
  } else {
    shinyOptions <- prodOptions
    cat("Using production options!")
  }
  # Run the application
  shinyApp <- shiny::shinyApp(
    ui = ui,
    server = server,

    # enableBookmarking = "server",
    options = shinyOptions,
  )
  return(shinyApp)
}
