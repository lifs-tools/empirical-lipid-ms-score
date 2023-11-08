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
#library(knitr)

appInfo <- list(
  "application.name"="EPoS-MoL",
  "application.version"="0.9.0",
  "application.date"=date(),
  "application.authors"="Nils Hoffmann, Harald Köfeler",
  "application.license"="MIT",
  "application.url"="https://github.com/lifs-tools/empirical-lipid-ms-score",
  "application.imprintAndPrivacyPolicy"="https://lifs-tools.org/imprint-privacy-policy.html",
  "application.issues"="https://github.com/lifs-tools/empirical-lipid-ms-score/issues"
)

cvMapTable <- loadCvMapTable(path = here("inst", "extdata", "shorthand_cv_map.xlsx"))
scoringTable <- loadScoringTable(path = here("inst", "extdata", "Table 1.xlsx"))
lipidCategoryAndClassMapTable <- loadCategoryAndClassMapTable(path = here("inst", "extdata", "class_map.xlsx"))
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
    includeCSS("R/www/custom.css"),
    useShinyjs(),
    #use_cicerone(),
    titlePanel("EPoS-ML Calculation"),
    sidebarLayout(
      sidebarPanel(
        # actionButton("help", icon=icon("question"), class="btn-info", label="Start Tour"),
        tags$br(),
        tabsetPanel(
          id = "sidebarPanels",
          tabPanel(
            "Upload",
            tags$br(),
            introBox(
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
              data.step = 1,
              data.intro = "Upload your lipid annotation data in EXCEL format here."
            ),
            tags$hr(),
            introBox(
              selectInput("tableSheet",
                label = "Select the sheet to load",
                choices = c()
              ),
              data.step = 2,
              data.intro = "Select the sheet to load your data from."
            ),
            introBox(
              selectInput(
                "tableFormat",
                label = "Select the table format",
                choices = c("wide", "long")
              ),
              data.step = 3,
              data.intro = "Choose either the wide or long table format. You can download an example file demonstrating both formats further down."
            ),
            tags$hr(),
            fluidRow(
              column(12,
                align="left",
                introBox(
                  style="display:inline-block;",
                  shinyjs::disabled(
                    actionButton("loadExcel", "Load table", class = "btn-primary")
                  ),
                  data.step = 4,
                  data.intro = "This button will be enabled as soon as you have uploaded a table and have chosen the sheet and table format. Click on it to load the data."
                ),
                introBox(
                  style="display:inline-block;",
                  actionButton("reset1", "Reset", class = "btn-danger"),
                  data.step = 5,
                  data.intro = "Click on this button to reset any inputs. This will also remove any intermediate tables generated from your data, so please be careful!"
                ),
                actionButton("loadExample", "Load example data", class = "btn-default", style="float:right"),
              )
            ),
          ),
          tabPanel(
            "Manual Input",
            tags$br(),
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
        introBox(
          style="display:inline-block;",
          downloadButton(
            "sampleFile",
            "Download Example",
            class = "btn-info",
            style="display:inline-block;",
            icon = icon("download")
          ),
          data.step = 6,
          data.intro = "Click here to download an example lipid annotation dataset. The file contains to sheets for the wide and long format."
        ),
        bookmarkButton(style="display:inline-block;float: right"),
      ),
      mainPanel(
        tabsetPanel(
          id = "mainPanels",
          tabPanel(
            "Getting Started",
            introBox(
              uiOutput("gettingStarted"),
              data.step = 7,
              data.intro = "This tab provides an overview of the EPoS-MOL application workflow and the data formats used."
            )
          ),
          tabPanel(
            "Total Scores",
            introBox(
              fluidRow(
                column(
                  12,
                  br(),
                  DT::dataTableOutput("totalLipidEvidenceScoreTable"),
                  introBox(
                    style="display: inline-block;",
                    disabled(actionButton(
                      "checkNames",
                      "Check Lipid Names",
                      class = "btn-success",
                      icon = icon("check")
                    )),
                    data.step = 9,
                    data.intro = "Click here to check and automatically normalize supported lipid names into the updated shorthand nomenclature."
                  ),
                  introBox(
                    style="display: inline-block;",
                    disabled(downloadButton("download", "Download Score Tables",
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
          tabPanel(
            "Individual Scores",
            introBox(
              fluidRow(column(
                12,
                br(),
                DT::dataTableOutput("lipidEvidenceScoreTable")
              )),
              data.step = 11,
              data.intro = "This tab provides the table of individual scores assigned to the user provided lipid evidence."
            )
          ),
          tabPanel(
            "Original Table",
            introBox(
              fluidRow(column(
                12,
                br(),
                DT::dataTableOutput("originalTable")
              )),
              data.step = 12,
              data.intro = "This tab shows the original table as provided or manually added to by the user."
            )
          ),
          tabPanel(
            "Reference Score Table",
            introBox(
              fluidRow(column(
                12,
                br(),
                DT::dataTableOutput("referenceScoreTable")
              )),
              data.step = 13,
              data.intro = "This tab provides the reference score table of EPoS-MOL for the different lipid classes and evidence levels."
            )
          ),
          tabPanel(
            "Lipid Category & Class Map",
            introBox(
              fluidRow(column(
                12,
                br(),
                DT::dataTableOutput("lipidCategoryAndClassMapTable")
              )),
              data.step = 14,
              data.intro = "This tab provides a mapping from LIPID MAPS category and classes to the (super-)classes used by EPoS-MOL."
            )
          ),
          # tabPanel(
          #   "Using the EPoS-MoL R Library",
          #   fluidRow(
          #     uiOutput('howtoUseMarkdown')
          #   ),
          # ),
          tabPanel(
            "About EPoS-MoL",
            introBox(
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
  values <- reactiveValues()
  values$tableFilePath <- NULL
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
    values$tableFilePath <- NULL
    values$tble <- lipidTableDataEmpty
    values$lipidScoresTableData <- lipidScoresTableDataEmpty
    values$totalLipidScoresTableData <- totalLipidScoresTableDataEmpty
  })

  observeEvent(input$reset2, {
    shinyjs::reset("tableFile")
    disable("checkNames")
    disable("download")
    disable("loadExcel")
    values$tableFilePath <- NULL
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
    values$tableFilePath <- input$tableFile$datapath
    updateSelectInput(session,
      "tableSheet",
      choices = readxl::excel_sheets(values$tableFilePath)
    )
  })

  observeEvent(input$loadExample, {
    enable("loadExcel")
    values$tableFilePath <- here("inst","extdata","Table S2.xlsx")
    updateSelectInput(session,
      "tableSheet",
      choices = readxl::excel_sheets(values$tableFilePath)
    )
  })

  observeEvent(input$loadExcel, {
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
          lipidScoresTableData <- readLongTable(tble, scoringTable)
        } else {
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
    values$totalLipidScoresTableData <- checkNames(values$totalLipidScoresTableData, cvMapTable)
  })

  # observeEvent(input$help, {
  #   mainPanelNames <- c(
  #     "Getting Started",
  #     "Total Scores",
  #     "Individual Scores",
  #     "Original Table",
  #     "Reference Score Table",
  #     "Lipid Category & Class Map",
  #     "About EPoS-MoL",
  #     "Getting Started"
  #   )
  #   for(panel in mainPanelNames) {
  #     updateTabsetPanel(session, inputId="mainPanels", selected = panel)
  #   }
  #    introjs(session,
  #            options = list("nextLabel"="Next",
  #                           "prevLabel"="Back",
  #                           "skipLabel"="Skip"),
  #            events = list(
  #              onbeforechange = readCallback("switchTabs")#,
  #              # oncomplete=I(
  #              #   'alert("Thanks for taking the tour!")'
  #              #   )
  #              )
  #    )
  #   }
  # )

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
    datatable(arrange(scoringTable, LipidCategoryOrClass, ID),
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

  output$sampleFile <- downloadHandler(
    filename = function() {
      paste0("EPOS-Mol-Examples", ".xlsx")
    },
    content = function(file) {
      file.copy(here("inst", "extdata", "Table S2.xlsx"), file)
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
    HTML(htmltools::includeMarkdown("R/www/gettingStarted.md"))
  })

  # output$howtoUseMarkdown <- renderUI({
  #   HTML(markdown::markdownToHTML(knit(here("vignettes","howto-use.Rmd"), quiet = TRUE)))
  # })

  output$applicationName <- shiny::renderText({appInfo$application.name})
  output$applicationVersion <- shiny::renderText({appInfo$application.version})
  output$applicationDate <- shiny::renderText({appInfo$application.date})
  output$applicationAuthors <- shiny::renderText({appInfo$application.authors})
  output$applicationLicense <- shiny::renderText({appInfo$application.license})
  output$applicationHomepage <- shiny::renderUI({tags$a(href=appInfo$application.url, target="_blank", appInfo$application.url)})
  output$imprintAndPrivacyPolicy <- shiny::renderUI({tags$a(href=appInfo$application.imprintAndPrivacyPolicy, target="_blank", appInfo$application.imprintAndPrivacyPolicy)})
  output$applicationIssues <- shiny::renderUI({tags$a(href=appInfo$application.issues, target="_blank", "Report an issue", class="btn btn-danger")})

  loadedLibraries <- reactive({
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
shinyApp <- shinyApp(
  ui = ui,
  server = server,
  enableBookmarking = "server",
  options = shinyOptions
)
return(shinyApp)
