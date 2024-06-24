library(shinytest2)

test_that("{shinytest2} recording: epos-load-wide", {
  shiny_app <- eposmol::run_eposmol_app(test.mode=TRUE)
  app <- AppDriver$new(shiny_app, variant = platform_variant(), name = "epos-load-wide", height = 1333,
      width = 1419)
  app$upload_file(tableFile = system.file("extdata","Table S2.xlsx", package="eposmol"))
  app$set_inputs(tableFormat = "wide")
  app$set_inputs(tableSheet = "Wide Format")
  app$click("loadExcel")
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-wide-example", {
  shiny_app <- eposmol::run_eposmol_app(test.mode=TRUE)
  app <- AppDriver$new(shiny_app, variant = platform_variant(), name = "epos-load-wide-example", height = 1333,
                       width = 1419)
  # app$click("loadExample")
  # app$set_inputs(tableFormat = "wide")
  # app$set_inputs(tableSheet = "Wide Format")
  # app$click("loadExcel")
  # app$expect_values()
  #

  app$set_window_size(width = 1619, height = 1049)
  app$click("loadExample")
  app$set_inputs(tableFormat = "wide")
  app$set_inputs(tableSheet = "Wide Format")
  app$click("loadExcel")
  # Update output value
  app$set_inputs(totalLipidEvidenceScoreTable_rows_current = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(totalLipidEvidenceScoreTable_rows_all = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(totalLipidEvidenceScoreTable_state = c(1719255275564, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-long", {
  shiny_app <- eposmol::run_eposmol_app(test.mode=TRUE)
  app <- AppDriver$new(shiny_app, variant = platform_variant(), name = "epos-load-long", height = 1333, width = 1419)
  app$upload_file(tableFile = system.file("extdata","eposmol::run_eposmol_app()Table S2.xlsx", package="eposmol"))
  app$set_inputs(tableSheet = "Long Format")
  app$set_inputs(tableFormat = "long")
  app$click("loadExcel")
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-long-reset1", {
  shiny_app <- eposmol::run_eposmol_app(test.mode=TRUE)
  app <- AppDriver$new(shiny_app, variant = platform_variant(), name = "epos-load-long-reset1", height = 1333, width = 1419)
  app$upload_file(tableFile = system.file("extdata","Table S2.xlsx", package="eposmol"))
  app$set_inputs(tableSheet = "Long Format")
  app$set_inputs(tableFormat = "long")
  app$click("loadExcel")
  app$click("reset1")
  app$expect_values()
})
