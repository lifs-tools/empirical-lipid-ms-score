library(shinytest2)

test_that("{shinytest2} recording: epos-load-wide", {
  app <- AppDriver$new(variant = platform_variant(), name = "epos-load-wide", height = 1333,
      width = 1419)
  app$upload_file(tableFile = file.path("..","..","inst","extdata","Table S2.xlsx"))
  app$set_inputs(tableFormat = "wide")
  app$set_inputs(tableSheet = "Wide Format")
  app$click("loadExcel")
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-wide-example", {
  app <- AppDriver$new(variant = platform_variant(), name = "epos-load-wide-example", height = 1333,
                       width = 1419)
  app$click("loadExample")
  app$set_inputs(tableFormat = "wide")
  app$set_inputs(tableSheet = "Wide Format")
  app$click("loadExcel")
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-long", {
  app <- AppDriver$new(variant = platform_variant(), name = "epos-load-long", height = 1333, width = 1419)
  app$upload_file(tableFile = file.path("..","..","inst","extdata","Table S2.xlsx"))
  app$set_inputs(tableSheet = "Long Format")
  app$set_inputs(tableFormat = "long")
  app$click("loadExcel")
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-long-reset1", {
  app <- AppDriver$new(variant = platform_variant(), name = "epos-load-long-reset1", height = 1333, width = 1419)
  app$upload_file(tableFile = file.path("..","..","inst","extdata","Table S2.xlsx"))
  app$set_inputs(tableSheet = "Long Format")
  app$set_inputs(tableFormat = "long")
  app$click("loadExcel")
  app$click("reset1")
  app$expect_values()
})
