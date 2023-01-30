library(shinytest2)

test_that("{shinytest2} recording: epos-load-wide", {
  app <- AppDriver$new(variant = platform_variant(), name = "epos-load-wide", height = 1333,
      width = 1419)
  app$upload_file(tableFile = file.path("..","..","inst","Table S2.xlsx"))
  app$set_inputs(tableFormat = "wide")
  app$set_inputs(tableSheet = "Wide Format")
  app$click("loadExcel")
  app$expect_values()
})

test_that("{shinytest2} recording: epos-load-long", {
  app <- AppDriver$new(variant = platform_variant(), name = "epos-load-long", height = 1333, width = 1419)
  app$upload_file(tableFile = file.path("..","..","inst","Table S2.xlsx"))
  app$set_inputs(tableSheet = "Long Format")
  app$set_inputs(tableFormat = "long")
  app$click("loadExcel")
  app$expect_values()
})
