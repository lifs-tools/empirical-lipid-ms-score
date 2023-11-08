library(purrr)
library(dplyr)
source("../../R/eposmol.R")

test_that("loadCvMapTable works", {
  cvMapTable <- loadCvMapTable(path = file.path("..","..","inst", "extdata", "shorthand_cv_map.xlsx"))
  expect_equal(nrow(cvMapTable), 10)
  expect_equal(ncol(cvMapTable), 4)
})

test_that("loadScoringTable works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  expect_equal(nrow(scoringTable), 609)
  expect_equal(ncol(scoringTable), 7)
})

test_that("loadCategoryAndClassMapTable works", {
  scoringTable <- loadCategoryAndClassMapTable(path=file.path("..","..","inst","extdata","class_map.xlsx"))
  expect_equal(nrow(scoringTable), 87)
})

test_that("readLongTable works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  tbleLong <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Long Format")
  expect_equal(nrow(tbleLong), 33)
  expect_equal(ncol(tbleLong), 5)
  tbleResult <- readLongTable(tbleLong, scoringTable)
  expect_equal(nrow(tbleResult), 33)
  expect_equal(ncol(tbleResult), 10)
})

test_that("readWideTable works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  tbleWide <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Wide Format")
  expect_equal(nrow(tbleWide), 7)
  expect_equal(ncol(tbleWide), 32)
  tbleResult <- readWideTable(tbleWide, scoringTable)
  expect_equal(nrow(tbleResult), 33)
  expect_equal(ncol(tbleResult), 10)
})

test_that("addRowManually empty works", {
  emptyTble <- tibble::tibble(
    Name = character(),
    LipidCategoryOrClass = character(),
    IonMode = character(),
    Feature = character(),
    Value = character(),
    Score = numeric()
  )
  expect_equal(nrow(emptyTble), 0)
  expect_equal(ncol(emptyTble), 6)
  tbleAddedResult <- addRowManually(
    emptyTble,
    lipidName = "Cer 18:1;O2/16:0",
    lipidCategoryOrClass = "SP",
    ionMode = "+",
    evidenceClassification = "Headgroup 2",
    evidenceScore = 10
  )
  expect_equal(nrow(tbleAddedResult), 1)
  expect_equal(ncol(tbleAddedResult), 6)

  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  manualTable <- readManualTable(tbleAddedResult, scoringTable)
  expect_equal(nrow(manualTable), 1)
  expect_equal(ncol(manualTable), 11)
})

test_that("addRowManually existing works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  tbleWide <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Wide Format")
  expect_equal(nrow(tbleWide), 7)
  expect_equal(ncol(tbleWide), 32)
  tbleResult <- readWideTable(tbleWide, scoringTable)
  expect_equal(nrow(tbleResult), 33)
  expect_equal(ncol(tbleResult), 10)
  tbleAddedResult <- addRowManually(
    tbleResult,
    lipidName = "Cer 18:1;O2/16:0",
    lipidCategoryOrClass = "SP",
    ionMode = "+",
    evidenceClassification = "Headgroup 2",
    evidenceScore = 10
  )
  expect_equal(nrow(tbleAddedResult), 34)
  expect_equal(ncol(tbleAddedResult), 10)

  manualTable <- readManualTable(tbleAddedResult, scoringTable)
  expect_equal(nrow(manualTable), 34)
  expect_equal(ncol(manualTable), 11)
})

test_that("calculateTotalLipidScoresTableData works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  tbleLong <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Long Format")
  tbleResult <- readLongTable(tbleLong, scoringTable)
  totalLipidScoresTableData <- calculateTotalLipidScoresTableData(tbleResult)
  expect_equal(nrow(totalLipidScoresTableData), 6)
  expect_equal(ncol(totalLipidScoresTableData), 4)
  expect_equal(35, totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/16:0") |> pluck("TotalScore"))
  expect_equal(60, totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/18:1") |> pluck("TotalScore"))
  expect_equal(70, totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/25:1") |> pluck("TotalScore"))
  expect_equal("SP", totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/25:1") |> pluck("LipidCategoryOrClass"))
  expect_equal("L1.2, L1.4, L2.2, L2.6, L2.7", totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/25:1") |> pluck("ScoreCode"))
  expect_equal(20, totalLipidScoresTableData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("TotalScore"))
  expect_equal(110, totalLipidScoresTableData |> filter(Name=="PC 18:2_24:6") |> pluck("TotalScore"))
  expect_equal(130, totalLipidScoresTableData |> filter(Name=="PI 18:1/18:1") |> pluck("TotalScore"))
})

test_that("checkNames works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1.xlsx"))
  cvMapTable <- loadCvMapTable(path = file.path("..","..","inst", "extdata", "shorthand_cv_map.xlsx"))
  tbleLong <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Long Format")
  tbleResult <- readLongTable(tbleLong, scoringTable)
  totalLipidScoresTableData <- calculateTotalLipidScoresTableData(tbleResult)
  nameData <- checkNames(totalLipidScoresTableData, cvMapTable)
  expect_equal(nrow(nameData), 6)
  expect_equal(ncol(nameData), 10)
  expect_equal(20, nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("TotalScore"))
  expect_equal("Unrecognised shorthand name", nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Message"))
  expect_equal("Unknown", totalLipidScoresTableData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("LipidCategoryOrClass"))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("CVTerm")))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Lipid.Maps.Category")))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Lipid.Maps.Main.Class")))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Normalized.Name")))

  expect_equal(130, nameData |> filter(Name=="PI 18:1/18:1") |> pluck("TotalScore"))
  expect_equal("", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Message"))
  expect_equal("GP", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Lipid.Maps.Category"))
  expect_equal("PI", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Lipid.Maps.Main.Class"))
  expect_equal("PI 18:1/18:1", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Normalized.Name"))
  expect_equal("SN_POSITION", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Shorthand.Level"))
  expect_equal("MS:1003317", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("CVTerm"))
})
