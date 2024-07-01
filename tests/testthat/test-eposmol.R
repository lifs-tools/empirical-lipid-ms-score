library(purrr)
library(dplyr)
library(tibble)

test_that("loadCvMapTable works", {
  cvMapTable <- eposmol::loadCvMapTable(path=system.file("extdata", "shorthand_cv_map.xlsx", package="eposmol"))
  expect_equal(nrow(cvMapTable), 10)
  expect_equal(ncol(cvMapTable), 4)
})

test_that("loadScoringTable works", {
  scoringTable <- eposmol::loadScoringTable(path=system.file("extdata","Table_1.xlsx", package="eposmol"))
  expect_equal(nrow(scoringTable), 609)
  expect_equal(ncol(scoringTable), 7)
})

test_that("loadCategoryAndClassMapTable works", {
  scoringTable <- eposmol::loadCategoryAndClassMapTable(path=system.file("extdata","class_map.xlsx", package="eposmol"))
  expect_equal(nrow(scoringTable), 87)
})

test_that("readLongTable works", {
  scoringTable <- eposmol::loadScoringTable(path=system.file("extdata","Table_1.xlsx", package="eposmol"))
  tbleLong <- openxlsx::read.xlsx(system.file("extdata","Table_S2.xlsx", package="eposmol"), sheet = "Long Format")
  expect_equal(nrow(tbleLong), 33)
  expect_equal(ncol(tbleLong), 5)
  tbleResult <- eposmol::readLongTable(tbleLong, scoringTable)
  expect_equal(nrow(tbleResult), 33)
  expect_equal(ncol(tbleResult), 10)
})

test_that("readWideTable works", {
  scoringTable <- eposmol::loadScoringTable(path=system.file("extdata","Table_1.xlsx", package="eposmol"))
  tbleWide <- openxlsx::read.xlsx(system.file("extdata","Table_S2.xlsx", package="eposmol"), sheet = "Wide Format")
  expect_equal(nrow(tbleWide), 7)
  expect_equal(ncol(tbleWide), 32)
  tbleResult <- eposmol::readWideTable(tbleWide, scoringTable)
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
  tbleAddedResult <- eposmol::addRowManually(
    emptyTble,
    lipidName = "Cer 18:1;O2/16:0",
    lipidCategoryOrClass = "SP",
    ionMode = "+",
    evidenceClassification = "Headgroup 2",
    evidenceScore = 10
  )
  expect_equal(nrow(tbleAddedResult), 1)
  expect_equal(ncol(tbleAddedResult), 6)

  scoringTable <- eposmol::loadScoringTable(path=system.file("extdata","Table_1.xlsx", package="eposmol"))
  manualTable <- eposmol::readManualTable(tbleAddedResult, scoringTable)
  expect_equal(nrow(manualTable), 1)
  expect_equal(ncol(manualTable), 11)
})

test_that("addRowManually existing works", {
  scoringTable <- eposmol::loadScoringTable(path=system.file("extdata","Table_1.xlsx", package="eposmol"))
  tbleWide <- openxlsx::read.xlsx(system.file("extdata","Table_S2.xlsx", package="eposmol"), sheet = "Wide Format")
  expect_equal(nrow(tbleWide), 7)
  expect_equal(ncol(tbleWide), 32)
  tbleResult <- eposmol::readWideTable(tbleWide, scoringTable)
  expect_equal(nrow(tbleResult), 33)
  expect_equal(ncol(tbleResult), 10)
  tbleAddedResult <- eposmol::addRowManually(
    tbleResult,
    lipidName = "Cer 18:1;O2/16:0",
    lipidCategoryOrClass = "SP",
    ionMode = "+",
    evidenceClassification = "Headgroup 2",
    evidenceScore = 10
  )
  expect_equal(nrow(tbleAddedResult), 34)
  expect_equal(ncol(tbleAddedResult), 10)

  manualTable <- eposmol::readManualTable(tbleAddedResult, scoringTable)
  expect_equal(nrow(manualTable), 34)
  expect_equal(ncol(manualTable), 11)
})

test_that("calculateTotalLipidScoresTableData works", {
  scoringTable <- eposmol::loadScoringTable(system.file("extdata","Table_1.xlsx", package="eposmol"))
  tbleLong <- openxlsx::read.xlsx(system.file("extdata","Table_S2.xlsx", package="eposmol"), sheet = "Long Format")
  tbleResult <- eposmol::readLongTable(tbleLong, scoringTable)
  totalLipidScoresTableData <- eposmol::calculateTotalLipidScoresTableData(tbleResult)
  expect_equal(nrow(totalLipidScoresTableData), 6)
  expect_equal(ncol(totalLipidScoresTableData), 4)
  expect_equal(35, totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/16:0") |> pluck("TotalScore"))
  expect_equal(60, totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/18:1") |> pluck("TotalScore"))
  expect_equal(80, totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/25:1") |> pluck("TotalScore"))
  expect_equal("SP", totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/25:1") |> pluck("LipidCategoryOrClass"))
  expect_equal("L1.2+, L1.4+, L2.2+, L2.6+, L2.7+", totalLipidScoresTableData |> filter(Name=="Cer 18:1;O2/25:1") |> pluck("ScoreCode"))
  expect_equal(50, totalLipidScoresTableData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("TotalScore"))
  expect_equal(114, totalLipidScoresTableData |> filter(Name=="PC 18:2_24:6") |> pluck("TotalScore"))
  expect_equal(134, totalLipidScoresTableData |> filter(Name=="PI 18:1/18:1") |> pluck("TotalScore"))
})

test_that("checkNames works", {
  scoringTable <- eposmol::loadScoringTable(path=system.file("extdata","Table_1.xlsx", package="eposmol"))
  cvMapTable <- eposmol::loadCvMapTable(path = system.file("extdata", "shorthand_cv_map.xlsx", package="eposmol"))
  tbleLong <- openxlsx::read.xlsx(system.file("extdata","Table_S2.xlsx", package="eposmol"), sheet = "Long Format")
  tbleResult <- eposmol::readLongTable(tbleLong, scoringTable)
  totalLipidScoresTableData <- eposmol::calculateTotalLipidScoresTableData(tbleResult)
  nameData <- eposmol::checkNames(totalLipidScoresTableData, cvMapTable)
  expect_equal(nrow(nameData), 6)
  expect_equal(ncol(nameData), 15)
  expect_equal(50, nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("TotalScore"))
  expect_equal("Unrecognised shorthand name", nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Message"))
  expect_equal("Unknown", totalLipidScoresTableData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("LipidCategoryOrClass"))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Shorthand.Level.CvTerm")))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Lipid.Maps.Category")))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Lipid.Maps.Main.Class")))
  expect_true(is.na(nameData |> filter(Name=="Lipid Feature m/z 752.56") |> pluck("Normalized.Name")))

  expect_equal(134, nameData |> filter(Name=="PI 18:1/18:1") |> pluck("TotalScore"))
  expect_equal("", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Message"))
  expect_equal("GP", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Lipid.Maps.Category"))
  expect_equal("PI", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Lipid.Maps.Main.Class"))
  expect_equal("PI 18:1/18:1", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Normalized.Name"))
  expect_equal("SN_POSITION", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Shorthand.Level"))
  expect_equal("MS:1003317", nameData |> filter(Name=="PI 18:1/18:1") |> pluck("Shorthand.Level.CvTerm"))
})

test_that("mapCategoryOrClass works", {
  classMap <- eposmol::loadCategoryAndClassMapTable(path=system.file("extdata","class_map.xlsx", package="eposmol"))
  expect_equal(nrow(classMap), 87)
  expect_equal(ncol(classMap), 2)
  expect_equal("SP", eposmol::mapCategoryOrClass("SP", classMap))
  expect_equal("Unknown", eposmol::mapCategoryOrClass("GP", classMap))
})
