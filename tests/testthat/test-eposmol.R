library(purrr)
library(dplyr)
source("../../R/eposmol.R")
test_that("loadScoringTable works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
  expect_equal(nrow(scoringTable), 609)
  expect_equal(ncol(scoringTable), 7)
})

test_that("loadCategoryAndClassMapTable works", {
  scoringTable <- loadCategoryAndClassMapTable(path=file.path("..","..","inst","extdata","class_map.xlsx"))
  expect_equal(nrow(scoringTable), 87)
})

test_that("readLongTable works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
  tbleLong <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Long Format")
  expect_equal(nrow(tbleLong), 33)
  expect_equal(ncol(tbleLong), 5)
  tbleResult <- readLongTable(tbleLong, scoringTable)
  expect_equal(nrow(tbleResult), 33)
  expect_equal(ncol(tbleResult), 10)
})

test_that("readWideTable works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
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

  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
  manualTable <- readManualTable(tbleAddedResult, scoringTable)
  expect_equal(nrow(manualTable), 1)
  expect_equal(ncol(manualTable), 11)
})

test_that("addRowManually existing works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
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
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
  tbleLong <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Long Format")
  tbleResult <- readLongTable(tbleLong, scoringTable)
  totalLipidScoresTableData <- calculateTotalLipidScoresTableData(tbleResult)
  expect_equal(nrow(totalLipidScoresTableData), 6)
  expect_equal(ncol(totalLipidScoresTableData), 4)
  expect_equal(35, totalLipidScoresTableData |> dplyr::filter(Name=="Cer 18:1;O2/16:0") |> purrr::pluck("TotalScore"))
  expect_equal(60, totalLipidScoresTableData |> dplyr::filter(Name=="Cer 18:1;O2/18:1") |> purrr::pluck("TotalScore"))
  expect_equal(70, totalLipidScoresTableData |> dplyr::filter(Name=="Cer 18:1;O2/25:1") |> purrr::pluck("TotalScore"))
  expect_equal("SP", totalLipidScoresTableData |> dplyr::filter(Name=="Cer 18:1;O2/25:1") |> purrr::pluck("LipidCategoryOrClass"))
  expect_equal("L1.2, L1.4, L2.2, L2.6, L2.7", totalLipidScoresTableData |> dplyr::filter(Name=="Cer 18:1;O2/25:1") |> purrr::pluck("ScoreCode"))
  expect_equal(20, totalLipidScoresTableData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("TotalScore"))
  expect_equal(110, totalLipidScoresTableData |> dplyr::filter(Name=="PC 18:2_24:6") |> purrr::pluck("TotalScore"))
  expect_equal(130, totalLipidScoresTableData |> dplyr::filter(Name=="PI 18:1/18:1") |> purrr::pluck("TotalScore"))
})

test_that("checkNames works", {
  scoringTable <- loadScoringTable(path=file.path("..","..","inst","extdata","Table 1_mod.xlsx"))
  tbleLong <- openxlsx::read.xlsx(file.path("..","..","inst","extdata","Table S2.xlsx"), sheet = "Long Format")
  tbleResult <- readLongTable(tbleLong, scoringTable)
  totalLipidScoresTableData <- calculateTotalLipidScoresTableData(tbleResult)
  nameData <- checkNames(totalLipidScoresTableData)
  expect_equal(nrow(nameData), 6)
  expect_equal(ncol(nameData), 8)
  expect_equal(20, nameData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("TotalScore"))
  expect_equal("Unrecognised shorthand name", nameData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("Message"))
  expect_equal("Unknown", totalLipidScoresTableData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("LipidCategoryOrClass"))
  expect_true(is.na(nameData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("Lipid.Maps.Category")))
  expect_true(is.na(nameData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("Lipid.Maps.Main.Class")))
  expect_true(is.na(nameData |> dplyr::filter(Name=="Lipid Feature m/z 752.56") |> purrr::pluck("Normalized.Name")))

  expect_equal(130, nameData |> dplyr::filter(Name=="PI 18:1/18:1") |> purrr::pluck("TotalScore"))
  expect_equal("", nameData |> dplyr::filter(Name=="PI 18:1/18:1") |> purrr::pluck("Message"))
  expect_equal("GP", nameData |> dplyr::filter(Name=="PI 18:1/18:1") |> purrr::pluck("Lipid.Maps.Category"))
  expect_equal("PI", nameData |> dplyr::filter(Name=="PI 18:1/18:1") |> purrr::pluck("Lipid.Maps.Main.Class"))
  expect_equal("PI 18:1/18:1", nameData |> dplyr::filter(Name=="PI 18:1/18:1") |> purrr::pluck("Normalized.Name"))
})
