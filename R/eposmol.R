loadScoringTable <- function(path=system.file("..", "inst", "extdata", "Table 1_mod.xlsx")) {
  scoringScheme <- readxl::read_excel(
    path = path,
    sheet = "Scoring scheme",
    range = "A1:Z30"
  )
  scoringSchemeLong <- scoringScheme |>
    tidyr::pivot_longer(
      cols = !Primary:ID,
      names_to = "LipidCategoryOrClass"
    )
  scoringSchemeLong
}

loadCategoryAndClassMapTable <- function(path=system.file("..", "inst", "extdata", "class_map.xlsx")) {
  lipidCategoryAndClassMap <- readxl::read_excel(
    path = path,
    sheet = "class_map",
    range = "A1:B88"
  )
  lipidCategoryAndClassMap
}

readLongTable <- function(tble, scoringTable) {
  tble |>
    tidyr::drop_na() |>
    dplyr::left_join(
      scoringTable,
      by = c(
        "LipidCategoryOrClass" = "LipidCategoryOrClass",
        "Feature" = "Evidence"
      )
    ) |>
    dplyr::rename(Score = value) |>
    dplyr::distinct(Name, LipidCategoryOrClass, IonMode, Feature, .keep_all = TRUE) |>
    dplyr::arrange(.by_group = TRUE)
}

readWideTable <- function(tble, scoringTable) {
  tble |>
    dplyr::group_by(Name, LipidCategoryOrClass, IonMode) |>
    tidyr::pivot_longer(
      4:last_col(),
      names_to = "Feature",
      values_to = "Value",
      values_ptypes = list(Value = character()),
      values_transform = as.character
    ) |>
    tidyr::drop_na() |>
    dplyr::left_join(
      scoringTable,
      by = c(
        "LipidCategoryOrClass" = "LipidCategoryOrClass",
        "Feature" = "Evidence"
      )
    ) |>
    dplyr::rename(Score = value) |>
    dplyr::group_by(Name, LipidCategoryOrClass, IonMode) |>
    dplyr::distinct(Name, LipidCategoryOrClass, IonMode, Feature, .keep_all = TRUE) |>
    dplyr::arrange(.by_group = TRUE)
}

addRowManually <- function(lipidScoresTableData, lipidName, lipidCategoryOrClass, ionMode, evidenceClassification, evidenceScore) {
  lipidScoresTableData |>
    dplyr::ungroup() |>
    dplyr::add_row(
      Name = lipidName,
      LipidCategoryOrClass = lipidCategoryOrClass,
      IonMode = ionMode,
      Feature = evidenceClassification,
      Value = "",
      Score = as.numeric(evidenceScore)
    ) |>
    dplyr::group_by(Name, LipidCategoryOrClass, IonMode)
}

readManualTable <- function(tble, scoringTable) {
  tble |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c('ID', 'Primary', 'Secondary', 'Fragment'))) |>
    dplyr::left_join(
      scoringTable,
      by = c(
        "LipidCategoryOrClass" = "LipidCategoryOrClass",
        "Feature" = "Evidence"
      )
    ) |>
    dplyr::group_by(Name, LipidCategoryOrClass, IonMode) |>
    dplyr::distinct(Name, LipidCategoryOrClass, IonMode, Feature, .keep_all = TRUE) |>
    dplyr::arrange(.by_group = TRUE)
}

calculateTotalLipidScoresTableData <- function(lipidScoresTableData) {
  lipidScoresTableData |>
    dplyr::group_by(Name, LipidCategoryOrClass) |>
    dplyr::summarise(TotalScore = sum(Score), ScoreCode = paste0(stringr::str_sort(ID, numeric = T), collapse = ", "))
}

checkNames <- function(totalLipidScoresTableData) {
  tryCatch(
    {
      goslinResm <-
        rgoslin::parseLipidNames(totalLipidScoresTableData$Name)
      messages <- goslinResm$Message
      validNames <- goslinResm$Message %in% c("NA")
      messages[validNames] <- ""
      messages[!validNames] <- "Unrecognised shorthand name"
      totalLipidScoresTableData$Message <- messages
      totalLipidScoresTableData$Lipid.Maps.Category <-
        goslinResm$Lipid.Maps.Category
      totalLipidScoresTableData$Lipid.Maps.Main.Class <-
        goslinResm$Lipid.Maps.Main.Class
      totalLipidScoresTableData$Normalized.Name <-
        goslinResm$Normalized.Name
      return(totalLipidScoresTableData)
    },
    error = function(cond) {
      return(NA)
    },
    warning = function(cond) {
      return(NA)
    }
  )
}

assemblePackageDescriptions <- function(packageNames) {
  purrr::map(packageNames, .f = ~ packageDescription(.x, fields=c("Package","Version","License","URL"))) |>
    purrr::map_df(~ .x |> purrr::keep(names(.) %in% c("Package","Version","License","URL")))
}