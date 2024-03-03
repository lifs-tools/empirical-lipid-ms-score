#' Load the scoring scheme table.
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @param path The path to the scoring scheme table.
#' @returns A tibble with the scoring scheme table.
#' @export
loadScoringTable <- function(path=system.file("..", "inst", "extdata", "Table 1.xlsx")) {
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

#' Load the category and class map table.
#' @importFrom readxl read_excel
#' @param path The path to the category and class map table.
#' @returns A tibble with the category and class map table.
#' @export
loadCategoryAndClassMapTable <- function(path=system.file("..", "inst", "extdata", "class_map.xlsx")) {
  lipidCategoryAndClassMap <- readxl::read_excel(
    path = path,
    sheet = "class_map",
    range = "A1:B88"
  )
  lipidCategoryAndClassMap
}

#' Load the shorthand CV map table.
#' @importFrom readxl read_excel
#' @param path The path to the shorthand CV map table.
#' @returns A tibble with the shorthand CV map table.
#' @export
loadCvMapTable <- function(path=system.file("..", "inst", "extdata", "shorthand_cv_map.xlsx")) {
  cvMap <- readxl::read_excel(
    path = path,
    sheet = "cv_term_map",
    range = "A1:D11"
  )
  cvMap
}

#' Read a long format table and join with the features defined in the scoring table. The long format table must have the following columns: Name, LipidCategoryOrClass, IonMode, Feature, Value.
#' @importFrom dplyr left_join rename distinct arrange
#' @importFrom tidyr drop_na
#' @param tble The long format table.
#' @param scoringTable The scoring table.
#' @returns A tibble with the long format table.
#' @export
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

#' Read a wide format table and join with the features defined in the scoring table. The wide format table must have the following columns: Name, LipidCategoryOrClass, IonMode, and the features as columns.
#' @importFrom dplyr group_by left_join rename distinct arrange
#' @importFrom tidyr pivot_longer drop_na
#' @param tble The wide format table.
#' @param scoringTable The scoring table.
#' @returns A tibble with the wide format table.
#' @export
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

#' Add a result row manually to the lipidScoresTableData tibble.
#' @importFrom dplyr add_row ungroup group_by
#' @param lipidScoresTableData The lipidScoresTableData tibble.
#' @param lipidName The lipid name.
#' @param lipidCategoryOrClass The lipid category or class.
#' @param ionMode The ion mode.
#' @param evidenceClassification The evidence classification.
#' @param evidenceScore The evidence score.
#' @returns A tibble with the added row.
#' @export
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

#' Read the manual table and join with the features defined in the scoring table. The manual table must have the following columns: Name, LipidCategoryOrClass, IonMode, Feature, Value.
#' @importFrom dplyr left_join group_by distinct arrange ungroup select any_of
#' @param tble The manual table.
#' @param scoringTable The scoring table.
#' @returns A tibble with the manual table data.
#' @export
readManualTable <- function(tble, scoringTable) {
  tble |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c('ID', 'Primary', 'Secondary', 'Fragment', 'maxScore'))) |>
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

#' Calculate the total lipid scores table data.
#' @importFrom dplyr group_by summarise
#' @importFrom stringr str_sort
#' @param lipidScoresTableData The lipid scores table data.
#' @returns A tibble with the total lipid scores table data.
#' @export
calculateTotalLipidScoresTableData <- function(lipidScoresTableData) {
  lipidScoresTableData |>
    dplyr::group_by(Name, LipidCategoryOrClass) |>
    dplyr::summarise(TotalScore = sum(Score), ScoreCode = paste0(stringr::str_sort(paste0(ID,IonMode,collapse=", "), numeric = T)))
}

#' Check if the lipid names are valid shorthand names using the rgoslin package.
#' @importFrom dplyr left_join rename
#' @importFrom purrr map_df keep
#' @importFrom rgoslin parseLipidNames
#' @param totalLipidScoresTableData The total lipid scores table data.
#' @param cvMapTable The shorthand CV map table.
#' @returns A tibble with the total lipid scores table data, joined with rgoslin parsing results.
#' @export
checkNames <- function(totalLipidScoresTableData, cvMapTable) {
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
      totalLipidScoresTableData$Shorthand.Level <- goslinResm$Level
      totalLipidScoresTableData$Species.Name <-
        goslinResm$Species.Name
      totalLipidScoresTableData$Molecular.Species.Name <-
        goslinResm$Molecular.Species.Name
      totalLipidScoresTableData$Sn.Position.Name <-
        goslinResm$Sn.Position.Name
      totalLipidScoresTableData$Structure.Defined.Name <-
        goslinResm$Structure.Defined.Name
      totalLipidScoresTableData$Full.Structure.Name <-
        goslinResm$Full.Structure.Name
      totalLipidScoresTableData <- totalLipidScoresTableData |> left_join(cvMapTable |> select(-ShorthandNomenclatureLevel,-CVTermName), by=c("Shorthand.Level")) |> rename(Shorthand.Level.CvTerm=CVTerm)
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
