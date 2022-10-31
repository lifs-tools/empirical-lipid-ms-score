loadScoringTable <- function() {
  scoringScheme <- read_excel(
    path = file.path("..","data", "Table 1_mod.xlsx"),
    sheet = "Scoring scheme", 
    range = "A1:X30"
  )
  scoringSchemeLong <- scoringScheme %>% 
    pivot_longer(
      cols=!Primary:Evidence, 
      names_to = "lipidClassification"
    )
  scoringSchemeLong
}