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

lipidClassFromName <- function(lipidName, registeredLipidClasses) {
  tryCatch({
    # try to match using goslin and Lipid.Maps.Main.Class
    lipidName <- "LPE 32:1"
    goslinResm <- rgoslin::parseLipidNames(lipidName)  
    return(goslinResm$Lipid.Maps.Category)
  }, error -> {
    # try to match prefix immediately
      # lipidName <- "N-mod PL 32:1"
    # registeredLipidClasses <- unique(scoringSchemeLong$name)
    classMatches <- startsWith(lipidName, registeredLipidClasses)
    if (sum(classMatches, na.rm = TRUE) == 1) {
      return(registeredLipidClasses[which(startsWith(lipidName, registeredLipidClasses))])
    }
    regx <- stringr::regex("(^[A-Za-z-]+) (.*)$")
    matches <- stringr::str_match(lipidName, regx)
    if (length(matches) == 3) {
      return(matches[2])
    } else {
      return(NA)
    }
  },
  warning -> {
    
  }
  )
  
}

calculateScoreLong <- function(scoringTable, longFormatTable) {
  longFormatTable |> left_join(longFormatTable, by=c("LipidCategoryOrClass"="lipidClassification","Feature"="Evidence"))
}