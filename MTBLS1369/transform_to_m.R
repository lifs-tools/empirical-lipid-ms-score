library(tidyverse)

transColNames = c(
  "MoleculeListName",
  "MoleculeName",
  "MoleculeFormula",
  "PrecursorIonFormula",
  "PrecursorNeutralFormula",
  "PrecursorAdduct",
  "PrecursorMz",
  "PrecursorCharge",
  "CollisionEnergy",
  "ExplicitCollisionEnergy",
  "ExplicitRetentionTime",
  "ExplicitRetentionTimeWindow",
  "ProductMz",
  "ProductCharge",
  "ProductIonFormula",
  "ProductNeutralFormula",
  "ProductAdduct"
)
lc_col = cols(
  `Molecule List Name` = col_character(),
  `Molecule Name` = col_character(),
  `Molecule Formula` = col_character(),
  `Precursor Ion Formula` = col_character(),
  `Precursor Neutral Formula` = col_character(),
  `Precursor Adduct` = col_character(),
  `Precursor Mz` = col_double(),
  `Precursor Charge` = col_double(),
  `Collision Energy` = col_number(),
  `Explicit Collision Energy` = col_character(),
  `Explicit Retention Time` = col_logical(),
  `Explicit Retention Time Window` = col_logical(),
  `Product Mz` = col_double(),
  `Product Charge` = col_double(),
  `Product Ion Formula` = col_character(),
  `Product Neutral Formula` = col_character(),
  `Product Adduct` = col_character()
)

lc_trans_prm_sched <-
  read_csv(
    "PRM_scheduled_HumanPlatelet_5donors_5conditions_2019-07-24_15-23-54-transitions-export.csv", 
    col_types = lc_col,
    locale = locale("de", decimal_mark = ",", grouping_mark = ".")
  )
colnames(lc_trans_prm_sched) <- transColNames

getSignString <- function(x) {
  vapply(x, function(y){
    sy <- sign(y)
    if(sy==0) {
      return("")
    } else if(sy<0) {
      return("-")
    } else {
      return("+")
    }  
  }, FUN.VALUE = character(1))
}

# define columns used in Skyline transition result export
skyl_col = cols(
  Peptide = col_character(),
  Protein = col_character(),
  Replicate = col_character(),
  `Precursor Mz` = col_double(),
  `Precursor Charge` = col_double(),
  `Product Mz` = col_double(), 
  `Product Charge` = col_double(),
  `Fragment Ion` = col_character(),
  `Retention Time` = col_double(),
  Area = col_double(),
  Background = col_character(),
  `Peak Rank` = col_integer()
)

loadResults <- function(skylResultsFile, loc=locale("de", decimal_mark = ",", grouping_mark = "."), group=NA, sampleNamePrefix="", skyl_cols=skyl_col) {
  skylineResults <- read_csv(
    file=skylResultsFile,
    na = c("NA", "#N/A"),
    col_types = skyl_cols,
    locale = loc,
    quote = "\""
  ) %>%
    rename(
      MoleculeName = Peptide,
      PrecursorMz = "Precursor Mz",
      PrecursorCharge = "Precursor Charge",
      ProductMz = "Product Mz",
      ProductCharge = "Product Charge",
      ProductName = "Fragment Ion",
      RetentionTime = "Retention Time",
      PeakRank = "Peak Rank"
    ) %>%
    mutate("SampleName" = paste0(sampleNamePrefix, .$Replicate),
           "Group" = group, PrecursorMz_chr=as.character(PrecursorMz),
           ProductMz_chr=as.character(ProductMz),
           Area = na_if(Area, 0)) %>%
    select(-Protein, -Background, -"PeakRank") %>%
    select(MoleculeName, everything())
  skylineResults
}

joinResultsAndTransitions <- function(skylineResultsTibble, skylineTransitionsTibble) {
  skylineTransitionsTibble_s <-
    skylineTransitionsTibble %>% 
    mutate(
      PrecursorMz_chr=as.character(PrecursorMz),
      ProductMz_chr=as.character(ProductMz),
      PrecursorAdduct=paste0(.$PrecursorAdduct,abs(.$PrecursorCharge),getSignString(.$PrecursorCharge)),
      ProductAdduct=paste0(.$ProductAdduct,abs(.$ProductCharge),getSignString(.$PrecursorCharge))
    ) %>%
    select(
      MoleculeName,
      MoleculeFormula,
      PrecursorAdduct,
      PrecursorMz_chr,
      ProductIonFormula,
      ProductNeutralFormula,
      ProductAdduct,
      ProductMz_chr
    )
  
  skyl_joined <- skylineResultsTibble %>% left_join(
    skylineTransitionsTibble_s,
    by = c(
      "MoleculeName" = "MoleculeName",
      "PrecursorMz_chr" = "PrecursorMz_chr",
      "ProductMz_chr" = "ProductMz_chr"
    )
  )
  skyl_joined
}

# map Skyline replicate / sample and file names to those used in MetaboLights
file_to_sample_lookup <- read_csv("QEx_measurements_records.csv")

# prm, scheduled
skyl_prm <- loadResults(skylResultsFile = "PRM_scheduled_HumanPlatelet_5donors_5conditions_2019-07-24_15-23-54-export-filename.csv", group="PRM", sampleNamePrefix="",skyl_cols = skyl_col)
skyl_prm_joined <- joinResultsAndTransitions(skyl_prm, lc_trans_prm_sched)
skyl_prm_joined <- skyl_prm_joined %>% select(-SampleName) %>% left_join(file_to_sample_lookup %>% select(FileName, SampleName), by=c("File Name"="FileName"))

summary(skyl_prm_joined)

skyl_prm_joined_pos <- skyl_prm_joined %>% filter(PrecursorCharge>0)
skyl_prm_joined_neutr <- skyl_prm_joined %>% filter(PrecursorCharge==0)
# we should not have neutral precursors here
stopifnot(nrow(skyl_prm_joined_neutr)==0)
skyl_prm_joined_neg <- skyl_prm_joined %>% filter(PrecursorCharge<0)

# Prepare the MetaboLights m table
prepareMtblTibble <- function(tibble, fctLevel) {
  summary <-
    tibble %>% replace_na(list("RetentionTime" = 0, "Area" = 0)) %>%
    unite(fragmentation,
          "ProductMz",
          "ProductName",
          sep = "|",
          remove = FALSE) %>%
    group_by(
      MoleculeName,
      !!sym("PrecursorMz"),
      !!sym("PrecursorMz_chr"),
      !!sym("PrecursorCharge"),
      !!sym("ProductMz"),
      !!sym("ProductMz_chr"),
      !!sym("ProductCharge"),
      !!sym("fragmentation")
    ) %>%
    summarise(
      retention_time = mean(!!sym("RetentionTime"), na.rm = TRUE),
      smallmolecule_abundance_sub = mean(!!sym("Area"), na.rm = TRUE),
      smallmolecule_abundance_stdev_sub = sd(!!sym("Area"), na.rm = TRUE),
      smallmolecule_abundance_std_error_sub = sd(!!sym("Area")) /
        sqrt(n())
    ) %>% select(-PrecursorMz,-ProductMz,-PrecursorCharge,-ProductCharge,-fragmentation) %>% ungroup()
  j <-
    tibble %>% select(-"Replicate", -"RetentionTime", -"File Name", -Group) %>% 
      pivot_wider(names_from = "SampleName", values_from = "Area") %>%
    unite(fragmentation,
          "ProductMz",
          "ProductName",
          sep = "|",
          remove = FALSE) %>% ungroup()
  jp <-
    j %>% left_join(
      summary,
      by = c("MoleculeName", "PrecursorMz", "PrecursorCharge", "ProductMz", "ProductCharge", "PrecursorMz_chr", "ProductMz_chr")
    ) %>% 
   ungroup() %>%
    mutate(retention_time = na_if(retention_time, 0)) %>%
    select(
      "MoleculeName",
      "retention_time",
      "smallmolecule_abundance_sub",
      "smallmolecule_abundance_stdev_sub",
      "smallmolecule_abundance_std_error_sub",
      everything()
    )
  return(jp)
}
# default columns for m table 
m_tbl <- tibble(
  database_identifier=character(),
  chemical_formula=character(),
  smiles=character(),
  inchi=character(),
  metabolite_identification=character(),
  mass_to_charge=double(),
  fragmentation=character(),
  modifications=character(),
  charge=integer(),
  retention_time=double(),
  taxid=character(),
  species=character(),
  database=character(),
  database_version=character(),
  reliability=character(),
  uri=character(),
  search_engine=character(),
  search_engine_score=double(),
  smallmolecule_abundance_sub=double(),
  smallmolecule_abundance_stdev_sub=double(),
  smallmolecule_abundance_std_error_sub=double()
)

rbind_m_prm_pos <-
  bind_rows(
    m_tbl,
    prepareMtblTibble(skyl_prm_joined_pos, "PRM-POS")
  ) %>%
  mutate(
    modifications = PrecursorAdduct,
    chemical_formula = MoleculeFormula,
    metabolite_identification = MoleculeName,
    mass_to_charge = as.double(PrecursorMz_chr),
    charge = PrecursorCharge
  ) %>%
  select(
    -MoleculeFormula,
    -PrecursorAdduct,
    -ProductIonFormula,
    -ProductNeutralFormula,
    -ProductAdduct,
    -MoleculeName,
    -PrecursorCharge,
    -ProductCharge,
    -ProductName,
    -PrecursorMz_chr,
    -ProductMz_chr
  )

rbind_m_prm_neg <-
  bind_rows(
    m_tbl,
    prepareMtblTibble(skyl_prm_joined_neg, "PRM-NEG")
  ) %>%
  mutate(
    modifications = PrecursorAdduct,
    chemical_formula = MoleculeFormula,
    metabolite_identification = MoleculeName,
    mass_to_charge = as.double(PrecursorMz_chr),
    charge = PrecursorCharge
  ) %>%
  select(
    -MoleculeFormula,
    -PrecursorAdduct,
    -ProductIonFormula,
    -ProductNeutralFormula,
    -ProductAdduct,
    -MoleculeName,
    -PrecursorCharge,
    -ProductCharge,
    -ProductName,
    -PrecursorMz_chr,
    -ProductMz_chr
  )

library(httr)
library(jsonlite)
library(xml2)

getSwissLipidsId <- function(originalLipidName) {
  print(paste0("Received original name '",originalLipidName, "'"))
  nameTibble <- tibble(metabolite_identification=originalLipidName) %>% 
    mutate(metabolite_identification=gsub("(.*)(-d[0-9]+)", "\\1", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("-", "_", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("(.*)O_(.*)p(.*)", "\\1O-\\2\\3", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("(.*)P_(.*)p(.*)", "\\1P-\\2\\3", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("ChE", "CE", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("MAG", "MG", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("DAG", "DG", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("TAG", "TG", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("([A-Za-z0-9]+) ([0-9]+:[0-9]+);1(.*)", "\\1 m\\2\\3", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("([A-Za-z0-9]+) ([0-9]+:[0-9]+);2(.*)", "\\1 d\\2\\3", metabolite_identification)) %>%
    mutate(metabolite_identification=gsub("([A-Za-z0-9]+) ([0-9]+:[0-9]+);3(.*)", "\\1 t\\2\\3", metabolite_identification))
  lipidName <- unique(nameTibble$metabolite_identification)[[1]]
  print(paste0("Resolving '",lipidName,"' against SwissLipids"))
  url <- URLencode(paste("https://www.swisslipids.org/api/advancedSearch?Name",lipidName, sep="="))
  print(paste0(url))
  response <- GET(url=url, add_headers(Accept = "application/json", "Content-Type" = "application/json"))
  if(response$status_code!=200) {
    print(paste0("Query for ",lipidName," returned with status code: ",response$status_code))
    tbl <- tibble("entity_id"=character(),"entity_name"=character(),"external_id"=character(),"entity_type"=character(),"classification_level"=character(),"query"=character())
    tbl[1,] <- c("","","","","",lipidName)
    return(tbl)
  } 
  xmlrl <- content(response, "parsed")
  cnt <- xml_find_all(xmlrl, ".//p/text()")
  itemId <- jsonlite::fromJSON(txt = xml_text(cnt[[1]])) %>% as_tibble(.)
  itemId$originalName <- originalLipidName
  itemId$query <- lipidName
  itemIdf <- itemId %>% filter(classification_level=="Species" | classification_level=="Structural subspecies" | classification_level=="Molecular subspecies" | classification_level=="Isomeric subspecies") %>% 
    select(-stats,-nb_exp_annot) %>% mutate(namelen=nchar(entity_name)) %>% arrange(namelen) %>% select(-namelen)
  print(paste0("Found ",nrow(itemIdf)," hits after filtering"))
  return(itemIdf %>% slice(1))
}

resolveLipidNames <- function(lipidNames) {
  lapply(lipidNames, getSwissLipidsId)
}

resolveLipids <- function(tibble) {
  mtble <- tibble 
  mnames <- as.list(unique(mtble$metabolite_identification))
  ln <- resolveLipidNames(mnames)
  allNames <- bind_rows(ln)
  date <- format(Sys.Date(),"%Y/%m/%d")
  resolvedLipids <- mtble %>% left_join(allNames, by=c("metabolite_identification"="originalName"))
  resolvedLipids <- resolvedLipids %>% ungroup() %>%
    mutate("database_identifier"=entity_id,
           "database"="SwissLipids", "database_version"=date, "reliability"="","uri"="",
           "search_engine"="", "search_engine_score"="") %>% 
    select(-"entity_id", -"entity_name",-"external_id",-"entity_type",-"classification_level",-"query") %>%
    select("database_identifier", "chemical_formula", "smiles", "inchi",
           "metabolite_identification",
           "mass_to_charge","fragmentation", "modifications",
           "charge", "retention_time", "taxid", "species",
           "database","database_version","reliability", "uri",
           "search_engine", "search_engine_score", everything())
  print(nrow(resolvedLipids))
  resolvedLipids
}

m_pos_resolved <- resolveLipids(rbind_m_prm_pos)
m_neg_resolved <- resolveLipids(rbind_m_prm_neg)

# putatively annotated compounds / lipid species or subspecies level with evidence from fragments
m_neg_resolved$reliability <-"MSI:2" 
m_neg_resolved$species <- "Homo sapiens"
m_neg_resolved$taxid <- "NCBITaxon:9606"
# id against SwissLipids
m_neg_resolved$database <- "SwissLipids"
m_neg_resolved$database_version <- "2019/12/18"

# putatively annotated compounds / lipid species or subspecies level with evidence from fragments
m_pos_resolved$reliability <-"MSI:2" 
m_pos_resolved$species <- "Homo sapiens"
m_pos_resolved$taxid <- "NCBITaxon:9606"
# id against SwissLipids
m_pos_resolved$database <- "SwissLipids"
m_pos_resolved$database_version <- "2019/12/18"

write_tsv(
  m_neg_resolved,
  "m_MTBLS1369_LC-MS_negative_reverse-phase_metabolite_profiling_v2_maf.tsv"
)

write_tsv(
  m_pos_resolved,
  "m_MTBLS1369_LC-MS_positive_reverse-phase_metabolite_profiling_v2_maf.tsv"
)


