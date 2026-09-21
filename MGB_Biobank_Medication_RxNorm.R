###############################################################
## RxNorm normalization of MGB medication strings
###############################################################

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(tibble)
library(openxlsx)

setwd("$HOME/PROJECT_FOLDER")


###############################################################
## Step 1. Combine PD and control raw medication records
###############################################################

Med_PD_raw_1 <- read.table("./data/MGB_Biobank/PD30K_2005P001191_20240621_170059-1/XD010_20240621_170059-1_Med.txt", sep="|", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE, comment.char = "")
Med_PD_raw_2 <- read.table("./data/MGB_Biobank/PD30K_2005P001191_20240621_170059-2/XD010_20240621_170059-2_Med.txt", sep="|", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE, comment.char = "")
Med_PD_raw <- bind_rows(Med_PD_raw_1, Med_PD_raw_2)

Med_health_raw_1 <- read.table("./data/MGB_Biobank/MGB_biobank_matched_health_control/1/XD010_20240630_131521-1_Med.txt", sep="|", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE, comment.char = "")
Med_health_raw_2 <- read.table("./data/MGB_Biobank/MGB_biobank_matched_health_control/2/XD010_20240630_131521-2_Med.txt", sep="|", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE, comment.char = "")
Med_health_raw_3 <- read.table("./data/MGB_Biobank/MGB_biobank_matched_health_control/3/XD010_20240630_131521-3_Med.txt", sep="|", header = TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE, comment.char = "")
Med_health_raw <- bind_rows(Med_health_raw_1, Med_health_raw_2, Med_health_raw_3)

Med_all <- bind_rows(
  Med_PD_raw,
  Med_health_raw
)

###############################################################
## Step 2. Prepare unique raw medication strings
###############################################################

MGB_unique_medications <- Med_all %>%
  transmute(
    Medication = as.character(Medication)
  ) %>%
  filter(
    !is.na(Medication),
    str_trim(Medication) != ""
  ) %>%
  mutate(
    Medication = str_trim(Medication)
  ) %>%
  distinct() %>%
  arrange(Medication)

cat(
  "Unique medication strings:",
  nrow(MGB_unique_medications),
  "\n"
)


###############################################################
## MGB medication normalization using RxNorm
##
## Pipeline
## ------------------------------------------------------------
## Raw Medication
##   -> light MGB-specific cleaning
##   -> flag likely non-drug/device/supply
##   -> RxNorm exact/normalized search
##   -> approximate rescue for unmatched terms
##   -> RxNorm concept name
##   -> ingredient-level concepts
##   -> preferentially retain IN as final DrugName
##   -> manual review workbook
##   -> checkpoint RDS files
###############################################################

###############################################################
## 0. Paths
###############################################################

output_dir <- "./data/processed_data/MGB_Biobank/RxNorm/"

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}

file_search_checkpoint <- file.path(
  output_dir,
  "01_MGB_RxNorm_search_checkpoint.rds"
)

file_name_checkpoint <- file.path(
  output_dir,
  "02_MGB_RxNorm_names_checkpoint.rds"
)

file_ingredient_checkpoint <- file.path(
  output_dir,
  "03_MGB_RxNorm_ingredient_checkpoint.rds"
)

file_final_mapping <- file.path(
  output_dir,
  "MGB_RxNorm_mapping_final.rds"
)

file_manual_review <- file.path(
  output_dir,
  "MGB_RxNorm_manual_review.xlsx"
)


###############################################################
## 1. Light cleaning
##
## Remove local MGB artifacts only.
## Do NOT remove valid drug ingredients/dose/formulation.
###############################################################

clean_rxnorm_query <- function(x) {
  
  x <- as.character(x)
  x <- str_trim(x)
  
  # Remove MGB LMR suffix, e.g. "- LMR 3141"
  x <- str_remove(
    x,
    regex(
      "\\s*-\\s*LMR\\s*\\d+\\s*$",
      ignore_case = TRUE
    )
  )
  
  # Remove Oncall suffix
  x <- str_remove(
    x,
    regex(
      "\\s*-?oncall\\s*$",
      ignore_case = TRUE
    )
  )
  
  # Normalize common formulation abbreviations
  x <- str_replace_all(
    x,
    regex("\\btab\\b", ignore_case = TRUE),
    "tablet"
  )
  
  x <- str_replace_all(
    x,
    regex("\\bcaps?\\b", ignore_case = TRUE),
    "capsule"
  )
  
  x <- str_replace_all(
    x,
    regex("\\bdr\\b", ignore_case = TRUE),
    "delayed release"
  )
  
  x <- str_replace_all(
    x,
    regex("\\ber\\b", ignore_case = TRUE),
    "extended release"
  )
  
  # Collapse repeated whitespace
  x <- str_replace_all(
    x,
    "\\s+",
    " "
  )
  
  str_trim(x)
}


###############################################################
## 2. Flag likely non-drug/device/supply records
##
## Flag only. Do NOT remove automatically yet.
###############################################################

###############################################################
## Step 2. Flag likely non-drug/device/supply records
###############################################################

non_drug_pattern <- paste(
  c(
    "\\bmeter\\b",
    "\\blancets?\\b",
    "\\btest strips?\\b",
    "\\bstrips\\b",
    "\\bcare kit\\b",
    "\\bglucose monitor\\b",
    "\\bglucometer\\b",
    "\\bgauge\\b",
    "\\borthotic\\b",
    "\\bprosthe",
    "\\bAFO\\b",
    "\\bfitting and adjustment\\b",
    "\\bwheelchair\\b",
    "\\bwalker\\b",
    "\\bbrace\\b",
    "\\bcompression stockings?\\b",
    "\\bbandage\\b",
    "\\bdressing\\b",
    "\\bmedical supply\\b",
    
    # Common glucose-monitoring/device brands
    "\\baccu[- ]?chek\\b",
    "\\bfastclix\\b",
    "\\bmulticlix\\b",
    "\\bsoftclix\\b",
    "\\bsafe[- ]?t[- ]?pro\\b"
  ),
  collapse = "|"
)

###############################################################
## 3. Generic retry wrapper
###############################################################

rxnorm_get_retry <- function(
    url,
    query = NULL,
    attempts = 4,
    timeout_seconds = 30,
    pause_seconds = 1
) {
  
  for (i in seq_len(attempts)) {
    
    response <- tryCatch(
      httr::GET(
        url,
        query = query,
        httr::timeout(timeout_seconds)
      ),
      error = function(e) NULL
    )
    
    if (
      !is.null(response) &&
      httr::status_code(response) == 200
    ) {
      return(response)
    }
    
    if (i < attempts) {
      Sys.sleep(
        pause_seconds * i
      )
    }
  }
  
  NULL
}


###############################################################
## 4. RxNorm exact / normalized search
##
## search = 2:
## exact -> normalized
###############################################################

rxnorm_exact_normalized <- function(term) {
  
  if (
    is.na(term) ||
    str_trim(term) == ""
  ) {
    return(NA_character_)
  }
  
  response <- rxnorm_get_retry(
    url = "https://rxnav.nlm.nih.gov/REST/rxcui.json",
    query = list(
      name = term,
      search = 2
    )
  )
  
  if (is.null(response)) {
    return(NA_character_)
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    is.null(parsed$idGroup$rxnormId)
  ) {
    return(NA_character_)
  }
  
  ids <- parsed$idGroup$rxnormId
  
  if (length(ids) == 0) {
    return(NA_character_)
  }
  
  as.character(ids[1])
}


###############################################################
## 5. Approximate search
##
## Used only after exact/normalized failure.
###############################################################

rxnorm_approximate <- function(term) {
  
  empty_result <- tibble(
    RxCUI_approx = NA_character_,
    approx_score = NA_real_,
    approx_rank = NA_integer_,
    approx_candidate_name = NA_character_
  )
  
  if (
    is.na(term) ||
    str_trim(term) == ""
  ) {
    return(empty_result)
  }
  
  response <- rxnorm_get_retry(
    url = "https://rxnav.nlm.nih.gov/REST/approximateTerm.json",
    query = list(
      term = term,
      maxEntries = 1,
      option = 1
    )
  )
  
  if (is.null(response)) {
    return(empty_result)
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    is.null(parsed$approximateGroup$candidate)
  ) {
    return(empty_result)
  }
  
  candidate <- parsed$approximateGroup$candidate
  
  if (
    is.null(candidate) ||
    nrow(candidate) == 0
  ) {
    return(empty_result)
  }
  
  tibble(
    RxCUI_approx =
      as.character(candidate$rxcui[1]),
    
    approx_score =
      as.numeric(candidate$score[1]),
    
    approx_rank =
      as.integer(candidate$rank[1]),
    
    approx_candidate_name =
      if ("name" %in% names(candidate)) {
        as.character(candidate$name[1])
      } else {
        NA_character_
      }
  )
}


###############################################################
## 6. Retrieve RxNorm concept name
###############################################################

rxnorm_get_name <- function(rxcui) {
  
  if (
    is.na(rxcui) ||
    rxcui == ""
  ) {
    return(NA_character_)
  }
  
  response <- rxnorm_get_retry(
    url = paste0(
      "https://rxnav.nlm.nih.gov/REST/rxcui/",
      rxcui,
      ".json"
    )
  )
  
  if (is.null(response)) {
    return(NA_character_)
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    is.null(parsed$idGroup$name)
  ) {
    return(NA_character_)
  }
  
  as.character(
    parsed$idGroup$name
  )
}


###############################################################
## 7. Retrieve ingredient-related concepts
##
## IN  = Ingredient
## PIN = Precise Ingredient
## MIN = Multiple Ingredients
##
## We retrieve all three for QC, but final automatic DrugName
## will preferentially use IN only.
###############################################################

rxnorm_get_ingredients <- function(rxcui) {
  
  empty_result <- tibble(
    ingredient_RxCUI = NA_character_,
    ingredient_name = NA_character_,
    ingredient_TTY = NA_character_
  )
  
  if (
    is.na(rxcui) ||
    rxcui == ""
  ) {
    return(empty_result)
  }
  
  response <- rxnorm_get_retry(
    url = paste0(
      "https://rxnav.nlm.nih.gov/REST/rxcui/",
      rxcui,
      "/related.json"
    ),
    query = list(
      tty = "IN PIN MIN"
    )
  )
  
  if (is.null(response)) {
    return(empty_result)
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      ),
      simplifyDataFrame = FALSE
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    is.null(parsed$relatedGroup$conceptGroup)
  ) {
    return(empty_result)
  }
  
  groups <- parsed$relatedGroup$conceptGroup
  
  output <- purrr::map_dfr(
    groups,
    function(group_i) {
      
      props <- group_i$conceptProperties
      
      if (
        is.null(props) ||
        length(props) == 0
      ) {
        return(NULL)
      }
      
      purrr::map_dfr(
        props,
        function(p) {
          
          tibble(
            ingredient_RxCUI =
              as.character(p$rxcui),
            
            ingredient_name =
              as.character(p$name),
            
            ingredient_TTY =
              as.character(p$tty)
          )
        }
      )
    }
  )
  
  if (nrow(output) == 0) {
    return(empty_result)
  }
  
  output %>%
    distinct()
}


###############################################################
## 8. Prepare unique medication strings
##
## REQUIRED INPUT:
## Med_all with column Medication
###############################################################

stopifnot(
  "Object Med_all does not exist" =
    exists("Med_all")
)

stopifnot(
  "Medication column missing from Med_all" =
    "Medication" %in% names(Med_all)
)

MGB_unique_medications <- Med_all %>%
  transmute(
    Medication =
      as.character(Medication)
  ) %>%
  filter(
    !is.na(Medication),
    str_trim(Medication) != ""
  ) %>%
  mutate(
    Medication =
      str_trim(Medication),
    
    Medication_query =
      clean_rxnorm_query(Medication),
    
    likely_non_drug =
      str_detect(
        Medication,
        regex(
          non_drug_pattern,
          ignore_case = TRUE
        )
      )
  ) %>%
  distinct(
    Medication,
    Medication_query,
    likely_non_drug
  ) %>%
  arrange(Medication)

cat(
  "Unique medication strings:",
  nrow(MGB_unique_medications),
  "\n"
)

cat(
  "Likely non-drug strings:",
  sum(MGB_unique_medications$likely_non_drug),
  "\n"
)


###############################################################
## 9. Optional TEST mode
##
## Set TEST_MODE <- TRUE first.
###############################################################

TEST_MODE <- FALSE
TEST_N <- 100

med_terms_run <- if (TEST_MODE) {
  
  MGB_unique_medications %>%
    slice_head(n = TEST_N)
  
} else {
  
  MGB_unique_medications
}

cat(
  "Medication strings to process:",
  nrow(med_terms_run),
  "\n"
)


###############################################################
## 10. Exact/normalized search
##
## Resume from checkpoint if it exists.
###############################################################

if (
  !TEST_MODE &&
  file.exists(file_search_checkpoint)
) {
  
  message(
    "Loading RxNorm search checkpoint..."
  )
  
  MGB_rxnorm_mapping <-
    readRDS(
      file_search_checkpoint
    )
  
} else {
  
  MGB_rxnorm_mapping <-
    med_terms_run %>%
    mutate(
      RxCUI_exact_normalized =
        map_chr(
          Medication_query,
          function(x) {
            
            Sys.sleep(0.1)
            
            rxnorm_exact_normalized(x)
          }
        )
    )
  
  if (!TEST_MODE) {
    
    saveRDS(
      MGB_rxnorm_mapping,
      file_search_checkpoint
    )
  }
}


###############################################################
## 11. Approximate rescue for unmatched
###############################################################

MGB_rxnorm_unmatched <-
  MGB_rxnorm_mapping %>%
  filter(
    is.na(RxCUI_exact_normalized)
  )

cat(
  "Exact/normalized unmatched:",
  nrow(MGB_rxnorm_unmatched),
  "\n"
)

approx_results <-
  map_dfr(
    MGB_rxnorm_unmatched$Medication_query,
    function(term) {
      
      Sys.sleep(0.1)
      
      result <-
        rxnorm_approximate(term)
      
      tibble(
        Medication_query = term
      ) %>%
        bind_cols(result)
    }
  )


MGB_rxnorm_mapping <-
  MGB_rxnorm_mapping %>%
  left_join(
    approx_results,
    by = "Medication_query"
  ) %>%
  mutate(
    RxCUI =
      coalesce(
        RxCUI_exact_normalized,
        RxCUI_approx
      ),
    
    mapping_method =
      case_when(
        !is.na(RxCUI_exact_normalized) ~
          "Exact/Normalized",
        
        !is.na(RxCUI_approx) ~
          "Approximate",
        
        TRUE ~
          "Unmapped"
      )
  )


if (!TEST_MODE) {
  
  saveRDS(
    MGB_rxnorm_mapping,
    file_search_checkpoint
  )
}


###############################################################
## 12. Retrieve RxNorm names
###############################################################

if (
  !TEST_MODE &&
  file.exists(file_name_checkpoint)
) {
  
  message(
    "Loading RxNorm name checkpoint..."
  )
  
  MGB_rxnorm_mapping <-
    readRDS(
      file_name_checkpoint
    )
  
} else {
  
  unique_RxCUIs <-
    MGB_rxnorm_mapping %>%
    filter(
      !is.na(RxCUI)
    ) %>%
    distinct(RxCUI) %>%
    mutate(
      RxNorm_Name =
        map_chr(
          RxCUI,
          function(x) {
            
            Sys.sleep(0.1)
            
            rxnorm_get_name(x)
          }
        )
    )
  
  MGB_rxnorm_mapping <-
    MGB_rxnorm_mapping %>%
    left_join(
      unique_RxCUIs,
      by = "RxCUI"
    )
  
  if (!TEST_MODE) {
    
    saveRDS(
      MGB_rxnorm_mapping,
      file_name_checkpoint
    )
  }
}


###############################################################
## 13. Initial quality classification
###############################################################

MGB_rxnorm_mapping <-
  MGB_rxnorm_mapping %>%
  mutate(
    mapping_quality =
      case_when(
        
        likely_non_drug ~
          "Likely non-drug",
        
        mapping_method ==
          "Exact/Normalized" &
          !is.na(RxNorm_Name) ~
          "High confidence",
        
        mapping_method ==
          "Approximate" &
          !is.na(RxNorm_Name) ~
          "Approximate - manual review",
        
        mapping_method ==
          "Approximate" &
          is.na(RxNorm_Name) ~
          "Unresolved / likely reject",
        
        mapping_method ==
          "Unmapped" ~
          "Unmapped",
        
        TRUE ~
          "Manual review"
      )
  )


###############################################################
## 14. Mapping QC
###############################################################

mapping_QC <-
  MGB_rxnorm_mapping %>%
  count(
    mapping_quality
  ) %>%
  mutate(
    Percent =
      round(
        100 * n / sum(n),
        2
      )
  )

print(mapping_QC)


###############################################################
## 15. Ingredient lookup for HIGH-CONFIDENCE mappings only
##
## Approximate mappings are NOT converted to ingredients yet.
## They must first pass manual review.
###############################################################

ingredient_lookup <-
  MGB_rxnorm_mapping %>%
  
  filter(
    !is.na(RxCUI),
    !likely_non_drug,
    !is.na(RxNorm_Name),
    
    # IMPORTANT
    mapping_method == "Exact/Normalized"
  ) %>%
  
  distinct(
    RxCUI
  ) %>%
  
  mutate(
    ingredient_data =
      map(
        RxCUI,
        function(x) {
          
          Sys.sleep(0.1)
          
          rxnorm_get_ingredients(x)
        }
      )
  ) %>%
  
  unnest(
    ingredient_data
  )


###############################################################
## 16. Merge ingredient information back
###############################################################

MGB_rxnorm_mapping_long <-
  MGB_rxnorm_mapping %>%
  left_join(
    ingredient_lookup,
    by = "RxCUI"
  )


###############################################################
## 17. Determine whether IN exists for each matched medication
###############################################################

MGB_rxnorm_mapping_long <-
  MGB_rxnorm_mapping_long %>%
  group_by(
    Medication
  ) %>%
  mutate(
    has_IN =
      any(
        ingredient_TTY == "IN",
        na.rm = TRUE
      )
  ) %>%
  ungroup()


###############################################################
## 18. Create ingredient-level candidate table
##
## Prefer IN.
##
## If there is no IN, preserve PIN/MIN only for manual review.
###############################################################

MGB_ingredient_candidates <-
  MGB_rxnorm_mapping_long %>%
  filter(
    !likely_non_drug,
    !is.na(RxCUI),
    !is.na(RxNorm_Name)
  ) %>%
  filter(
    (
      has_IN &
        ingredient_TTY == "IN"
    ) |
      (
        !has_IN &
          ingredient_TTY %in%
          c("PIN", "MIN")
      )
  ) %>%
  mutate(
    ingredient_status =
      case_when(
        
        has_IN &
          ingredient_TTY == "IN" ~
          "IN available",
        
        !has_IN &
          ingredient_TTY == "PIN" ~
          "PIN only - manual review",
        
        !has_IN &
          ingredient_TTY == "MIN" ~
          "MIN only - manual review",
        
        TRUE ~
          "Manual review"
      )
  ) %>%
  distinct()


###############################################################
## 19. Automatically accepted ingredient-level mappings
##
## Conservative rule:
##
## - must have IN
## - must not be likely non-drug
## - must have a valid RxNorm concept name
##
## Exact/normalized mappings are high confidence.
## Approximate mappings still require manual review before use.
###############################################################

MGB_ingredient_auto_high_confidence <-
  MGB_ingredient_candidates %>%
  filter(
    ingredient_TTY == "IN",
    mapping_method == "Exact/Normalized"
  ) %>%
  transmute(
    Medication,
    Medication_query,
    
    Product_RxCUI =
      RxCUI,
    
    RxNorm_Name,
    
    Drug_RxCUI =
      ingredient_RxCUI,
    
    DrugName =
      ingredient_name,
    
    mapping_method,
    mapping_quality
  ) %>%
  distinct()


###############################################################
## 20. Approximate ingredient mappings requiring review
###############################################################

MGB_ingredient_approx_review <-
  MGB_ingredient_candidates %>%
  filter(
    mapping_method == "Approximate"
  ) %>%
  transmute(
    Medication,
    Medication_query,
    
    Product_RxCUI =
      RxCUI,
    
    RxNorm_Name,
    
    approx_score,
    approx_rank,
    approx_candidate_name,
    
    Drug_RxCUI =
      ingredient_RxCUI,
    
    DrugName_candidate =
      ingredient_name,
    
    ingredient_TTY,
    ingredient_status
  ) %>%
  distinct()


###############################################################
## 21. Medication-level manual review table
###############################################################

manual_review_table <-
  MGB_rxnorm_mapping %>%
  
  mutate(
    
    review_priority =
      case_when(
        
        likely_non_drug ~
          1L,
        
        mapping_method == "Approximate" &
          !is.na(RxNorm_Name) ~
          2L,
        
        mapping_method == "Approximate" &
          is.na(RxNorm_Name) ~
          3L,
        
        mapping_method == "Unmapped" ~
          4L,
        
        TRUE ~
          5L
      ),
    
    manual_status =
      case_when(
        
        # Exact/normalized can be provisionally accepted
        mapping_method == "Exact/Normalized" &
          !likely_non_drug &
          !is.na(RxNorm_Name) ~
          "Auto accept",
        
        likely_non_drug ~
          "Review non-drug",
        
        mapping_method == "Approximate" ~
          "Review approximate",
        
        mapping_method == "Unmapped" ~
          "Review unmapped",
        
        TRUE ~
          "Review"
      ),
    
    manual_DrugName =
      NA_character_,
    
    manual_note =
      NA_character_
  ) %>%
  
  arrange(
    review_priority,
    desc(approx_score)
  ) %>%
  
  select(
    Medication,
    Medication_query,
    likely_non_drug,
    
    RxCUI_exact_normalized,
    RxCUI_approx,
    
    approx_score,
    approx_rank,
    approx_candidate_name,
    
    RxCUI,
    RxNorm_Name,
    
    mapping_method,
    mapping_quality,
    
    review_priority,
    manual_status,
    manual_DrugName,
    manual_note
  )

###############################################################
## 22. Ingredient QC
###############################################################

ingredient_QC <-
  MGB_rxnorm_mapping_long %>%
  summarise(
    total_unique_medication_strings =
      n_distinct(Medication),
    
    mapped_RxNorm =
      n_distinct(
        Medication[
          !is.na(RxCUI)
        ]
      ),
    
    high_confidence_exact_normalized =
      n_distinct(
        Medication[
          mapping_method ==
            "Exact/Normalized" &
            !is.na(RxNorm_Name)
        ]
      ),
    
    approximate =
      n_distinct(
        Medication[
          mapping_method ==
            "Approximate"
        ]
      ),
    
    with_IN =
      n_distinct(
        Medication[
          has_IN
        ]
      ),
    
    likely_non_drug =
      n_distinct(
        Medication[
          likely_non_drug
        ]
      ),
    
    unmapped =
      n_distinct(
        Medication[
          mapping_method ==
            "Unmapped"
        ]
      )
  )

print(
  ingredient_QC
)


###############################################################
## 23. Save outputs
###############################################################

if (TEST_MODE) {
  
  write.xlsx(
    list(
      Mapping =
        MGB_rxnorm_mapping,
      
      IngredientLong =
        MGB_rxnorm_mapping_long,
      
      AutoHighConfidence =
        MGB_ingredient_auto_high_confidence,
      
      ApproxReview =
        MGB_ingredient_approx_review,
      
      ManualReview =
        manual_review_table
    ),
    
    file.path(
      output_dir,
      "TEST_MGB_RxNorm_first100.xlsx"
    ),
    
    overwrite = TRUE
  )
  
  message(
    "TEST MODE complete. Review first 100 before full run."
  )
  
} else {
  
  write.xlsx(
    list(
      Mapping =
        MGB_rxnorm_mapping,
      
      IngredientLong =
        MGB_rxnorm_mapping_long,
      
      AutoHighConfidence =
        MGB_ingredient_auto_high_confidence,
      
      ApproxReview =
        MGB_ingredient_approx_review,
      
      ManualReview =
        manual_review_table
    ),
    
    file_manual_review,
    
    overwrite = TRUE
  )
  
  saveRDS(
    MGB_rxnorm_mapping_long,
    file_final_mapping
  )
  
  message(
    "FULL RxNorm mapping complete."
  )
}


###############################################################
## 24. Console summary
###############################################################

cat("\n")
cat("====================================================\n")
cat("MGB RxNorm normalization summary\n")
cat("====================================================\n")

print(
  mapping_QC
)

print(
  ingredient_QC
)

cat("\nTEST_MODE:", TEST_MODE, "\n")
cat("Output directory:", output_dir, "\n")
cat("====================================================\n")



###############################################################
Integreient 
###############################################################

###############################################################
## MGB RxNorm: Convert mapped RxCUIs to ingredient-level drugs
##
## Input:
##   MGB_rxnorm_mapping
##
## Required columns:
##   Medication
##   Medication_query
##   RxCUI
##   RxNorm_Name
##   mapping_method
##   likely_non_drug
##
## Output:
##   MGB_rxnorm_ingredient_long
##
## Final analysis-level drug:
##   ingredient_name
###############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(httr)
  library(jsonlite)
  library(tibble)
  library(openxlsx)
})


###############################################################
## Step 0. Output paths
###############################################################

output_dir <- "./data/processed_data/MGB_Biobank/RxNorm" 

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}

tty_checkpoint_file <- file.path(
  output_dir,
  "04_RxNorm_TTY_lookup.rds"
)

ingredient_checkpoint_file <- file.path(
  output_dir,
  "05_RxNorm_IN_lookup.rds"
)

ingredient_final_file <- file.path(
  output_dir,
  "06_MGB_RxNorm_ingredient_long.rds"
)

ingredient_excel_file <- file.path(
  output_dir,
  "06_MGB_RxNorm_ingredient_QC.xlsx"
)


###############################################################
## Step 1. Generic RxNorm GET with retry
###############################################################

rxnorm_get_retry <- function(
    url,
    query = NULL,
    attempts = 4,
    timeout_seconds = 30,
    pause_seconds = 1
) {
  
  for (i in seq_len(attempts)) {
    
    response <- tryCatch(
      httr::GET(
        url,
        query = query,
        httr::timeout(timeout_seconds)
      ),
      error = function(e) NULL
    )
    
    if (
      !is.null(response) &&
      httr::status_code(response) == 200
    ) {
      return(response)
    }
    
    if (i < attempts) {
      Sys.sleep(
        pause_seconds * i
      )
    }
  }
  
  NULL
}


###############################################################
## Step 2. Get properties of one RxCUI
##
## Main purpose:
## retrieve RxNorm concept TTY
###############################################################

rxnorm_get_properties <- function(rxcui) {
  
  empty_result <- tibble(
    RxCUI = as.character(rxcui),
    concept_name = NA_character_,
    concept_TTY = NA_character_
  )
  
  if (
    is.na(rxcui) ||
    rxcui == ""
  ) {
    return(empty_result)
  }
  
  response <- rxnorm_get_retry(
    paste0(
      "https://rxnav.nlm.nih.gov/REST/rxcui/",
      rxcui,
      "/properties.json"
    )
  )
  
  if (is.null(response)) {
    return(empty_result)
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    is.null(parsed$properties)
  ) {
    return(empty_result)
  }
  
  p <- parsed$properties
  
  tibble(
    RxCUI = as.character(rxcui),
    
    concept_name =
      if (!is.null(p$name)) {
        as.character(p$name)
      } else {
        NA_character_
      },
    
    concept_TTY =
      if (!is.null(p$tty)) {
        as.character(p$tty)
      } else {
        NA_character_
      }
  )
}


###############################################################
## Step 3. Get related IN concepts
##
## IN = ingredient
##
## A combination drug may return multiple IN concepts.
###############################################################

rxnorm_get_related_IN <- function(rxcui) {
  
  empty_result <- tibble(
    ingredient_RxCUI = NA_character_,
    ingredient_name = NA_character_,
    ingredient_TTY = NA_character_
  )
  
  if (
    is.na(rxcui) ||
    rxcui == ""
  ) {
    return(empty_result)
  }
  
  response <- rxnorm_get_retry(
    url = paste0(
      "https://rxnav.nlm.nih.gov/REST/rxcui/",
      rxcui,
      "/related.json"
    ),
    query = list(
      tty = "IN"
    )
  )
  
  if (is.null(response)) {
    return(empty_result)
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      ),
      simplifyDataFrame = FALSE
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(parsed) ||
    is.null(parsed$relatedGroup$conceptGroup)
  ) {
    return(empty_result)
  }
  
  groups <- parsed$relatedGroup$conceptGroup
  
  result <- purrr::map_dfr(
    groups,
    function(group_i) {
      
      if (
        is.null(group_i$conceptProperties) ||
        length(group_i$conceptProperties) == 0
      ) {
        return(NULL)
      }
      
      purrr::map_dfr(
        group_i$conceptProperties,
        function(p) {
          
          tibble(
            ingredient_RxCUI =
              as.character(p$rxcui),
            
            ingredient_name =
              as.character(p$name),
            
            ingredient_TTY =
              as.character(p$tty)
          )
        }
      )
    }
  )
  
  if (nrow(result) == 0) {
    return(empty_result)
  }
  
  result %>%
    filter(
      ingredient_TTY == "IN"
    ) %>%
    distinct()
}


###############################################################
## Step 4. Prepare unique mapped RxCUIs
###############################################################

stopifnot(
  "MGB_rxnorm_mapping does not exist" =
    exists("MGB_rxnorm_mapping")
)

required_cols <- c(
  "Medication",
  "RxCUI",
  "RxNorm_Name",
  "mapping_method",
  "likely_non_drug"
)

missing_cols <- setdiff(
  required_cols,
  names(MGB_rxnorm_mapping)
)

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  )
}


unique_rxcui <- MGB_rxnorm_mapping %>%
  filter(
    !is.na(RxCUI),
    RxCUI != "",
    !likely_non_drug
  ) %>%
  distinct(
    RxCUI
  )

cat(
  "Unique RxCUIs:",
  nrow(unique_rxcui),
  "\n"
)


###############################################################
## Step 5. Retrieve TTY for each unique RxCUI
###############################################################

if (file.exists(tty_checkpoint_file)) {
  
  message(
    "Loading existing TTY checkpoint..."
  )
  
  RxNorm_TTY_lookup <-
    readRDS(
      tty_checkpoint_file
    )
  
} else {
  
  batch_size <- 500
  
  tty_results <- list()
  
  for (
    start_i in seq(
      1,
      nrow(unique_rxcui),
      by = batch_size
    )
  ) {
    
    end_i <- min(
      start_i + batch_size - 1,
      nrow(unique_rxcui)
    )
    
    cat(
      "TTY lookup:",
      start_i,
      "to",
      end_i,
      "of",
      nrow(unique_rxcui),
      "\n"
    )
    
    ###########################################################
    ## Important:
    ## directly extract RxCUI vector
    ###########################################################
    
    batch_ids <-
      unique_rxcui$RxCUI[
        start_i:end_i
      ]
    
    batch_result <-
      purrr::map_dfr(
        batch_ids,
        function(id) {
          
          Sys.sleep(0.1)
          
          rxnorm_get_properties(
            id
          )
        }
      )
    
    tty_results[[
      length(tty_results) + 1
    ]] <- batch_result
    
    RxNorm_TTY_lookup <-
      bind_rows(
        tty_results
      ) %>%
      distinct(
        RxCUI,
        .keep_all = TRUE
      )
    
    saveRDS(
      RxNorm_TTY_lookup,
      tty_checkpoint_file
    )
  }
}

###############################################################
## Step 6. QC of RxNorm concept types
###############################################################

RxNorm_TTY_summary <-
  RxNorm_TTY_lookup %>%
  count(
    concept_TTY,
    sort = TRUE
  ) %>%
  mutate(
    Percent =
      round(
        100 * n / sum(n),
        2
      )
  )

print(
  RxNorm_TTY_summary
)


###############################################################
## Step 7. Build ingredient lookup
##
## Logic:
##
## If concept itself is IN:
##     use itself as ingredient
##
## Otherwise:
##     query related IN
###############################################################

if (file.exists(ingredient_checkpoint_file)) {
  
  message(
    "Loading existing ingredient checkpoint..."
  )
  
  RxNorm_IN_lookup <-
    readRDS(
      ingredient_checkpoint_file
    )
  
} else {
  
  batch_size <- 500
  
  ingredient_results <- list()
  
  for (
    start_i in seq(
      1,
      nrow(RxNorm_TTY_lookup),
      by = batch_size
    )
  ) {
    
    end_i <- min(
      start_i + batch_size - 1,
      nrow(RxNorm_TTY_lookup)
    )
    
    cat(
      "Ingredient lookup:",
      start_i,
      "to",
      end_i,
      "of",
      nrow(RxNorm_TTY_lookup),
      "\n"
    )
    
    batch <-
      RxNorm_TTY_lookup[
        start_i:end_i,
      ]
    
    batch_result <-
      purrr::map_dfr(
        seq_len(nrow(batch)),
        function(i) {
          
          rxcui_i <-
            batch$RxCUI[i]
          
          tty_i <-
            batch$concept_TTY[i]
          
          name_i <-
            batch$concept_name[i]
          
          #####################################################
          ## Case 1:
          ## concept itself is already an ingredient
          #####################################################
          
          if (
            !is.na(tty_i) &&
            tty_i == "IN"
          ) {
            
            return(
              tibble(
                RxCUI =
                  rxcui_i,
                
                ingredient_RxCUI =
                  rxcui_i,
                
                ingredient_name =
                  name_i,
                
                ingredient_TTY =
                  "IN",
                
                ingredient_source =
                  "Self IN"
              )
            )
          }
          
          
          #####################################################
          ## Case 2:
          ## retrieve related ingredient(s)
          #####################################################
          
          Sys.sleep(0.1)
          
          ing <-
            rxnorm_get_related_IN(
              rxcui_i
            )
          
          if (
            nrow(ing) == 0 ||
            all(
              is.na(
                ing$ingredient_name
              )
            )
          ) {
            
            return(
              tibble(
                RxCUI =
                  rxcui_i,
                
                ingredient_RxCUI =
                  NA_character_,
                
                ingredient_name =
                  NA_character_,
                
                ingredient_TTY =
                  NA_character_,
                
                ingredient_source =
                  "No IN found"
              )
            )
          }
          
          ing %>%
            mutate(
              RxCUI =
                rxcui_i,
              
              ingredient_source =
                "Related IN"
            ) %>%
            select(
              RxCUI,
              ingredient_RxCUI,
              ingredient_name,
              ingredient_TTY,
              ingredient_source
            )
        }
      )
    
    ingredient_results[[
      length(ingredient_results) + 1
    ]] <-
      batch_result
    
    RxNorm_IN_lookup <-
      bind_rows(
        ingredient_results
      ) %>%
      distinct()
    
    saveRDS(
      RxNorm_IN_lookup,
      ingredient_checkpoint_file
    )
  }
}


###############################################################
## Step 8. Merge TTY and ingredient mapping
###############################################################

RxNorm_concept_dictionary <-
  RxNorm_TTY_lookup %>%
  
  left_join(
    RxNorm_IN_lookup,
    by = "RxCUI"
  )


###############################################################
## Step 9. Merge ingredient mapping back to medication strings
###############################################################

MGB_rxnorm_ingredient_long <-
  MGB_rxnorm_mapping %>%
  
  left_join(
    RxNorm_concept_dictionary,
    by = "RxCUI"
  )


###############################################################
## Step 10. Create final candidate DrugName
##
## At this point:
##
## final_DrugName_candidate = ingredient_name
##
## Approximate mappings still require manual review.
###############################################################

MGB_rxnorm_ingredient_long <-
  MGB_rxnorm_ingredient_long %>%
  
  mutate(
    
    final_DrugName_candidate =
      ingredient_name,
    
    ingredient_mapping_status =
      case_when(
        
        likely_non_drug ~
          "Likely non-drug",
        
        is.na(RxCUI) ~
          "No RxNorm mapping",
        
        !is.na(ingredient_name) &
          mapping_method ==
          "Exact/Normalized" ~
          "High-confidence ingredient",
        
        !is.na(ingredient_name) &
          mapping_method ==
          "Approximate" ~
          "Approximate ingredient - review",
        
        is.na(ingredient_name) ~
          "No IN ingredient",
        
        TRUE ~
          "Review"
      )
  )


###############################################################
## Step 11. Overall ingredient QC
###############################################################

ingredient_level_QC <-
  MGB_rxnorm_ingredient_long %>%
  
  summarise(
    
    total_unique_medication_strings =
      n_distinct(
        Medication
      ),
    
    mapped_RxNorm =
      n_distinct(
        Medication[
          !is.na(
            RxCUI
          )
        ]
      ),
    
    with_IN =
      n_distinct(
        Medication[
          !is.na(
            ingredient_name
          )
        ]
      ),
    
    exact_with_IN =
      n_distinct(
        Medication[
          mapping_method ==
            "Exact/Normalized" &
            !is.na(
              ingredient_name
            )
        ]
      ),
    
    approximate_with_IN =
      n_distinct(
        Medication[
          mapping_method ==
            "Approximate" &
            !is.na(
              ingredient_name
            )
        ]
      ),
    
    without_IN =
      n_distinct(
        Medication[
          !is.na(
            RxCUI
          ) &
            is.na(
              ingredient_name
            )
        ]
      ),
    
    likely_non_drug =
      n_distinct(
        Medication[
          likely_non_drug
        ]
      )
  )

print(
  ingredient_level_QC
)


###############################################################
## Step 12. Unique ingredient count
###############################################################

ingredient_summary <-
  MGB_rxnorm_ingredient_long %>%
  
  filter(
    !is.na(
      ingredient_name
    )
  ) %>%
  
  group_by(
    ingredient_RxCUI,
    ingredient_name
  ) %>%
  
  summarise(
    
    n_source_medication_strings =
      n_distinct(
        Medication
      ),
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    desc(
      n_source_medication_strings
    )
  )


###############################################################
## Step 13. Targeted QC: common brand names
###############################################################

brand_QC <-
  MGB_rxnorm_ingredient_long %>%
  
  filter(
    str_detect(
      str_to_lower(
        Medication
      ),
      paste(
        c(
          "cozaar",
          "norvasc",
          "sinemet",
          "rytary",
          "mirapex",
          "requip",
          "neupro"
        ),
        collapse = "|"
      )
    )
  ) %>%
  
  select(
    Medication,
    Medication_query,
    RxCUI,
    RxNorm_Name,
    concept_TTY,
    mapping_method,
    approx_score,
    ingredient_RxCUI,
    ingredient_name,
    ingredient_source
  ) %>%
  
  distinct() %>%
  
  arrange(
    Medication,
    ingredient_name
  )


###############################################################
## Step 14. Approximate mappings requiring manual review
###############################################################

approx_ingredient_review <-
  MGB_rxnorm_ingredient_long %>%
  
  filter(
    mapping_method ==
      "Approximate",
    !likely_non_drug
  ) %>%
  
  select(
    Medication,
    Medication_query,
    approx_score,
    approx_candidate_name,
    RxCUI,
    RxNorm_Name,
    concept_TTY,
    ingredient_RxCUI,
    ingredient_name,
    ingredient_source
  ) %>%
  
  distinct() %>%
  
  arrange(
    desc(
      approx_score
    )
  )


###############################################################
## Step 15. Records with RxNorm concept but no ingredient
###############################################################

no_IN_review <-
  MGB_rxnorm_ingredient_long %>%
  
  filter(
    !is.na(
      RxCUI
    ),
    is.na(
      ingredient_name
    ),
    !likely_non_drug
  ) %>%
  
  select(
    Medication,
    Medication_query,
    RxCUI,
    RxNorm_Name,
    concept_TTY,
    mapping_method,
    approx_score
  ) %>%
  
  distinct()


###############################################################
## Step 16. Save outputs
###############################################################

saveRDS(
  MGB_rxnorm_ingredient_long,
  ingredient_final_file
)


write.xlsx(
  list(
    
    TTY_summary =
      RxNorm_TTY_summary,
    
    Ingredient_QC =
      ingredient_level_QC,
    
    Ingredient_summary =
      ingredient_summary,
    
    Brand_QC =
      brand_QC,
    
    Approx_review =
      approx_ingredient_review,
    
    No_IN_review =
      no_IN_review
    
  ),
  
  ingredient_excel_file,
  
  overwrite = TRUE
)


###############################################################
## Step 17. Console summary
###############################################################

cat("\n")
cat("====================================================\n")
cat("RxNorm ingredient-level normalization complete\n")
cat("====================================================\n")

print(
  RxNorm_TTY_summary
)

print(
  ingredient_level_QC
)

cat(
  "\nUnique ingredient names:",
  n_distinct(
    MGB_rxnorm_ingredient_long$
      ingredient_name,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Output RDS:",
  ingredient_final_file,
  "\n"
)

cat(
  "QC workbook:",
  ingredient_excel_file,
  "\n"
)

cat("====================================================\n")

###############################################################
## MGB medication history -> ingredient-level drug library
##
## IMPORTANT:
## Preserve original medication record identity.
##
## One original medication record may map to multiple
## ingredient-level rows for combination products.
###############################################################

library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(openxlsx)


###############################################################
## Step 1. Prepare medication-to-ingredient dictionary
###############################################################

MGB_ingredient_dictionary <- MGB_rxnorm_ingredient_long %>%
  
  filter(
    !likely_non_drug,
    !is.na(ingredient_name),
    ingredient_name != ""
  ) %>%
  
  transmute(
    
    Medication,
    
    RxCUI,
    
    RxNorm_Name,
    
    concept_TTY,
    
    mapping_method,
    
    approx_score,
    
    ingredient_RxCUI,
    
    DrugName =
      str_to_title(
        ingredient_name
      )
    
  ) %>%
  
  distinct()


###############################################################
## Step 2. Dictionary summary
###############################################################

dictionary_summary <- MGB_ingredient_dictionary %>%
  summarise(
    
    unique_raw_medications =
      n_distinct(Medication),
    
    unique_drugs =
      n_distinct(DrugName),
    
    exact_normalized_raw_terms =
      n_distinct(
        Medication[
          mapping_method ==
            "Exact/Normalized"
        ]
      ),
    
    approximate_raw_terms =
      n_distinct(
        Medication[
          mapping_method ==
            "Approximate"
        ]
      )
  )


###############################################################
## Step 3. Prepare original patient-level medication records
##
## Add medication_record_id BEFORE ingredient expansion.
###############################################################

MGB_medication_raw <- Med_all %>%
  
  mutate(
    
    medication_record_id =
      row_number(),
    
    EMPI =
      as.character(EMPI),
    
    Medication =
      str_trim(
        as.character(Medication)
      ),
    
    Medication_Date =
      as.Date(
        Medication_Date,
        format = "%m/%d/%Y"
      )
    
  ) %>%
  
  filter(
    !is.na(EMPI),
    !is.na(Medication),
    Medication != ""
  )


###############################################################
## Step 4. Merge ingredient dictionary back to patient history
##
## Combination drugs naturally expand into multiple rows.
##
## Example:
##
## record 100:
## Hyzaar
##
## becomes:
## 100 -> Losartan
## 100 -> Hydrochlorothiazide
###############################################################

MGB_medication_ingredient_history <- MGB_medication_raw %>%
  
  left_join(
    MGB_ingredient_dictionary,
    by = "Medication"
  )


###############################################################
## Step 5. Keep successfully standardized medication records
##
## DO NOT collapse same patient/date/drug yet.
###############################################################

drug_library_separated_MGB <- MGB_medication_ingredient_history %>%
  
  filter(
    !is.na(DrugName),
    DrugName != ""
  ) %>%
  
  select(
    
    medication_record_id,
    
    EMPI,
    
    Medication_Date,
    
    Medication,
    
    DrugName,
    
    ingredient_RxCUI,
    
    RxCUI,
    
    RxNorm_Name,
    
    concept_TTY,
    
    mapping_method,
    
    approx_score
    
  ) %>%
  
  distinct(
    medication_record_id,
    DrugName,
    .keep_all = TRUE
  )


###############################################################
## Step 6. Combination-drug QC
##
## IMPORTANT:
## Do not summarize the full patient-level table.
## Use the medication dictionary instead.
###############################################################

combination_record_summary <- MGB_ingredient_dictionary %>%
  group_by(
    Medication
  ) %>%
  summarise(
    n_ingredients = n_distinct(DrugName),
    
    ingredients = paste(
      sort(unique(DrugName)),
      collapse = " / "
    ),
    
    .groups = "drop"
  ) %>%
  filter(
    n_ingredients >= 2
  )


###############################################################
## Step 7. Patient-level medication coverage QC
###############################################################

medication_history_QC <- MGB_medication_ingredient_history %>%
  
  summarise(
    
    total_original_records =
      n_distinct(
        medication_record_id
      ),
    
    mapped_original_records =
      n_distinct(
        medication_record_id[
          !is.na(DrugName)
        ]
      ),
    
    unmapped_original_records =
      n_distinct(
        medication_record_id[
          is.na(DrugName)
        ]
      ),
    
    mapping_percent =
      round(
        100 *
          n_distinct(
            medication_record_id[
              !is.na(DrugName)
            ]
          ) /
          n_distinct(
            medication_record_id
          ),
        2
      ),
    
    unique_patients =
      n_distinct(EMPI),
    
    patients_with_mapped_drug =
      n_distinct(
        EMPI[
          !is.na(DrugName)
        ]
      )
    
  )


###############################################################
## Step 8. Drug-level summary
##
## Here n_records means original medication records
## contributing to each ingredient.
###############################################################

drug_level_summary <- drug_library_separated_MGB %>%
  
  group_by(
    DrugName
  ) %>%
  
  summarise(
    
    n_patients =
      n_distinct(EMPI),
    
    n_original_records =
      n_distinct(
        medication_record_id
      ),
    
    n_dates =
      n_distinct(
        paste(
          EMPI,
          Medication_Date
        )
      ),
    
    first_date =
      min(
        Medication_Date,
        na.rm = TRUE
      ),
    
    last_date =
      max(
        Medication_Date,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
    
  ) %>%
  
  arrange(
    desc(n_patients)
  )


###############################################################
## Step 9. Approximate mappings for lightweight manual review
##
## Prioritize by number of patients and original records.
###############################################################

lightweight_review_table <- MGB_medication_ingredient_history %>%
  
  filter(
    mapping_method ==
      "Approximate",
    
    !is.na(DrugName)
  ) %>%
  
  group_by(
    
    Medication,
    
    RxNorm_Name,
    
    DrugName,
    
    RxCUI,
    
    approx_score
    
  ) %>%
  
  summarise(
    
    n_patients =
      n_distinct(EMPI),
    
    n_original_records =
      n_distinct(
        medication_record_id
      ),
    
    .groups =
      "drop"
    
  ) %>%
  
  arrange(
    desc(n_patients),
    desc(n_original_records),
    desc(approx_score)
  ) %>%
  
  mutate(
    
    manual_status =
      NA_character_,
    
    manual_DrugName =
      NA_character_,
    
    manual_note =
      NA_character_
    
  )


###############################################################
## Step 10. Key-drug targeted review
###############################################################

key_drug_review <- MGB_medication_ingredient_history %>%
  
  filter(
    str_detect(
      str_to_lower(
        paste(
          Medication,
          DrugName
        )
      ),
      paste(
        c(
          "losartan",
          "cozaar",
          "hyzaar",
          "amlodipine",
          "norvasc",
          "levodopa",
          "carbidopa",
          "sinemet",
          "rytary",
          "pramipexole",
          "mirapex",
          "ropinirole",
          "requip",
          "rotigotine",
          "neupro",
          "apomorphine"
        ),
        collapse = "|"
      )
    )
  ) %>%
  
  group_by(
    
    Medication,
    
    RxNorm_Name,
    
    DrugName,
    
    mapping_method,
    
    approx_score
    
  ) %>%
  
  summarise(
    
    n_patients =
      n_distinct(EMPI),
    
    n_original_records =
      n_distinct(
        medication_record_id
      ),
    
    .groups =
      "drop"
    
  ) %>%
  
  arrange(
    DrugName,
    desc(n_patients)
  )


###############################################################
## Step 11. Suspicious multi-ingredient mappings
##
## Flag only.
###############################################################

multi_ingredient_review <- MGB_ingredient_dictionary %>%
  
  group_by(
    
    Medication,
    
    RxCUI,
    
    RxNorm_Name,
    
    mapping_method,
    
    approx_score
    
  ) %>%
  
  summarise(
    
    n_ingredients =
      n_distinct(DrugName),
    
    ingredients =
      paste(
        sort(
          unique(DrugName)
        ),
        collapse = " / "
      ),
    
    .groups =
      "drop"
    
  ) %>%
  
  filter(
    n_ingredients >= 2
  ) %>%
  
  mutate(
    
    obvious_combo_text =
      str_detect(
        str_to_lower(Medication),
        paste(
          c(
            "\\+",
            "/",
            "\\band\\b",
            "\\bwith\\b",
            "\\bcombo\\b"
          ),
          collapse = "|"
        )
      ),
    
    review_flag =
      mapping_method ==
      "Approximate" &
      !obvious_combo_text
    
  ) %>%
  
  arrange(
    desc(review_flag),
    approx_score
  )


###############################################################
## Step 12. Save final objects
###############################################################

saveRDS(
  MGB_ingredient_dictionary,
  "./data/processed_data/MGB_Biobank/RxNorm/MGB_ingredient_dictionary.rds"
)

saveRDS(
  MGB_medication_ingredient_history,
  "./data/processed_data/MGB_Biobank/RxNorm/MGB_medication_ingredient_history.rds"
)

saveRDS(
  drug_library_separated_MGB,
  "./data/processed_data/MGB_Biobank/RxNorm/drug_library_separated_MGB_RxNorm.rds"
)


###############################################################
## Step 13. Save lightweight review workbook
###############################################################

write.xlsx(
  
  list(
    
    Drug_summary =
      drug_level_summary,
    
    Combination_records =
      combination_record_summary,
    
    Approximate_review =
      lightweight_review_table,
    
    Key_drug_review =
      key_drug_review,
    
    Multi_ingredient_review =
      multi_ingredient_review
    
  ),
  
  "./data/processed_data/MGB_Biobank/RxNorm/MGB_drug_library_lightweight_review.xlsx",
  
  overwrite = TRUE
)


###############################################################
## Step 14. Final summary
###############################################################

cat("\n")
cat("====================================================\n")
cat("MGB ingredient-level medication library complete\n")
cat("====================================================\n")

print(
  medication_history_QC
)

cat(
  "\nUnique DrugNames:",
  n_distinct(
    drug_library_separated_MGB$DrugName
  ),
  "\n"
)

cat(
  "Unique patients:",
  n_distinct(
    drug_library_separated_MGB$EMPI
  ),
  "\n"
)

cat(
  "Original medication records represented:",
  n_distinct(
    drug_library_separated_MGB$
      medication_record_id
  ),
  "\n"
)

cat("====================================================\n")