##### Data cleaning of medication history records from MGB Biobank

### =======
## PD cases
### =======

setwd("~/project/ReRx/")

process_missing_values <- function(line) {
  split_line <- unlist(strsplit(line, "\\|"))
  split_line <- ifelse(trimws(split_line) == "", NA, split_line)
  return(split_line)
}

data1 <- readLines("./data/raw_data/MGB_Biobank/PD_patients/1/XD010_20240621_170059-1_Med.txt")
header <- unlist(strsplit(data1[1], "\\|"))
data1 <- data1[-1]
data1 <- do.call(rbind, lapply(data1, process_missing_values))
Med1 <- as.data.frame(data1, stringsAsFactors = FALSE)
colnames(Med1) <- header

data2 <- readLines("./data/raw_data/MGB_Biobank/PD_patients/2/XD010_20240621_170059-2_Med.txt")
data2 <- data2[-1]
data2 <- do.call(rbind, lapply(data2, process_missing_values))
Med2 <- as.data.frame(data2, stringsAsFactors = FALSE)
colnames(Med2) <- header

Med <- bind_rows(Med1, Med2)

Med[is.na(Med)] <- ""

# Category by Code_Type
unique_code_types <- unique(Med$Code_Type)

# Split sub-data sets
setwd("./data/processed_data/MGB_Biobank/PD/")
for (code_type in unique_code_types) {
  subset_data <- Med %>% filter(Code_Type == code_type)
  file_name <- paste0(code_type, ".txt")
  write.table(subset_data, file_name, sep="|", row.names=FALSE, col.names=TRUE, quote=FALSE)
  assign(paste0("Med_", code_type), subset_data, envir = .GlobalEnv)
}

# Extract medication/drug_name with different rules #

# specific rule for EPIC-PRC
unique_code_types_EPIC_PRC_with_Medication_extracted <- read.csv('unique_code_types_EPIC-PRC.csv')
Med_EPIC_PRC_with_Medication_extracted <- left_join(`Med_EPIC-PRC`, unique_code_types_EPIC_PRC_with_Medication_extracted, by = "Medication")

# specific rule for HCPCS
extract_medication <- function(medication) {
  if (str_detect(medication, "^(Inj|Injection|Infusion)")) {
    
    words <- str_split(medication, ",\\s*")[[1]]
    if (length(words) > 1) {
      second_part <- words[2]
      return(str_split(second_part, "\\s+")[[1]][1])
    } else {
      
      words <- str_split(medication, "\\s+")[[1]]
      if (length(words) > 1) {
        return(words[2])
      } else {
        return(NA_character_)
      }
    }
  } else {
    
    words <- str_split(medication, "\\s+")[[1]]
    return(words[1])
  }
}

Med_HCPCS_with_Medication_extracted <- Med_HCPCS %>%
  rowwise() %>%
  mutate(Medication_extracted = extract_medication(Medication)) %>%
  ungroup()

# specific rule for others
extract_medication <- function(medication) {
  words <- str_split(medication, "\\s+")[[1]]
  return(words[1])
}

all_data_frames <- ls()

code_types_to_process <- all_data_frames[grepl("^Med_", all_data_frames) & !grepl("Med_HCPCS|Med_EPIC-PRC|_with_Medication_extracted$", all_data_frames)]

for (df_name in code_types_to_process) {
  
  df <- get(df_name)
  
  df <- df %>%
    rowwise() %>%
    mutate(Medication_extracted = extract_medication(Medication)) %>%
    ungroup()
  
  new_df_name <- paste0(df_name, "_with_Medication_extracted")
  
  assign(new_df_name, df)
}

# merge all the Med files
all_data_frames <- ls()
data_frames_to_merge <- all_data_frames[grepl("^Med_.*_with_Medication_extracted$", all_data_frames)]
all_dfs <- list()

for (Med_name in data_frames_to_merge) {
  dfs <- get(Med_name)
  all_dfs[[Med_name]] <- dfs
}

Med_PD_with_Medication_extracted <- bind_rows(all_dfs) %>%
  mutate(Medication_extracted = str_trim(Medication_extracted)) %>%
  mutate(Medication_extracted = str_to_title(Medication_extracted)) %>%
  mutate(Medication_extracted = ifelse(grepl("-[oO]ncall", Medication_extracted), word(Medication_extracted, 1, sep = "-[oO]ncall"), Medication_extracted))

Med_PD_with_Medication_extracted_unique <- unique(Med_PD_with_Medication_extracted$Medication_extracted)

### ===============
## Healthy controls
### ===============
setwd("~/project/ReRx/")

data1 <- readLines("./data/raw_data/MGB_Biobank/health_control/1/XD010_20240630_131521-1_Med.txt")
header <- unlist(strsplit(data1[1], "\\|"))
data1 <- data1[-1]
data1 <- do.call(rbind, lapply(data1, process_missing_values))
Med1 <- as.data.frame(data1, stringsAsFactors = FALSE)
colnames(Med1) <- header

data2 <- readLines("./data/raw_data/MGB_Biobank/health_control/2/XD010_20240630_131521-2_Med.txt")
data2 <- data2[-1]
data2 <- do.call(rbind, lapply(data2, process_missing_values))
Med2 <- as.data.frame(data2, stringsAsFactors = FALSE)
colnames(Med2) <- header

data3 <- readLines("./data/raw_data/MGB_Biobank/health_control/3/XD010_20240630_131521-3_Med.txt")
data3 <- data3[-1]
data3 <- do.call(rbind, lapply(data3, process_missing_values))
Med3 <- as.data.frame(data3, stringsAsFactors = FALSE)
colnames(Med3) <- header

Med <- bind_rows(Med1, Med2, Med3)

Med[is.na(Med)] <- ""

# Category by Code_Type
unique_code_types <- unique(Med$Code_Type)
print(unique_code_types)

# Split sub-data sets
setwd("./project/data/processed_data/MGB_Biobank/HC/")

for (code_type in unique_code_types) {
  subset_data <- Med %>% filter(Code_Type == code_type)
  file_name <- paste0(code_type, ".txt")
  write.table(subset_data, file_name, sep="|", row.names=FALSE, col.names=TRUE, quote=FALSE)
  assign(paste0("Med_", code_type), subset_data, envir = .GlobalEnv)
}

# Extract medication/drug_name with different rules #

# specific rule for EPIC-PRC
unique_code_types_EPIC_PRC_with_Medication_extracted <- read.csv('unique_Med_EPIC_PRC.csv')
Med_EPIC_PRC_with_Medication_extracted <- left_join(`Med_EPIC-PRC`, unique_code_types_EPIC_PRC_with_Medication_extracted, by = "Medication")

# specific rule for HCPCS
extract_medication <- function(medication) {
  if (str_detect(medication, "^(Inj|Injection|Infusion)")) {
    
    words <- str_split(medication, ",\\s*")[[1]]
    if (length(words) > 1) {
      second_part <- words[2]
      return(str_split(second_part, "\\s+")[[1]][1])
    } else {
      
      words <- str_split(medication, "\\s+")[[1]]
      if (length(words) > 1) {
        return(words[2])
      } else {
        return(NA_character_)
      }
    }
  } else {
    
    words <- str_split(medication, "\\s+")[[1]]
    return(words[1])
  }
}

Med_HCPCS_with_Medication_extracted <- Med_HCPCS %>%
  rowwise() %>%
  mutate(Medication_extracted = extract_medication(Medication)) %>%
  ungroup()

# specific rule for others
extract_medication <- function(medication) {
  words <- str_split(medication, "\\s+")[[1]]
  return(words[1])
}

all_data_frames <- ls()

code_types_to_process <- all_data_frames[grepl("^Med_", all_data_frames) & !grepl("Med_HCPCS|Med_EPIC-PRC|_with_Medication_extracted$", all_data_frames)]

for (df_name in code_types_to_process) {
  
  df <- get(df_name)
  
  df <- df %>%
    rowwise() %>%
    mutate(Medication_extracted = extract_medication(Medication)) %>%
    ungroup()
  
  new_df_name <- paste0(df_name, "_with_Medication_extracted")
  
  assign(new_df_name, df)
}

# merge all the Med files
all_data_frames <- ls()
data_frames_to_merge <- all_data_frames[grepl("^Med_.*_with_Medication_extracted$", all_data_frames)]
all_dfs <- list()

for (Med_name in data_frames_to_merge) {
  dfs <- get(Med_name)
  all_dfs[[Med_name]] <- dfs
}

Med_health_with_Medication_extracted <- bind_rows(all_dfs) %>%
  mutate(Medication_extracted = str_trim(Medication_extracted)) %>%
  mutate(Medication_extracted = str_to_title(Medication_extracted)) %>%
  mutate(Medication_extracted = ifelse(grepl("-[oO]ncall", Medication_extracted), word(Medication_extracted, 1, sep = "-[oO]ncall"), Medication_extracted))

Med_health_with_Medication_extracted_unique <- unique(Med_health_with_Medication_extracted$Medication_extracted)

### ===========================
## Prepare the medication table
### ===========================

# merge all the unique Medication_extracted
Med_all_with_Medication_extracted_unique <- bind_rows(Med_PD_with_Medication_extracted_unique, Med_health_with_Medication_extracted_unique) %>% 
  distinct()

# match drug names
drug_dictionary_MGB <- read.csv('drug_dictionary_MGB.csv')
Med_all_with_Medication_extracted <- bind_rows(Med_health_with_Medication_extracted, Med_PD_with_Medication_extracted)

MGB_medication <- merge(Med_all_with_Medication_extracted, drug_dictionary_MGB, by = "Medication_extracted", all.x = TRUE) %>%
  select(EMPI, Medication_Date, Medication, Medication_extracted, DrugName)
MGB_medication_final <- MGB_medication[!is.na(MGB_medication$DrugName) & MGB_medication$DrugName != "", ]

# split and count
drug_library_separated_MGB <- MGB_medication_final %>%
  separate_rows(DrugName, sep = "/") %>%
  mutate(DrugName = trimws(DrugName),
         DrugName = str_to_title(DrugName)) %>%
  filter(!is.na(DrugName), DrugName != "", DrugName != "Unclear")



##### Data cleaning of medication history records from two cohorts of AMP-PD

### ================================================
## PPMI (Parkinson’s Progression Markers Initiative)
### ================================================

setwd("~/project/ReRx/")
PPMI_medication_raw <- read.csv("./data/raw_data/PPMI/Concomitant_Medication_Log_27Sep2023.csv") %>%
  select(PATNO, CMTRT, STARTDT) %>%
  mutate(DT = str_sub(STARTDT, start = -4))

# eliminate redundancy of drug names
PPMI_drug_dictionary_raw <- PPMI_medication_raw %>%
  select(CMTRT) %>%
  distinct(CMTRT, .keep_all = TRUE)

# match DrugName
PPMI_medication_index <- read_excel("./data/processed_data/PPMI/drug_dictionary_update_PPMI.xlsx")
PPMI_medication_final <- PPMI_medication_raw
get_active_ingredients <- function(medications) {
  indices <- match(unlist(strsplit(medications, ";")), PPMI_medication_index$CMTRT)
  ingredients <- ifelse(is.na(indices), NA, PPMI_medication_index$DrugName[indices])
  paste(na.omit(ingredients), collapse = ";")
}
PPMI_medication_final$DrugName <- sapply(PPMI_medication_final$CMTRT, get_active_ingredients)

# split
PPMI_drug_library_separated <- PPMI_medication_final %>%
  separate_rows(DrugName, sep = ",") %>%
  mutate(DrugName = trimws(DrugName)) %>%
  filter(!is.na(DrugName), DrugName != "", DrugName != "Unclear") %>%
  mutate(medication_date = as.Date(paste0("01/", STARTDT), format = "%d/%m/%Y"))

### ============================================
## PDBP (Parkinson's Disease Biomarkers Program)
### ============================================

# restructure misaligned data
PDBP_medication_history <- read_excel("./data/raw_data/PDBP/Medication_history.xlsx")
colnames(PDBP_medication_history)[colnames(PDBP_medication_history) == "Study ID"] <- "Study_ID"
colnames(PDBP_medication_history)[colnames(PDBP_medication_history) == "PriorAndConcomitantMeds.Required Fields.GUID"] <- "PATNO"
colnames(PDBP_medication_history)[colnames(PDBP_medication_history) == "PriorAndConcomitantMeds.Other Medications.MedctnPriorConcomName"] <- "CMTRT"
colnames(PDBP_medication_history)[colnames(PDBP_medication_history) == "PriorAndConcomitantMeds.Required Fields.VisitTypPDBP"] <- "EVENT_ID"
PDBP_medication_history_fill1 <- tidyr::fill(PDBP_medication_history, Study_ID, .direction = "down")
PDBP_medication_history_fill2 <- tidyr::fill(PDBP_medication_history_fill1, PATNO, .direction = "down")
PDBP_medication_history_fill3 <- tidyr::fill(PDBP_medication_history_fill2, EVENT_ID, .direction = "down")
PDBP_medication_history_filled <- na_if(PDBP_medication_history_fill3, NA)

# 207 (Study_ID) rule
colnames(PDBP_medication_history_filled)[colnames(PDBP_medication_history_filled) == "PriorAndConcomitantMeds.Other Medications.MedctnPriorConcomIndTxt"] <- "disease"
special_characters_207 <- c("QHS", "BID", "QD", "QW", "PRN", "TID", "5QD", "8QD", "up to 5 PRN", "QD 6 days/week", "QDam", "IHQD", "2x BID", "4x PRN", "5xday", "1/2QID", "1/2 pill", "2 QD", "3 QD", "1 every other day", "5/day", "2 x 5QD", "3QW", "2-5 /day", "1/2QD", "2QD", "4x/week", "5QW", "6QD", "1 per 6mo", "5 times a week", "6/day", "2Qam", "2BID", "9 per week", "1.5 pills x QID", "1 per month", "4QW", "3x/week", "10QD", "2/week", "Once per week", "Once a week", "BID 5x/week", "once weekly", "every other day", "Every 6 days", "QID")
PDBP_medication_history_filled_trans <- apply(PDBP_medication_history_filled, 1, function(row) {
  if (row["CMTRT"] %in% special_characters_207) {
    return(row["disease"])
  } else {
    return(row["CMTRT"])
  }
})
PDBP_medication_history_filled_trans_207 <- cbind(PDBP_medication_history_filled, NewColumn = PDBP_medication_history_filled_trans)
colnames(PDBP_medication_history_filled_trans_207)[colnames(PDBP_medication_history_filled_trans_207) == "NewColumn"] <- "CMTRT_new"
PDBP_medication_history_filled_trans_207 <- na_if(PDBP_medication_history_filled_trans_207, NA)

# 205 (Study_ID)rule
colnames(PDBP_medication_history_filled_trans_207)[colnames(PDBP_medication_history_filled_trans_207) == "PriorAndConcomitantMeds.Other Medications.MedctnPriorConcomRteTyp"] <- "W"
colnames(PDBP_medication_history_filled_trans_207)[colnames(PDBP_medication_history_filled_trans_207) == "PriorAndConcomitantMeds.Other Medications.MedctnPriorConcomDoseMsrTxt"] <- "T"
special_characters_205 <- c("five times a day", "BID", "PRN", "QD", "QHS", "once a week", "TID", "6QD", "QID", "ocular", "seven times a day", "every 3 months", "qod", "5QD", "1 hr prior to dentist", "twice per year", "QAW", "QPM")
PDBP_medication_history_filled_trans_new <- apply(PDBP_medication_history_filled_trans_207, 1, function(row) {
  if (row["W"] %in% special_characters_205) {
    return(row["T"])
  } else {
    return(row["CMTRT_new"])
  }
})
PDBP_medication_history_filled_trans_final <- cbind(PDBP_medication_history_filled_trans_207, NewColumn = PDBP_medication_history_filled_trans_new)
colnames(PDBP_medication_history_filled_trans_final)[colnames(PDBP_medication_history_filled_trans_final) == "NewColumn"] <- "CMTRT_final"
PDBP_medication_history_filled_trans_final_new <- PDBP_medication_history_filled_trans_final[complete.cases(PDBP_medication_history_filled_trans_final$CMTRT_final), ]

# eliminate redundancy of drug names
PDBP_drug_library_raw <- PDBP_medication_history_filled_trans_final_new %>%
  distinct(CMTRT_final, .keep_all = TRUE)

# match DrugName
PDBP_medication_index <- read_excel("./data/processed_data/PDBP/drug_dictionary_update_PDBP.xlsx")
PDBP_medication_final <- PDBP_medication_history_filled_trans_final_new
get_active_ingredients <- function(medications) {
  indices <- match(unlist(strsplit(medications, ";")), PDBP_medication_index$CMTRT_final)
  ingredients <- ifelse(is.na(indices), NA, PDBP_medication_index$DrugName[indices])
  paste(na.omit(ingredients), collapse = ";")
}
PDBP_medication_final$DrugName <- sapply(PDBP_medication_final$CMTRT_final, get_active_ingredients)

# split
PDBP_drug_library_separated <- PDBP_medication_final %>%
  separate_rows(DrugName, sep = ",") %>%
  mutate(DrugName = trimws(DrugName)) %>%
  filter(!is.na(DrugName), DrugName != "", DrugName != "Unclear") %>%
  filter(EVENT_ID == "Baseline")