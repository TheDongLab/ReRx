library(broom)
library(broom.mixed)
library(cobalt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lme4)
library(lmerTest)
library(lubridate)
library(MatchIt)
library(Matrix)
library(meta)
library(openxlsx)
library(patchwork)
library(RColorBrewer)
library(readxl)
library(reshape2)
library(stringr)
library(tidyr)
library(tidyverse)

# Extract medication content from the CCAE sub-dataset
setwd("/data/MarketScan_data/cohort")
CCAE_medication_case <- read.csv("./CCAE_ASL_D.csv")
CCAE_medication_control <- read.csv("./CCAE_ASL_controls_D.csv")
CCAE_medication_combined <- bind_rows(CCAE_medication_case, CCAE_medication_control)
CCAE_medication_Generic_unique <- CCAE_medication_combined %>%
  select(GENERID) %>%
  filter(!is.na(GENERID)) %>%
  distinct(GENERID, .keep_all = TRUE)

Medication_Dictionary <- read.csv("/data/MarketScan_data/dictionary/REDBOOK.csv") %>%
  select(GENERID, GENNME) %>%
  distinct(GENERID, .keep_all = TRUE)

CCAE_medication_all <- merge(CCAE_medication_Generic_unique, Medication_Dictionary, by = "GENERID", all.x = TRUE)
write.csv(CCAE_medication_all, "/home/yh753/Medication/CCAE_medication_all.csv")











