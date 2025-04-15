# ReRx_drug_repurposing_multicohort

This dataset provides scripts, data, and other relevant information on the systematic analysis of the study entitled **"Drug Repurposing for Parkinson’s Disease: A Large-Scale Multi-Cohort Study"**, which aims to identify drugs associated with the risk and progression of PD to provide insights into potential therapeutics and targets. 

We leveraged data from the Mass General Brigham (MGB) Biobank for discovery and the Accelerating Medicines Partnership Parkinson’s Disease (AMP-PD) program for replication. Specifically, two largest AMP-PD sub-cohorts (PPMI and PDBP) were integrated to forge the replication cohort.

**This readme file was generated on 2025-04-14 by Yuxuan Hu with the help of Weiqiang Liu and Xianjun Dong (PI).**

**Xianjun Dong, PhD**  
Associate Professor, Departments of Neurology and Biomedical Informatics and Data Science  
Yale School of Medicine’s Stephen & Denise Adams Center for Parkinson’s Disease Research  
100 College Street  
New Haven, CT 06510  
Email: [xianjun.dong@yale.edu](mailto:xianjun.dong@yale.edu)  
ORCID: [0000-0002-8052-9320](https://orcid.org/0000-0002-8052-9320)

## 🗓️ Date of Raw Data Collection

- **PPMI**: 2023-10-01  
- **PDBP**: 2023-12-04  
- **MGB Biobank**: 2024-06-21  
- **atccodes**: 2024-12-01  
- **export_Touchstone**: 2025-02-07  

## 🧰 Software Dependencies

Tested with **R 4.4.1**. Required R packages:

```r
library(broom)
library(broom.mixed)
library(cobalt)
library(DESeq2)
library(dplyr)
library(edgeR)
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
library(Seurat)
library(stringr)
library(tidyr)
library(tidyverse)

~/project/ReRx/
│
├── data
│   ├── raw_data
│   │   ├── atccodes.csv
│   │   ├── export_Touchstone.txt
│   │   ├── MGB_Biobank/
│   │   ├── PDBP/
│   │   └── PPMI/
│   └── processed_data
│       ├── drug_dictionary_MGB.csv
│       ├── drug_dictionary_update_PDBP.xlsx
│       └── drug_dictionary_update_PPMI.xlsx
│
├── results
│   ├── PD_risk_results_AMPPD.xlsx
│   ├── PD_risk_results_MGB.xlsx
│   ├── AMPPD_with_ATC_update.xlsx
│   └── MGB_with_ATC_update.xlsx
│
├── scripts
│   ├── Characteristic.R
│   ├── Drug_dictionary.R
│   ├── Medication.R
│   ├── PD_progression.R
│   ├── PD_risk.R
│   ├── Replication.R
│   ├── Sensitivity_analysis.R
│   └── Target_genes.R
│
└── ReRx.txt
