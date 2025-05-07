# ReRx: A toolkit for drug repurposing analysis

This code repository provides scripts on the systematic analysis of the study entitled **"Drug Repurposing for Parkinson’s Disease: A Large-Scale Multi-Cohort Study"**, which aims to identify drugs associated with the risk and progression of PD to provide insights into potential therapeutics and targets.

We leveraged data from the Mass General Brigham (MGB) Biobank for discovery and the Accelerating Medicines Partnership Parkinson’s Disease (AMP-PD) program for replication. Specifically, two largest AMP-PD sub-cohorts (PPMI and PDBP) were integrated to forge the replication cohort.

## Date of Raw Data Collection

- **PPMI**: 2023-10-01  
- **PDBP**: 2023-12-04  
- **MGB Biobank**: 2024-06-21  
- **atccodes**: 2024-12-01  
- **export_Touchstone**: 2025-02-07  

## Software Dependencies

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
```

## Analysis Process
### 1. Data Cleaning of Medication Records
The Medication.R and Drug_dictionary.R scripts are used to clean and standardize medication records from all three cohorts.

Scripts
- **`Medication.R`**:
Prepares cleaned medication datasets for downstream modeling.

- **`Drug_dictionary.R`**:
Maps medication records to generic drug names using PubChem API and DrugBank.

### 2. Drug-PD Association Analysis
Includes propensity score matching, logistic regression, replication, and sensitivity analyses.

Scripts
- **`PD_risk.R`**:
Main analysis: PSM, plots, logistic regression.

- **`Replication.R`**:
Identifies replicated drugs (by name and by ATC codes).

- **`Sensitivity_analysis.R`**:
Dose effect and reverse causality sensitivity analyses.

- **`Characteristic.R`**:
Calculates demographic characteristics.

- **`PD_progression.R`**:
Linear mixed-effects model for cognitive and motor progression.

### 3. Drug-Target Gene Expression Analysis
The **`Target_genes.R`** script maps drug target genes to cell types to explore therapeutic potential.

## Data Access and Sharing
Raw data is available upon reasonable request, subject to approval by MGB Biobank and AMP-PD.

## Citation
Publications based on this code repository should cite the associated paper and credit the authors.

## Credit 
This readme file was generated on 2025-04-14 by Yuxuan Hu with the help of Weiqiang Liu and Xianjun Dong (PI).

How to cite: [![DOI](https://zenodo.org/badge/978369804.svg)](https://doi.org/10.5281/zenodo.15360497)
