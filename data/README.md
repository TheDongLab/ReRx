# Data inputs and access

No participant-level source data are distributed in this folder. A public code license does not grant access to restricted data.

The dates below are **collection dates reported in the revised [KRT](https://doi.org/10.5281/zenodo.22802221)**; they are not independently verified dataset versions or observation-period endpoints.

| Resource | Study role and reported collection date | Access route |
| --- | --- | --- |
| MGB Biobank | Discovery EHR data; 2024-06-21 | [MGB Biobank](https://www.massgeneralbrigham.org/en/research-and-innovation/participate-in-research/biobank); request access through the institution and Biobank under the applicable approvals. |
| AMP-PD | Harmonized replication resources; 2023-10-01 | [AMP-PD](https://amp-pd.org/), now redirecting to AMP-PDRD; follow its registration and applicable access agreements. |
| PPMI | Replication cohort; 2023-10-01 | [PPMI data access](https://www.ppmi-info.org/access-data-specimens/download-data); approved application and data-use agreement. |
| PDBP | Replication cohort; 2023-12-04 | [PDBP](https://pdbp.ninds.nih.gov/); follow its researcher access process and applicable agreement. |
| Merative MarketScan MDCR | TTE claims data; 2025-12-17 | [Merative real-world evidence](https://www.merative.com/real-world-evidence); obtain an institutional licence and authorized extracts. REDBOOK is an additional licensed input. |
| PD brain single-cell/nucleus data | Disease transcriptomic signatures; 2025-02-27 | Zhu et al., [doi:10.1126/scitranslmed.abo1997](https://doi.org/10.1126/scitranslmed.abo1997); use the source study's data-availability route and confirm the exact accession/object preparation. |
| Tahoe-100M | Sacubitril/valsartan perturbation signatures; 2025-09-18 | [Tahoe-100M source study](https://doi.org/10.1101/2025.02.20.639398); recover the exact release/subset and preprocessing for `tahoe.h5ad`. |
| RxNorm | Medication ingredient normalization | [NLM RxNorm APIs](https://lhncbc.nlm.nih.gov/RxNav/APIs/RxNormAPIs.html); preserve query dates, mapping caches, and curation decisions. |

Request the exact study extracts and preparation details from the corresponding author once provider access is approved. This does not replace provider authorization. The manuscript describes no new primary data collection.

## Files and schemas read by this code

These are the principal inputs found in the supplied scripts. Field names are case-sensitive. This table is an orientation to the actual code, not a complete provider data dictionary. Consult the source scripts for every read, join, selected column, and date conversion.

| Branch | Required files or objects | Required content and handoff |
| --- | --- | --- |
| MGB medication | Two `XD010_20240621_170059-{1,2}_Med.txt` exports and three `XD010_20240630_131521-{1,2,3}_Med.txt` control exports | Pipe-delimited medication exports including patient key `EMPI`, medication text, and medication dates. Preserve source record linkage for ingredient splitting. |
| MGB primary | Corresponding `_Dem.txt` and `_Dia.txt` exports for those PD/control extracts | Demographics include `EMPI`, `Date_of_Birth`, `Age`, `Gender_Legal_Sex`, `Race_Group`; diagnosis data must retain code, code type, dates, and clinic information used by the scripts. Use the delimiters specified at each read call. |
| MGB primary/validation | `drug_library_separated_MGB_RxNorm.rds` | Derived ingredient library with `EMPI`, `medication_record_id`, `Medication_Date`, `DrugName` and additional fields consumed by the code. This is participant-level data. The producing/consuming paths currently differ. |
| MGB sensitivity | `Neurologist_clinic_classification.xlsx` | Curated clinic classification used to identify neurology-supported diagnoses; exact curation provenance must be supplied. |
| PPMI | `PPMI_Curated_Data_Cut_Public_20230612.csv`, `Demographics_27Sep2023.csv`, session object `PPMI_drug_library_separated` | Participant key `PATNO`; baseline `EVENT_ID`, cohort classification and demographic covariates; `BIRTHDT` for enrollment-date derivation. Medication object needs `PATNO`, `DrugName`, `medication_date`. |
| PDBP | `PD_risk_status_follow_up.xlsx`, `Demographics.xlsx`, session object `PDBP_drug_library_separated` | Study ID, GUID and original field labels that `PD_risk.R` renames; prepared medication object with `PATNO`, `DrugName`. No preparation script for that object is supplied. |
| Replication tables | `PD_risk_results_MGB_revised.xlsx`, `PD_risk_results_AMPPD_revised.xlsx` | `DrugName`, `OR`, `CI_lower`, `CI_upper`, `Y_case`, `N_case`, `Y_ctrl`, `N_ctrl`, `p_value`; MGB also requires lowercase `fdr_value`. `Y_*`/`N_*` are ever-user/never-user counts. Effect estimates are dimensionless. |
| MDCR primary | `MDCR_hypertension_pt_final.parquet`, `MDCR_T.parquet`, `MDCR_I.parquet`, `MDCR_O_limit.parquet`, `MDCR_D.parquet`, `REDBOOK.csv` | Provider-derived hypertension, enrollment, inpatient, outpatient, and dispensing extracts. `ENROLID` is the person key; enrollment uses `DOBYR`, `SEX`, `DTSTART`, `DTEND`; drugs use `GENERID`, `SVCDATE`; dictionary uses `GENERID`, `GENNME`, `STRNGTH`. Preserve all diagnosis/service fields required by the code. |
| MDCR extensions | `outcome_ready_dataset.parquet`, `drug_dictionary.parquet`, `outcome_ready_dataset_expanded_covariates.parquet`, `class_specific_outcome_ready_with_prodromal_covariates.parquet` | Derived, version-matched cohorts with participant keys, index/risk/censor/event dates, exposure groups, and covariates; see workflow for creation order and commented exports. |
| PD brain | `PDnuclei.rds`, `meta.csv` | Seurat RNA counts with `sample4`/`population` metadata; sample table with matching `sample4`, `AGE`, `PMI`, `DIAGNOSIS`, `SEX`. The Seurat object was constructed using the script/method provided in Zhu et al., [doi:10.1126/scitranslmed.abo1997](https://doi.org/10.1126/scitranslmed.abo1997) |
| Tahoe | `tahoe.h5ad` | AnnData expression matrix; `.obs` fields `drugname_drugconc`, `cell_name`, `plate`; gene identifiers in `.var_names`. Preserve normalization/count conventions and plate matching. |
| Enrichment/plots | PD and drug DEG CSVs, then `GSEA_*_BP.csv` and `GSEA_*_KEGG.csv` | Input/output naming and column conventions are inconsistent in the supplied working file; resolve as described in the workflow. GO/KEGG resource versions and retrieval dates also need recording. |

## MarketScan Data Provenance

We sent the following data description/query to MarketScan (https://datamed.library.medicine.yale.edu/marketscan/request_form), and the MarketScan data manager generated a virtual machine (VM) for the project and made the requested data available in the VM. 

### Data Description:
For this analysis, we included patients with at least one hypertension diagnosis between 2009 to 2024 in the MarketScan data. To reduce confounding, we will limit our cohort to MDCR patients with essential (primary) hypertension (ICD-10: I10) and exclude those with secondary hypertension (ICD-10: I15.x), as the latter may involve distinct etiologies and treatment strategies that could influence Parkinson’s disease risk independently. We also needed that included patients are: (1) age ≥ 50; (2) have at least 5 year’s follow-up records and 1 year record before enrollment; (3) didn’t have PD diagnosis (G20) before they enrolled the insurance.

Within the MarketScan VM, the data generated for this request were stored at: `/data/MarketScan_data/hypertension_cohort_update`, with the following directory structure:

```
├── MDCD_hypertension_pt_final.parquet
├── MDCD_hypertension_pt_followup.parquet
├── MDCD_hypertension_pt.parquet
├── MDCD_pd_drug_date.parquet
├── MDCD_pd_index_date.parquet
├── MDCR_A.parquet
├── MDCR_D.parquet
├── MDCR_F.parquet
├── MDCR_hypertension_pt_final.parquet
├── MDCR_hypertension_pt_followup.parquet
├── MDCR_hypertension_pt.parquet
├── MDCR_I.parquet
├── MDCR_O_limit.parquet
├── MDCR_pd_drug_date.parquet
├── MDCR_pd_index_date.parquet
├── MDCR_R.parquet
├── MDCR_S.parquet
├── MDCR_T.parquet
```
The information above reproduces the original MarketScan data request and documents how the source data for this project were obtained. The request reflects the specifications submitted when the data were requested and is retained here for data provenance. Subsequent data processing, cohort construction, exposure definitions, eligibility criteria, and statistical analyses may have been further developed during the project. Therefore, this data-request specification should not be interpreted as the final analytical protocol. The final implementation is documented in the corresponding analysis scripts and manuscript.
