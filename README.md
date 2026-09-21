# ReRx: A toolkit for drug repurposing analysis

Analysis code for **“Antihypertensive medications and risk of Parkinson’s Disease: multi-cohort analyses integrating target trial emulation and transcriptomic evidence”** by Hu and colleagues. This folder contains the revised study scripts: medication-wide screening in the Mass General Brigham (MGB) Biobank, replication in AMP-PD cohorts (PPMI and PDBP), target trial emulation (TTE) in the Merative MarketScan Medicare Database (MDCR), and exploratory transcriptomic analyses of PD brain tissue and Tahoe-100M drug perturbations.

**Release status:** this is a revised working collection, not a validated end-to-end pipeline. The 13 research scripts retain their supplied analytical content. Some require restricted data, manually prepared intermediates, or objects from earlier sections. The analyses estimate observational associations. This code is research software, not a clinical decision tool.

- Project repository: [TheDongLab/ReRx](https://github.com/TheDongLab/ReRx).
- Code archive cited in the manuscript: [Zenodo DOI 10.5281/zenodo.15360497](https://doi.org/10.5281/zenodo.15360497).
- Key Resource Table (KRT): [Zenodo DOI 10.5281/zenodo.22802220](https://doi.org/10.5281/zenodo.22802220).

## Repository contents

All 13 research scripts are in this directory. The table below describes their roles in the revised manuscript.

| Script | Purpose and execution context |
| --- | --- |
| [MGB_Biobank_Medication_RxNorm.R](MGB_Biobank_Medication_RxNorm.R) | Normalize MGB medication strings to RxNorm ingredients; save dictionaries, review workbooks, and the ingredient-level medication history. Uses the RxNorm web API and curation steps. |
| [PD_risk.R](PD_risk.R) | MGB cohort definition, propensity-score matching, and medication-wide logistic regressions; later sections analyze PPMI/PDBP and require pre-existing medication objects. |
| [MGB_Biobank_Validation1.R](MGB_Biobank_Validation1.R) | Assess timing of anti-PD medication relative to the first PD code in the final MGB cohort; requires primary-analysis session objects. |
| [MGB_Biobank_Validation2.R](MGB_Biobank_Validation2.R) | Analyze antihypertensive treatment intensity over five pre-index years; requires primary-analysis session objects. |
| [MGB_Biobank_Sensitivity.R](MGB_Biobank_Sensitivity.R) | Losartan/amlodipine sensitivity analyses for case definitions, diagnosis delay, exposure lags, shifted index dates, and medication burden. |
| [Replication.R](Replication.R) | Combine existing MGB and AMP-PD association workbooks, harmonize drug names, and create replicated-drug and antihypertensive tables/plots. It does not generate the upstream cohort analyses. |
| [TTE_MDCR_Primary_Analysis.R](TTE_MDCR_Primary_Analysis.R) | Prepare the MDCR new-user cohort, define outcomes/censoring and baseline covariates, estimate IPTW/Cox models, and provide sensitivity-analysis sections. |
| [TTE_MDCR_Autonomic_Prodromal_PD.R](TTE_MDCR_Autonomic_Prodromal_PD.R) | Add prodromal/autonomic covariates, fit expanded propensity-score models, and run outcome-lag analyses. |
| [TTE_MDCR_Drug_Classes_Final.R](TTE_MDCR_Drug_Classes_Final.R) | Compare ARB with ACE inhibitor and CCB initiation using primary-cohort and expanded-covariate intermediates. |
| [TTE_MDCR_ARB_Subgroup_BBB_Individual.R](TTE_MDCR_ARB_Subgroup_BBB_Individual.R) | Analyze blood-brain barrier classifications and individual ARB agents after primary, expanded, and class-specific analyses. |
| [TTE_MDCR_Positive_Control_Stroke.R](TTE_MDCR_Positive_Control_Stroke.R) | Construct a stroke positive-control outcome in the primary-analysis session; no fitted stroke model or export is included. |
| [TTE_MDCR_Sensitivity.R](TTE_MDCR_Sensitivity.R) | Plot manually embedded aggregate HRs/CIs; it does not re-estimate the sensitivity models. |
| [Transcriptomic_analysis.R](Transcriptomic_analysis.R) | Working collection of PD pseudobulk DESeq2, GO/KEGG enrichment for both PD brain and Tahoe100M DEG resutls, and effect size correlation analysis. |
| [Transcriptomic_analysis.Tahoe100M.py](Transcriptomic_analysis.Tahoe100M.py) | Python script to analyze the Tahoe100M single-cell RNAseq data and perform MAST differential expression anaysis. |

Supporting files:

| Path | Contents |
| --- | --- |
| [dependencies.tsv](dependencies.tsv) | Direct R dependency inventory and reported versions |
| [data/README.md](data/README.md) | Data access, provenance, filenames, and required input fields. |
| [CITATION.cff](CITATION.cff), [LICENSE](LICENSE) | Machine-readable citations and the existing MIT license. |
| [CONTRIBUTING.md](CONTRIBUTING.md), [CHANGELOG.md](CHANGELOG.md) | Contribution guidance and documentation change history. |

## Installation and system requirements

Download the revised release when available, or clone the project and select the commit containing this file collection:

```bash
git clone https://github.com/TheDongLab/ReRx.git
cd ReRx
```

Select the release or commit containing the revised scripts listed above. A downloaded copy of this folder can be used directly; Git is not needed to run the example.

# Expected outputs and manuscript mapping

The table maps code to the **revised manuscript supplied for this update**. It describes code intent and visible export commands; no controlled-data analyses were rerun to verify numeric agreement. Figure assembly outside these scripts and manual edits should be recorded before release.

Research scripts retain their own output paths. This directory is the default destination only for the new aggregate example. Scripts may overwrite prior workbooks, logs, or plots: use separate authorized run directories for different parameters.

| Revised manuscript result | Source scripts | Visible outputs and limitations |
| --- | --- | --- |
| Medication-wide discovery and replication; Figure 1A | `MGB_Biobank_Medication_RxNorm.R`, `PD_risk.R`, `Replication.R` | RxNorm ingredient libraries; logistic OR/CI/P/FDR tables; `PD_risk_results.combined.xlsx`, `five_replicated_drugs_discovery_replication.xlsx`, `five_replicated_drugs_forest_plot.pdf` and PNG. MGB result export and revised-workbook handoffs need completion. |
| Screening sensitivity analyses; Figure 1B | `MGB_Biobank_Sensitivity.R` | `MGB_Losartan_Amlodipine_sensitivity_FINAL.xlsx`; plotting objects are also constructed, but no `ggsave()` appears in this script. Record final figure export/assembly. |
| Diagnosis timing validation | `MGB_Biobank_Validation1.R` | `Validation1_PD_diagnosis_delay_final_cohort.xlsx`, including aggregate summaries and a participant-level sheet. |
| Five-year antihypertensive trajectories | `MGB_Biobank_Validation2.R` | `Validation2_antihypertensive_5yr_trajectory_by_class.xlsx`, `Validation2_antihypertensive_intensity_trajectory.pdf`, `Validation2_antihypertensive_class_trajectory.pdf`. |
| Antihypertensive screening summaries; Supplementary Figure 4 cited in manuscript | `Replication.R` | `antihypertensive_medications_integrated_results.xlsx`, `significant_antihypertensive_forest_plot_revised.pdf` and PNG. |
| TTE participant selection, baseline characteristics, HRs and cumulative risks; Figure 2A–C, Tables 1–2 | `TTE_MDCR_Primary_Analysis.R` | Cohort/intermediate Parquet files, flow/diagnostic CSVs, weighted balance and survival plotting code, Cox model objects. Some exports are commented and comparison selection is manual. Full panel A flow-diagram assembly is not identified in this folder. |
| TTE sensitivity forest plot; Figure 2D | `TTE_MDCR_Sensitivity.R` | `target_trial_sensitivity_forest_plot_updated_1.pdf`, currently at a hard-coded absolute destination. Drawn from embedded HR/CI values, not fitted here. |
| Expanded PS, prodromal adjustment, PD outcome lags | `TTE_MDCR_Autonomic_Prodromal_PD.R` | Expanded cohort Parquet files, `expanded_PS_weighted_Cox_results_all.csv`, balance/overlap plots, lag and residual-adjustment summaries. |
| Active comparator analyses | `TTE_MDCR_Drug_Classes_Final.R` | `class_specific_cohort_flow.csv`, class-specific Parquet cohorts, `Table1_*`, `balance_*`, `incidence_*`, and `Cox_*` CSVs and diagnostic plots. |
| BBB and individual ARB analyses | `TTE_MDCR_ARB_Subgroup_BBB_Individual.R` | `FINAL_BBB_Cox_results.csv`, `FINAL_BBB_balance_summary.csv`, `FINAL_BBB_model_QC.csv`, individual-agent results and reportability/QC summaries. |
| Stroke positive control (not used in the final manuscript) | `TTE_MDCR_Positive_Control_Stroke.R` | In-memory `stroke_outcome`; no model fitting or disk export supplied. |
| PD/drug gene and pathway comparison; Figure 3, Supplementary Figure 6 | `Transcriptomic_analysis.R`,`Transcriptomic_analysis.Tahoe100M.py` | Intended PD pseudobulk DEG, Tahoe100M drug perturbation DEG, GSEA CSVs and `DotPlot_*_Reversal.pdf` pathway plots. Mixed-language and schema problems prevent a complete run. |

OR denotes odds ratio; HR denotes hazard ratio; CI denotes confidence interval; FDR denotes false-discovery-rate-adjusted P value; NES denotes normalized enrichment score. ORs, HRs and NES values are dimensionless. Date differences/follow-up durations use the units specified at the corresponding calculations (MDCR commonly calculates days and converts to years with 365.25). Do not infer units or final manuscript rounding from filenames alone.

## Data access and research execution

MGB Biobank, AMP-PD/PPMI/PDBP, and MarketScan participant-level records are not included. Obtain provider and institutional authorization before accessing them. Public transcriptomic resources also require the exact source versions and preprocessing used for this study. The [data guide](data/README.md) provides provider links, collection dates from the KRT, and input requirements.

Treat participant-bearing derived objects and workbooks as controlled data. In particular, Validation1 includes a `Patient_level` sheet; medication histories, matched cohorts, trajectories, and MDCR analysis datasets may retain participant keys or row-level records. A file being an “output,” CSV, or review workbook does not make it publicly shareable. Deposit aggregate result tables and figure source data only after the applicable disclosure and data-use review; document access routes for restricted derivatives.

## Citation

Cite the code version actually used and the corresponding manuscript. [CITATION.cff](CITATION.cff) provides machine-readable metadata and the revised manuscript author list. Add the revised version-specific code DOI after the release is deposited.

> Hu Y, Liu W, Zhou Y, Waits M, Zhao Y, Olivero-Acosta MI, Sheng K, Zhang L, Xu H, Scherzer CR, Dong X. Antihypertensive medications and risk of Parkinson’s Disease: multi-cohort analyses integrating target trial emulation and transcriptomic evidence. Revised manuscript; publication identifier not supplied.

Code availability and resource identifiers are linked above and in the manuscript KRT.

## License and acknowledgments

The existing [MIT License](LICENSE) applies to repository code and its accompanying documentation. It does not license participant data or supersede the terms of MarketScan/REDBOOK, MGB Biobank, AMP-PD, or other third-party resources.

This research was funded in whole or in part by Aligning Science Across Parkinson’s **ASAP-000301 (X.D. and C.R.S.)** and **ASAP-028376 (Y.H.)** through the Michael J. Fox Foundation for Parkinson’s Research (MJFF). Yuxuan Hu was also supported by the Exploration World Program from China Pharmaceutical University for his first-year visit to Brigham and Women’s Hospital.

We acknowledge the study participants, MGB Biobank, AMP-PD and its contributing PPMI and PDBP cohorts, Merative MarketScan Research Databases, and the producers of the PD brain and Tahoe-100M datasets. Consult the manuscript and current provider terms for the full required data acknowledgments.

## Contact and contributions

Scientific and reproducibility contact: Xianjun Dong, `xianjun.dong@yale.edu`. Report problems through the [project issue tracker](https://github.com/TheDongLab/ReRx/issues), providing the script, code version, command, and a minimal non-sensitive example. See [CONTRIBUTING.md](CONTRIBUTING.md).
