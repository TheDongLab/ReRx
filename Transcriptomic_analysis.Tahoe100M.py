#!/usr/bin/env python

import os
import re
import sys
import logging
import warnings

import numpy as np
import pandas as pd
import scanpy as sc
import anndata2ri

from rpy2.robjects import r, pandas2ri
import rpy2.rinterface_lib.callbacks


# -----------------------------
# 0. Settings
# -----------------------------
warnings.filterwarnings("ignore")
sc.settings.verbosity = 0
rpy2.rinterface_lib.callbacks.logger.setLevel(logging.ERROR)

pandas2ri.activate()
anndata2ri.activate()


# -----------------------------
# 1. R function for MAST DEG
# -----------------------------
r(
    '''
library(MAST)

find_de_MAST_RE <- function(adata_) {
    sca <- SceToSingleCellAssay(adata_, class = "SingleCellAssay")
    sca <- sca[freq(sca) > 0.01, ]

    cdr2 <- colSums(assay(sca) > 0)
    colData(sca)$ngeneson <- scale(cdr2)

    label <- factor(colData(sca)$label)
    label <- relevel(label, "ctrl")
    colData(sca)$label <- label

    colData(sca)$celltype <- factor(colData(sca)$cell_name)
    colData(sca)$group <- factor(paste0(colData(adata_)$label))

    zlmCond <- zlm(
        formula = ~ ngeneson + group,
        sca = sca,
        method = "bayesglm",
        ebayes = TRUE,
        strictConvergence = FALSE
    )

    summaryCond <- summary(zlmCond, doLRT = "groupdrug")
    summaryDt <- summaryCond$datatable

    result <- merge(
        summaryDt[
            contrast == "groupdrug" & component == "H",
            .(primerid, `Pr(>Chisq)`)
        ],
        summaryDt[
            contrast == "groupdrug" & component == "logFC",
            .(primerid, coef)
        ],
        by = "primerid"
    )

    result[, coef := coef / log(2)]
    result[, FDR := p.adjust(`Pr(>Chisq)`, "fdr")]
    result <- stats::na.omit(as.data.frame(result))

    colnames(result) <- c(
        "gene",
        "pvalue",
        "log2FoldChange",
        "padj"
    )

    return(result)
}
'''
)


# -----------------------------
# 2. Load Tahoe-100M data
# -----------------------------
adata = sc.read("./tahoe.h5ad")

# Drug list
drugs = np.unique(adata.obs["drugname_drugconc"])
drugs = drugs[drugs != "[('DMSO_TF', 0.0, 'uM')]"]

cell_lines = [
    "A-172",
    "H4",
    "SW 1088",
    "CHP-212",
]

outdir = "Tahoe_MAST"
os.makedirs(outdir, exist_ok=True)

# task_id from command line
task_id = int(sys.argv[1]) if len(sys.argv) > 1 else 0
drug = drugs[task_id]

print(f"Selected drug: {task_id} {drug}")


# -----------------------------
# 3. Helper function
# -----------------------------
def prep_anndata(adata_):
    if not isinstance(adata_.X, np.ndarray):
        X = adata_.X.toarray()
    else:
        X = adata_.X

    df = pd.DataFrame(
        X,
        index=adata_.obs_names,
        columns=adata_.var_names,
    )
    df = df.join(adata_.obs)

    adata_clean = sc.AnnData(
        df[adata_.var_names],
        obs=df.drop(columns=adata_.var_names),
    )

    sc.pp.filter_genes(adata_clean, min_cells=3)
    adata_clean.X = adata_clean.X.astype("float64")

    return adata_clean


# -----------------------------
# 4. Run DEG for each cell line
# -----------------------------
for cell_line in cell_lines:
    print(f"Processing {cell_line} with {drug}")

    drug_clean = re.sub(r"[\/\[\]\(\)\', ]", "", drug)
    outfile = os.path.join(
        outdir,
        f"{cell_line}_{drug_clean}.deg.csv",
    )

    if os.path.exists(outfile):
        print(
            f"Skipping {cell_line} with {drug}, "
            "result already exists."
        )
        continue

    # Subset drug and matched DMSO control
    adata_test = adata[
        (adata.obs["cell_name"] == cell_line)
        & (
            adata.obs["drugname_drugconc"].isin(
                [
                    drug,
                    "[('DMSO_TF', 0.0, 'uM')]",
                ]
            )
        )
    ].copy()

    if adata_test.n_obs == 0:
        print(f"No cells found for {cell_line} with {drug}")
        continue

    # Keep controls from the same plates as drug-treated cells
    plates = (
        adata_test[
            adata_test.obs["drugname_drugconc"] == drug
        ]
        .obs["plate"]
        .unique()
        .tolist()
    )

    adata_test = adata_test[
        adata_test.obs["plate"].isin(plates)
    ].copy()

    adata_test.obs["label"] = (
        adata_test.obs["drugname_drugconc"].replace(
            {
                drug: "drug",
                "[('DMSO_TF', 0.0, 'uM')]": "ctrl",
            }
        )
    )

    adata_test.obs["cell_name"] = [
        x.replace(" ", "_").replace("+", "")
        for x in adata_test.obs["cell_name"]
    ]

    adata_test = prep_anndata(adata_test)
    sc.pp.log1p(adata_test)

    r.assign("adata_test", adata_test)
    r("res <- find_de_MAST_RE(adata_test)")
    res = r("res")

    res.to_csv(outfile, index=False)

    print(f"Saved {outfile}")