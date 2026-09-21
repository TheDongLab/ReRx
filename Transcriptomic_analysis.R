library(Seurat)
library(DESeq2)
library(Matrix)

## Required input PDnuclei.rds and meta.csv were provided by Dr. Zhang based on their publication Zhu et al., [doi:10.1126/scitranslmed.abo1997](https://doi.org/10.1126/scitranslmed.abo1997)

setwd("$HOME/PROJECT_FOLDER")

#####################
## PD DE analysis
#####################


# -----------------------------
# 1. Read Seurat object
# -----------------------------
obj <- readRDS("PDnuclei.rds")
obj <- UpdateSeuratObject(obj)

# -----------------------------
# 2. Create pseudo-bulk counts
#    Aggregate by sample and cell type
# -----------------------------
obj$group <- paste0(obj$sample4, "_", obj$population)

pseudo_bulk <- AggregateExpression(
  obj,
  group.by = "group",
  assays = "RNA",
  slot = "counts"
)
pseudo_bulk <- as.matrix(pseudo_bulk$RNA)

# -----------------------------
# 3. Read metadata
# -----------------------------
meta <- read.csv("meta.csv", stringsAsFactors = FALSE)

meta$AGE2 <- scale(meta$AGE, center = TRUE, scale = TRUE)
meta$PMI2 <- scale(meta$PMI, center = TRUE, scale = TRUE)

meta$DIAGNOSIS <- factor(meta$DIAGNOSIS)
meta$SEX <- factor(meta$SEX)

# -----------------------------
# 4. Differential expression by cell type
# -----------------------------
celltypes <- c("Astro", "Endo", "ExN", "InN", "MG", "Oligo", "OPC", "T")

res_all <- data.frame()

for (ct in celltypes) {
  message("Processing cell type: ", ct)
  
  counts <- pseudo_bulk[, grep(ct, colnames(pseudo_bulk)), drop = FALSE]
  
  if (ncol(counts) == 0) {
    message("No samples found for ", ct)
    next
  }
  
  # remove celltype suffix to match sample IDs in metadata
  colnames(counts) <- gsub(paste0("_", ct, "$"), "", colnames(counts))
  colnames(counts) <- gsub(paste0("-", ct, "$"), "", colnames(counts))
  
  meta_sub <- meta[match(colnames(counts), meta$sample4), ]
  
  keep_idx <- !is.na(meta_sub$sample4)
  counts <- counts[, keep_idx, drop = FALSE]
  meta_sub <- meta_sub[keep_idx, , drop = FALSE]
  
  stopifnot(all(colnames(counts) == meta_sub$sample4))
  
  # low-count filtering
  keep_genes <- rowSums(counts >= 10) >= 2
  counts <- counts[keep_genes, , drop = FALSE]
  
  if (nrow(counts) == 0) {
    message("No genes passed filtering for ", ct)
    next
  }
  
  dds <- DESeqDataSetFromMatrix(
    countData = round(counts),
    colData = meta_sub,
    design = ~ SEX + AGE2 + PMI2 + DIAGNOSIS
  )
  
  dds <- DESeq(dds, parallel = TRUE)
  
  res <- results(dds, contrast = c("DIAGNOSIS", "PD", "CTR"))
  res <- as.data.frame(res)
  res$gene <- rownames(res)
  res$celltype <- ct
  
  res_all <- rbind(res_all, res)
}

# -----------------------------
# 5. Clean and annotate results
# -----------------------------
res_all <- na.omit(res_all)

wanted_cols <- c(
  "gene", "celltype", "baseMean", "log2FoldChange",
  "lfcSE", "stat", "pvalue", "padj"
)
wanted_cols <- wanted_cols[wanted_cols %in% colnames(res_all)]
res_all <- res_all[, wanted_cols]

res_all$type <- "No Sig."
res_all$type[res_all$log2FoldChange < -1 & res_all$padj < 0.05] <- "Down"
res_all$type[res_all$log2FoldChange > 1 & res_all$padj < 0.05] <- "Up"

# -----------------------------
# 6. Save results
# -----------------------------
write.csv(res_all, "PD_brain_pseudobulk_DEG.csv", row.names = FALSE)

res_all_export <- res_all
res_all_export$gene <- paste0("gene:", res_all_export$gene)
write.csv(res_all_export, "PD_brain_pseudobulk_DEG_addGenePrefix.csv", row.names = FALSE)

#####################
## PD GSEA
#####################

library(clusterProfiler)
library(org.Hs.eg.db)
# Covert entrezid to gene symbol by dt1$gene and dt1$entrez
convert_entrez_to_symbol <- function(entrez_ids, dt1) {
  symbols <- sapply(entrez_ids, function(entrez) {
    symbol <- dt1$gene[dt1$entrez == entrez]
    if (length(symbol) > 0) {
      return(symbol)
    } else {
      return(NA)
    }
  })
  return(symbols)
}
# PD vs HC
res_all <- read.csv("PD_brain_pseudobulk_DEG_addGenePrefix.csv")
for (cell in c("Astro", "MG")) {
  dt1 <- res_all[res_all$celltype == cell,]
  dt1$gene <- gsub("gene:", "", dt1$gene)
  # convert gene symbol to entrezid
  dt1$entrez <- mapIds(org.Hs.eg.db, keys = dt1$gene, column = "ENTREZID", keytype = "SYMBOL", multiVals = "first")
  dt1 <- dt1[!is.na(dt1$entrez),]
  # log10(pvalue) * sign(log2FoldChange)
  dt1$metric <- -log10(dt1$pvalue) * sign(dt1$log2FoldChange)
  gene_list <- dt1$metric
  
  names(gene_list) <- dt1$entrez
  gene_list <- sort(gene_list, decreasing = TRUE)
  
  ### BP, CC, MF, KEGG GSEA table
  bp <- gseGO(geneList = gene_list,
              ont = "BP",
              keyType = "ENTREZID",
              OrgDb = org.Hs.eg.db,
              minGSSize = 10,
              maxGSSize = 500,
              pvalueCutoff = 1,
              verbose = TRUE,
              seed = TRUE)
  cc <- gseGO(geneList = gene_list,
              ont = "CC",
              keyType = "ENTREZID",
              OrgDb = org.Hs.eg.db,
              minGSSize = 10,
              maxGSSize = 500,
              pvalueCutoff = 1,
              verbose = TRUE,
              seed = TRUE)
  mf <- gseGO(geneList = gene_list,
              ont = "MF",
              keyType = "ENTREZID",
              OrgDb = org.Hs.eg.db,
              minGSSize = 10,
              maxGSSize = 500,
              pvalueCutoff = 1,
              verbose = TRUE,
              seed = TRUE)
  kegg <- gseKEGG(geneList = gene_list,
                  organism = 'hsa',
                  minGSSize = 10,
                  maxGSSize = 500,
                  pvalueCutoff = 1,
                  verbose = TRUE,
                  seed = TRUE)
  bp_dt <- as.data.frame(bp)
  cc_dt <- as.data.frame(cc)
  mf_dt <- as.data.frame(mf)
  kegg_dt <- as.data.frame(kegg)
  
  bp_dt$geneID_symbol <- sapply(bp_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- convert_entrez_to_symbol(entrez_ids, dt1)
    paste(symbols, collapse = "/")
  })
  cc_dt$geneID_symbol <- sapply(cc_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- convert_entrez_to_symbol(entrez_ids, dt1)
    paste(symbols, collapse = "/")
  })
  mf_dt$geneID_symbol <- sapply(mf_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- convert_entrez_to_symbol(entrez_ids, dt1)
    paste(symbols, collapse = "/")
  })
  kegg_dt$geneID_symbol <- sapply(kegg_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- convert_entrez_to_symbol(entrez_ids, dt1)
    paste(symbols, collapse = "/")
  })
  write.csv(bp_dt, "GSEA_",cell,"_BP.csv", row.names = FALSE)
  write.csv(cc_dt, "GSEA_",cell,"_CC.csv", row.names = FALSE)
  write.csv(mf_dt, "GSEA_",cell,"_MF.csv", row.names = FALSE)
  write.csv(kegg_dt, "GSEA_",cell,"_KEGG.csv", row.names = FALSE)
}

#####################
### Tahoe DE analysis
#####################
## run Transcriptomic_analysis.Tahoe100M.py


#####################
### Tahoe GSEA
#####################

for (f in list.files("./Tahoe_MAST", 
                     pattern = "A-172_SacubitrilValsartan.*.deg.csv", full.names = TRUE)) {
  dt2 <- read.csv(f)
  colnames(dt2)
  # [1] "primerid"   "Pr..Chisq." "coef"       "FDR"
  dt2$metric <- -log10(dt2$Pr..Chisq.) * sign(dt2$coef)
  dt2$entrez <- mapIds(org.Hs.eg.db, keys = dt2$primerid, column = "ENTREZID", keytype = "SYMBOL", multiVals = "first")             
  dt2 <- dt2[!is.na(dt2$entrez),]
  gene_list2 <- dt2$metric
  names(gene_list2) <- dt2$entrez
  gene_list2 <- sort(gene_list2, decreasing = TRUE)
  ### BP, CC, MF, KEGG GSEA table
  bp2 <- gseGO(geneList = gene_list2,
               ont = "BP",
               keyType = "ENTREZID",
               OrgDb = org.Hs.eg.db,
               minGSSize = 10,
               maxGSSize = 500,
               pvalueCutoff = 1,
               verbose = TRUE,
               seed = TRUE)
  cc2 <- gseGO(geneList = gene_list2,
               ont = "CC",
               keyType = "ENTREZID",
               OrgDb = org.Hs.eg.db,
               minGSSize = 10,
               maxGSSize = 500,
               pvalueCutoff = 1,
               verbose = TRUE,
               seed = TRUE)
  mf2 <- gseGO(geneList = gene_list2,
               ont = "MF",
               keyType = "ENTREZID",
               OrgDb = org.Hs.eg.db,
               minGSSize = 10,
               maxGSSize = 500,
               pvalueCutoff = 1,
               verbose = TRUE,
               seed = TRUE)
  kegg2 <- gseKEGG(geneList = gene_list2,
                   organism = 'hsa',
                   minGSSize = 10,
                   maxGSSize = 500,
                   pvalueCutoff = 1,
                   verbose = TRUE,
                   seed = TRUE)
  bp2_dt <- as.data.frame(bp2)
  cc2_dt <- as.data.frame(cc2)
  mf2_dt <- as.data.frame(mf2)
  kegg2_dt <- as.data.frame(kegg2)
  # Covert entrezid to gene symbol by dt2$primerid and dt2$entrez
  bp2_dt$geneID_symbol <- sapply(bp2_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- sapply(entrez_ids, function(entrez) {
      symbol <- dt2$primerid[dt2$entrez == entrez]
      if (length(symbol) > 0) {
        return(symbol)
      } else {
        return(NA)
      }
    })
    paste(symbols, collapse = "/")
  })
  cc2_dt$geneID_symbol <- sapply(cc2_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- sapply(entrez_ids, function(entrez) {
      symbol <- dt2$primerid[dt2$entrez == entrez]
      if (length(symbol) > 0) {
        return(symbol)
      } else {
        return(NA)
      }
    })
    paste(symbols, collapse = "/")
  })
  mf2_dt$geneID_symbol <- sapply(mf2_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- sapply(entrez_ids, function(entrez) {
      symbol <- dt2$primerid[dt2$entrez == entrez]
      if (length(symbol) > 0) {
        return(symbol)
      } else {
        return(NA)
      }
    })
    paste(symbols, collapse = "/")
  })
  kegg2_dt$geneID_symbol <- sapply(kegg2_dt$core_enrichment, function(x) {
    entrez_ids <- unlist(strsplit(x, "/"))
    symbols <- sapply(entrez_ids, function(entrez) {
      symbol <- dt2$primerid[dt2$entrez == entrez]
      if (length(symbol) > 0) {
        return(symbol)
      } else {
        return(NA)
      }
    })
    paste(symbols, collapse = "/")
  })
  prefix <- gsub(".deg.csv", "", basename(f))
  write.csv(bp2_dt, paste0("GSEA_",prefix,"_BP.csv"), row.names = FALSE)
  write.csv(cc2_dt, paste0("GSEA_",prefix,"_CC.csv"), row.names = FALSE)
  write.csv(mf2_dt, paste0("GSEA_",prefix,"_MF.csv"), row.names = FALSE)
  write.csv(kegg2_dt, paste0("GSEA_",prefix,"_KEGG.csv"), row.names = FALSE)
}

#####################
## Reversal GSEA Dot Plot Script
## PD vs HC  vs  Valsartan
## Dot plot: NES (x), leading-edge size (dot), -log10(FDR) (color)
## Save reversal pathway CSV with leading-edge genes
#####################

library(tidyverse)
library(stringr)
library(ggplot2)

## -------------------------------
## 1. Helper: read GSEA csv
## -------------------------------
read_gsea <- function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    transmute(
      ID,
      Description,
      NES,
      p.adjust,
      core_enrichment,
      geneID_symbol,
      leading_n = str_count(core_enrichment, "/") + 1
    )
}

## -------------------------------
## 2. Helper: build reversal dot plot + save CSV
## -------------------------------
make_reversal_dotplot <- function(pd_file,
                                  drug_file,
                                  title_text,
                                  out_pdf,
                                  out_csv) {
  
  ## read
  pd_df   <- read_gsea(pd_file)
  drug_df <- read_gsea(drug_file)
  
  ## merge PD and Drug
  merged <- pd_df %>%
    rename(
      NES_PD   = NES,
      p_PD     = p.adjust,
      lead_PD  = leading_n,
      leading_genes_PD = geneID_symbol
    ) %>%
    inner_join(
      drug_df %>%
        rename(
          NES_drug  = NES,
          p_drug    = p.adjust,
          lead_drug = leading_n,
          leading_genes_drug = geneID_symbol
        ),
      by = c("ID", "Description")
    ) %>%
    mutate(
      reversed = sign(NES_PD) != sign(NES_drug)
    )
  
  ## filter: BOTH significant + reversed
  rev_df <- merged %>%
    filter(
      p_PD   < 0.05,
      p_drug < 0.05,
      reversed
    )
  
  if (nrow(rev_df) == 0) {
    message("No reversal pathways found for: ", title_text)
    return(NULL)
  }
  
  ## ---- SAVE CSV (raw data for dot plot) ----
  write_csv(
    rev_df %>%
      select(
        ID,
        Description,
        NES_PD,
        p.adjust_PD = p_PD,
        leading_PD  = lead_PD,
        leading_genes_PD,
        NES_drug,
        p.adjust_drug = p_drug,
        leading_drug = lead_drug,
        leading_genes_drug,
        reversed
      ),
    out_csv
  )
  
  ## ---- Prepare dot plot data ----
  dot_df <- bind_rows(
    rev_df %>%
      transmute(
        Description,
        NES = NES_PD,
        p.adjust = p_PD,
        leading_n = lead_PD,
        source = "PD vs HC"
      ),
    rev_df %>%
      transmute(
        Description,
        NES = NES_drug,
        p.adjust = p_drug,
        leading_n = lead_drug,
        source = "Drug"
      )
  ) %>%
    mutate(
      neglog10_p = -log10(p.adjust)
    )
  
  ## y-axis order: by |NES_PD|
  y_order <- rev_df %>%
    arrange(desc(abs(NES_PD))) %>%
    pull(Description)
  
  dot_df <- dot_df %>%
    mutate(
      Description = factor(Description, levels = rev(y_order))
    )
  
  ## plot
  p <- ggplot(dot_df, aes(x = NES, y = Description)) +
    geom_point(
      aes(size = leading_n, color = neglog10_p),
      alpha = 0.9
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
    facet_wrap(~ source, ncol = 2, scales = "free_x") +
    scale_size_continuous(
      name = "Leading-edge\ngenes",
      range = c(2, 8)
    ) +
    scale_color_gradient(
      name = expression(-log[10]("adj. P")),
      low  = "grey80",
      high = "#E41A1C"   # JAMA Neurology–friendly deep red (#8B0000)
    ) +
    labs(
      title = title_text,
      x = "Normalized Enrichment Score (NES)",
      y = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(color = "black"),
      legend.position = "right"
    )
  
  ## save WITHOUT cairo (no X11 warning)
  ggsave(
    filename = out_pdf,
    plot     = p,
    width    = 12,
    height   = 6,
    units    = "in"
  )
}

## -------------------------------
## 3. File paths
## -------------------------------

base_dir <- "/Users/itsupport/Desktop/manuscript/GSEA_dotplot"

drug_dir <- file.path(base_dir, "Drug")
pd_dir   <- file.path(base_dir, "PDvsHC")

## -------------------------------
## 4. Generate 4 dot plots + CSVs
## -------------------------------

## ---- BP : Drug vs PD Astro ----
make_reversal_dotplot(
  pd_file   = file.path(pd_dir, "GSEA_Astro_BP.csv"),
  drug_file = file.path(drug_dir, "GSEA_A-172_SacubitrilValsartan5.0uM_BP.csv"),
  title_text = "Reversal GSEA Dot Plot (BP): Valsartan vs PD Astrocytes",
  out_pdf    = "DotPlot_BP_Drug_vs_PD_Astro_Reversal.pdf",
  out_csv    = "ReversalPathways_BP_Drug_vs_PD_Astro.csv"
)

## ---- BP : Drug vs PD Microglia ----
make_reversal_dotplot(
  pd_file   = file.path(pd_dir, "GSEA_MG_BP.csv"),
  drug_file = file.path(drug_dir, "GSEA_A-172_SacubitrilValsartan5.0uM_BP.csv"),
  title_text = "Reversal GSEA Dot Plot (BP): Valsartan vs PD Microglia",
  out_pdf    = "DotPlot_BP_Drug_vs_PD_MG_Reversal.pdf",
  out_csv    = "ReversalPathways_BP_Drug_vs_PD_MG.csv"
)

## ---- KEGG : Drug vs PD Astro ----
make_reversal_dotplot(
  pd_file   = file.path(pd_dir, "GSEA_Astro_KEGG.csv"),
  drug_file = file.path(drug_dir, "GSEA_A-172_SacubitrilValsartan5.0uM_KEGG.csv"),
  title_text = "Reversal GSEA Dot Plot (KEGG): Valsartan vs PD Astrocytes",
  out_pdf    = "DotPlot_KEGG_Drug_vs_PD_Astro_Reversal.pdf",
  out_csv    = "ReversalPathways_KEGG_Drug_vs_PD_Astro.csv"
)

## ---- KEGG : Drug vs PD Microglia ----
make_reversal_dotplot(
  pd_file   = file.path(pd_dir, "GSEA_MG_KEGG.csv"),
  drug_file = file.path(drug_dir, "GSEA_A-172_SacubitrilValsartan5.0uM_KEGG.csv"),
  title_text = "Reversal GSEA Dot Plot (KEGG): Valsartan vs PD Microglia",
  out_pdf    = "DotPlot_KEGG_Drug_vs_PD_MG_Reversal.pdf",
  out_csv    = "ReversalPathways_KEGG_Drug_vs_PD_MG.csv"
)
