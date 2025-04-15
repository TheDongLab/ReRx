##### Drug - target gene - cell relationship analysis

### ============================================================
## Extract target genes of drugs in the list using CMap database
### ============================================================

df1 <- read_excel("~/drug_list.xlsx") # change to the path you set
df2 <- read.table("~/project/ReRx/data/raw_data/export_Touchstone.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df1$Medication_lower <- tolower(df1$Medication)
merged_df <- merge(df1, df2, by.x = "Medication_lower", by.y = "Name", all = TRUE) %>%
  filter(!is.na(Medication)) %>%
  distinct(Medication, Target, .keep_all = TRUE) %>%
  select(Medication, ATC_code, Category, Target,MoA)

### ========================
## Single-cell data analysis
### ========================

# read single cell seurat obj
obj <- readRDS("PDnuclei.rds")
obj <- UpdateSeuratObject(obj)

# read drug-related gene list
target_raw <- merged_df %>%
  select(Medication, Target)
# reshape target to nerrow format, column gene is split by ', '
target <- data.frame()
for (i in 1:nrow(target_raw)) {
  gene <- strsplit(target_raw$Target[i], ", ")[[1]]
  for (j in 1:length(gene)) {
    target <- rbind(target, cbind(target_raw[i,1], gene = gene[j]))
  }
}
colnames(target) <- c("drug", "gene")

# subset obj by target gene
genes <- rownames(obj)
genes <- strsplit(genes, "\\.")
genes <- sapply(genes, function(x) x[1])
genes <- unique(genes)
# overlap gene in the target and obj
overlap_genes <- intersect(genes, target$gene)
# plot heatmap of overlap genes in obj
obj_tar <- subset(obj, features = overlap_genes)

# pseudo-bulk gene expression matrix
obj$group <- paste0(obj$sample4, "_", obj$population)
pseudo_bulk <- AggregateExpression(obj, group.by = "group", assays = "RNA", slot = "counts")
pseudo_bulk <- as.matrix(pseudo_bulk$RNA)

# read metadata of single cell data
meta <- read.csv("meta.csv")
meta$AGE2 <- scale(meta$AGE, center = TRUE, scale = TRUE)
meta$PMI2 <- scale(meta$PMI, center = TRUE, scale = TRUE)
# Identify the DEG
res_all <- data.frame()
for(ct in c("Astro", "Endo", "ExN", "InN",   "MG", "Oligo", "OPC", "T")) {
  counts <- pseudo_bulk[, grep(ct, colnames(pseudo_bulk))]
  colnames(counts) <- gsub(paste0("-",ct), "", colnames(counts))
  
  dds <- DESeqDataSetFromMatrix(
    countData = counts,
    colData = meta,
    design = ~ DIAGNOSIS + SEX + AGE2 + PMI2
  )
  dds <- DESeq(dds,parallel=T)
  
  res <- results(dds, contrast = c("DIAGNOSIS", "PD", "CTR"))
  res <- as.data.frame(res)
  res$gene <- rownames(res)
  
  if (nrow(res) > 0) {
    res$celltype <- ct
    res_all <- rbind(res_all, res)
  } else {
    print(paste0(ct, " has no significant DEGs"))
  }
}

res_all <- res_all[,c(7:8,1:6)]
res_all <- res_all[,c(1:4,7,8)]
res_all <- na.omit(res_all)
res_all$type <- "No Sig."
res_all$type[res_all$log2FoldChange < -1 & res_all$pvalue < 0.05] <- "Down"
res_all$type[res_all$log2FoldChange > 1 & res_all$pvalue < 0.05] <- "Up"
res_all <- res_all[,c(1,2,4,7,8)]
write.csv(res_all,"DEG.csv", row.names = FALSE)

# plot #

# volcano plot
ggplot(res_all,aes(x=log2FoldChange, y=-log10(pvalue), color=type)) + 
  geom_point() + 
  scale_color_manual(values = c( "blue","grey","red")) +
  geom_text_repel(data = target_dt,aes(x=log2FoldChange, y=-log10(pvalue),label = gene),color='black',
                  nudge_y = 4)+
  theme_linedraw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~celltype, scales = "free",ncol = 4)

# babble plot
plist <- list()
target_dt <- target_dt[target_dt$gene %in% target_dt$gene[target_dt$pvalue < 0.05],]
target_dt$celltype <- factor(target_dt$celltype, levels = rev(c( "ExN","InN","Oligo","Astro","OPC","MG","T","Endo")))
for (i in unique(target_dt$gene)) {
  tmp <- target_dt[target_dt$gene == i,]
  p <- ggplot(tmp) + 
    geom_point(aes(y=celltype,x=-10*log10(pvalue),color=log2FoldChange,size = log(baseMean+1))) + 
    scale_colour_gradient2(low = 'blue',mid = 'white',high = 'red',midpoint = 0)+
    labs(x='-10log10(p)',y='Cell type',
         color=sprintf("log2(Fold Change)"),
         size=sprintf("Mean expression level\n(log-transformed)"),prase=T)+
    guides(
      color = guide_colorbar(order = 1),
      size = guide_legend(order = 2)
    )+
    facet_wrap(~gene, scales = "free",ncol = 2)+
    
    theme_linedraw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p
  plist[[i]] <- p
}
# figure size 15*12
plist[[1]]+plist[[2]]+plist[[3]]+plist[[4]]+plist[[5]]+plist[[6]]+plist[[7]]+plist[[8]]+plot_layout(ncol = 2)

target_dt$celltype <- factor(target_dt$celltype, levels = rev(c( "ExN","InN","Oligo","Astro","OPC","MG","T","Endo")))
ggplot(target_dt) + 
  geom_point(aes(y=celltype,x=-10*log10(pvalue),color=type,size = baseMean)) + 
  scale_color_manual(values = c( "blue","grey","red")) +
  facet_wrap(~gene, scales = "free_x",ncol = 2)+
  labs(x='-10log10(p)',y='Cell type',size="Mean expression level",color="")+
  theme_linedraw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# plot single gene 
cpm <- cpm(pseudo_bulk)

# boxplot in all cell type
g <- 'CFTR'
plotdt <- melt(cpm[rownames(cpm) %in% g,])

plotdt$sample <- gsub("[-].*", "", rownames(plotdt))
plotdt$group <- gsub("[0].*", "", plotdt$sample)
plotdt$cell <- gsub(".*[-]", "", rownames(plotdt))

plotdt2 <- res_all[res_all$gene == g,c(2,4)]
colnames(plotdt2) <- c("cell","pvalue")

plotdt_max <- plotdt %>%
  group_by(cell) %>%
  summarise(max_value = max(value), .groups = 'drop')

plotdt2 <- plotdt2 %>%
  left_join(plotdt_max, by = "cell")

p1 <- ggplot(plotdt, aes(x = cell, y = value)) +
  geom_boxplot(aes(fill = group),outliers = FALSE, position = position_dodge2(0.8)) +
  scale_fill_manual(values = c("#66B3FF", "#FF9999")) +
  geom_text(data = plotdt2, aes(x = cell, y = max_value, label =sprintf("italic(P) == %.4f", pvalue)), 
            vjust = -0.5, size = 3.5, color = "black",parse = TRUE) +
  labs(title = "Gene expression of CFTR",
       x = "Cell type",
       y = "Expression (CPM)") +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black",  hjust = 1))

print(p1)

g <- 'NR3C2' 
plotdt <- melt(cpm[rownames(cpm) %in% g,])

plotdt$sample <- gsub("[-].*", "", rownames(plotdt))
plotdt$group <- gsub("[0].*", "", plotdt$sample)
plotdt$cell <- gsub(".*[-]", "", rownames(plotdt))

plotdt2 <- res_all[res_all$gene == g,c(2,4)]
colnames(plotdt2) <- c("cell","pvalue")

plotdt_max <- plotdt %>%
  group_by(cell) %>%
  summarise(max_value = max(value), .groups = 'drop')

plotdt2 <- plotdt2 %>%
  left_join(plotdt_max, by = "cell")

p2 <- ggplot(plotdt, aes(x = cell, y = value)) +
  geom_boxplot(aes(fill = group),outliers = FALSE, position = position_dodge2(0.8)) +
  scale_fill_manual(values = c("#66B3FF", "#FF9999")) +
  geom_text(data = plotdt2, aes(x = cell, y = max_value, label =sprintf("italic(P) == %.4f", pvalue)), 
            vjust = -0.5, size = 3.5, color = "black",parse = TRUE) +
  labs(title = "Gene expression of NR3C2",
       x = "Cell type",
       y = "Expression (CPM)") +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black",  hjust = 1))

print(p2)

p1/p2

# plot single gene in single celltype
g <- 'CFTR' 
c <- 'Oligo'
plotdt <- melt(cpm[rownames(cpm) %in% g,grep(c, colnames(cpm))])
plotdt$sample <- gsub("[-].*", "", rownames(plotdt))
plotdt$group <- gsub("[0].*", "", plotdt$sample)

p1 <- ggplot(plotdt, aes(x=group, y=value,fill = group)) + 
  geom_boxplot(width=0.5,outliers = F) + 
  geom_jitter(width = 0.2) + 
  scale_fill_manual(values = c("#66B3FF","#FF9999")) +
  scale_x_discrete(labels = c("Health", "PD")) +
  labs(title = paste0(g," in ",c), x = NULL, y = "Expression (CPM)") +
  theme_minimal()+
  theme(axis.text.x = element_text(color = 'black'),
        legend.position = "none")

g <- 'NR3C2' 
c <- 'MG'
plotdt <- melt(cpm[rownames(cpm) %in% g,grep(c, colnames(cpm))])
plotdt$sample <- gsub("[-].*", "", rownames(plotdt))
plotdt$group <- gsub("[0].*", "", plotdt$sample)

p2 <- ggplot(plotdt, aes(x=group, y=value,fill = group)) + 
  geom_boxplot(width=0.5,outliers = F) + 
  geom_jitter(width = 0.2) + 
  scale_fill_manual(values = c("#66B3FF","#FF9999")) +
  scale_x_discrete(labels = c("Health", "PD")) +
  labs(title = paste0(g," in ",c), x = NULL, y = "Expression (CPM)") +
  theme_minimal()+
  theme(axis.text.x = element_text(color = 'black'),
        legend.position = "none")

p1/p2