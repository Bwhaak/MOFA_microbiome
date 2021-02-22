library(ggpubr)

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

################
## Load model ##
################

source("/Users/ricard/mofa/MOFA_microbiome/3_downstream_analysis/load_model.R")

io$outdir <- paste0(io$basedir,"/results/metabolites/revision")

opts$metabolites <- c(
  "Butyrate_mg_feces",
  "Acetate_mg_feces",
  "Propionate_mg_feces"
  # "Isobutyrate_mg_feces"
)

############################################################
## Plot correlation between metabolites and factor values ##
############################################################

pdf(sprintf("%s/Factors_vs_metabolites_pearson.pdf",io$outdir), width=7, height=6, useDingbats = F)
correlate_factors_with_covariates(mofa, 
  covariates = opts$metabolites,
  abs = FALSE,
  plot = "r",
  col = colorRampPalette(c("blue","white","red"))(200)
)
dev.off()

# pdf(sprintf("%s/Factors_vs_metabolites_logpval.pdf",io$outdir), width=7, height=6)
correlate_factors_with_covariates(mofa, 
  covariates = opts$metabolites,
  plot = "log_pval",
  cluster_rows = F, cluster_cols = F
)
# dev.off()

#######################################################
## Plot factor valoues coloured by metabolite levels ##
#######################################################

# Cap values
mofa@samples_metadata$cap_Butyrate_mg_feces <- mofa@samples_metadata$Butyrate_mg_feces
mofa@samples_metadata$cap_Butyrate_mg_feces[mofa@samples_metadata$Butyrate_mg_feces>20] <- 20

mofa@samples_metadata$cap_Acetate_mg_feces <- mofa@samples_metadata$Acetate_mg_feces
mofa@samples_metadata$cap_Acetate_mg_feces[mofa@samples_metadata$Acetate_mg_feces>60] <- 60

mofa@samples_metadata$cap_Propionate_mg_feces <- mofa@samples_metadata$Propionate_mg_feces
mofa@samples_metadata$cap_Propionate_mg_feces[mofa@samples_metadata$Propionate_mg_feces>20] <- 20

p_list <- list()
for (i in opts$metabolites) {
  p_list[[i]] <- plot_factors(mofa, c(1,3), color_by = paste0("cap_",i), shape_by="Category", dot_size = 3.5) +
    scale_fill_gradient(low = "white", high = "#BF3EFF") +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
    )
  
  pdf(sprintf("%s/Factor1_vs_Factor3_%s.pdf",io$outdir,i), width=6, height=5, useDingbats = F)
  print(p_list[[i]])
  dev.off()
}

p <- cowplot::plot_grid(plotlist = p_list, nrow=1)
# pdf(sprintf("%s/Factor1_vs_Factor3_all.pdf",io$outdir), width=14, height=4, useDingbats = F)
print(p)
# dev.off()

################################################
## Boxplots of metabolite levels per Category ##
################################################

to.plot <- mofa@samples_metadata[,c("Category",opts$metabolites)] %>%
  as.data.table %>%
  melt(id.vars=c("Category"), variable.name="metabolite") %>%
  .[,metabolite:=stringr::str_replace_all(metabolite,"_mg_feces","")]

# Comparisons to test statistical significance
my.comparisons <- list( 
  c("Healthy, no antibiotics", "Healthy, antibiotics"), 
  c("Healthy, no antibiotics", "Sepsis"), 
  c("Healthy, no antibiotics", "Non septic ICU")
)

p <- ggboxplot(to.plot, x="Category", y="value", fill="Category") +
  facet_wrap(~metabolite) +
  stat_summary(fun.data = give.n, geom = "text", size=4) +
  # stat_compare_means(aes(label = paste0("p = ", ..p.format..)), method="t.test") +
  stat_compare_means(method = "wilcox.test", comparisons = my.comparisons, label = "p.signif", symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("****", "***", "**", "*"))) +
  # stat_compare_means(method = "wilcox.test", comparisons = my.comparisons, label = paste0("p = ", ..p.format..)) +
  scale_fill_manual(values=opts$colors) +
  labs(x="", y="Levels in feces (mg)") +
  theme(
    axis.title.y = element_text(size=rel(1.3), color="black"),
    strip.text = element_text(size=rel(1.3), color="black"),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  )

# pdf(sprintf("%s/Metabolites_boxplots.pdf",io$outdir), width=10, height=5, useDingbats = F)
# print(p)
# dev.off()


######################################
## Correlate with 16s/18s PCR ratio ##
######################################

io$pcr <- "/Users/ricard/data/mofa_microbiome/16S_PCR.csv"

pcr <- fread(io$pcr) %>%
  .[,fungal_to_bacterial_ratio:=-log2(corrected_18s/corrected_16s)] %>%
  .[,c("sample","fungal_to_bacterial_ratio")] %>%
  merge(metadata[,c("sample","Butyrate_mg_feces", "Acetate_mg_feces", "Propionate_mg_feces","Category")], by="sample") %>%
  melt(id.vars=c("sample","fungal_to_bacterial_ratio","Category"), variable.name="metabolite", value.name="concentration") %>%
  .[,metabolite:=stringr::str_replace_all(metabolite,"_mg_feces","")]

# pcr[,log_concentration:=log10(concentration + min(.SD[concentration>0,concentration])), by="metabolite"]
pcr[,log_concentration:=log2(concentration+0.1), by="metabolite"]

p <- ggscatter(pcr, x="fungal_to_bacterial_ratio", y="log_concentration", shape=21, fill="Category", size=3, facet.by="metabolite",
  add="reg.line", add.params = list(color="gray70", fill="lightgray", alpha=0.7), conf.int=TRUE) +
  ggpubr::stat_cor(method = "pearson", label.sep="\n", output.type = "latex", size = 5, color = "black") +
  scale_fill_manual(values=opts$colors) +
  coord_cartesian(ylim=c(-4.5,6.7)) +
  # facet_wrap(~metabolite, scales="free_y") +
  labs(x="Fungal to Bacterial ratio (log2)", y="Metabolite concentration (log2)") +
  theme(
    legend.position = "none",
    axis.text = element_text(size=rel(0.9)),
    strip.text = element_text(size=rel(1.3))
    
  )

pdf(sprintf("%s/FBratio_vs_Metabolites_scatterplots.pdf",io$outdir), width=14, height=4, useDingbats = F)
print(p)
dev.off()

