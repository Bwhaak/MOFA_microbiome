library(ggpubr)

############################################################
## Plot correlation between metabolites and factor values ##
############################################################

opts$metabolites <- c(
  "Butyrate_mg_feces",
  "Acetate_mg_feces",
  "Propionate_mg_feces"
  # "Isobutyrate_mg_feces"
)


pdf(sprintf("%s/Factors_vs_metabolites_pearson.pdf",io$outdir), width=7, height=6, useDingbats = F)
correlate_factors_with_covariates(mofa, 
  covariates = opts$metabolites,
  plot = "r",
)
dev.off()

pdf(sprintf("%s/Factors_vs_metabolites_logpval.pdf",io$outdir), width=7, height=6)
correlate_factors_with_covariates(mofa, 
  covariates = opts$metabolites,
  plot = "log_pval",
  cluster_rows = F, cluster_cols = F
)
dev.off()

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

for (i in opts$metabolites) {
  p <- plot_factors(mofa, c(1,3), color_by = paste0("cap_",i), shape_by="Category", dot_size = 3) +
    scale_fill_gradient(low = "white", high = "#BF3EFF") +
    theme(
      legend.title = element_blank(),
    )
  
  pdf(sprintf("%s/Factor1_vs_Factor3_%s.pdf",io$outdir,i), width=6, height=5, useDingbats = F)
  print(p)
  dev.off()
}

################################################
## Boxplots of metabolite levels per Category ##
################################################

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

to.plot <- mofa@samples_metadata[,c("Category",opts$metabolites)] %>%
  as.data.table %>%
  melt(id.vars=c("Category"), variable.name="metabolite") %>%
  .[,metabolite:=stringr::str_replace_all(metabolite,"_mg_feces","")]

p <- ggboxplot(to.plot, x="Category", y="value", fill="Category") +
  facet_wrap(~metabolite) +
  stat_summary(fun.data = give.n, geom = "text", size=4) +
  # stat_compare_means(aes(label = paste0("p = ", ..p.format..)), method="t.test") +
  scale_fill_manual(values=opts$colors) +
  labs(x="", y="Levels in feces (mg)") +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  )

pdf(sprintf("%s/Metabolites_boxplots.pdf",io$outdir), width=10, height=5, useDingbats = F)
print(p)
dev.off()
