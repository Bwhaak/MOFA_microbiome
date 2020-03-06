################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")
source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

############################################
## Scatter plots between pairs of factors ##
############################################

p <- plot_factors(mofa, c(1,3), color_by = "Category", dot_size = 4) +
  scale_fill_manual(values=opts$colors) +
  theme(
    legend.title = element_blank(),
  )

pdf(sprintf("%s/Factor1_vs_Factor3_category.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()
