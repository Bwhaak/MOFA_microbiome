library(ggpubr)

################
## Load model ##
################

source("/Users/ricard/MOFA_microbiome/ricard/load_model.R")

io$outdir <- "/Users/ricard/data/mofa_microbiome/pdf/antibiotics"

antibiotics <- c("Penicillins", "Cephalosporins", "Carbapenems", 
  "Clavulanic_acid", "Macrolides", "Aminoglycosides", 
  "Quinolones", "Co_trimoxazole", "Metronidazole", "Vancomycin")

############################################################
## Plot correlation between antibiotics and factor values ##
############################################################

pdf(sprintf("%s/Factors_vs_antibiotics_pearson.pdf",io$outdir), width=7, height=6, useDingbats = F)
correlate_factors_with_covariates(mofa, 
  covariates = antibiotics,
  plot = "r",
)
dev.off()

pdf(sprintf("%s/Factors_vs_antibiotics_logpval.pdf",io$outdir), width=7, height=6)
correlate_factors_with_covariates(mofa, 
  covariates = antibiotics,
  plot = "log_pval",
  cluster_rows = F, cluster_cols = F
)
dev.off()
