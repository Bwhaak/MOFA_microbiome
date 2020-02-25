library(ggpubr)

############################################################
## Plot correlation between antibiotics and factor values ##
############################################################

antibiotics <- c("Penicillins", "Cephalosporins", "Carbapenems", 
  "Clavulanic_acid", "Beta_lactam", "Macrolides", "Aminoglycosides", 
  "Quinolones", "Co_trimoxazole", "Metronidazole", "Vancomycin")

# pdf(sprintf("%s/Factors_vs_antibiotics_pearson.pdf",io$outdir), width=7, height=6, useDingbats = F)
correlate_factors_with_covariates(mofa, 
  covariates = antibiotics,
  plot = "r",
)
# dev.off()

# pdf(sprintf("%s/Factors_vs_antibiotics_logpval.pdf",io$outdir), width=7, height=6)
correlate_factors_with_covariates(mofa, 
  covariates = antibiotics,
  plot = "log_pval",
  cluster_rows = F, cluster_cols = F
)
# dev.off()
