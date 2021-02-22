library(ggpubr)

################
## Load model ##
################

source("/Users/ricard/mofa/MOFA_microbiome/3_downstream_analysis/load_model.R")

#####################
## Define settings ##
#####################

io$outdir <- paste0(io$basedir,"/results/antibiotics")

antibiotics <- c("Penicillins", "Cephalosporins", "Carbapenems", 
  "Clavulanic_acid", "Macrolides", "Aminoglycosides", 
  "Quinolones", "Co_trimoxazole", "Metronidazole", "Vancomycin")

############################################################
## Plot correlation between antibiotics and factor values ##
############################################################

pdf(sprintf("%s/revision3_Factors_vs_antibiotics_pearson_reverse_colour.pdf",io$outdir), width=7, height=6, useDingbats = F)
correlate_factors_with_covariates(mofa, 
  covariates = antibiotics,
  plot = "r",
  col = colorRampPalette(c("blue","white","red"))(200)
)
dev.off()

pdf(sprintf("%s/Factors_vs_antibiotics_logpval.pdf",io$outdir), width=7, height=6)
correlate_factors_with_covariates(mofa, 
  covariates = antibiotics,
  plot = "log_pval",
  cluster_rows = F, cluster_cols = F
)
dev.off()
