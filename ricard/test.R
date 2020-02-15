library(MOFA2)
library(ggplot2)

io <- list()
io$basedir <- "/Users/ricard/data/mofa_microbiome"
io$mofa <- paste0(io$basedir, "/rds/MOFA_microbiome.rds")

load(io$mofa)
mofa <- MOFAobject.trained


plot_variance_explained(mofa)
