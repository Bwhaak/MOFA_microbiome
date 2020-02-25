library(ggplot2)
library(tidyverse)
library(reshape2)
library(MOFA2)

io <- list()
io$basedir <- "/Users/ricard/data/mofa_microbiome"
io$mofa <- paste0(io$basedir, "/rds/MOFA_microbiome.rds")
io$outdir <- paste0(io$basedir, "/pdf")

# load model
load(io$mofa)
# mofa <- MOFAobject.trained
# rm(model)

# add metadata to the model
samples_metadata(mofa) <- metadata %>% mutate(group = "single_group") 

# subset factors
threshold.var <- 0.05
r2 <- mofa@cache$variance_explained$r2_per_factor[[1]]
factors <- which(apply(r2,1,sum) >= threshold.var)
mofa <- subset_factors(mofa, factors)

# rename factors
factors_names(mofa) <- paste("Factor",1:get_dimensions(mofa)[["K"]], sep=" ")

# rename views
views_names(mofa) <- c("Bacteria","Fungi","Viruses")
