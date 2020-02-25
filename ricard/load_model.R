library(ggplot2)
library(data.table)
library(purrr)
library(MOFA2)

source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")

io$mofa <- paste0(io$basedir, "/hdf5/test.hdf5")
io$metadata <- paste0(io$basedir, "/metadata.txt.gz")
io$outdir <- paste0(io$basedir, "/pdf")

# load model
mofa <- load_model(io$mofa)

# add metadata to the model
metadata <- fread(io$metadata) %>%
  .[sample%in%unlist(samples_names(mofa))] %>%
  setkey(sample) %>% .[unlist(samples_names(mofa))]
samples_metadata(mofa) <- metadata

# subset factors
threshold.var <- 5
r2 <- mofa@cache$variance_explained$r2_per_factor[[1]]*100
factors <- which(apply(r2,1,sum) >= threshold.var)
mofa <- subset_factors(mofa, factors)

# rename factors
factors_names(mofa) <- paste("Factor",1:get_dimensions(mofa)[["K"]], sep=" ")

# rename views
# views_names(mofa) <- c("Bacteria","Fungi","Viruses")
