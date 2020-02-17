library(MOFA2)
library(ggplot2)
library(tidyverse)
library(reshape2)

io <- list()
io$basedir <- "/Users/ricard/data/mofa_microbiome"
io$mofa <- paste0(io$basedir, "/rds/MOFA_microbiome.rds")
io$outdir <- paste0(io$basedir, "/pdf")

# load model
load(io$mofa)
mofa <- MOFAobject.trained

factors(mofa) <- paste("Factor",1:get_dimensions(mofa)[["K"]], sep=" ")

# add metadata to the model
foo <- metadata %>% 
  mutate(group = "single_group") 

samples_metadata(mofa) <- foo
