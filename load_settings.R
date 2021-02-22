suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(MOFA2))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggpubr))

################
## Define I/O ##
################

io <- list()
io$basedir <- "/Users/ricard/data/mofa/mofa_microbiome"
io$mofa.hdf5 <- paste0(io$basedir, "/trained_model/model.hdf5")
io$mofa.rds <- paste0(io$basedir, "/trained_model/model.rds")
io$metadata <- paste0(io$basedir, "/metadata.txt.gz")
io$unnormalised.data <- paste0(io$basedir, "/data/original/source_data_MOFA_microbiome.RDS")
io$normalised.data <- paste0(io$basedir, "/data/data.txt.gz")

####################
## Define options ##
####################

opts <- list()

# RColorBrewer::brewer.pal(n = 4, name = 'Set2')
opts$colors <- c(
  "Healthy, no antibiotics" = "#66C2A5", 
  "Healthy, antibiotics" = "#8DA0CB",
  "Sepsis" = "#E78AC3",
  "Non septic ICU" = "#FC8D62"
)

# RColorBrewer::brewer.pal(n=3,name="Set1")
opts$colors.views <- c(
  "Bacteria" = "#E41A1C",
  "Fungi" = "#377EB8" ,
  "Viruses" = "#4DAF4A"
)
  
###############
## Load data ##
###############

metadata <- fread(io$metadata)
