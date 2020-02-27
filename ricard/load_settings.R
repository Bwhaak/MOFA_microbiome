library(data.table)
library(purrr)
library(MOFA)
library(ggplot2)
library(ggpubr)

################
## Define I/O ##
################

io <- list()
io$basedir <- "/Users/ricard/data/mofa_microbiome"
io$mofa <- paste0(io$basedir, "/hdf5/test.hdf5")
io$metadata <- paste0(io$basedir, "/metadata.txt.gz")

####################
## Define options ##
####################

opts <- list()

# RColorBrewer::brewer.pal(n = 4, name = 'Set2')
opts$colors <- c(
  "Sepsis" = "#E78AC3",
  "Non septic ICU" = "#FC8D62",
  "Healthy, no antibiotics" = "#66C2A5", 
  "Healthy, antibiotics" = "#8DA0CB"
)

# RColorBrewer::brewer.pal(n=3,name="Set1")
opts$colors.views <- c(
  "Bacteria" = "#E41A1C",
  "Fungi" = "#377EB8" ,
  "Viruses" = "#4DAF4A"
)
  