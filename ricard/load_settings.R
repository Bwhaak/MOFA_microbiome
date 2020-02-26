
io <- list()
io$basedir <- "/Users/ricard/data/mofa_microbiome"

opts <- list()

# RColorBrewer::brewer.pal(n = 4, name = 'Set2')
opts$colors <- c(
  "Sepsis" = "#E78AC3",
  "Non septic ICU" = "#FC8D62",
  "Healthy, no antibiotics" = "#66C2A5", 
  "Healthy, antibiotics" = "#8DA0CB"
)
