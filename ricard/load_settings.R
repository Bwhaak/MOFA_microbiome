
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


opts$good.bacteria <- c(
  "Agathobacter",
  "Roseburia",
  "Faecalibacterium",
  "Ruminococcus",
  "Coprococcus",
  # "Butyricicoccus",
  "Blautia",
  # "Anaerostipes",
  # "Bifidobacterium",
  "Lachnospiraceae"
  # "Ruminococcaceae"
)

opts$bad.bacteria <- c(
  "Enterococcus", 
  "Klebsiella",
  "Staphylococcus",
  "Enterobacter",
  "Erysipelotrichaceae",
  "Escherichia/Shigella"
)

# opts$unknown.bacteria <- c(
#   "Adlercreutzia",
#   "CAG-56",
#   "GCA-900066575",
#   "Marvinbryantia",
#   "Tyzzerella_3"
# )

opts$bad.fungi <- c(
  "Saccharomyces",
  "Aspergillus",
  "Penicillium",
  "Debaryomyces",
  "Candida"
)

opts$good.fungi <- c(
  "Dipodascus",
  "Vishniacozyma",
  "Filobasidium",
  "Paraphaeosphaeria"
)

opts$good.viruses <- c(
  "Megavirales",
  "Rhodococcus phage",
  "Sphingomonas phage",
  "Picobirnavirus"
)

opts$bad.viruses <- c(
  "Enterococcus phage",
  "Enterobacteriaceae phage",
  "Escherichia/Shigella phage",
  "Klebsiella phage",
  "Staphylococcus phage"
)



# TO-DO:
# opts$good.viruses <- c(
#   "Megavirales",
#   "Rhodococcus phage",
#   "Sphingomonas phage",
#   "Picobirnavirus",
#   "Bacteroides phage"
# )
# opts$bad.viruses <- c(
#   "Enterococcus phage",
#   "Enterobacteriaceae phage",
#   "Escherichia/Shigella phage",
#   "Klebsiella phage",
#   "Staphylococcus phage",
#   "Streptococcus phage", #maybe not in the context of factor 1
#   "Chrysovirus" #infects pathogenic fungi (Aspergillus/Penicillium)
#   "Lactobacillus phage" # I know, crazy, but really associated with a lot of antibiotic exposure
#   "Parabacteroides phage" #counterintuitive, but penicillin resistant and often a causative pathogen during abdominal infections
# ) (edited) 