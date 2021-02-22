library(tidyverse)
library(compositions)
library(reshape2)
library(readr)
# library(DESeq2)
library(ggpubr)

#####################
## Define settings ##
#####################

source("/Users/ricard/MOFA_microbiome/load_settings.R")

###############
## Load data ##
###############

load(io$unnormalised.data)
# metadata <- read.table(io$metadata, header=T) %>% as.tibble

##############
## Bacteria ##
##############

bacteria %>% as.

to.rename <- c(
  "Ruminococcus_2" = "Ruminococcus",
  "Ruminococcus_1" = "Ruminococcus",
  "Lachnospiraceae_FCS020_group" = "Lachnospiraceae",
  "Lachnospiraceae_ND3007_group" = "Lachnospiraceae",
  "Lachnospiraceae_NK4A136_group" = "Lachnospiraceae",
  "Lachnospiraceae_UCG-001" = "Lachnospiraceae",
  "Lachnospiraceae_UCG-004" = "Lachnospiraceae",
  "Lachnospiraceae_UCG-008" = "Lachnospiraceae",
  "Lachnospiraceae_UCG-010" = "Lachnospiraceae",
  "Ruminococcaceae_NK4A214_group" = "Ruminococcaceae",
  "Ruminococcaceae_UCG-002" = "Ruminococcaceae",
  "Ruminococcaceae_UCG-003" = "Ruminococcaceae",
  "Ruminococcaceae_UCG-004" = "Ruminococcaceae",
  "Ruminococcaceae_UCG-005" = "Ruminococcaceae",
  "Ruminococcaceae_UCG-013" = "Ruminococcaceae",
  "Ruminococcaceae_UCG-014" = "Ruminococcaceae",
  "Tyzzerella_3" = "Tyzzerella",
  "Tyzzerella_4" = "Tyzzerella",
  "Coprococcus_1" = "Coprococcus",
  "Coprococcus_2" = "Coprococcus",
  "Coprococcus_3" = "Coprococcus"
)

bacteria.dt <- bacteria %>%
  mutate(Genus = stringr::str_replace_all(Genus,to.rename)) %>%
  as.data.table

# Plot coverage at the Species level
to.plot.species <- bacteria.dt %>%
  .[!is.na(Species)] %>%
  .[,.(N=log2(sum(numseqs,na.rm=T)+1)), by=c("Species")]

ggbarplot(to.plot, x="Species", y="N", sort.val = "desc", color="gray70") +
  coord_flip() +
  labs(y="Average number of reads (log10)", x="Species") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# Plot coverage at the Genus level
to.plot.genus <- bacteria.dt %>%
  .[!is.na(Genus)] %>%
  .[,.(N=log2(sum(numseqs,na.rm=T)+1)), by=c("Genus")]

ggbarplot(to.plot, x="Genus", y="N", sort.val = "desc", color="gray70") +
  coord_flip() +
  labs(y="Average number of reads (log10)", x="Species") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


to.plot <- rbind(
  to.plot.genus %>% .[,c("N")] %>% .[,class:="Genus"], 
  to.plot.species %>% .[,c("N")] %>% .[,class:="Species"]
)
gghistogram(to.plot, x="N", fill="class", bins=50)




# to.plot <- bacteria.dt %>%
#   .[,.(N=sum(numseqs,na.rm=T)), by=c("Genus","Species")] %>% 
#   .[!is.na(Genus) & !is.na(Species)]
