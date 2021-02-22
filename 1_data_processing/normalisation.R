library(tidyverse)
library(compositions)
library(reshape2)
library(readr)
# library(DESeq2)
library(corrplot)

#####################
## Define settings ##
#####################

source("/Users/ricard/MOFA_microbiome/load_settings.R")

io$basedir <- "/Users/ricard/data/mofa_microbiome"
io$unnormalised_data <- paste0(io$basedir, "/data/original/source_data_MOFA_microbiome.RDS")
io$outfile <- paste0(io$basedir, "/data/data.txt.gz")

###############
## Load data ##
###############

load(io$unnormalised_data)
metadata <- read.table(io$metadata, header=T) %>% as.tibble

##############
## Bacteria ##
##############

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

# Preprocess
count_b <- bacteria %>%
  mutate(Genus = if_else(is.na(Genus), Family, Genus)) %>%
  mutate(Genus = stringr::str_replace_all(Genus,to.rename)) %>%
  group_by(sample, Genus) %>% summarize(counts = sum(numseqs)) %>%
  filter(!is.na(Genus)) %>%
  dcast(sample ~ Genus, value.var="counts") %>% 
  column_to_rownames("sample") %>%
  as.matrix

# Subset samples
count_b <- count_b[rownames(count_b) %in% samples,]

# remove OTUs with little variabiltiy
count_b <- count_b[,apply(count_b,2,var)>10]

# Visualise
hist(count_b)
hist(apply(count_b,1,sum))
hist(apply(count_b,2,sum))
tail(sort(count_b), n=25)
# count_b[,grep("Coprococcus",colnames(count_b))]
# corrplot(cor(count_b[,grep("Coprococcus",colnames(count_b))]))

# Normalise with CLR
norm_b.clr <- as.data.frame(clr(count_b+0.1)) %>%
  rownames_to_column(var = "sample") %>%
  melt(id.var = "sample", variable.name = "feature") %>%
  mutate(view = "Bacteria")

#####################
## Normalise fungi ##
#####################

count_f <- fungi %>%
  mutate(Genus = gsub("g__", "", Genus),
         Family = gsub("f__", "", Family),
         Genus = if_else(is.na(Genus), Family, Genus)) %>%
  group_by(sample, Genus) %>% summarize(counts = sum(numseqs))%>%
  filter(!is.na(Genus)) %>%
  dcast(sample ~ Genus, value.var="counts") %>%
  column_to_rownames("sample") %>%
  as.matrix

# Subset samples
count_f <- count_f[rownames(count_f) %in% samples,]

# Visualise
hist(count_f)
hist(apply(count_f,1,sum))
hist(apply(count_f,2,sum))
tail(sort(count_f), n=25)


# Normalise with CLR
norm_f.clr <- as.data.frame(clr(count_f+0.1)) %>%
  tibble::rownames_to_column(var = "sample") %>%
  melt(id.var = "sample", variable.name = "feature") %>%
  mutate(view = "Fungi")

#####################
## Normalise virus ##
#####################

count_v <- viruses %>%
  group_by(sample, feature) %>%
  summarize(value = sum(num_reads)) %>%
  ungroup %>%
  filter(complete.cases(.)) %>%
  complete(feature, nesting(sample), fill = list(value = 0)) %>%
  dcast(sample ~ feature) %>%
  column_to_rownames("sample") %>%
  as.matrix

# Subset samples
count_v <- count_v[rownames(count_v) %in% samples,]

# Visualise
hist(count_v)
hist(apply(count_v,1,sum))
hist(apply(count_v,2,sum))

# remove outliers with overly large counts
count_v <- count_v[,apply(count_v,2,sum) < 50000]

# Normalise with CLR
norm_v.clr <- as.data.frame(clr(count_v+0.1)) %>%
  rownames_to_column(var = "sample") %>%
  melt(id.var = "sample", variable.name = "feature") %>%
  mutate(view = "Viruses")

data <- rbind(norm_b.clr, norm_f.clr, norm_v.clr)
fwrite(data, io$outfile, sep="\t")