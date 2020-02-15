library(tidyverse)
library(compositions)
library(reshape2)
library(readr)
library(MOFA2)
library(reticulate)


use_condaenv("C:/Users/..../..../", required=TRUE)
mofa <- import("mofapy2")
mofa_entrypoint <- mofa$run.entry_point$entry_point()

load("~/source_data_MOFA_microbiome.RDS")

#BACTERIA
count <- bacteria%>%
  mutate(Genus = if_else(is.na(Genus), Family, Genus))%>%
  group_by(sample, Genus)%>%
  summarize(counts = sum(numseqs)+0.1)%>%
  filter(!is.na(Genus))%>%
  dcast(sample ~ Genus)
row.names(count) <- count$sample
count <- count %>%
  select(-sample)
count <- as.matrix(count)

bacteria <- as.data.frame(clr(count))%>%
  tibble::rownames_to_column(var = "sample")%>%
  melt(id.var = "sample", variable.name = "feature")%>%
  mutate(view = "bacteria")


#FUNGI
count_f <- fungi%>%
  mutate(Genus = gsub("g__", "", Genus),
         Family = gsub("f__", "", Family),
         Genus = if_else(is.na(Genus), Family, Genus))%>%
  group_by(sample, Genus)%>%
  summarize(counts = sum(numseqs)+0.1)%>%
  filter(!is.na(Genus))%>%
  dcast(sample ~ Genus)
row.names(count_f) <- count_f$sample
count_f <- count_f %>%
  select(-sample)

count_f <- as.matrix(count_f)

fungi <- as.data.frame(clr(count_f))%>%
  tibble::rownames_to_column(var = "sample")%>%
  melt(id.var = "sample", variable.name = "feature")%>%
  mutate(view = "fungi")

#VIRUSES
count_v <- viruses %>%
  group_by(sample, feature)%>%
  summarize(value = sum(num_reads)+0.1)%>%
  ungroup()%>%
  filter(complete.cases(.))%>%
  complete(feature, nesting(sample), fill = list(value = 0.1))%>%
  dcast(sample ~ feature)
row.names(count_v) <- count_v$sample
count_v <- count_v %>%
  select(-sample)
count_v <- as.matrix(count_v)


viruses <- as.data.frame(clr(count_v))%>%
  tibble::rownames_to_column(var = "sample")%>%
  melt(id.var = "sample", variable.name = "feature")%>%
  mutate(view = "viruses")

#COMBINE FILES AND RUN MOFA
mofa_file <- rbind(bacteria, fungi, viruses)
hist(mofa_file$value)

MOFAobject <- create_mofa(mofa_file)
print(MOFAobject)
plot_data_overview(MOFAobject)  
data_opts <- get_default_data_options(MOFAobject)

data_opts
model_opts <- get_default_model_options(MOFAobject)
head(model_opts)
train_opts <- get_default_training_options(MOFAobject)
head(train_opts)


MOFAobject <- prepare_mofa(
  object = MOFAobject,
  data_options = data_opts,
  model_options = model_opts,
  training_options = train_opts
  # stochastic_options = stochastic_opts # optional
)
outfile = "~/training.hdf5"
MOFAobject.trained <- run_mofa(MOFAobject, outfile)
model <- MOFAobject.trained