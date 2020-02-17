# gm_mean = function(x, na.rm=TRUE){
#   exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
# }

library(tidyverse)
library(compositions)
library(reshape2)
library(readr)
library(DESeq2)

#####################
## Define settings ##
#####################

io <- list()
io$data <- "/Users/ricard/data/mofa_microbiome/rds/source_data_MOFA_microbiome.RDS"

###############
## Load data ##
###############

load(io$data)

########################
## Normalise bacteria ##
########################

# Preprocess
count_b <- bacteria %>%
  mutate(Genus = if_else(is.na(Genus), Family, Genus))%>%
  group_by(sample, Genus)%>%
  summarize(counts = sum(numseqs))%>%
  filter(!is.na(Genus)) %>%
  dcast(sample ~ Genus, value.var="counts") %>% 
  column_to_rownames("sample") %>%
  as.matrix

# Visualise
hist(count_b)
hist(apply(count_b,1,sum))
hist(apply(count_b,2,sum))
tail(sort(count_b), n=25)

# Normalise with CLR
norm_b.clr <- as.data.frame(clr(count_b+0.1)) %>%
  # rownames_to_column(var = "sample") %>%
  t
  # melt(id.var = "sample", variable.name = "feature") %>%
  # mutate(view = "bacteria")

# Normalise with DESeq2
coldata <- DataFrame( foo=factor(rep(1,nrow(count_b))) )
dds <- DESeqDataSetFromMatrix(t(count_b), colData=coldata, design=~1)
dds <- varianceStabilizingTransformation(dds, blind=TRUE) 
norm_b.deseq <- assay(dds)

# Compare CLR vs DESeq2
plot(colMeans(norm_b.deseq), colMeans(norm_b.clr), xlab="DESeq2 (mean expr per cell)", ylab="CLR (mean expr per cell)")
plot(rowMeans(norm_b.deseq), rowMeans(norm_b.clr), xlab="DESeq2 (mean expr per gene)", ylab="CLR (mean expr per gene)")
hist(diag(cor(norm_b.deseq, norm_b.clr)), xlim=c(0,1), main="Correlation coefficient per gene (across cells)")
hist(diag(cor(t(norm_b.deseq), t(norm_b.clr))), xlim=c(0,1), main="Correlation coefficient per cell (across genes)")


#####################
## Normalise fungi ##
#####################

count_f <- fungi%>%
  mutate(Genus = gsub("g__", "", Genus),
         Family = gsub("f__", "", Family),
         Genus = if_else(is.na(Genus), Family, Genus))%>%
  group_by(sample, Genus)%>%
  summarize(counts = sum(numseqs)+0.1)%>%
  filter(!is.na(Genus))%>%
  dcast(sample ~ Genus, value.var="counts") %>%
  column_to_rownames("sample") %>%
  as.matrix

# Visualise
hist(count_f)
hist(apply(count_f,1,sum))
hist(apply(count_f,2,sum))
tail(sort(count_f), n=25)


# Normalise with CLR
fungi <- as.data.frame(clr(count_f))%>%
  tibble::rownames_to_column(var = "sample")%>%
  melt(id.var = "sample", variable.name = "feature")%>%
  mutate(view = "fungi")

#####################
## Normalise virus ##
#####################

count_v <- viruses %>%
  group_by(sample, feature)%>%
  summarize(value = sum(num_reads)+0.1)%>%
  ungroup %>%
  filter(complete.cases(.))%>%
  complete(feature, nesting(sample), fill = list(value = 0.1))%>%
  dcast(sample ~ feature) %>%
  column_to_rownames("sample") %>%
  as.matrix

# Visualise
hist(count_v)
hist(apply(count_v,1,sum))
hist(apply(count_v,2,sum))
tail(sort(count_v), n=25)

# Normalise with CLR
viruses <- as.data.frame(clr(count_v))%>%
  tibble::rownames_to_column(var = "sample")%>%
  melt(id.var = "sample", variable.name = "feature")%>%
  mutate(view = "viruses")
