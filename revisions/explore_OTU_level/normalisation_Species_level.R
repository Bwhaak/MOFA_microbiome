library(compositions)
library(pheatmap)

matrix.please<-function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[[1]]
  m
}

#####################
## Define settings ##
#####################

source("/Users/ricard/MOFA_microbiome/load_settings.R")

io$mofa.outfile <- paste0(io$basedir,"/mofa_OTUs.hdf5")
io$data.outfile <- paste0(io$basedir, "/data/data_OTUs.txt.gz")
io$outdir <- paste0(io$basedir, "/results/gender_vs_OTU")

###############
## Load data ##
###############

load(io$unnormalised.data)
# metadata <- read.table(io$metadata, header=T) %>% as.tibble

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
  as.data.table %>%
  .[,Genus := ifelse(is.na(Genus), Family, Genus)] %>%
  .[,Genus := stringr::str_replace_all(Genus,to.rename)] %>%
  .[!(is.na(Genus) | is.na(Species))] %>%
  .[,genus_specie:=paste(Genus,Species,sep="_")] %>%
  .[,.(counts=sum(numseqs,na.rm=T)), by=c("sample","genus_specie")] %>%
  data.table::dcast(sample ~ genus_specie, value.var="counts") %>% 
  matrix.please

# remove OTUs with little variabiltiy
count_b <- count_b[,apply(count_b,2,var)>10]

# Normalise with CLR
norm_b <- as.data.table(clr(count_b+0.1)) %>%
  .[,sample:=rownames(count_b)] %>%
  melt(id.var = "sample", variable.name = "feature") %>%
  .[,view:="Bacteria"]

###########
## Fungi ##
###########

count_f <- fungi %>%
  as.data.table %>%
  .[,Genus := gsub("g__", "", Genus)] %>%
  .[,Family := gsub("f__", "", Family)] %>%
  .[,Genus := ifelse(is.na(Genus), Family, Genus)] %>%
  .[!(is.na(Genus) | is.na(Species))] %>%
  .[,genus_specie:=paste(Genus,Species,sep="_")] %>%
  .[,.(counts=sum(numseqs,na.rm=T)), by=c("sample","genus_specie")] %>%
  data.table::dcast(sample ~ genus_specie, value.var="counts") %>% 
  matrix.please
  
# Normalise with CLR
norm_f <- as.data.table(clr(count_f+0.1)) %>%
  .[,sample:=rownames(count_f)] %>%
  melt(id.var = "sample", variable.name = "feature") %>%
  .[,view:="Fungi"]

#############
## Viruses ##
#############

# Preprocess
count_v <- viruses %>%
  as.data.table %>%
  .[!is.na(feature)] %>%
  .[,.(counts=sum(num_reads,na.rm=T)), by=c("sample","feature")] %>%
  data.table::dcast(sample ~ feature, value.var="counts", fill=0) %>% 
  matrix.please

# remove outliers with overly large counts
count_v <- count_v[,apply(count_v,2,sum) < 50000]

# Normalise with CLR
norm_v <- as.data.table(clr(count_v+0.1)) %>%
  .[,sample:=rownames(count_v)] %>%
  melt(id.var = "sample", variable.name = "feature") %>%
  .[,view:="Viruses"]

##########
## Save ##
##########

data <- rbind(norm_b, norm_f, norm_v)
# fwrite(data, io$outfile, sep="\t")

##########
## MOFA ##
##########

# Create MOFA object
mofa <- create_mofa(data)

# Model options
model_opts <- get_default_model_options(mofa)
model_opts$num_factors <- 10

# Training options
train_opts <- get_default_training_options(mofa)
train_opts$convergence_mode <- "medium"

# Prepare MOFA object
mofa <- prepare_mofa(mofa, model_options = model_opts, training_options = train_opts)

# Train the model 
# mofa <- run_mofa(mofa, io$mofa.outfile)
mofa <- load_model(io$mofa.outfile)

# add metadata to the model
metadata <- metadata %>%
  .[sample%in%unlist(samples_names(mofa))] %>%
  setkey(sample) %>% .[unlist(samples_names(mofa))] %>%
  .[,Category:=factor(Category, levels=c("Healthy, no antibiotics", "Healthy, antibiotics", "Sepsis", "Non septic ICU"))]
samples_metadata(mofa) <- metadata

# Load original model and compare factor values
mofa.original <- load_model(io$mofa.hdf5)

# Get factors
factors <- get_factors(mofa)[[1]]
factors.original <- get_factors(mofa.original)[[1]]
stopifnot(rownames(factors) == rownames(factors.original))

# Correlate factors
r <- cor(factors,factors.original) %>% abs
r <- r[,apply(r,1,which.max)]
rownames(r) <- paste("Factor",1:nrow(r), sep=" "); colnames(r) <- paste("Factor",1:ncol(r), sep=" ")

pdf(sprintf("%s/correlation_factors_OTU_vs_Gender.pdf", io$outdir), onefile = F)
pheatmap(r,
         color = colorRampPalette(c("white",RColorBrewer::brewer.pal(n = 7, name = "YlOrRd")))(100),
         cluster_rows = F, cluster_cols = F
)
dev.off()


# Scattterplot of Factor values
p <- plot_factors(mofa, c(1,3), color_by = "Category", dot_size = 4) +
  scale_fill_manual(values=opts$colors) +
  theme(
    legend.title = element_blank(),
  )

pdf(sprintf("%s/Factor1_vs_Factor3_category.pdf",io$outdir), width=7, height=5, useDingbats = F)
print(p)
dev.off()
