source("/Users/ricard/MOFA_microbiome/ricard/load_settings.R")

################
## Load model ##
################

# Load model
mofa <- load_model(io$mofa)

# add metadata to the model
metadata <- fread(io$metadata) %>%
  .[sample%in%unlist(samples_names(mofa))] %>%
  setkey(sample) %>% .[unlist(samples_names(mofa))] %>%
  .[,Category:=factor(Category, levels=c("Healthy, no antibiotics", "Healthy, antibiotics", "Sepsis", "Non septic ICU"))]
samples_metadata(mofa) <- metadata

#################
## Parse model ##
#################

# subset factors
threshold.var <- 5
r2 <- mofa@cache$variance_explained$r2_per_factor[[1]]*100
factors <- which(apply(r2,1,sum) >= threshold.var)
mofa <- subset_factors(mofa, factors)

# rename factors
factors_names(mofa) <- paste("Factor",1:get_dimensions(mofa)[["K"]], sep=" ")
