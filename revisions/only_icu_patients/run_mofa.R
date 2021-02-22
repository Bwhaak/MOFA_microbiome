####################
# Define settings ##
####################

source("/Users/ricard/mofa/MOFA_microbiome/load_settings.R")
io$outfile <- paste0(io$basedir, "/icu.hdf5")

#######################
# Create MOFA object ##
#######################

data <- fread(paste0(io$basedir,"/data/data_Genus.txt.gz"))

# Subset ICU patients
metadata[Category%in%c("Healthy, no antibiotics")]
table(metadata$Category)
mofa <- create_mofa(data)

# Visualise data structure
plot_data_overview(mofa)

####################
## Define options ##
####################

# Model options
model_opts <- get_default_model_options(mofa)
model_opts$num_factors <- 5

# Training options
train_opts <- get_default_training_options(mofa)
train_opts$convergence_mode <- "medium"
train_opts$seed <- 42

#########################
## Prepare MOFA object ##
#########################

mofa <- prepare_mofa(mofa,
  model_options = model_opts,
  training_options = train_opts
)

#####################
## Train the model ##
#####################

mofa <- run_mofa(mofa, io$outfile)

