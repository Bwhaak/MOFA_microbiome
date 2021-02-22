

#####################
## Define settings ##
#####################

source("/Users/ricard/MOFA_microbiome/load_settings.R")

# I/O
io$outdir <- paste0(io$basedir,"/results/downsampling")

# Options
opts$downsampling.fraction <- seq(from=0.50, to=1, by=0.05)
opts$trials <- 10

###############
## Load data ##
###############

data <- fread(io$normalised.data)

######################################################
## Fit a MOFA object for each downsampling fraction ##
######################################################

for (i in opts$downsampling) {
  for (j in 1:opts$trials) {
    set.seed(j)
    
    # Downsample
    samples <- unique(data$sample)
    downsampled.samples <- sample(samples, size=round(length(samples)*i))
    data_filt <- data[sample%in%downsampled.samples]
    
    # Create MOFA object
    mofa <- create_mofa(data_filt)
    
    # Define model options
    model_opts <- get_default_model_options(mofa)
    model_opts$num_factors <- 10
    
    # Define training options
    train_opts <- get_default_training_options(mofa)
    train_opts$convergence_mode <- "medium"
    
    # Prepare MOFA object
    mofa <- prepare_mofa(mofa,
      model_options = model_opts,
      training_options = train_opts
    )
    
    # Train the model 
    outfile <- sprintf("%s/%s_%s.hdf5", io$outdir,i,j)
    run_mofa(mofa, outfile)
  }
}

