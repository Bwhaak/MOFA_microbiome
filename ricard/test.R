plot_variance_explained(mofa, max_r2 = 30)

plot_factors(mofa, factors = c(1,2), color_by = "Category", dot_size = 4) +
  scale_fill_manual(values=opts$colors)

plot_top_weights(mofa, factors=2, view="Bacteria", sign = "positive", abs=F, nfeatures = 25)

correlate_factors_with_covariates(mofa, 
  # covariates = c("length_of_stay","In_hospital_survival","SOFA","APACHE","survival_day_90","Comorbidities","Shock","AKI","ALI"),
  covariates = c("In_hospital_survival","SOFA"),
  plot = "log_pval",
)

dt <- data.table(
  x = mofa@samples_metadata$In_hospital_survival, 
  y=mofa@expectations$Z$single_group[,"Factor 5"]
)

ggboxplot(dt, x="x", y="y")
