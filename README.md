## This is a reproducibility archive for the paper "Comparing Likert and Visual Analogue Scales in Ecological Momentary Assessment" (Preprint: https://osf.io/preprints/psyarxiv/yt8xw)

The reproducibility archive contains code- and output files that allow one to in principle fully reproduce all results in the paper. 

We are not allowed to share the raw data openly in accordance with the data sharing guidelines of the NSMD consortium. However, they can be obtained at reasonable request from Anne Roefs <a.roefs@maastrichtuniversity.nl>. 

However, we make all downstream files openly available, which includes all the person-wise characteristics computed in the paper (e.g., person-wise means, SDs, etc.), which allow the reader to reproduce all our figures and modeling from that point.

### R-Files:

- `0_aux_Functions.R` contains functions for calculating bimodality, the initial elevation bias, and for plotting some figures
- `1_preprocess.R` takes the raw data from the experiment and prepares it for further analysis. Produces some of the files listed below.
- `2_compute_measues.R` filters out subjects that do not meet inclusion criteria and computes measures that are used in further figures and regression modeling. Produces some of the files listed below.
- `3_LikertVAS_Models_Fit.Rmd` fits Bayesian multilevel-models. Produces some of the files listed below.
- `4_LikertVAS_Models_Results.Rmd` summarizes results from the Bayesian multilevel models and produces parameter plots from the model posterior draws. Produces some of the files listed below.
- `5_plot_figures.R` takes the output of `2_compute_measues.R` and `4_LikertVAS_Models_Results.Rmd` and plots all figures in the paper


### Folders & Files

- `Data` is a folder with the raw data, which we are not allowed to share openly in accordance with the data sharing guidelines of the NSMD consortium. However, they can be obtained at reasonable request from Anne Roefs <a.roefs@maastrichtuniversity.nl>. 
- `Files` contains `*.RDS` files use to pass data and data aggregates between scripts
  - `data_BSI.RDS` is a data frame with subject ids and the BSI sum score
  - `data_PostQuestions.RDS` is a data frame with subject ids and six questions reflecting on the experience of filling in the ESM surveys after they are completed
  - `data_ESM_Lik.RDS` the raw ESM data for the Likert group *[these are effectively still the raw data, which we are currently not allowed to share, so this file is currently not included in the repository]*
  - `data_ESM_VAS.RDS` the raw ESM data for the VAS group *[these are effectively still the raw data, which we are currently not allowed to share, so this file is currently not included in the repository]*
  - `data_ESM_Lik_incl.RDS` the ESM data for the Likert group, only including subjects passing inclusion criteria *[these are effectively still the raw data, which we are currently not allowed to share, so this file is currently not included in the repository]*
  - `data_ESM_VAS_incl.RDS` the ESM data for the VAS group, only including subjects passing inclusion criteria *[these are effectively still the raw data, which we are currently not allowed to share, so this file is currently not included in the repository]*
  - `NA_prop.RDS` a list specifying the proportion of missing data for each subject; only for subjects meeting incl criteria
  - `Dropout.RDS` a list specifying the last (out of 84) measurement point for each subject; only for subjects meeting incl criteria
  - `Duration.RDS` a list specifying the mean duration for the morning / the several day / the evening ESM surveys for each subject in both groups; ; only for subjects meeting incl criteria
  - `AggData.RDS` a list with the following distribution characteristics, separately for Likert, VAS, and VAS2Likert, only for the subject passing the inclusion criteria: means, SDs, RMSSD, AR(1), mean abs correlation between all variables, bimodality, skewness, and initial elevation bias
  - `data_BSI_with_ESM.RDS` the same data as in `data_BSI.RDS`, but only for subjects passing the inclusion criteria and with the means of the ESM items appended, in separate entries for Likert, VAS, and VAS2Likert
  - `figuredata.Rdata` contains posterior samples from fitted Bayesian multilevel-models that are used to create parameter plots
  - Fitted Bayesian multilevel models`mod_AR_noLiktoVAS_fit.Rdata` contains fitted Bayesian multilevel-model for autoregression-parameters, `mod_CorLag0_beta_noLiktoVAS_fit.Rdata` contains fitted Bayesian multilevel-model for mean item correations, `mod_Mean_zib_fit.Rdata` contains fitted Bayesian multilevel-model for means including all three answer format (Likert, VAS, VAS mapped to Likert), `mod_Mean_zib_noLiktoVAS_fit.Rdata` contains fitted Bayesian multilevel-model for means, `mod_Modality_noLiktoVAS_fit.Rdata` contains fitted Bayesian multilevel-model for multi-modality, `mod_RMSSD_zib_noLiktoVAS_fit.Rdata` contains fitted Bayesian multilevel-model for RMSSDs, `mod_SD_zib_noLiktoVAS_fit.Rdata` contains fitted Bayesian multilevel-model for SDs, `mod_Skew_noLiktoVAS_fit.Rdata`skewnesses
  - Fitted Bayesian multilevel models including the VAS to Likert-scale mapped data: `Model_BSI_VAS2Lik_group_fixed_fit.Rdata` `Model_DASS_A_VAS2Lik_group_fixed_fit.Rdata` `Model_DASS_D_VAS2Lik_group_fixed_fit.Rdata` `Model_DASS_S_VAS2Lik_group_fixed_fit.Rdata` `mod_CorLag0_beta_fit.Rdata` `mod_AR_fit.Rdata` `mod_RMSSD_zib_fit.Rdata` `mod_Skew_fit.Rdata`
  - `EMM_EMV.Rdata`A single R-object containing all estimated marginal means and variances (i.e., “fixed effects”) from all models. This should allow plots that everyone understands for both the model-estimated means and variances under both response scales as well as their estimated differences (as well as proportions in % for variances) across response scales. The variables/columns in the object are:
“Median” stands for the posterior median, “lower” and “upper” for the 90% HDI boundaries.
“Likert” stands for the estimate on the Likert scale.
“VAS” stands for the estimate on the VAS.
“Diff” stands for the estimated difference between response scales
“Model” stands for the model (means, SDs, autorcorrelation, RMSSD, durations, proportions of missings, modality,…)
“Component” stands for the model component: Mean, Variance, Zero-proportion, Zero-or-one proportion (the latter exists only for the zero-one inflated beta regression model for missing proportions)
“prop” indicates proportions of parameter estimates across response scales (VAS / Likert).
  - `EMM_Mapping.Rdata` and `EMM_Mapping_Valence.Rdata` contain estimated marginal means with 90% HDIs from the VAS to Likert scale thresholding-models, and Mapping_Medians_HDIs.Rdata contains the item-level estimated correlations incl. 90% HDIs therefrom for the four external criterion variables
  - Four objects containing estimated correlations with 90% HDIs for the external criterion variables:
    `Correlations_BSI_summary_stats.Rdata`
    `Correlations_DASS_A_summary_stats.Rdata`
    `Correlations_DASS_D_summary_stats.Rdata`
    `Correlations_DASS_S_summary_stats.Rdata`
- `Figures` contains all the figures plotted by `5_plot_figures.R` except the ones sanity checking all the raw data, which we are not allowed to share (see above)