# Readme

Files included here are R scripts and include all portions of data cleaning, wrangling and analysis used to generate the outputs included in the paper. Code was generated by Mark Green (lead), Ben Barr and David Hughes (both support). You may be able to tell who wrote what given the different styles to coding! Specific files include:

1. clean_test_data.R - this cleans up the test data and imputes missing ethnicity data (just one imputation at the moment - can do pooled analysis across multiple imputations later if needed).  It outputs a clean dataset of record level data into other_data. If we can all use this (and agree any analysis designs - e.g person id variable) - that will ensure consistency. 

2. spatial_models_XXXX.R - Script for running the spatial models (BYM model) for ecological (LSOA) analyses. The ‘XXXX’ bit refers to the specific outcome of interest (e.g. uptake - number of people who got a test, multiple_lfts - number of people who got more than 1 test, and positivity - 

3. tidy_analytical_XXXX.R - Series of scripts that tidy and clean datasets based on Ben’s previous code to create the LSOA level analytical dataset which includes - counts for each outcome, expected counts by age, sex and ethnicity, and matched socioeconomic data from Ben’s scrips. The XXXX bit relates to what the specific outcome is. Scripts are called within the spatial model scripts.

4. summary_plots.R - Script creates summary statistics and graphs to be included in the paper.

5. calculate_descriptives.R - Script creates a table of summary statistics for each outcome, that is presented in the paper (see Table 1 and Appendix B).

6. Test_descriptives.R - Creates a table for summary statistics by numbers of tests (rather than persons)

7. multi_level_models.R - Cleans individual/record level data and analyses in multi-level model for Sensitivity Analyses in the paper (only possible for multiple tests and positivity). 

8. create_reg_plots.R - Creates the summary plots for the spatial regression models (Figures 2-4).

9. confirmatory_PCRs.R - Runs analysis presented in Appendix F for checking how often conformatory PCRs were taken on positive LFTs.

10. appendix_maps.R - Generates all maps presented in Appendix G.

In terms of workflow, clean_test_data.R was run first to process the raw data. Then the 'spatial_models' files were run individually, which each clean files further and run all analyses including descriptives and Bayesian analytical models (the scripts call the 'descriptives' and 'tidy' files within them). Summary_plots.R and create_reg_plots.R were used for creating additional plots for the paper. Finally, multi_level_models.R, confirmatory_PCRs.R and appendix_maps.R were used for supplementary/sensitivity analyses within the Appendix.

We are unable to share data openly here due to the sensitive nature of the data. Data are accessible via CIPHA. Requests can be made to the Data Access Committee for extracts of the larger-scale data which cannot be released openly due to information governance requirements.
