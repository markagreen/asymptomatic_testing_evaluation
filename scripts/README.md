# Readme

Files included here are R scripts and include all portions of data cleaning, wrangling and analysis used to generate the outputs included in the paper. Specifically:

1. clean_test_data.R - this cleans up the test data and imputes missing ethnicity data (just one imputation at the moment - can do pooled analysis across multiple imputations later if needed).  It outputs a clean dataset of record level data into other_data. If we can all use this (and agree any analysis designs - e.g person id variable) - that will ensure consistency. 

2. lsoa_risk_compiler - this compiles a load of datasets all at the lsoa level for England and Wales that made be useful for later analysis. 

3.  construct_national_lsoa_ethnic_age_pop.R this constructs a population dataset split by lsoa, age group, sex, and ethnic group. The LSOA populations should all sum to the 2019 population estimates, 
And the proportion in ethnic group in each lsoa should match the census proportions - I have taken the age distribution for each ethnic group within each lsoa from the associated MSOA in the census data (lsoa breakdown is not available on NOMIS). I’ve used this as numerators in subsequent analysis. 

4. lft_uptake_analysis_28_12_20120

I have modelled uptake including the variables in Marks previous model plus 
Individual level data on age, sex, ethnicity (imputed where missing)  and 
Population density, proportion of student / population in lsoa, the proportion of care home beds / population. 

2 models - 

1 general estimating equation - multi-level using population  estimates from 3 as an offset 
- this has a few problems - population estimates  in some cells is zero - but with tests. 
In many cells the number of tests is > than population 

2. Indirectly standardised by age/sex/ethnicity - so estimates the expected number of tests in each lsoa based on  age/sex/ethnicity overall rates for Liverpool - then used this as an offset on modelling lsoa level data.  i.e modelling the log of the standardised uptake ratio - controlling for age sex and ethnicity. 

5. spatial_models_XXXX.R

Script for running the spatial models (BYM model) for ecological (LSOA) analyses. The ‘XXXX’ bit refers to the specific outcome of interest (e.g. uptake - number of people who got a test, multiple_lfts - number of people who got more than 1 test, and positivity - 

6. tidy_analytical_XXXX.R

Series of scripts that tidy and clean datasets based on Ben’s previous code to create the LSOA level analytical dataset which includes - counts for each outcome, expected counts by age, sex and ethnicity, and matched socioeconomic data from Ben’s scrips. The XXXX bit relates to what the specific outcome is. Scripts are called within the spatial model scripts.

7. summary_plots.R

Script creates summary statistics and graphs to be included in the paper.

8. calculate_descriptices.R

Script creates a table of summary statistics for each outcome, that is presented in the paper (see Table 1 and Appendix B).

9. Test_descriptives.R

Creates a table for summary statistics by numbers of tests (rather than persons)

10. multi_level_models.R

Cleans individual/record level data and analyses in multi-level model for Sensitivity Analyses in the paper (only possible for multiple tests and positivity). 
