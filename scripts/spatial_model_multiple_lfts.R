######################################
### Spatial Model - multiple LFTs  ###
######################################

# Purpose: To analyse patterns for number of people who received multiple LFTs in Liverpool using a spatial modelling framework.

# Libraries
library(data.table)
library(spatialreg)
library(ggplot2)
library(spdep)
library(INLA)
library(sf)

### 1. Create analytical dataset ####

#  load clean test data
load(normalizePath("other_data/clean_testing.Rdata"))

# Subset liverpool LA and positive lft tests
lft_tests=testing[lft==1 & lad11cd=="E08000012"]

# Subset data into time periods for analysis
lft_tests_pilot <- lft_tests[lft_tests$t_date >= "2020-11-06 00:00:00" & lft_tests$t_date < "2020-12-03 00:00:00"] # Pilot period
lft_tests_xmas <- lft_tests[lft_tests$t_date >= "2020-12-03 00:00:00" & lft_tests$t_date < "2021-01-06 00:00:00"] # Christmas period
lft_tests_lockdown <- lft_tests[lft_tests$t_date >= "2021-01-06 00:00:00"] # Lockdown


### 2. Descriptives for paper  ###

# Clean lft data to get LSOA data
source("./scripts/tidy_analytical_multiple.R")

# Get descriptives
source("./scripts/calculate_descriptives.R")
table1 # Print
write.csv(table1, "./output/descriptives_table_multiple.csv") # Save


### 3. Setting up the spatial model ###

# Join onto spatial data

lsoas <- read_sf(dsn = "./other_data/liverpool_lsoa/england_lsoa_2011.shp") # Load LSOA shapefiles
lsoas <- st_transform(lsoas, 4326) # Set CRS
lsoas.nb <- poly2nb(lsoas, snap=0.0002) # Identify neighbours of each LSOA

# Index for the latent model
n <- nrow(lsoas)
lsoas$idx <- 1:n

# Define adjacency using a row-standardised matrix
lw <- nb2listw(lsoas.nb)
W <- as(as_dgRMatrix_listw(lw), "CsparseMatrix")

# Create graph for neighbours
# nb2INLA("./other_data/liverpool.graph", lsoas.nb) # Calculate graph of neighbour locations
g <- inla.read.graph(filename = "./other_data/liverpool.graph") # Load graph in


### 4. Model 1 - Tests over the whole period ###

# Join on data onto shapefile
model_data <- merge(lsoas, lsoa_lft_all, by.x = "code", by.y = "lsoa11", all.x = TRUE) 

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + walkDistAvg + factor(grp_label) + f(idx, model = "bym", graph = g)

# Fit model
model1 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model1) # Print results

# # Calculate posterior distribution of a coefficient via
# marginal_imd <- inla.smarginal(mod$marginals.fixed$imd_score) # Calculate marginal distribution
# marginal_imd <- data.frame(marginal_imd) # Convert format for plotting
# ggplot(marginal_imd, aes(x = x, y = y)) + # Plot
#   geom_line() +
#   labs(x = "Deprivation (IMD score)", y = "Density") +
#   geom_vline(xintercept = 0, col = "black", linetype="dotted") 

# If we want to calculate the relative risk for LSOAs
model_data$rr <- model1$summary.fitted.values[, "mean"] # Wrangle data
model_data$lower <- model1$summary.fitted.values[, "0.025quant"]
model_data$upper <- model1$summary.fitted.values[, "0.975quant"]

plot_m1 <- ggplot(model_data) + # Plot
  geom_sf(aes(fill = rr), lwd = 0) +
  scale_fill_gradientn(colours = c("red", "white", "blue"), # Define colours to plot
                       values = scales::rescale(c(0.18,0.9,1,1.1,7.1)), # Define values for colours
                       limits = c(0.18, 7.1)) +
  labs(fill = "Relative Risk") +
  ggtitle("6th Nov-13th Jan") +
  xlab("Longitude") +
  ylab("Latitude")
plot_m1

# # If we want to plot the random effects
# model_data$re <- mod$summary.random$idx[1:298, "mean"] # 298 is number of areas, with first column the spatial random effect
# 
# plot_re <- ggplot(model_data) + 
#   geom_sf(aes(fill = re)
#   )
# plot_re # Kind of interesting as shows higher values in student facing areas

### 5. Model 2 - Tests over pilot (time period 1) ###

# Join on data onto shapefile
model_data <- merge(lsoas, lsoa_lft_pilot, by.x = "code", by.y = "lsoa11", all.x = TRUE) 

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + walkDistAvg + factor(grp_label) + f(idx, model = "bym", graph = g)

# Fit model
model2 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model2) # Print results

# Plot Relative risk
model_data$rr <- model2$summary.fitted.values[, "mean"] # Wrangle data

plot_m2 <- ggplot(model_data) + # Plot
  geom_sf(aes(fill = rr), lwd = 0) +
  scale_fill_gradientn(colours = c("red", "white", "blue"), # Define colours to plot
                       values = scales::rescale(c(0.18,0.9,1,1.1,7.1)), # Define values for colours
                       limits = c(0.18, 7.1)) +
  labs(fill = "Relative Risk") +
  ggtitle("6th Nov-2nd Dec") +
  xlab("Longitude") +
  ylab("Latitude")
plot_m2


### 6. Model 3 - Tests over christmas (time period 2) ###

# Join on data onto shapefile
model_data <- merge(lsoas, lsoa_lft_xmas, by.x = "code", by.y = "lsoa11", all.x = TRUE) 

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + distance + factor(grp_label) + f(idx, model = "bym", graph = g)

# Fit model
model3 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model3) # Print results

# Plot Relative risk
model_data$rr <- model3$summary.fitted.values[, "mean"] # Wrangle data

plot_m3 <- ggplot(model_data) + # Plot
  geom_sf(aes(fill = rr), lwd = 0) +
  scale_fill_gradientn(colours = c("red", "white", "blue"), # Define colours to plot
                       values = scales::rescale(c(0.18,0.9,1,1.1,7.1)), # Define values for colours
                       limits = c(0.18, 7.1)) +
  labs(fill = "Relative Risk") +
  ggtitle("3rd Dec-5th Jan") +
  xlab("Longitude") +
  ylab("Latitude")
plot_m3


### 7. Model 4 - Tests over lockdown (time period 3) ###

# Join on data onto shapefile
model_data <- merge(lsoas, lsoa_lft_lockdown, by.x = "code", by.y = "lsoa11", all.x = TRUE) 

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + distance + factor(grp_label) + f(idx, model = "bym", graph = g)

# Fit model
model4 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model4) # Print results

# Plot Relative risk
model_data$rr <- model4$summary.fitted.values[, "mean"] # Wrangle data

plot_m4 <- ggplot(model_data) + # Plot
  geom_sf(aes(fill = rr), lwd = 0) +
  scale_fill_gradientn(colours = c("red", "white", "blue"), # Define colours to plot
                       values = scales::rescale(c(0.18,0.9,1,1.1,7.1)), # Define values for colours
                       limits = c(0.18, 7.1)) +
  labs(fill = "Relative Risk") +
  ggtitle("6th-13th Jan") +
  xlab("Longitude") +
  ylab("Latitude")
plot_m4


### 8. Create summary table for outputs ###


# Disable scientific notation (i.e. get rid of the blasted e+ bit)
options(scipen=999)

# Store FE effects (raw)
fe_1 <- as.data.frame(model1$summary.fixed) # Model 1
fe_1 <- fe_1[c("mean", "0.025quant", "0.975quant")] # Subset vars
colnames(fe_1) <- paste("m1", colnames(fe_1), sep = "_") # Rename

fe_2 <- as.data.frame(model2$summary.fixed) # Model 2
fe_2 <- fe_2[c("mean", "0.025quant", "0.975quant")]
colnames(fe_2) <- paste("m2", colnames(fe_2), sep = "_") # Rename

fe_3 <- as.data.frame(model3$summary.fixed) # Model 3
fe_3 <- fe_3[c("mean", "0.025quant", "0.975quant")]
colnames(fe_3) <- paste("m3", colnames(fe_3), sep = "_") # Rename

fe_4 <- as.data.frame(model4$summary.fixed) # Model 4
fe_4 <- fe_4[c("mean", "0.025quant", "0.975quant")]
colnames(fe_4) <- paste("m4", colnames(fe_4), sep = "_") # Rename

fe_table <- cbind(fe_1, fe_2, fe_3, fe_4) # Combine
fe_table # Print
write.csv(fe_table, "./output/fe_multiple_model_raw.csv") # Save

fe_table <- exp(fe_table) # Calculate relative risk
write.csv(fe_table, "./output/fe_multiple_model_rr.csv") # Save
rm(fe_1, fe_2, fe_3, fe_4) # Tidy

# Store RE effects
re_1 <- as.data.frame(model1$summary.hyperpar) # Model1
re_1 <- re_1[c("mean", "0.025quant", "0.975quant")] # Subset vars
colnames(re_1) <- paste("m1", colnames(re_1), sep = "_") # Rename

re_2 <- as.data.frame(model2$summary.hyperpar) # Model2
re_2 <- re_2[c("mean", "0.025quant", "0.975quant")] # Subset vars
colnames(re_2) <- paste("m2", colnames(re_2), sep = "_") # Rename

re_3 <- as.data.frame(model3$summary.hyperpar) # Model3
re_3 <- re_3[c("mean", "0.025quant", "0.975quant")] # Subset vars
colnames(re_3) <- paste("m3", colnames(re_3), sep = "_") # Rename

re_4 <- as.data.frame(model4$summary.hyperpar) # Model4
re_4 <- re_4[c("mean", "0.025quant", "0.975quant")] # Subset vars
colnames(re_4) <- paste("m4", colnames(re_4), sep = "_") # Rename

re_table <- cbind(re_1, re_2, re_3, re_4) # Combine
re_table # Print
write.csv(re_table, "./output/re_multiple_models.csv") # Save
rm(re_1, re_2, re_3, re_4) # Tidy


### 9. Put plots together ###

# Combine all plots together
library(patchwork)
map <- plot_m1 + plot_m2 + plot_m3 + plot_m4 +
  plot_layout(ncol = 2, guides = "collect")
map

ggsave(plot = map, filename = "./output/rr_maps_multiple_highres.tiff", dpi = 300)
ggsave(plot = map, filename = "./output/rr_maps_multiple_lowres.jpeg")


### 10. Sensitivity analyses ###

# Load Ofcom data file and add to covariates
ofcom <- read.csv("./other_data/Ofcom_iUsage_by_LSOA.csv") # Load
lsoa_lft_all <- merge(lsoa_lft_all, ofcom, by.x = "lsoa11", by.y = "LSOA11CD", all.x = TRUE) # Join

# Join on data onto shapefile
model_data <- merge(lsoas, lsoa_lft_all, by.x = "code", by.y = "lsoa11", all.x = TRUE) 

# Median data usage (GB)

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + walkDistAvg + Median.data.usage..GB. + f(idx, model = "bym", graph = g)

# Fit model
model_5 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model_5) # Print results

# Interaction effects

# IUC

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + walkDistAvg + factor(grp_label) + factor(grp_label)*imd_score + f(idx, model = "bym", graph = g)

# Fit model
model_6 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model_6) # Print results

# Median data

# Define formula
formula1 <- lft ~ imd_score + prop_students + ch_prop + walkDistAvg + Median.data.usage..GB. + Median.data.usage..GB.*imd_score + f(idx, model = "bym", graph = g)

# Fit model
model_7 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model_7) # Print results

# Create outputs

# Disable scientific notation (i.e. get rid of the blasted e+ bit)
options(scipen=999)

# Store FE effects (raw)
fe_5 <- as.data.frame(model_5$summary.fixed) # Model 1
fe_5 <- fe_5[c("mean", "0.025quant", "0.975quant")] # Subset vars

fe_6 <- as.data.frame(model_6$summary.fixed) # Model 2
fe_6 <- fe_6[c("mean", "0.025quant", "0.975quant")]

fe_7 <- as.data.frame(model_7$summary.fixed) # Model 3
fe_7 <- fe_7[c("mean", "0.025quant", "0.975quant")]

fe_table2 <- rbind(fe_5, fe_6, fe_7) # Combine
fe_table2 # Print
write.csv(fe_table2, "./output/fe_multiple_sensitivity_raw.csv") # Save

fe_table2 <- exp(fe_table2) # Calculate relative risk
write.csv(fe_table2, "./output/fe_multiple_sensitivity_rr.csv") # Save
rm(fe_5, fe_6, fe_7) # Tidy

# Tidy
rm(list=ls()) # Get rid of all data/objects
gc()