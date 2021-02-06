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

# Drop all tests where age <= 5 (there should not be any, so assume erroneous data)
testing <- testing[age>5]

# Subset liverpool LA and positive lft tests
lft_tests=testing[lft==1 & lad11cd=="E08000012" & t_date >= "2020-11-06 00:00:00" & t_date < "2021-02-01 00:00:00"]

# Subset data into time periods for analysis
lft_tests_pilot <- lft_tests[lft_tests$t_date >= "2020-11-06 00:00:00" & lft_tests$t_date < "2020-12-03 00:00:00"] # Pilot period
lft_tests_xmas <- lft_tests[lft_tests$t_date >= "2020-12-03 00:00:00" & lft_tests$t_date < "2021-01-06 00:00:00"] # Christmas period
lft_tests_lockdown <- lft_tests[lft_tests$t_date >= "2021-01-06 00:00:00" & lft_tests$t_date < "2021-02-01 00:00:00"] # Lockdown


### 2. Descriptives for paper  ###

# Clean lft data to get LSOA data
source("./scripts/tidy_analytical_multiple.R")

# Get descriptives
source("./scripts/calculate_descriptives.R")
table1 # Print
write.csv(table1, "./output/descriptives_table_multiple.csv") # Save

# Tidy
rm(lft_tests, lft_tests_pilot, lft_tests_xmas, lft_tests_lockdown, table1)


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

# Standardise/centre continuous variables
model_data$zimd_score <- scale(model_data$imd_score, center = TRUE, scale = TRUE)
model_data$zwalkDistAvg <- scale(model_data$walkDistAvg, center = TRUE, scale = TRUE)
model_data$zprop_students <- scale(model_data$prop_students, center = TRUE, scale = TRUE)

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zwalkDistAvg + factor(grp_label) + f(idx, model = "bym", graph = g)

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
  ggtitle("6th Nov-31st Jan") +
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

# Standardise/centre continuous variables
model_data$zimd_score <- scale(model_data$imd_score, center = TRUE, scale = TRUE)
model_data$zwalkDistAvg <- scale(model_data$walkDistAvg, center = TRUE, scale = TRUE)
model_data$zprop_students <- scale(model_data$prop_students, center = TRUE, scale = TRUE)

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zwalkDistAvg + factor(grp_label) + f(idx, model = "bym", graph = g)

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

# Standardise/centre continuous variables
model_data$zimd_score <- scale(model_data$imd_score, center = TRUE, scale = TRUE)
model_data$zdistance <- scale(model_data$distance, center = TRUE, scale = TRUE)
model_data$zprop_students <- scale(model_data$prop_students, center = TRUE, scale = TRUE)

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zdistance + factor(grp_label) + f(idx, model = "bym", graph = g)

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

# Standardise/centre continuous variables
model_data$zimd_score <- scale(model_data$imd_score, center = TRUE, scale = TRUE)
model_data$zdistance <- scale(model_data$distance, center = TRUE, scale = TRUE)
model_data$zprop_students <- scale(model_data$prop_students, center = TRUE, scale = TRUE)

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zdistance + factor(grp_label) + f(idx, model = "bym", graph = g)

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
  ggtitle("6th-31st Jan") +
  xlab("Longitude") +
  ylab("Latitude")
plot_m4


### 8. Create summary table for outputs ###


# Disable scientific notation (i.e. get rid of the blasted e+ bit)
options(scipen=999)

# Store FE effects (raw)
fe_1 <- setDT(model1$summary.fixed, keep.rownames = "variable") # Model 1
fe_1 <- fe_1[2:14,] # Drop intercept
fe_1 <- rbind(fe_1, data.table("variable" = "e-Veterans", "mean" = 0, "sd" = 0, "0.025quant" = 0, "0.5quant" = 0, "0.975quant" = 0, "mode" = 0, "kld" = 0)) # Add in reference category
fe_1$model <- "6th Nov - 31st Jan" # Add model label

fe_2 <- setDT(model2$summary.fixed, keep.rownames = "variable") # Model 2
fe_2 <- fe_2[2:14,] # Drop intercept
fe_2 <- rbind(fe_2, data.table("variable" = "e-Veterans", "mean" = 0, "sd" = 0, "0.025quant" = 0, "0.5quant" = 0, "0.975quant" = 0, "mode" = 0, "kld" = 0)) # Add in reference category
fe_2$model <- "6th Nov - 2nd Dec" # Add model label

fe_3 <- setDT(model3$summary.fixed, keep.rownames = "variable") # Model 3
fe_3 <- fe_3[2:14,] # Drop intercept
fe_3 <- rbind(fe_3, data.table("variable" = "e-Veterans", "mean" = 0, "sd" = 0, "0.025quant" = 0, "0.5quant" = 0, "0.975quant" = 0, "mode" = 0, "kld" = 0)) # Add in reference category
fe_3$model <- "3rd Dec - 5th Jan" # Add model label

fe_4 <- setDT(model4$summary.fixed, keep.rownames = "variable") # Model 4
fe_4 <- fe_4[2:14,] # Drop intercept
fe_4 <- rbind(fe_4, data.table("variable" = "e-Veterans", "mean" = 0, "sd" = 0, "0.025quant" = 0, "0.5quant" = 0, "0.975quant" = 0, "mode" = 0, "kld" = 0)) # Add in reference category
fe_4$model <- "6th Jan - 31st Jan" # Add model label

fe_table <- rbind(fe_1, fe_2, fe_3, fe_4) # Combine
fe_table # Print
write.csv(fe_table, "./output/fe_multiple_model_raw.csv") # Save

fe_table_rr <- cbind(fe_table[,c(9,1)], exp(fe_table[,2:7])) # Calculate relative risk
write.csv(fe_table_rr, "./output/fe_multiple_model_rr.csv") # Save
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

# Store model fit
mlik_1 <- as.data.frame(model1$mlik) # Model1
names(mlik_1)[names(mlik_1) == "V1"] <- "model1" # Rename column

mlik_2 <- as.data.frame(model2$mlik) # Model2
names(mlik_2)[names(mlik_2) == "V1"] <- "model2" # Rename column

mlik_3 <- as.data.frame(model3$mlik) # Model3
names(mlik_3)[names(mlik_3) == "V1"] <- "model3" # Rename column

mlik_4 <- as.data.frame(model4$mlik) # Model4
names(mlik_4)[names(mlik_4) == "V1"] <- "model4" # Rename column

mlik_table <- cbind(mlik_1, mlik_2, mlik_3, mlik_4) # Combine # Join together
write.csv(mlik_table, "./output/mlik_multiple_model.csv") # Save
rm(mlik_1, mlik_2, mlik_3, mlik_4) # Tidy



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

# Median data usage (GB) #

# Standardise/centre continuous variables
model_data$zimd_score <- scale(model_data$imd_score, center = TRUE, scale = TRUE)
model_data$zwalkDistAvg <- scale(model_data$walkDistAvg, center = TRUE, scale = TRUE)
model_data$zprop_students <- scale(model_data$prop_students, center = TRUE, scale = TRUE)
model_data$zmedian_data <- scale(model_data$Median.data.usage..GB., center = TRUE, scale = TRUE)

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zwalkDistAvg + zmedian_data + f(idx, model = "bym", graph = g)

# Fit model
model_5 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model_5) # Print results

# Interaction effects #

# 1. IUC

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zwalkDistAvg + factor(grp_label) + factor(grp_label)*zimd_score + f(idx, model = "bym", graph = g)

# Fit model
model_6 <- inla(formula1, family="poisson", data=model_data, E = model_data$exp_num, control.predictor = list(compute = TRUE))
summary(model_6) # Print results

# 2. Median data downloads

# Define formula
formula1 <- lft ~ zimd_score + zprop_students + ch_binary + zwalkDistAvg + zmedian_data + zmedian_data*zimd_score + f(idx, model = "bym", graph = g)

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

# Store model fit
mlik_5 <- as.data.frame(model_5$mlik) # Model5
names(mlik_5)[names(mlik_5) == "V1"] <- "model5" # Rename column

mlik_6 <- as.data.frame(model_6$mlik) # Model2
names(mlik_6)[names(mlik_6) == "V1"] <- "model6" # Rename column

mlik_7 <- as.data.frame(model_7$mlik) # Model3
names(mlik_7)[names(mlik_7) == "V1"] <- "model7" # Rename column

mlik_table <- cbind(mlik_5, mlik_6, mlik_7) # Combine # Join together
write.csv(mlik_table, "./output/mlik_multiple_sensitivity_model.csv") # Save
rm(mlik_5, mlik_6, mlik_7) # Tidy


# Tidy
rm(list=ls()) # Get rid of all data/objects
gc()
