##############################
### Make maps for Appendix ###
##############################

# Libraries
library(ggplot2)
library(viridis)
library(patchwork)
library(sf)


# 1. Load covariates for analysis from Ben's data #

load(normalizePath("other_data/lsoa_risk.RData")) # Load
ag_risk_lsoa$prop_students <- ag_risk_lsoa$prop_students / 100 # Variable is percentage, so convert to proportion for consistency
ag_risk_lsoa$ch_prop <- ag_risk_lsoa$ch_prop / 100 # Same for care home beds
# Recode ch_prop as binary (care home in LSOA)
ag_risk_lsoa$ch_binary <- NA
ag_risk_lsoa$ch_binary[ag_risk_lsoa$ch_prop == 0] <- 0
ag_risk_lsoa$ch_binary[ag_risk_lsoa$ch_prop > 0] <- 1
ag_risk_lsoa$grp_label <- as.factor(ag_risk_lsoa$grp_label) # To define as factor
ag_risk_lsoa$grp_label <- relevel(ag_risk_lsoa$grp_label, ref = "e-Veterans") # Set reference group
# Accessibility data
access <- read.csv("./other_data/distance_to_nearest_site_18Jan21.csv") # Access to test sites post pilot
access$distance <- access$distance / 1000 # Convert to km
ag_risk_lsoa <- merge(ag_risk_lsoa, access, by = "lsoa11", all.x = TRUE) # Join onto main file
access2 <- read.csv("./other_data/uptake data/access_wtdavg_lsoa.csv") # Access to test site during pilot
access2 <- access2[,c("lsoa11cd", "walkDistAvg")] # Drop variables dont need
access2$walkDistAvg <- access2$walkDistAvg / 1000 # Convert to km
ag_risk_lsoa <- merge(ag_risk_lsoa, access2, by.x = "lsoa11", by.y = "lsoa11cd", all.x = TRUE) # Join onto main file


# 2. Join to LSOA shapefile #

lsoas <- read_sf(dsn = "./other_data/liverpool_lsoa/england_lsoa_2011.shp") # Load LSOA shapefiles
lsoas <- st_transform(lsoas, 4326) # Set CRS
lsoas <- merge(lsoas, ag_risk_lsoa, by.x = "code", by.y = "lsoa11", all.x = TRUE) # Join on data onto shapefile


# 3. Make some maps woohoo #

# a. Deprivation score #

plot1 <- ggplot(lsoas) + # Plot
  geom_sf(aes(fill = imd_score), lwd = 0) +
  scale_fill_viridis() +
  labs(fill = "Score") +
  theme_void() # Remove ggplot2 stuff as a map
plot1
ggsave(plot = plot1, filename = "./output/appendix_maps_imd.jpeg") # Save

# b. Care home locations #

plot2 <- ggplot(lsoas) + # Plot
  geom_sf(aes(fill = as.factor(ch_binary)), lwd = 0) +
  scale_fill_viridis(discrete = TRUE, labels = c("No", "Yes")) +
  labs(fill = "Care home in LSOA") +
  theme_void() # Remove ggplot2 stuff as a map
plot2
ggsave(plot = plot2, filename = "./output/appendix_maps_ch.jpeg") # Save

# c. Proportion students #

plot3 <- ggplot(lsoas) + # Plot
  geom_sf(aes(fill = prop_students), lwd = 0) +
  scale_fill_viridis() +
  labs(fill = "Proportion") +
  theme_void() # Remove ggplot2 stuff as a map
plot3
ggsave(plot = plot3, filename = "./output/appendix_maps_students.jpeg") # Save

# d. Accessibility #

plot4 <- ggplot(lsoas) + # Plot
  geom_sf(aes(fill = walkDistAvg), lwd = 0) +
  scale_fill_viridis() +
  labs(fill = "Distance (km)") +
  theme_void() # Remove ggplot2 stuff as a map
plot4
ggsave(plot = plot4, filename = "./output/appendix_maps_access.jpeg") # Save

# e. IUC #

plot5 <- ggplot(lsoas) + # Plot
  geom_sf(aes(fill = grp_label), lwd = 0) +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "Area type") +
  theme_void() # Remove ggplot2 stuff as a map
plot5
ggsave(plot = plot5, filename = "./output/appendix_maps_iuc.jpeg") # Save
