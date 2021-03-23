####################################
### Create nice regression plots ###
####################################

# Purpose: To create plots for regression coefficients from spatial models.

# Libraries
library(ggplot2)
library(viridis)
library(patchwork)

## Figure 2 ##

# Load data and tidy
uptake <- read.csv("./output/fe_uptake_model_rr.csv") # Load
levels(uptake$variable)[levels(uptake$variable) == "zwalkDistAvg"] <- "zdistance" # Change so variables match up
uptake$model <- factor(uptake$model, levels = c("6th Nov - 31st Jan", "6th Nov - 2nd Dec", "3rd Dec - 5th Jan", "6th Jan - 31st Jan")) # Set order for plotting
pd <- position_dodge(1) # For defining spacing out

# Define if credible intervals contain 1
uptake$contain1 <- ifelse(((uptake$X0.975quant < 1 & uptake$mean < 1) | (uptake$X0.025quant > 1 & uptake$mean > 1)), 1, 0.8)

# Plot - group by period
fig2 <- ggplot(uptake, aes(x = variable, y = mean, group = model, color = model)) +
  geom_point(position = pd) +
  geom_pointrange(aes(ymin = X0.025quant, ymax = X0.975quant, group = model, color = model), position = pd) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_x_discrete(labels = c("Care home in area","e-Veterans (ref)", "Digital Seniors", "e-Cultural Creators", "e-Mainstream", "e-Professionals", "e-Rational Utilitarians", "e-Withdrawn", "Passive and Uncommitted Users", "Settled Offline Comunities", "Youthful Urban Fringe", "Access to test site", "Deprivation score", "Proportion students")) +
  ylab("Relative risk") + # Axis labels
  xlab("Variable") +
  scale_alpha_continuous(guide=FALSE) + # Make alpha legend disappear
  guides(color=guide_legend(title="Time period")) +
  coord_flip() + # Flip x and y axis around
  scale_color_viridis(discrete = TRUE) + theme_bw() # Adjust for colour vision deficiency
fig2

# Save
ggsave(plot = fig2, filename = "./output/figure2_highres.tiff", dpi = 300)
ggsave(plot = fig2, filename = "./output/figure2_lowres.jpeg")
ggsave(plot = fig2, filename = "./output/figure2.svg")


## Figure 3 ##

# Load data and tidy
multiple <- read.csv("./output/fe_multiple_model_rr.csv") # Load
levels(multiple$variable)[levels(multiple$variable) == "zwalkDistAvg"] <- "zdistance" # Change so variables match up
multiple$model <- factor(multiple$model, levels = c("6th Nov - 31st Jan", "6th Nov - 2nd Dec", "3rd Dec - 5th Jan", "6th Jan - 31st Jan")) # Set order for plotting
pd <- position_dodge(1) # For defining spacing out

# Define if credible intervals contain 1
multiple$contain1 <- ifelse(((multiple$X0.975quant < 1 & multiple$mean < 1) | (multiple$X0.025quant > 1 & multiple$mean > 1)), 1, 0.6)

# Plot - group by period
fig3 <- ggplot(multiple, aes(x = variable, y = mean, group = model, color = model)) +
  geom_point(position = pd) +
  geom_pointrange(aes(ymin = X0.025quant, ymax = X0.975quant, group = model, color = model), position = pd) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_x_discrete(labels = c("Care home in area","e-Veterans (ref)", "Digital Seniors", "e-Cultural Creators", "e-Mainstream", "e-Professionals", "e-Rational Utilitarians", "e-Withdrawn", "Passive and Uncommitted Users", "Settled Offline Comunities", "Youthful Urban Fringe", "Access to test site", "Deprivation score", "Proportion students")) +
  ylab("Relative risk") + # Axis labels
  xlab("Variable") +
  guides(color=guide_legend(title="Time period")) +
  scale_alpha_continuous(guide=FALSE) +
  coord_flip() + # Flip x and y axis around
  scale_color_viridis(discrete = TRUE) + theme_bw() # Adjust for colour vision deficiency
fig3

# Save
ggsave(plot = fig3, filename = "./output/figure3_highres.tiff", dpi = 300)
ggsave(plot = fig3, filename = "./output/figure3_lowres.jpeg")
ggsave(plot = fig3, filename = "./output/figure3.svg")


## Figure 4 ##

# Load data and tidy
positive <- read.csv("./output/fe_positivity_model_rr.csv") # Load
positive$model <- factor(positive$model, levels = c("6th Nov - 31st Jan", "6th Nov - 2nd Dec", "3rd Dec - 5th Jan", "6th Jan - 31st Jan")) # Set order for plotting
pd <- position_dodge(1) # For defining spacing out

# Define if credible intervals contain 1
positive$contain1 <- ifelse(((positive$X0.975quant < 1 & positive$mean < 1) | (positive$X0.025quant > 1 & positive$mean > 1)), 1, 0.6)

# Plot - group by period
fig4 <- ggplot(positive, aes(x = variable, y = mean, group = model, color = model)) +
  geom_point(position = pd) +
  geom_pointrange(aes(ymin = X0.025quant, ymax = X0.975quant, group = model, color = model), position = pd) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_x_discrete(labels = c("Care home in area", "Deprivation score", "Proportion students")) +
  ylab("Relative risk") + # Axis labels
  xlab("Variable") +
  scale_alpha_continuous(guide=FALSE) +
  guides(color=guide_legend(title="Time period")) +
  coord_flip() + # Flip x and y axis around
  scale_color_viridis(discrete = TRUE) + theme_bw() # Adjust for colour vision deficiency
fig4

# Save
ggsave(plot = fig4, filename = "./output/figure4_highres.tiff", dpi = 300)
ggsave(plot = fig4, filename = "./output/figure4_lowres.jpeg")
ggsave(plot = fig4, filename = "./output/figure4.svg")


# Tidy
rm(list=ls()) # Get rid of all data/objects
gc()
