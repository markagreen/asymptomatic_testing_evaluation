# Create descriptives 

### Whole period ###

# Create summary table
table1 <- data.frame(Var1 = character(), Freq = numeric(), Pop = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table1[1,] <- c("Whole period", 0, 0) # Data type
table1[2,] <- c("Total", nrow(lft_tests), 498042) # Total

hold <- as.data.frame(table(lft_tests$sex)) # Sex
hold$Pop <- c(249334, 248708) # Population (f,m)
table1 <- rbind(table1, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests$age)) # Repeat for age
hold$Pop <- c(82416, 168548, 195336, 51742)
table1 <- rbind(table1, hold)

hold <- as.data.frame(table(lft_tests$eth_group_imp)) # Repeat for ethnicity
hold$Pop <- c(19403, 12308, 11756, 8277, 414671) # Asian, Black, Mixed, Other, White
table1 <- rbind(table1, hold)

# Repeat for IMD quintile - Liverpool
hold <- lsoa_lft_all[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "quint_imd"] # Aggregate tests and pop by Liverpool quintile
names(hold)[names(hold) == "quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Liv_quintile_", as.character(hold$Var1)) # Rename to make clear
table1 <- rbind(table1, hold)

# Repeat for IMD quintile - England
hold <- lsoa_lft_all[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "eng_quint_imd"] # Aggregate tests and pop by England quintile
names(hold)[names(hold) == "eng_quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Eng_quintile_", as.character(hold$Var1)) # Rename to make clear
table1 <- rbind(table1, hold)

# Calculate percentages
table1$Freq <- as.numeric(table1$Freq) # Not sure why converts to character but convert back
table1$Pop <- as.numeric(table1$Pop)
table1$Percentage <- (table1$Freq / table1$Pop) * 100

### Pilot ###

# Create summary table
table2 <- data.frame(Var1 = character(), Freq = numeric(), Pop = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table2[1,] <- c("Pilot", 0, 0) # Data type
table2[2,] <- c("Total", nrow(lft_tests_pilot), 498042) # Total

hold <- as.data.frame(table(lft_tests_pilot$sex)) # Sex
hold$Pop <- c(249334, 248708) # Population (f,m)
table2 <- rbind(table2, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests_pilot$age)) # Repeat for age
hold$Pop <- c(82416, 168548, 195336, 51742)
table2 <- rbind(table2, hold)

hold <- as.data.frame(table(lft_tests_pilot$eth_group_imp)) # Repeat for ethnicity
hold$Pop <- c(19403, 12308, 11756, 8277, 414671) # Asian, Black, Mixed, Other, White
table2 <- rbind(table2, hold)

# Repeat for IMD quintile - Liverpool
hold <- lsoa_lft_pilot[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "quint_imd"] # Aggregate tests and pop by Liverpool quintile
names(hold)[names(hold) == "quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Liv_quintile_", as.character(hold$Var1)) # Rename to make clear
table2 <- rbind(table2, hold)

# Repeat for IMD quintile - England
hold <- lsoa_lft_pilot[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "eng_quint_imd"] # Aggregate tests and pop by England quintile
names(hold)[names(hold) == "eng_quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Eng_quintile_", as.character(hold$Var1)) # Rename to make clear
table2 <- rbind(table2, hold)

# Calculate percentages
table2$Freq <- as.numeric(table2$Freq) # Not sure why converts to character but convert back
table2$Pop <- as.numeric(table2$Pop)
table2$Percentage <- (table2$Freq / table2$Pop) * 100

### Christmas ###

# Create summary table
table3 <- data.frame(Var1 = character(), Freq = numeric(), Pop = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table3[1,] <- c("Christmas", 0, 0) # Data type
table3[2,] <- c("Total", nrow(lft_tests_xmas), 498042) # Total

hold <- as.data.frame(table(lft_tests_xmas$sex)) # Sex
hold$Pop <- c(249334, 248708) # Population (f,m)
table3 <- rbind(table3, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests_xmas$age)) # Repeat for age
hold$Pop <- c(82416, 168548, 195336, 51742)
table3 <- rbind(table3, hold)

hold <- as.data.frame(table(lft_tests_xmas$eth_group_imp)) # Repeat for ethnicity
hold$Pop <- c(19403, 12308, 11756, 8277, 414671) # Asian, Black, Mixed, Other, White
table3 <- rbind(table3, hold)

# Repeat for IMD quintile - Liverpool
hold <- lsoa_lft_xmas[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "quint_imd"] # Aggregate tests and pop by Liverpool quintile
names(hold)[names(hold) == "quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Liv_quintile_", as.character(hold$Var1)) # Rename to make clear
table3 <- rbind(table3, hold)

# Repeat for IMD quintile - England
hold <- lsoa_lft_xmas[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "eng_quint_imd"] # Aggregate tests and pop by England quintile
names(hold)[names(hold) == "eng_quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Eng_quintile_", as.character(hold$Var1)) # Rename to make clear
table3 <- rbind(table3, hold)

# Calculate percentages
table3$Freq <- as.numeric(table3$Freq) # Not sure why converts to character but convert back
table3$Pop <- as.numeric(table3$Pop)
table3$Percentage <- (table3$Freq / table3$Pop) * 100


### Lockdown ###

# Create summary table
table4 <- data.frame(Var1 = character(), Freq = numeric(), Pop = numeric(), stringsAsFactors = FALSE) # Create blank table for storing results
table4[1,] <- c("Lockdown", 0, 0) # Data type
table4[2,] <- c("Total", nrow(lft_tests_lockdown), 498042) # Total

hold <- as.data.frame(table(lft_tests_lockdown$sex)) # Sex
hold$Pop <- c(249334, 248708) # Population (f,m)
table4 <- rbind(table4, hold) # Join onto main table

hold <- as.data.frame(table(lft_tests_lockdown$age)) # Repeat for age
hold$Pop <- c(82416, 168548, 195336, 51742)
table4 <- rbind(table4, hold)

hold <- as.data.frame(table(lft_tests_lockdown$eth_group_imp)) # Repeat for ethnicity
hold$Pop <- c(19403, 12308, 11756, 8277, 414671) # Asian, Black, Mixed, Other, White
table4 <- rbind(table4, hold)

# Repeat for IMD quintile - Liverpool
hold <- lsoa_lft_lockdown[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "quint_imd"] # Aggregate tests and pop by Liverpool quintile
names(hold)[names(hold) == "quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Liv_quintile_", as.character(hold$Var1)) # Rename to make clear
table4 <- rbind(table4, hold)

# Repeat for IMD quintile - England
hold <- lsoa_lft_lockdown[, list(Freq = sum(lft, na.rm=T), Pop = sum(total_pop, na.rm=T)), by = "eng_quint_imd"] # Aggregate tests and pop by England quintile
names(hold)[names(hold) == "eng_quint_imd"] <- "Var1" # Rename
hold$Var1 <- paste("Eng_quintile_", as.character(hold$Var1)) # Rename to make clear
table4 <- rbind(table4, hold)

# Calculate percentages
table4$Freq <- as.numeric(table4$Freq) # Not sure why converts to character but convert back
table4$Pop <- as.numeric(table4$Pop)
table4$Percentage <- (table4$Freq / table4$Pop) * 100


### Combine ###

table1 <- rbind(table1, table2, table3, table4)
rm(table2, table3, table4, hold)
