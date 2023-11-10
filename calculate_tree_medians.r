# Read data
tree_data <- read.csv('bees1041_tree_data_2023.csv')

# Remove trees with incorrect height measurements 
tree_data$Angle_radians <- tree_data$Angle * (pi/180)
tree_data$tree_height_calc <- tree_data$Eye_height_m + (tree_data$Distance_m * tan(tree_data$Angle_radians))
tree_data$height_error <- abs(tree_data$Tree_height_m - tree_data$tree_height_calc)
tree_data <- subset(tree_data, height_error < 5) # Use a treshold error of 5 m

# Remove trees that are not Eucalyptus grandis
tree_data <- subset(tree_data, Tree_type == 'Eucalyptus grandis')

# Calculate median dbh and remove outliers
trees_median_dbh <- aggregate(tree_data$dbh_cm, by = list(tree_data$Name), FUN = median)
colnames(trees_median_dbh) <- c("Name", "median_dbh_cm")
tree_data_merged <- merge(tree_data, trees_median_dbh, by = 'Name')                  # merge the median values into the original data
tree_data_merged$dbh_dif <- tree_data_merged$dbh_cm - tree_data_merged$median_dbh_cm # calculate the difference between the values and the medians
tree_data_subset <- subset(tree_data_merged, dbh_dif <= 5)                           # only keep data where the difference is less than 5 cm

# Calculate median height, latitude and longitude
tree_data_median <- aggregate(cbind(tree_data_subset$dbh_cm, tree_data_subset$Tree_height_m, tree_data_subset$Latitude, tree_data_subset$Longitude),
                              by = list(tree_data_subset$Name), FUN = median)
colnames(tree_data_median) <- c("Name", "dbh_cm", "height_m", "latitude", "longitude")

# Save data to CSV file
write.csv(tree_data_median, "bees1041_tree_medians_2023.csv", row.names=FALSE)

# Calculate the median distance between the tree locations and the median locations
tree_data_merged <- merge(tree_data_subset, tree_data_median, by = 'Name')           # merge the median values into the original data
tree_data_merged$loc_diff <- sqrt((tree_data_merged$latitude - tree_data_merged$Latitude)^2 + (tree_data_merged$longitude - tree_data_merged$Longitude)^2)
median_degrees_loc_dif <- median(tree_data_merged$loc_diff) # Median difference in location (degrees)
median_degrees_loc_dif * 100000                             # Approximate conversion to metres (only works at Sydney's latitude)