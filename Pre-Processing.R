library(dplyr)

sightings_data <- read.csv(
  "Data/Raw Data/Sightings Data/records-2025-10-20.csv"
)

# Define column groups
species <- c("scientificName", "vernacularName", "individualCount")
taxonomy <- c("taxonConceptID", "species", "family", "genus", "order")
geographic <- c("decimalLatitude", "decimalLongitude")
temporal <- c("eventDate")

# Combine all required columns
columns <- c(species, taxonomy, geographic, temporal)

# Select only the required columns
sightings_data <- sightings_data %>% select(all_of(columns))

# Set missing individualCount to 1 (assume single bird sighting)
sightings_data$individualCount[is.na(sightings_data$individualCount)] <- 1

# Save the processed data
write.csv(sightings_data,
          "Data/Pre - Processed Data/sightings_data.csv",
          row.names = FALSE)
