library(dplyr)

sightings_data <- read.csv(
  "Data/Raw Data/Sightings Data/records-2025-10-20.csv"
)

# Define column groups
species <- c("scientificName", "species", "vernacularName", "individualCount")
taxonomy <- c("family", "genus", "order")
geographic <- c("decimalLatitude", "decimalLongitude")
temporal <- c("eventDate")

# Combine all required columns
columns <- c(species, taxonomy, geographic, temporal)

# Select only the required columns
sightings_data <- sightings_data %>% select(all_of(columns))

sightings_data %>%
  distinct(scientificName) %>%
  nrow()

# Set missing individualCount to 1 (assume single bird sighting)
sightings_data$individualCount[is.na(sightings_data$individualCount)] <- 1

sightings_data <- sightings_data %>%
  filter(
    !grepl(
      "Streptopelia|Lalage|AVES",
      scientificName,
      ignore.case = TRUE
    )
  )

# For "Porzana" in scientific name, set species name and common name
sightings_data <- sightings_data %>%
  mutate(
    species = ifelse(
      grepl("Porzana", scientificName, ignore.case = TRUE),
      "Porzana fluminea",
      species
    ),
    vernacularName = ifelse(
      grepl("Porzana", scientificName, ignore.case = TRUE),
      "Australian Spotted Crake",
      vernacularName
    )
  )

sightings_data <- sightings_data %>% select(-scientificName)

colnames(sightings_data)[
  colnames(sightings_data) == "species"
] <- "scientificName"

# Save the processed data
write.csv(
  sightings_data,
  "Data/Pre - Processed Data/sightings_data.csv",
  row.names = FALSE
)
