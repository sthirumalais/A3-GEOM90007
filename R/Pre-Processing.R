library(dplyr)

# Read data
sightings_data <- read.csv(
  "Data/Sightings Data/records-2025-10-20.csv"
)
wikipedia_data <- read.csv(
  "Data/Description Data/bird_wikipedia_data.csv"
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

# Clean data
sightings_data$individualCount[is.na(sightings_data$individualCount)] <- 1

sightings_data <- sightings_data %>%
  filter(
    !grepl("Streptopelia|Lalage|AVES", scientificName, ignore.case = TRUE)
  ) %>%
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
  ) %>%
  select(-scientificName)

colnames(sightings_data)[
  colnames(sightings_data) == "species"
] <- "scientificName"

# Add image paths and descriptions
image_dirs <- list.dirs(
  "Data/Image Data",
  full.names = FALSE,
  recursive = FALSE
)
image_mapping <- data.frame(
  scientificName = gsub("_", " ", image_dirs),
  images = file.path("Data/Image Data", image_dirs),
  stringsAsFactors = FALSE
)

wikipedia_mapping <- wikipedia_data %>%
  select(scientific_name, extract) %>%
  rename(scientificName = scientific_name, description = extract)

sightings_data <- sightings_data %>%
  left_join(image_mapping, by = "scientificName") %>%
  left_join(wikipedia_mapping, by = "scientificName")

# Clean Unicode characters
clean_unicode <- function(text) {
  if (is.na(text)) {
    return(text)
  }
  text <- iconv(text, to = "UTF-8", sub = "")
  text <- gsub("\u2013|\u2014", "-", text)
  text <- gsub("\u201C|\u201D", '"', text)
  text <- gsub("\u2018|\u2019", "'", text)
  text <- gsub("\u2026", "...", text)
  text <- gsub("\u00B0", " degrees", text)
  text <- gsub("\u00D7", "x", text)
  text <- gsub("\u00B1", "+/-", text)
  text <- gsub("[^\x20-\x7E]", "", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

sightings_data$vernacularName <- sapply(
  sightings_data$vernacularName,
  clean_unicode
)
sightings_data$family <- sapply(sightings_data$family, clean_unicode)
sightings_data$genus <- sapply(sightings_data$genus, clean_unicode)
sightings_data$order <- sapply(sightings_data$order, clean_unicode)
sightings_data$description <- sapply(sightings_data$description, clean_unicode)

frequency <- table(sightings_data$scientificName)
p <- quantile(frequency, c(.5, .75, .95))
categories_map <- function(n) {
  ifelse(
    n >= p[3],
    "Common",
    ifelse(
      n >= p[2],
      "Fairly Common",
      ifelse(n >= p[1], "Uncommon", ifelse(n >= 10, "Rare", "Vagrant"))
    )
  )
}
categories <- data.frame(
  scientificName = names(frequency),
  frequency_category = categories_map(frequency),
  stringsAsFactors = FALSE
)
sightings_data <- merge(
  sightings_data,
  categories,
  by = "scientificName",
  all.x = TRUE
)

sightings_data$thumbnail <- file.path(
  "Data/Icons",
  paste0(sightings_data$order, ".svg")
)

write.csv(
  sightings_data,
  "Data/Pre - Processed Data/data.csv",
  row.names = FALSE
)
