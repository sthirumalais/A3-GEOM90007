library(magick)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(stringr)

bird_data <- read.csv("Data/Pre - Processed Data/data.csv")

birds <- unique(bird_data$scientificName)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Function to query Wikipedia summary API
get_wikipedia_info <- function(scientific_name) {
  query <- gsub(" ", "_", scientific_name)
  url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", query)

  resp <- GET(url)
  if (status_code(resp) != 200) {
    return(tibble(
      scientific_name = scientific_name,
      title = NA,
      extract = NA,
      image_url = NA,
      wikipedia_url = paste0(
        "https://en.wikipedia.org/wiki/",
        gsub(" ", "_", scientific_name)
      ),
      license = NA,
      image_flag = "missing"
    ))
  }

  res <- fromJSON(content(resp, "text", encoding = "UTF-8"))

  tibble(
    scientific_name = scientific_name,
    title = res$title %||% "",
    extract = res$extract %||% "",
    image_url = res$thumbnail$source %||% "",
    wikipedia_url = res$content_urls$desktop$page %||%
      paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", scientific_name)),
    license = "CC BY-SA 4.0 (Wikipedia/Wikimedia Commons)",
    image_flag = ifelse(is.null(res$thumbnail$source), "missing", "available")
  )
}

# Apply to all data
bird_info <- lapply(birds, get_wikipedia_info)

bird_df <- bind_rows(bird_info)

write.csv(
  bird_df,
  "Data/Description Data/bird_wikipedia_data.csv",
  row.names = FALSE
)

missing_species <- bird_df %>%
  filter(image_flag == "missing") %>%
  select(scientific_name, wikipedia_url)

print(missing_species)
cat("Total missing images:", nrow(missing_species), "\n")
