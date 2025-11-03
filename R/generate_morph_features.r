library(dplyr)
library(readxl)
library(stringr)

bird_data <- read.csv("Data/Pre - Processed Data/data.csv")

birds <- unique(bird_data$scientificName)

read_avonet_sheet <- function(sheet_name, species_col) {
  read_excel(
    "Data/Morphological Data/AVONET%20Supplementary%20dataset%201.xlsx",
    sheet = sheet_name
  ) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      species_clean = tolower(trimws(.data[[species_col]])),
      source = sheet_name
    )
}

avonet1 <- read_avonet_sheet("AVONET1_BirdLife", "Species1")
avonet2 <- read_avonet_sheet("AVONET2_eBird", "Species2")
avonet3 <- read_avonet_sheet("AVONET3_BirdTree", "Species3")

avonet_all <- bind_rows(avonet1, avonet2, avonet3) %>%
  distinct(species_clean, .keep_all = TRUE)

bird_df <- tibble(scientific_name = birds) %>%
  mutate(scientific_name_clean = tolower(trimws(scientific_name)))

manual_replace <- c(
  "parvipsitta pusilla" = "glossopsitta pusilla",
  "parvipsitta porphyrocephala" = "glossopsitta porphyrocephala"
)

bird_df <- bird_df %>%
  mutate(
    join_name = case_when(
      scientific_name_clean %in% names(manual_replace) ~ manual_replace[
        scientific_name_clean
      ],
      scientific_name_clean %in%
        avonet_all$species_clean ~ scientific_name_clean,
      TRUE ~ NA_character_
    ),
    match_status = case_when(
      scientific_name_clean %in% names(manual_replace) ~ "replaced",
      scientific_name_clean %in% avonet_all$species_clean ~ "exact",
      TRUE ~ "unmatched"
    )
  )

final_df <- bird_df %>%
  left_join(avonet_all, by = c("join_name" = "species_clean"))

cat("Total species in list:", nrow(bird_df), "\n")
cat("Exact matches:", sum(final_df$match_status == "exact"), "\n")
cat("Replaced matches:", sum(final_df$match_status == "replaced"), "\n")
cat("Unmatched:", sum(final_df$match_status == "unmatched"), "\n")

write.csv(
  final_df,
  "Data/Morphological Data/bird_list_with_avonet_traits.csv",
  row.names = FALSE
)
