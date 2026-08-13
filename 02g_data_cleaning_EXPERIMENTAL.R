# Trying out different methods for data cleaning

# --------------  KAMI PACKAGE ----------------

# Load libraries
library(move2)
library(move2utils)
library(tidyverse)

# Read in subset of data that needs to be cleaned
dataforcleaning <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/DATA/DataForCleaning/"
dbpath <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/DATA/database/MoveTrait.v0.1_individual.sum_20260807.rds"

# Load mass and mode lookup from database
db <- readRDS(dbpath)
mode_map <- c(fly = "flying", swim = "swimming", walk = "running", arboreal = "running")
db$locomotion_mode <- mode_map[db$movement.mode]
mass_lookup <- db[, c("individual_id", "animal_mass", "locomotion_mode", "species", "common_name")]

fls <- list.files(dataforcleaning, pattern = "\\.rds$", full.names = FALSE)[1:25]
data_list <- lapply(fls, function(f) {
  trk <- readRDS(paste0(dataforcleaning, f))
  sf::st_as_sf(trk, coords = c("x_", "y_"), crs = 4326, remove = FALSE) |>
    mt_as_move2(time_column = "t_", track_id_column = "individual_local_identifier")
})
names(data_list) <- tools::file_path_sans_ext(fls)

# Apply mt_clean_track to each track, joining mass and mode per individual
data_list_flagged <- lapply(data_list, function(trk) try({
  ind_id      <- unique(mt_track_data(trk)$individual_id)
  meta        <- mass_lookup[mass_lookup$individual_id %in% ind_id, ]
  mass <- if (nrow(meta) > 0 && !is.na(meta$animal_mass[1])) meta$animal_mass[1] / 1000 else NULL
  mode <- if (!is.null(mass)) meta$locomotion_mode[1] else NULL
  mt_clean_track(trk, mass = mass, mode = mode, remove = FALSE, consensus = "evidence_corroborated")
}))

failed <- sapply(data_list_flagged, inherits, "try-error")
if (any(failed)) message("Failed tracks: ", paste(names(data_list_flagged)[failed], collapse = ", "))

# Save flagged tracks as individual RDS, xlsx, and summary figures
outpath <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/DATA/DataCleanedKAMI/"
dir.create(outpath, showWarnings = FALSE)
lapply(names(data_list_flagged), function(nm) {
  trk <- data_list_flagged[[nm]]
  if (inherits(trk, "try-error")) return(NULL)
  df <- sf::st_drop_geometry(trk)
  n_total <- nrow(df)
  meta        <- mass_lookup[mass_lookup$individual_id %in% unique(df$individual_id), ]
  common_name <- if (nrow(meta) > 0) meta$common_name[1] else NA
  species     <- if (nrow(meta) > 0) meta$species[1] else NA

  # Spatial figure: track points coloured by outlier status
  p_map <- ggplot(df, aes(x = x_, y = y_, colour = is_outlier)) +
    geom_path(colour = "grey70", linewidth = 0.3) +
    geom_point(aes(size = is_outlier), alpha = 0.7) +
    scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "green3"),
                        labels = c("TRUE" = "outlier", "FALSE" = "clean")) +
    scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0.8), guide = "none") +
    labs(title = paste0(common_name, " (", species, ")"),
         subtitle = nm,
         x = "Longitude", y = "Latitude", colour = NULL,
         caption = paste0(sum(df$is_outlier), " outliers / ", n_total, " fixes")) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(paste0(outpath, nm, "_KAMI_map.pdf"), p_map, width = 6, height = 5)

  # Convert is_outlier to yes/no for both outputs
  df$is_outlier <- ifelse(df$is_outlier, "yes", "no")
  saveRDS(df, paste0(outpath, nm, "_KAMI.rds"))
  writexl::write_xlsx(df, paste0(outpath, nm, "_KAMI.xlsx"))
})
