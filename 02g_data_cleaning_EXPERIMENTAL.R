
# WORKFKLOW TO CLEAN DATA VIA KAMI PACKAGE AND EXPORT FOR SHIR APP INTEGRATION

library(lubridate);library(metafor);library(tidyverse);library(amt);library(Hmisc)
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr); library(bit64)
library(mapview); library(move2utils)

# --------------  CLEANING WITH KAMI PACKAGE ----------------

# Read in subset of data that needs to be cleaned (119 tracks from both turkey vultures and wallabies)
dataforcleaning <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_notcleaned/"
dbpath          <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_12 Data from Anne/DATA/database/MoveTrait.v0.1_individual.sum_20260807.rds"

# Load mass and mode lookup from database
db <- readRDS(dbpath)
mode_map <- c(fly = "flying", swim = "swimming", walk = "running", arboreal = "running")
db$locomotion_mode <- mode_map[db$movement.mode]
mass_lookup <- db[, c("study_id", "individual_id", "animal_mass", "locomotion_mode", "species", "common_name")]

# Output folders
outpath_move2        <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
pthamt1h_flagoutlier <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/5.MB_indv_amt_1h_outlspeed/"
dir.create(outpath_move2,        showWarnings = FALSE)
dir.create(pthamt1h_flagoutlier, showWarnings = FALSE)

# Load tracks
fls <- list.files(dataforcleaning, pattern = "\\.rds$", full.names = FALSE)
data_list <- lapply(fls, function(f) {
  trk <- readRDS(paste0(dataforcleaning, f))
  sf::st_as_sf(trk, coords = c("x_", "y_"), crs = 4326, remove = FALSE) |>
    mt_as_move2(time_column = "t_", track_id_column = "individual_local_identifier")
})
names(data_list) <- tools::file_path_sans_ext(fls)

# Clean each track; save the mt_clean_track map to maps/ as a side effect
keep_only_isoutlier_col <- TRUE
core_cols <- c("x_", "y_", "t_", "burst_", "individual_local_identifier",
               "tag_local_identifier", "individual_id", "study_id", "timestamp", "is_outlier")

data_list_flagged <- lapply(names(data_list), function(nm) {
  trk      <- data_list[[nm]]
  ind_id   <- as.character(unique(trk[["individual_id"]]))
  study_id <- as.character(unique(trk[["study_id"]]))
  meta     <- mass_lookup[mass_lookup$individual_id %in% ind_id, ]
  if (nrow(meta) == 0) meta <- mass_lookup[mass_lookup$study_id %in% study_id, ]
  mass        <- if (nrow(meta) > 0 && !is.na(meta$animal_mass[1])) meta$animal_mass[1] / 1000 else NULL
  mode        <- if (!is.null(mass)) meta$locomotion_mode[1] else NULL
  try(mt_clean_track(trk, mass = mass, mode = mode, remove = FALSE,
                     consensus = "evidence_corroborated", plot = FALSE, silent = TRUE))
})
names(data_list_flagged) <- names(data_list)

failed <- sapply(data_list_flagged, inherits, "try-error")
if (any(failed)) message("Failed tracks: ", paste(names(data_list_flagged)[failed], collapse = ", "))

# Save cleaned data
lapply(names(data_list_flagged), function(nm) {
  trk <- data_list_flagged[[nm]]
  if (inherits(trk, "try-error")) return(NULL)

  # Full move2 object
  saveRDS(trk, paste0(outpath_move2, nm, "_KAMI.rds"))

  # amt object (core columns + is_outlier)
  df <- sf::st_drop_geometry(trk)
  df$is_outlier <- ifelse(df$is_outlier, "yes", "no")
  if (keep_only_isoutlier_col) df <- select(df, all_of(core_cols))
  df <- structure(df, class = c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
  saveRDS(df, file = paste0(pthamt1h_flagoutlier, nm, ".rds"))
})

# --------------  OPTIONAL: Save maps ----------------
# Self-contained — run independently after cleaning, no need to re-run the cleaning section.

library(move2); library(move2utils); library(sf); library(bit64)

.outpath_move2 <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
.outpath_maps  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_20 Exploring the KAMI package/Data_Shir_cleaned_visualization_plots/map_plots/"
dir.create(.outpath_maps, recursive = TRUE, showWarnings = FALSE)

lapply(list.files(.outpath_move2, full.names = TRUE, pattern = "\\.rds$"), function(f) {
  trk         <- readRDS(f)
  nm          <- tools::file_path_sans_ext(basename(f))
  common_name <- if (startsWith(nm, "481458_")) "Turkey Vulture" else if (startsWith(nm, "268904527_")) "Wallaby" else "unknown species"
  suffix      <- if (startsWith(nm, "481458_")) "_turkey_vulture" else if (startsWith(nm, "268904527_")) "_wallaby" else ""

  cc      <- sf::st_coordinates(trk)
  flagged <- which(trk$is_outlier)
  col_pts <- ifelse(is.na(trk$block_id[flagged]), "red", "orange")
  n_flag  <- length(flagged)
  pct     <- round(100 * n_flag / nrow(trk), 2)

  png(paste0(.outpath_maps, nm, suffix, ".png"), width = 2000, height = 2000, res = 150)
  par(mar = c(3, 3, 4, 1))
  plot(cc[, 1], cc[, 2], type = "l", col = "grey20", lwd = 0.8,
       xlab = "Longitude", ylab = "Latitude", asp = 1)
  if (n_flag > 0) {
    points(cc[flagged, , drop = FALSE], col = adjustcolor(col_pts, 0.8), pch = 20, cex = 1.2)
    legend("topright", legend = c("individual outlier", "block outlier"),
           col = c("red", "orange"), pch = 20, bty = "n", bg = "white")
  }
  title(main = paste0(common_name, "  |  ", nm, "\n", n_flag, " flagged (", pct, "%)"))
  dev.off()
})

# --------------  OPTIONAL: Save diagnostic plots ----------------
# Self-contained — run independently after cleaning, no need to re-run the cleaning section.

library(move2); library(move2utils); library(sf); library(bit64)

.outpath_move2 <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
.outpath_diag  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_20 Exploring the KAMI package/Data_Shir_cleaned_visualization_plots/diagnostic_plots/"
dir.create(.outpath_diag, recursive = TRUE, showWarnings = FALSE)

lapply(list.files(.outpath_move2, full.names = TRUE, pattern = "\\.rds$"), function(f) {
  trk         <- readRDS(f)
  nm          <- tools::file_path_sans_ext(basename(f))
  common_name <- if (startsWith(nm, "481458_")) "Turkey Vulture" else if (startsWith(nm, "268904527_")) "Wallaby" else "unknown species"

  png(paste0(.outpath_diag, nm, ".png"), width = 3200, height = 2000, res = 150)
  par(oma = c(0, 0, 3, 0))
  mt_diagnose_clean_track(trk, cex_scale = 1.6, silent = TRUE)
  mtext(paste0(common_name, "  |  ", nm), outer = TRUE, side = 3, line = 0.5, cex = 1.5, font = 2)
  dev.off()
})

# -------------- OPTIONAL: Save species-level diagnostic summary (one PDF per species) ----------------
# Self-contained — run independently after cleaning, no need to re-run the cleaning section.

library(move2); library(move2utils); library(sf); library(bit64)

.outpath_move2 <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
.outpath_diag  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_20 Exploring the KAMI package/Data_Shir_cleaned_visualization_plots/diagnostic_plots/"
dir.create(.outpath_diag, recursive = TRUE, showWarnings = FALSE)

fls_all <- list.files(.outpath_move2, full.names = TRUE, pattern = "\\.rds$")

species_prefix <- list("Turkey Vulture" = "481458_", "Wallaby" = "268904527_")

for (species_name in names(species_prefix)) {
  prefix        <- species_prefix[[species_name]]
  species_files <- fls_all[startsWith(basename(fls_all), prefix)]
  if (length(species_files) == 0) next

  pdf(paste0(.outpath_diag, gsub(" ", "_", species_name), "_all_diagnostics.pdf"),
      width = 21.3, height = 13.3)   # ~3200x2000 px at 150 dpi
  for (f in species_files) {
    trk <- readRDS(f)
    nm  <- tools::file_path_sans_ext(basename(f))
    par(oma = c(0, 0, 3, 0))
    mt_diagnose_clean_track(trk, cex_scale = 1.6, silent = TRUE)
    mtext(paste0(species_name, "  |  ", nm), outer = TRUE, side = 3, line = 0.5, cex = 1.5, font = 2)
  }
  dev.off()
  message("Saved: ", species_name, " (", length(species_files), " tracks)")
}
