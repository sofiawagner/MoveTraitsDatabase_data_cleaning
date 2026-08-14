library(lubridate);library(metafor);library(tidyverse);library(amt);library(Hmisc)
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr); library(bit64)
library(mapview)

# path to directory
pathTOfolder <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/"

# --------------  CLEANING WITH KAMI PACKAGE ----------------

# Load libraries
library(move2utils)

# Read in subset of data that needs to be cleaned
dataforcleaning <- paste0(pathTOfolder, "Data_Shir_notcleaned/")
dbpath <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/DATA/database/MoveTrait.v0.1_individual.sum_20260807.rds"

# Load mass and mode lookup from database
db <- readRDS(dbpath)
mode_map <- c(fly = "flying", swim = "swimming", walk = "running", arboreal = "running")
db$locomotion_mode <- mode_map[db$movement.mode]
mass_lookup <- db[, c("individual_id", "animal_mass", "locomotion_mode", "species", "common_name")]

fls <- list.files(dataforcleaning, pattern = "\\.rds$", full.names = FALSE)
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

# --------------  CLEANING WITH KAMI PACKAGE FINISHED ----------------

# Set to TRUE to keep only core columns + is_outlier; FALSE to keep all columns
keep_only_isoutlier_col <- TRUE
core_cols <- c("x_", "y_", "t_", "burst_", "individual_local_identifier",
               "tag_local_identifier", "individual_id", "study_id", "timestamp", "is_outlier")

# output folders
outpath_move2        <- paste0(pathTOfolder, "Data_Shir_cleaned/")          # full move2 object
pthamt1h_flagoutlier <- paste0(pathTOfolder, "5.MB_indv_amt_1h_outlspeed/") # amt object for app
dir.create(outpath_move2,        showWarnings = FALSE)
dir.create(pthamt1h_flagoutlier, showWarnings = FALSE)

lapply(names(data_list_flagged), function(nm) {
  trk <- data_list_flagged[[nm]]
  if (inherits(trk, "try-error")) return(NULL)

  # Save full move2 object
  saveRDS(trk, paste0(outpath_move2, nm, "_KAMI.rds"))

  # Save as amt object
  df <- sf::st_drop_geometry(trk)
  df$is_outlier <- ifelse(df$is_outlier, "yes", "no")
  pthamt1h_outlier <- df
  if (keep_only_isoutlier_col) pthamt1h_outlier <- select(pthamt1h_outlier, all_of(core_cols))
  pthamt1h_outlier <- structure(pthamt1h_outlier, class = c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
  saveRDS(pthamt1h_outlier, file = paste0(pthamt1h_flagoutlier, nm, ".rds"))
})

# View generated files
mv2   <- readRDS(list.files(outpath_move2, full.names = TRUE)[119])
trk   <- readRDS(list.files(pthamt1h_flagoutlier, full.names = TRUE)[119])

View(mv2)
View(trk)
