
# WORKFKLOW TO CLEAN DATA VIA KAMI PACKAGE AND EXPORT FOR SHIR APP VISUALIZATION

library(lubridate);library(metafor);library(tidyverse);library(amt);library(Hmisc)
library(adehabitatHR); library(move2); library(epitools); library(suncalc); library(purrr); library(bit64)
library(mapview); library(move2utils)

# --------------  CLEANING WITH KAMI PACKAGE ----------------

# UNPOOLED AND V_MAX

# Read in data that needs to be cleaned
dataforcleaning <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_notcleaned/"
dbpath          <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_12 Data from Anne/DATA/database/MoveTrait.v0.1_individual.sum_20260807.rds"

# Optional: load mass and mode instead of using the automatically inferred speed cap


# Output folders
outpath_move2        <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/03_Tracks/Unpooled/RDS files/"
pthamt1h_flagoutlier <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/03_Tracks/Unpooled/AMT objects/"
outpath_autoplots    <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/04_Plots/Unpooled/"
dir.create(outpath_move2,        showWarnings = FALSE, recursive = TRUE)
dir.create(pthamt1h_flagoutlier, showWarnings = FALSE, recursive = TRUE)
dir.create(outpath_autoplots,    showWarnings = FALSE, recursive = TRUE)

# Load tracks
fls <- list.files(dataforcleaning, pattern = "\\.rds$", full.names = FALSE)
data_list <- lapply(fls, function(f) {
  trk <- readRDS(paste0(dataforcleaning, f))
  sf::st_as_sf(trk, coords = c("x_", "y_"), crs = 4326, remove = FALSE) |>
    mt_as_move2(time_column = "t_", track_id_column = "individual_local_identifier")
})
names(data_list) <- tools::file_path_sans_ext(fls)

# Load species lookup from the individual summary file received from Anne
db             <- readRDS(dbpath)
species_lookup <- unique(db[, c("study_id", "individual_id", "species", "common_name")])

# Optional: also load mass and mode to use allometric speed cap instead of auto-inferred
  # mode_map    <- c(fly = "flying", swim = "swimming", walk = "running", arboreal = "running")
  # db$locomotion_mode <- mode_map[db$movement.mode]
  # mass_lookup <- db[, c("study_id", "individual_id", "animal_mass", "locomotion_mode", "species", "common_name")]

# Clean each track; save the mt_clean_track map to maps/ as a side effect
keep_only_isoutlier_col <- TRUE
core_cols <- c("x_", "y_", "t_", "burst_", "individual_local_identifier",
               "tag_local_identifier", "individual_id", "study_id", "timestamp", "is_outlier")

data_list_flagged <- lapply(names(data_list), function(nm) {
  trk      <- data_list[[nm]]
  ind_id   <- as.character(unique(trk[["individual_id"]]))
  study_id <- as.character(unique(trk[["study_id"]]))
  # meta     <- mass_lookup[mass_lookup$individual_id %in% ind_id, ]
  # if (nrow(meta) == 0) meta <- mass_lookup[mass_lookup$study_id %in% study_id, ]
  # mass        <- if (nrow(meta) > 0 && !is.na(meta$animal_mass[1])) meta$animal_mass[1] / 1000 else NULL
  # mode        <- if (!is.null(mass)) meta$locomotion_mode[1] else NULL
  sp_row      <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  ind_from_file <- strsplit(nm, "_")[[1]][2]
  sp_name       <- if (nrow(sp_row) > 0) gsub(" ", "_", tolower(sp_row$common_name[1])) else "unknown"
  plot_nm       <- paste0(study_id[1], "_", ind_from_file, "_", sp_name, "_unpooled_autoplot.png")
  png(paste0(outpath_autoplots, plot_nm), width = 1600, height = 1200, res = 150)
  result <- try(mt_clean_track(trk, remove = FALSE,
                               consensus = "evidence_corroborated", plot = TRUE, silent = TRUE))
  dev.off()
  result
})
names(data_list_flagged) <- names(data_list)

failed <- sapply(data_list_flagged, inherits, "try-error")
if (any(failed)) message("Failed tracks: ", paste(names(data_list_flagged)[failed], collapse = ", "))

# Save cleaned data
lapply(names(data_list_flagged), function(nm) {
  trk <- data_list_flagged[[nm]]
  if (inherits(trk, "try-error")) return(NULL)
  study_id <- as.character(unique(trk[["study_id"]]))[1]
  ind_id   <- as.character(unique(trk[["individual_id"]]))[1]
  file_nm  <- paste0(study_id, "_", ind_id, "_cleaned.rds")

  # Full move2 object
  saveRDS(trk, paste0(outpath_move2, file_nm))

  # amt object (core columns + is_outlier)
  df <- sf::st_drop_geometry(trk)
  df$is_outlier <- ifelse(df$is_outlier, "yes", "no")
  if (keep_only_isoutlier_col) df <- select(df, all_of(core_cols))
  df <- structure(df, class = c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
  saveRDS(df, file = paste0(pthamt1h_flagoutlier, file_nm))
})


# POOLED

# Thresholds are fitted on all tracks of the same species group (outer), then flags are
# unioned back per individual track (inner = individual_local_identifier, which is the
# column that move2 stores in track-level data). species_group must be added to track data
# before passing to mt_clean_track.

track_summary <- do.call(rbind, lapply(names(data_list_flagged), function(nm) {
  trk      <- data_list_flagged[[nm]]
  if (inherits(trk, "try-error")) return(NULL)
  ind_id   <- as.character(unique(trk[["individual_id"]]))
  study_id <- as.character(unique(trk[["study_id"]]))
  sp_row   <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  data.frame(
    track       = nm,
    species     = if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown species",
    n_fixes     = nrow(trk),
    n_outlier   = sum(trk$is_outlier, na.rm = TRUE),
    pct_outlier = round(100 * mean(trk$is_outlier, na.rm = TRUE), 2)
  )
}))

track_summary$animal_group <- ifelse(grepl("wallaby", track_summary$species, ignore.case = TRUE), "Wallaby", "Vulture")

# Build individual_local_identifier -> species_group map
species_group_map <- do.call(rbind, lapply(names(data_list), function(nm) {
  fix_df  <- sf::st_drop_geometry(data_list[[nm]])
  ind_id  <- as.character(unique(fix_df$individual_id))
  sid     <- as.character(unique(fix_df$study_id))
  sp_row  <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% sid, ]
  sp_name <- if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown"
  data.frame(
    individual_local_identifier = as.character(unique(fix_df$individual_local_identifier)),
    species_group = ifelse(grepl("wallaby", sp_name, ignore.case = TRUE), "Wallaby", "Vulture"),
    stringsAsFactors = FALSE
  )
}))

run_pooled <- function(track_nms) {
  combined <- do.call(rbind, data_list[track_nms])
  td       <- mt_track_data(combined)
  td       <- merge(td, species_group_map, by = "individual_local_identifier", all.x = TRUE)
  attr(combined, "track_data") <- td
  result <- tryCatch(
    mt_clean_track(combined, pool_by = c("species_group", "individual_local_identifier"),
                   remove = FALSE, plot = FALSE, silent = TRUE),
    error = function(e) { message("Pooled run error: ", e$message); NULL }
  )
  if (is.null(result)) return(NULL)
  # Split immediately into per-individual list so the large combined object can be freed
  out <- lapply(track_nms, function(nm) {
    ind_loc_id <- as.character(unique(sf::st_drop_geometry(data_list[[nm]])$individual_local_identifier))
    result[result[["individual_local_identifier"]] %in% ind_loc_id, ]
  })
  setNames(out, track_nms)
}

save_pooled_plots <- function(pooled_list, track_nms) {
  for (nm in track_nms) {
    trk      <- pooled_list[[nm]]
    fix_df   <- sf::st_drop_geometry(data_list[[nm]])
    study_id <- as.character(unique(fix_df$study_id)[1])
    ind_id   <- as.character(unique(fix_df$individual_id))
    ind_from_file <- strsplit(nm, "_")[[1]][2]
    sp_row   <- species_lookup[species_lookup$individual_id %in% ind_id, ]
    if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
    sp_name  <- if (nrow(sp_row) > 0) gsub(" ", "_", tolower(sp_row$common_name[1])) else "unknown"
    plot_nm  <- paste0(study_id, "_", ind_from_file, "_", sp_name, "_pooled_autoplot.png")
    png(paste0(outpath_pooling_plots, plot_nm), width = 1600, height = 1200, res = 150)
    tryCatch(
      mt_diagnose_clean_track(trk, silent = TRUE),
      error = function(e) message("Plot error (", nm, "): ", e$message)
    )
    dev.off()
    message("Saved pooled plot: ", plot_nm)
  }
}

outpath_pooling_plots <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/04_Plots/Pooled/"
dir.create(outpath_pooling_plots, recursive = TRUE, showWarnings = FALSE)

wallaby_nms    <- track_summary$track[track_summary$animal_group == "Wallaby" & track_summary$n_fixes >= 10]
vulture_nms    <- track_summary$track[track_summary$animal_group == "Vulture" & track_summary$n_fixes >= 10]
wallaby_pooled <- run_pooled(wallaby_nms)
vulture_pooled <- run_pooled(vulture_nms)

compare_pooling <- function(pooled_list, track_nms, label) {
  do.call(rbind, lapply(track_nms, function(nm) {
    pooled_trk   <- pooled_list[[nm]]
    unpooled_trk <- data_list_flagged[[nm]]
    data.frame(
      track                = nm,
      group                = label,
      n_fixes              = nrow(data_list[[nm]]),
      pct_outlier_unpooled = round(100 * mean(unpooled_trk$is_outlier, na.rm = TRUE), 2),
      pct_outlier_pooled   = round(100 * mean(pooled_trk$is_outlier,   na.rm = TRUE), 2)
    )
  }))
}

pooling_comparison <- rbind(
  compare_pooling(wallaby_pooled, wallaby_nms, "Wallaby"),
  compare_pooling(vulture_pooled, vulture_nms, "Vulture")
)
pooling_comparison$difference <- pooling_comparison$pct_outlier_pooled - pooling_comparison$pct_outlier_unpooled
print(pooling_comparison[order(-abs(pooling_comparison$difference)), ])

# Save pooling comparison table and pooled RDS/AMT files
outpath_docs        <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/02_Documentation/"
outpath_pooled_rds  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/03_Tracks/Pooled/RDS files/"
outpath_pooled_amt  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_09_14 Advancing with the KAMI package/03_Tracks/Pooled/AMT objects/"
dir.create(outpath_docs,       recursive = TRUE, showWarnings = FALSE)
dir.create(outpath_pooled_rds, recursive = TRUE, showWarnings = FALSE)
dir.create(outpath_pooled_amt, recursive = TRUE, showWarnings = FALSE)

write.csv(pooling_comparison[order(-abs(pooling_comparison$difference)), ],
          paste0(outpath_docs, "pooled_unpooled_comparison.csv"), row.names = FALSE)
message("Saved: pooled_unpooled_comparison.csv")

# Save each individual's pooled move2 and AMT objects
save_pooled <- function(pooled_list, track_nms) {
  for (nm in track_nms) {
    trk_pooled <- pooled_list[[nm]]
    fix_df     <- sf::st_drop_geometry(data_list[[nm]])
    study_id   <- as.character(unique(fix_df$study_id))[1]
    ind_id     <- as.character(unique(fix_df$individual_id))[1]
    file_nm    <- paste0(study_id, "_", ind_id, "_cleaned.rds")
    saveRDS(trk_pooled, paste0(outpath_pooled_rds, file_nm))
    message("Saved pooled RDS: ", file_nm)
    df <- sf::st_drop_geometry(trk_pooled)
    df$is_outlier <- ifelse(df$is_outlier, "yes", "no")
    if (keep_only_isoutlier_col) df <- select(df, all_of(core_cols))
    df <- structure(df, class = c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
    saveRDS(df, paste0(outpath_pooled_amt, file_nm))
    message("Saved pooled AMT: ", file_nm)
  }
}

save_pooled(wallaby_pooled, wallaby_nms)
save_pooled(vulture_pooled, vulture_nms)


# --------------  OPTIONAL: Pooled autoplots (slow — runs mt_clean_track per individual) ----------------

save_pooled_plots(wallaby_pooled, wallaby_nms)
save_pooled_plots(vulture_pooled, vulture_nms)


# OPTIONAL STEPS FROM HERE ONWARDS

# --------------  OPTIONAL: Outlier rate along track length ----------------

track_summary <- do.call(rbind, lapply(names(data_list_flagged), function(nm) {
  trk      <- data_list_flagged[[nm]]
  if (inherits(trk, "try-error")) return(NULL)
  ind_id   <- as.character(unique(trk[["individual_id"]]))
  study_id <- as.character(unique(trk[["study_id"]]))
  sp_row   <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  data.frame(
    track       = nm,
    species     = if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown species",
    n_fixes     = nrow(trk),
    n_outlier   = sum(trk$is_outlier, na.rm = TRUE),
    pct_outlier = round(100 * mean(trk$is_outlier, na.rm = TRUE), 2)
  )
}))

print(track_summary[order(-track_summary$pct_outlier), ])
cat("Spearman correlation (n_fixes vs pct_outlier):",
    round(cor(track_summary$n_fixes, track_summary$pct_outlier, method = "spearman"), 3), "\n")

track_summary$animal_group <- ifelse(grepl("wallaby", track_summary$species, ignore.case = TRUE), "Wallaby", "Vulture")
track_sorted <- track_summary[order(track_summary$n_fixes), ]
track_sorted$rank <- seq_len(nrow(track_sorted))

library(patchwork)

p1 <- ggplot(track_sorted, aes(x = rank, y = pct_outlier, colour = animal_group)) +
  geom_line(colour = "grey60", linewidth = 0.5) +
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = c("Vulture" = "#E69F00", "Wallaby" = "#0072B2")) +
  labs(y = "Outlier (%)", x = NULL,
       title = "Outlier rate along track length (shortest to longest)",
       colour = "Species group") +
  theme_bw() +
  theme(axis.text.x = element_blank())

p2 <- ggplot(track_sorted, aes(x = rank, y = n_fixes, colour = animal_group)) +
  geom_line(colour = "grey60", linewidth = 0.5) +
  geom_point(size = 2, alpha = 0.8) +
  scale_colour_manual(values = c("Vulture" = "#E69F00", "Wallaby" = "#0072B2")) +
  scale_y_log10(labels = scales::comma) +
  labs(y = "Number of fixes (log)", x = "Tracks sorted from shortest to longest",
       colour = "Species group") +
  theme_bw()

p1 / p2 + plot_layout(heights = c(2, 1), guides = "collect")

# --------------  OPTIONAL: Compare auto speed cap vs mass/mode allometric cap ----------------

mode_map    <- c(fly = "flying", swim = "swimming", walk = "running", arboreal = "running")
db$locomotion_mode <- mode_map[db$movement.mode]
mass_lookup <- db[, c("study_id", "individual_id", "animal_mass", "locomotion_mode", "species", "common_name")]

data_list_flagged_massmode <- lapply(names(data_list), function(nm) {
  trk      <- data_list[[nm]]
  ind_id   <- as.character(unique(trk[["individual_id"]]))
  study_id <- as.character(unique(trk[["study_id"]]))
  meta     <- mass_lookup[mass_lookup$individual_id %in% ind_id, ]
  if (nrow(meta) == 0) meta <- mass_lookup[mass_lookup$study_id %in% study_id, ]
  mass     <- if (nrow(meta) > 0 && !is.na(meta$animal_mass[1])) meta$animal_mass[1] / 1000 else NULL
  mode     <- if (!is.null(mass)) meta$locomotion_mode[1] else NULL
  try(mt_clean_track(trk, mass = mass, mode = mode, remove = FALSE,
                     consensus = "evidence_corroborated", plot = FALSE, silent = TRUE))
})
names(data_list_flagged_massmode) <- names(data_list)

comparison_cap <- do.call(rbind, lapply(names(data_list), function(nm) {
  trk_auto <- data_list_flagged[[nm]]
  trk_mm   <- data_list_flagged_massmode[[nm]]
  if (inherits(trk_auto, "try-error") || inherits(trk_mm, "try-error")) return(NULL)
  ind_id   <- as.character(unique(trk_auto[["individual_id"]]))
  study_id <- as.character(unique(trk_auto[["study_id"]]))
  sp_row   <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  data.frame(
    track        = nm,
    species      = if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown",
    n_fixes      = nrow(trk_auto),
    pct_auto     = round(100 * mean(trk_auto$is_outlier, na.rm = TRUE), 2),
    pct_massmode = round(100 * mean(trk_mm$is_outlier,   na.rm = TRUE), 2)
  )
}))

comparison_cap$difference   <- comparison_cap$pct_massmode - comparison_cap$pct_auto
comparison_cap$animal_group <- ifelse(grepl("wallaby", comparison_cap$species, ignore.case = TRUE), "Wallaby", "Vulture")

total_fixes <- sum(comparison_cap$n_fixes)
cat("Total outliers (auto cap):      ", round(sum(comparison_cap$pct_auto     * comparison_cap$n_fixes / 100)), "\n")
cat("Total outliers (mass/mode cap): ", round(sum(comparison_cap$pct_massmode * comparison_cap$n_fixes / 100)), "\n")
rate_auto    <- round(sum(comparison_cap$pct_auto     * comparison_cap$n_fixes / 100) / total_fixes * 100, 2)
rate_massmode <- round(sum(comparison_cap$pct_massmode * comparison_cap$n_fixes / 100) / total_fixes * 100, 2)
cat("Overall outlier rate (auto cap):     ", rate_auto,     "%\n")
cat("Overall outlier rate (mass/mode cap):", rate_massmode, "%\n")
cat("Overall reduction with auto cap:     ", round(rate_auto - rate_massmode, 2), "pp",
    paste0("(", round((rate_massmode - rate_auto) / rate_massmode * 100), "% relative reduction)\n\n"))

for (grp in c("Wallaby", "Vulture")) {
  sub       <- comparison_cap[comparison_cap$animal_group == grp, ]
  grp_fixes <- sum(sub$n_fixes)
  r_auto    <- round(sum(sub$pct_auto     * sub$n_fixes / 100) / grp_fixes * 100, 2)
  r_mm      <- round(sum(sub$pct_massmode * sub$n_fixes / 100) / grp_fixes * 100, 2)
  cat(grp, "outlier rate (auto cap):     ", r_auto, "%\n")
  cat(grp, "outlier rate (mass/mode cap):", r_mm,   "%\n")
  cat(grp, "reduction with auto cap:     ", round(r_auto - r_mm, 2), "pp",
      paste0("(", round((r_mm - r_auto) / r_mm * 100), "% relative reduction)\n\n"))
}
print(comparison_cap[order(-abs(comparison_cap$difference)), ])

comparison_long <- tidyr::pivot_longer(comparison_cap,
  cols      = c(pct_auto, pct_massmode),
  names_to  = "method",
  values_to = "pct_outlier"
)
comparison_long$method <- ifelse(comparison_long$method == "pct_auto", "Auto cap", "Mass/mode cap")
comparison_cap$track   <- factor(comparison_cap$track, levels = comparison_cap$track[order(comparison_cap$difference)])

print(
  ggplot() +
    geom_segment(data = comparison_cap,
                 aes(x = pct_auto, xend = pct_massmode,
                     y = track, yend = track, colour = animal_group), alpha = 0.5) +
    geom_point(data = comparison_long,
               aes(x = pct_outlier, y = track, shape = method, colour = animal_group), size = 2) +
    scale_colour_manual(values = c("Vulture" = "#E69F00", "Wallaby" = "#0072B2")) +
    facet_wrap(~animal_group, scales = "free_y") +
    labs(x = "Outlier (%)", y = NULL,
         title = "Outlier rate: auto speed cap vs mass/mode cap",
         subtitle = "Tracks sorted by difference (auto minus mass/mode)",
         colour = "Species group", shape = "Method") +
    theme_bw() +
    theme(axis.text.y = element_text(size = 6))
)


# --------------  OPTIONAL: Save maps ----------------

library(move2); library(move2utils); library(sf); library(bit64)

.outpath_move2 <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
.outpath_maps  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_20 Exploring the KAMI package/Data_Shir_cleaned_visualization_plots/map_plots/"
dir.create(.outpath_maps, recursive = TRUE, showWarnings = FALSE)

lapply(list.files(.outpath_move2, full.names = TRUE, pattern = "\\.rds$"), function(f) {
  trk         <- readRDS(f)
  nm          <- tools::file_path_sans_ext(basename(f))
  study_id    <- as.character(unique(trk[["study_id"]]))
  ind_id      <- as.character(unique(trk[["individual_id"]]))
  sp_row      <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  common_name <- if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown species"
  suffix      <- if (nrow(sp_row) > 0) paste0("_", gsub(" ", "_", tolower(common_name))) else ""

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

library(move2); library(move2utils); library(sf); library(bit64)

.outpath_move2 <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
.outpath_diag  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_20 Exploring the KAMI package/Data_Shir_cleaned_visualization_plots/diagnostic_plots/"
dir.create(.outpath_diag, recursive = TRUE, showWarnings = FALSE)

lapply(list.files(.outpath_move2, full.names = TRUE, pattern = "\\.rds$"), function(f) {
  trk         <- readRDS(f)
  nm          <- tools::file_path_sans_ext(basename(f))
  study_id    <- as.character(unique(trk[["study_id"]]))
  ind_id      <- as.character(unique(trk[["individual_id"]]))
  sp_row      <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  common_name <- if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown species"

  png(paste0(.outpath_diag, nm, ".png"), width = 3200, height = 2000, res = 150)
  par(oma = c(0, 0, 3, 0))
  mt_diagnose_clean_track(trk, cex_scale = 1.6, silent = TRUE)
  mtext(paste0(common_name, "  |  ", nm), outer = TRUE, side = 3, line = 0.5, cex = 1.5, font = 2)
  dev.off()
})

# -------------- OPTIONAL: Save species-level diagnostic summary (one PDF per species) ----------------

library(move2); library(move2utils); library(sf); library(bit64)

.outpath_move2 <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_14 Data for Shir/Data_Shir_cleaned/"
.outpath_diag  <- "/Users/sofiawagner/Desktop/CleaningRoutines_Sofia/2026_08_20 Exploring the KAMI package/Data_Shir_cleaned_visualization_plots/diagnostic_plots/"
dir.create(.outpath_diag, recursive = TRUE, showWarnings = FALSE)

fls_all <- list.files(.outpath_move2, full.names = TRUE, pattern = "\\.rds$")

get_common_name <- function(f) {
  trk     <- readRDS(f)
  study_id <- as.character(unique(trk[["study_id"]]))
  ind_id   <- as.character(unique(trk[["individual_id"]]))
  sp_row   <- species_lookup[species_lookup$individual_id %in% ind_id, ]
  if (nrow(sp_row) == 0) sp_row <- species_lookup[species_lookup$study_id %in% study_id, ]
  if (nrow(sp_row) > 0) sp_row$common_name[1] else "unknown species"
}

species_groups <- split(fls_all, sapply(fls_all, get_common_name))

for (species_name in names(species_groups)) {
  species_files <- species_groups[[species_name]]
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
