library(ggplot2)

# ==============================================================================
# STEP 1: CONFIGURATION
# ==============================================================================

target_participant <- 3
target_session_type <- "Guided Breathing"

# Define the exact pairs you want to calculate
pairs_of_interest <- list(
  c("Anxiety", "Focus"),
  c("Worry", "Stress")
)

# ==============================================================================
# STEP 2: CALCULATE SPECIFIC PAIRS ONLY
# ==============================================================================

# Filter data
subset_data <- subset(all_data, 
                      Participant_ID == target_participant & 
                        Session_Type == target_session_type)

days <- sort(unique(subset_data$Day_Num))
results_list <- list()

print(paste("Tracking", length(pairs_of_interest), "pairs over", length(days), "days..."))

for (d in days) {
  
  # 1. Get data for the day
  day_data <- subset(subset_data, Day_Num == d)
  
  # 2. Reshape to Wide (REQUIRED to align the timestamps)
  # Even though we don't need a full matrix, we need 'Anxiety' and 'Focus' 
  # to be in the same row for the same time point to correlate them.
  day_clean <- aggregate(Intensity ~ Time_Offset + Dimension, 
                         data = day_data, FUN = mean, na.rm = TRUE)
  
  wide <- reshape(day_clean, idvar = "Time_Offset", timevar = "Dimension", direction = "wide")
  
  # Clean column names (remove "Intensity." prefix)
  colnames(wide) <- gsub("Intensity.", "", colnames(wide))
  
  # 3. Loop through ONLY your specific pairs
  for (pair in pairs_of_interest) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    # Check if both columns exist for this day
    if (var1 %in% colnames(wide) && var2 %in% colnames(wide)) {
      
      # --- DIRECT CALCULATION (No Matrix) ---
      # We just feed the two specific vectors into cor()
      r_value <- cor(wide[[var1]], wide[[var2]], use = "pairwise.complete.obs")
      
      # Store result
      results_list[[length(results_list) + 1]] <- data.frame(
        Day = d,
        Pair = paste(var1, "vs", var2),
        Correlation = r_value
      )
    }
  }
}

# Combine results
plot_data <- do.call(rbind, results_list)

# ==============================================================================
# STEP 3: PLOT
# ==============================================================================

if (!is.null(plot_data) && nrow(plot_data) > 0) {
  
  ggplot(plot_data, aes(x = Day, y = Correlation, color = Pair, group = Pair)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ylim(-1, 1) +
    labs(title = paste("Correlation Evolution:", target_session_type),
         y = "Correlation (r)",
         x = "Day") +
    theme_minimal()
  
} else {
  print("No data found for these pairs.")
}