# ==============================================================================
# STEP 1: SETUP & LIST GENERATION
# ==============================================================================

# 1. Get the list of ALL possible dimensions (Clean out NAs just in case)
all_dimensions <- unique(all_data$Dimension)
all_dimensions <- all_dimensions[!is.na(all_dimensions) & all_dimensions != ""]

# 2. Update the lists we loop through
# These should already exist in all_data from the compilation step
session_list <- unique(all_data$Unique_Session_ID)
group_list <- unique(all_data$Group_ID)

print(paste("Processing", length(session_list), "unique sessions..."))
print(paste("Grouping into", length(group_list), "blocks..."))

# ==============================================================================
# STEP 2: CALCULATE INDIVIDUAL MATRICES
# ==============================================================================

all_cor_matrices <- list()

# Loop through every single session (e.g., "003_Day10_GB")
for (s_id in session_list) {
  
  # A. Filter data for this specific session
  s_data <- subset(all_data, Unique_Session_ID == s_id)
  
  # B. Safety Aggregation (The "Anti-Crash" Patch)
  # Ensures there is exactly ONE value per Dimension per Time_Offset
  s_data_clean <- aggregate(Intensity ~ Time_Offset + Dimension, 
                            data = s_data, 
                            FUN = mean, na.rm = TRUE)
  
  # C. Reshape to Wide Format
  wide <- reshape(s_data_clean, 
                  idvar = "Time_Offset", 
                  timevar = "Dimension", 
                  direction = "wide")
  
  # Clean up column names (remove "Intensity." prefix)
  colnames(wide) <- gsub("Intensity.", "", colnames(wide))
  
  # D. Create the Master Template (Full Size, Filled with NAs)
  # This ensures every matrix has the exact same shape (e.g. 10x10)
  full_matrix <- matrix(NA, nrow = length(all_dimensions), ncol = length(all_dimensions),
                        dimnames = list(all_dimensions, all_dimensions))
  
  # E. Calculate Correlation (only if we have at least 2 dimensions)
  if (ncol(wide) >= 3) {
    
    # Calculate correlation for the dimensions present in this session
    day_cor <- cor(wide[, -1, drop=FALSE], use = "pairwise.complete.obs")
    
    # Fill the Master Template
    present <- intersect(all_dimensions, colnames(day_cor))
    if(length(present) > 0) {
      full_matrix[present, present] <- day_cor[present, present]
    }
  }
  
  # Store it in our list
  all_cor_matrices[[s_id]] <- full_matrix
}

print("Individual matrices calculated successfully!")

# ==============================================================================
# STEP 3: AVERAGE BY GROUP & PLOT
# ==============================================================================

# Create output folder
if(!dir.exists("Mean_Correlation_Blocks")) dir.create("Mean_Correlation_Blocks")

print("Generating Heatmaps...")

for (g_id in group_list) {
  
  # A. Identify which sessions belong to this Block/Group
  sessions_in_group <- unique(all_data$Unique_Session_ID[all_data$Group_ID == g_id])
  
  # B. Retrieve the matrices for these sessions
  group_matrices <- all_cor_matrices[names(all_cor_matrices) %in% sessions_in_group]
  
  # Only proceed if we have data
  if (length(group_matrices) > 0) {
    
    # C. Stack them into a 3D Array (Dimensions x Dimensions x NumberOfDays)
    stack <- array(unlist(group_matrices), 
                   dim = c(length(all_dimensions), length(all_dimensions), length(group_matrices)),
                   dimnames = list(all_dimensions, all_dimensions, NULL))
    
    # D. Calculate the MEAN across the stack (ignoring NAs)
    mean_matrix <- apply(stack, c(1, 2), mean, na.rm = TRUE)
    
    # E. Cleanup: Remove rows/cols that are entirely NA for this block
    # (e.g. if "Anger" was never measured in this block, remove it)
    keep <- apply(mean_matrix, 1, function(x) !all(is.na(x)))
    final_plot_matrix <- mean_matrix[keep, keep, drop=FALSE]
    
    # F. Generate Plot (if valid)
    if (ncol(final_plot_matrix) >= 2) {
      
      file_name <- paste0("Mean_Correlation_Blocks/Mean_Cor_", g_id, ".png")
      
      # Save to PNG
      png(file_name, width = 1000, height = 1000)
      
      library(corrplot)
      corrplot(final_plot_matrix,
               method = "color",
               type = "full",
               addCoef.col = "black", # Show numbers
               tl.col = "black",      # Black text labels
               tl.srt = 45,           # Rotate labels
               title = paste("Average Correlation:", g_id),
               mar = c(0,0,2,0),      # Margin for title
               diag = TRUE,
               na.label = " ")        # Leave missing values blank
      
      dev.off()
    }
  }
}

print("------------------------------------------------")
print("All done! Check the 'Mean_Correlation_Blocks' folder.")