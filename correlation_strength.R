target_participant <- 2
target_session_type <- "MindWandering1"
threshold <- 0.3  # cutoff for displaying strength


subset_data <- subset(all_data, 
                      Participant_ID == target_participant & 
                        Session_Type == target_session_type)

days <- unique(subset_data$Day_Num)
all_dims <- sort(unique(subset_data$Dimension))
all_dims <- all_dims[all_dims != ""]

daily_matrices <- list()

for (d in days) {
  day_data <- subset(subset_data, Day_Num == d)
  

  day_clean <- aggregate(Intensity ~ Time_Offset + Dimension, 
                         data = day_data, FUN = mean, na.rm = TRUE)
  
  wide <- reshape(day_clean, idvar = "Time_Offset", timevar = "Dimension", direction = "wide")
  colnames(wide) <- gsub("Intensity.", "", colnames(wide))
  
  mat <- matrix(NA, nrow = length(all_dims), ncol = length(all_dims),
                dimnames = list(all_dims, all_dims))
  
  if (ncol(wide) >= 3) {
    c <- cor(wide[, -1, drop=FALSE], use = "pairwise.complete.obs")
    present <- intersect(colnames(c), all_dims)
    mat[present, present] <- c[present, present]
  }
  
  daily_matrices[[as.character(d)]] <- mat
}




stack <- array(unlist(daily_matrices), 
               dim = c(length(all_dims), length(all_dims), length(daily_matrices)),
               dimnames = list(all_dims, all_dims, names(daily_matrices)))

mean_matrix <- apply(stack, c(1, 2), mean, na.rm = TRUE)


upper_tri <- upper.tri(mean_matrix)
pair_list <- data.frame(
  Var1 = rownames(mean_matrix)[row(mean_matrix)[upper_tri]],
  Var2 = colnames(mean_matrix)[col(mean_matrix)[upper_tri]],
  Pearson_r = mean_matrix[upper_tri]
)


strong_pairs <- subset(pair_list, Pearson_r > threshold)
strong_pairs <- strong_pairs[order(-strong_pairs$Pearson_r), ]

print("---------------------------------------------------------")
print(paste("PAIRS WITH AVERAGE CORRELATION >", threshold))
print("---------------------------------------------------------")
print(strong_pairs)