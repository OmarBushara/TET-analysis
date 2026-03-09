session_list <- unique(complete_data$Session_ID)
length(session_list)

dir.create("Session_Heatmaps")
for (current_session in session_list){
  print(paste("Processing:", current_session))
  session_data <- subset(complete_data, Session_ID == current_session)
  clean_data <- session_data[, c("Time_Offset, Dimension", "Intensity")]
  if(nrow(clean_data)>0){
    wide_data <- reshape(clean_data,
                         idvar = "Time_Offset"
                         timevar = "Dimension"
                         direction = "wide")
    cor_matrix <- cor(wide_Data[, -1], use = "pairwise.complete.obs")
    file_name <- paste0("Session_Heatmaps/Heatmap_", current_session, ".png")
    png(file_name, width = 1000, height = 1000)
    corrplot(cor_matrix,
             method = "color",
             type = "full",
             order = "hclust",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45
             diag = TRUE)
    dev.off()
  }
}