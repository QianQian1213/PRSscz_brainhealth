library(tidyverse)
input_file <- "allmatrix_renamed.csv"

scores_output_file <- "tracts_ALL_PCs_scores.csv"

details_output_dir <- "pca_analysis_details"
if (!dir.exists(details_output_dir)) {
  dir.create(details_output_dir, recursive = TRUE)
}

allmatrix_renamed <- read.csv(input_file)

n_tracts <- 48
pca_results_list <- list()

for (i in 1:n_tracts) {
  
  tract_cols_pattern <- paste0("tract", i, "_")
  tract_colnames <- grep(tract_cols_pattern, colnames(allmatrix_renamed), value = TRUE)
  tract_data <- allmatrix_renamed[, tract_colnames]
  
  if (length(tract_colnames) != 8) {
    warning(paste("Warning: Tract", i, "did not have 8 metric columns. Skipping."))
    next 
  }
  
  pca_result <- prcomp(tract_data, center = TRUE, scale. = TRUE)
  
  pca_summary <- summary(pca_result)
  variance_explained <- as.data.frame(pca_summary$importance)
  summary_filename <- file.path(details_output_dir, paste0("summary_tract_", i, ".csv"))
  write.csv(variance_explained, file = summary_filename)
  
  loadings <- as.data.frame(pca_result$rotation)
  loadings_filename <- file.path(details_output_dir, paste0("loadings_tract_", i, ".csv"))
  write.csv(loadings, file = loadings_filename)
  
  
  if (i %in% c(8, 9, 16)) {
    
    plot_filename <- file.path(details_output_dir, paste0("screeplot_tract_", i, ".png"))
    png(filename = plot_filename, width = 800, height = 600)
    
    screeplot(pca_result, type = "lines", 
              main = paste("Scree Plot for Tract", i))
              
    abline(h = 1, col = "red", lty = 2)
    dev.off()
    
    print(paste("--- Scree plot saved for Tract", i, "---"))
  }
  

  all_pcs_df <- as.data.frame(pca_result$x)
  colnames(all_pcs_df) <- paste0("tract", i, "_PC", 1:ncol(all_pcs_df))
  pca_results_list[[i]] <- all_pcs_df
  
  print(paste("Tract", i, ": PCA complete. Details saved."))
}

pca_results_df <- do.call(cbind, pca_results_list)
covariates <- allmatrix_renamed[, 1:12]
final_df <- cbind(covariates, pca_results_df)
write.csv(final_df, file = scores_output_file, row.names = FALSE)

print(paste("PCA analysis complete. All PC scores saved to:", scores_output_file))
print(paste("Detailed summaries, loadings, and selected plots saved in:", details_output_dir))
