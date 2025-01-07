# Load the two CSV files
file1 <- read.csv("C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/quad 50/RF_output_correlated_to_combined_kynurenine_quad_50_redone.csv")
file2 <- read.csv("C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/quad 50/quad_50_supplemental_table.csv")

# Merge the two datasets by the 'features' column, keeping only the matching rows
merged_data <- merge(file1, file2, by = "features", all = FALSE)  # all=FALSE performs an inner join

# Display the first few rows of the merged data
head(merged_data)

# Optionally, write the merged data to a new CSV file
write.csv(merged_data, "merged_file_supplemental_w_spearman_quad50.csv", row.names = FALSE)
