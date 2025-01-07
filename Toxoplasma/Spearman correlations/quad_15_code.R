library(dplyr)

# Load your data
csv_file_path <- "C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/quad 15/varImp_rffit_quad_features_WT_15_INF_UNINF.csv"
RF_output <- read.csv(csv_file_path)
head(RF_output)

# Select rows where MeanDecreaseAccuracy > 1
MeanDecrease_bigger_than_1 <- RF_output$MeanDecreaseAccuracy > 1
selected_rows <- RF_output[MeanDecrease_bigger_than_1, ]
head(selected_rows)
dim(selected_rows)


# Load features data
features <- read.csv("C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/quad 15/quad_15.csv")
head(features)


# Define columns_to_keep from selected_rows$X
columns_to_keep <- selected_rows$X

# Add the additional columns to columns_to_keep if they are not already included
additional_columns <- c("X209.091_0.47")
columns_to_keep <- unique(c(columns_to_keep, additional_columns))  # Ensure no duplicates

# Now select the columns from the features dataframe
sig_by_RF <- features %>%
  select(all_of(columns_to_keep))


# Display the first few rows to check the result
head(sig_by_RF)

# Add metadata back to filtered data (assuming metadata is in the first 14 columns)
sig_by_RF_wMeta <- cbind(features[1:14], sig_by_RF)


# Select columns to calculate Spearman correlation (from column 15 where your data starts, to the end)
columns_to_correlate <- names(sig_by_RF_wMeta)[15:ncol(sig_by_RF_wMeta)]

# Initialize vectors to store correlation results and p-values
correlations <- numeric(length(columns_to_correlate))
p_values <- numeric(length(columns_to_correlate))

# Calculate Spearman correlations between the combined variable and other columns
for (i in 1:length(columns_to_correlate)) {
  colname <- columns_to_correlate[i]
  
  # Correlation with the combined variable
  correlation_result <- cor.test(sig_by_RF_wMeta$X209.091_0.47, sig_by_RF_wMeta[[colname]], method = "spearman")
  correlations[i] <- correlation_result$estimate
  p_values[i] <- correlation_result$p.value
}

# Adjust p-values using FDR
adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Create a data frame to store the results
correlation_df <- data.frame(Column = columns_to_correlate,
                             Spearman_Correlation = correlations,
                             pval = p_values,
                             Adjusted_P_Value = adjusted_p_values)

# Write the results to a CSV file
write.csv(correlation_df, "RF_output_correlated_to_combined_kynurenine_quad_15_redone.csv", row.names = FALSE)
