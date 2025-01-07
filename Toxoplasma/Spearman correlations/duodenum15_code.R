library(dplyr)

# Load your data
csv_file_path <- "C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/SI duodenum 15/varImp_rffit_si_duodenum_features_WT_15_INF_UNINF.csv"
RF_output <- read.csv(csv_file_path)
head(RF_output)

# Select rows where MeanDecreaseAccuracy > 1
MeanDecrease_bigger_than_1 <- RF_output$MeanDecreaseAccuracy > 1
selected_rows <- RF_output[MeanDecrease_bigger_than_1, ]
head(selected_rows)
dim(selected_rows)

write.csv(selected_rows, "check_duodenum_15.csv", row.names = FALSE)

# Load features data
features <- read.csv("C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/SI duodenum 15/SI_D_15.csv")
head(features)

columns_to_keep<-selected_rows$X
sig_by_RF <- features %>%
  select(all_of(columns_to_keep))
head(sig_by_RF)

write.csv(sig_by_RF, "sig_by_rf.csv", row.names = FALSE)

# Add metadata back to filtered data (assuming metadata is in the first 14 columns)
sig_by_RF_wMeta <- cbind(features[1:13], sig_by_RF)

# Create a new combined column (e.g., the sum of X209.091_0.47 and X192.065_0.48)
sig_by_RF_wMeta$Combined <- rowSums(sig_by_RF_wMeta[, c("X209.091_0.47", "X192.065_0.48")])

write.csv(sig_by_RF_wMeta, "sig_by_rf_wmeta2.csv", row.names = FALSE)

# Select columns to calculate Spearman correlation (from column 14 where your data starts, to the end)
columns_to_correlate <- names(sig_by_RF_wMeta)[14:ncol(sig_by_RF_wMeta)]

# Initialize vectors to store correlation results and p-values
correlations <- numeric(length(columns_to_correlate))
p_values <- numeric(length(columns_to_correlate))

# Calculate Spearman correlations between the combined variable and other columns
for (i in 1:length(columns_to_correlate)) {
  colname <- columns_to_correlate[i]
  
  # Correlation with the combined variable
  correlation_result <- cor.test(sig_by_RF_wMeta$Combined, sig_by_RF_wMeta[[colname]], method = "spearman")
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
write.csv(correlation_df, "RF_output_correlated_to_combined_kynurenine_duodenum_15_redone_v2.csv", row.names = FALSE)
