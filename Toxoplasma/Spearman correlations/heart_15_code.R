library(dplyr)
csv_file_path <-"C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/heart 15/varImp_rffit_heart_features_WT_15_INF_UNINF.csv"
RF_output<-read.csv(csv_file_path)
head(RF_output)
MeanDecrease_bigger_than_1 <- RF_output$MeanDecreaseAccuracy > 1 #select those that meet the cutoff
selected_rows <- RF_output[MeanDecrease_bigger_than_1, ]
head(selected_rows)
dim(selected_rows)
features<-read.csv("C:/Users/caitl/OneDrive/Desktop/Toxoplasma/RF analysis/spearman correlations redo/heart 15/heart_15.csv")
head(features)
columns_to_keep<-selected_rows$X
dim(columns_to_keep)
head(features)
sig_by_RF <- features %>%
  select(all_of(columns_to_keep))
# Check the column names and select only the feature columns (starting from the first actual feature)
# Assuming the feature columns start from column 15
feature_columns <- colnames(features)[15:ncol(features)]

# Ensure column names in selected_rows$X match with feature columns
columns_to_keep <- selected_rows$X[selected_rows$X %in% feature_columns]  # Filter matching columns
dim(selected_rows)
dim(features)
dim(sig_by_RF)
head(sig_by_RF) #number of columns in sig_by_RF should match with the number of rows in selected_rows
sig_by_RF_wMeta<-cbind(features[1:14], sig_by_RF)
dim(sig_by_RF_wMeta)
head(sig_by_RF_wMeta)
# Create a new combined column (e.g., the average of X209.091_0.47 and X192.065_0.48)
sig_by_RF_wMeta$Combined <- rowMeans(sig_by_RF_wMeta[, c("X209.091_0.47", "X192.065_0.48")])

# Now calculate the Spearman correlation between this combined column and the other columns
columns_to_correlate <- names(sig_by_RF_wMeta)[14:ncol(sig_by_RF_wMeta)] # Select columns to calculate Spearman correlation

# Initialize vectors to store correlation results and p-values
correlations <- numeric(length(columns_to_correlate))
p_values <- numeric(length(columns_to_correlate))

for (i in 1:length(columns_to_correlate)) {
  colname <- columns_to_correlate[i]
  
  # Correlation with the combined variable
  correlation_result <- cor.test(sig_by_RF_wMeta$Combined, sig_by_RF_wMeta[[colname]], method = "spearman")
  correlations[i] <- correlation_result$estimate
  p_values[i] <- correlation_result$p.value
}

# Adjust p-values
adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Create a data frame to store the results
correlation_df <- data.frame(Column = columns_to_correlate,
                             Spearman_Correlation = correlations,
                             pval = p_values,
                             Adjusted_P_Value = adjusted_p_values)

# Write the results to a CSV file
write.csv(correlation_df, "RF_output_correlated_to_combined_kynurenine_heart_15_redone.csv", row.names = FALSE)
