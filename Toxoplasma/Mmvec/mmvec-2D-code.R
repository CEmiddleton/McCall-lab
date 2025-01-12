getwd()
install.packages("pca3d")
install.packages("rgl")


devtools::install_version("pca3d",version="0.10.2")

library(pca3d)
library(rgl)
## before loading this file to R delete the first column
data_merge_dropnomatch_CN_NAdrop_RFdrop <- read.csv(file = "data_merge_dropnomatch_CN_NAdrop_RFdrop_nofamily5_abfamily.csv", header= TRUE)

cond.pca <- prcomp(data_merge_dropnomatch_CN_NAdrop_RFdrop[,2:15], scale.=TRUE)
pca3d(cond.pca)
snapshotPCA3d(file="First_plot.png")

cf_names <- factor(data_merge_dropnomatch_CN_NAdrop_RFdrop[,27])
cond.pca$CF_class  <- data_merge_dropnomatch_CN_NAdrop_RFdrop$CF_class

##substitute classes with colors####

data_merge_dropnomatch_CN_NAdrop_RFdrop$CF_class
#USe the class list to make the list of original list (like below)


original_words <- data_merge_dropnomatch_CN_NAdrop_RFdrop$CF_class
# Define a list of words to replace and their replacements
words_to_replace <- c("Carboxylic acids and derivatives",
                      "Cinnamic acids and derivatives",
                      "Fatty Acyls",
                      "Glycerophospholipids",
                      "Imidazopyrimidines" ,
                      "Indoles and derivatives",
                      "Isoflavonoids",
                      "Flavonoids",
                      "Organonitrogen compounds",
                      "Organooxygen compounds",  
                      "Purine nucleosides",
                      "Pteridines and derivatives",
                      "Pyridines and derivatives",
                      "Saccharolipids",
                      "Steroids and steroid derivatives",
                      "Tetrapyrroles and derivatives")
replacement_cols <- c( "#FF8C00",
                        "#FFFF00",
                        "#FF4500",
                        "#7FFF00" ,
                        "#9370DB",
                        "#FF83FA",
                       "#CD2990",
                       "#CD3333",
                        "#00FFFF",
                       "#FFB6C1",  
                       "#A2CD5A",
                       "#924900",
                       "#CAFF70",
                       "olivedrab2",
                       "#b6dbff",
                       "black")



replace_words_in_list <- function(original_list, words_to_replace, replacement_cols) {
  modified_list <- original_list
  for (i in seq_along(original_list)) {
    if (original_list[i] %in% words_to_replace) {
      modified_list[i] <- replacement_cols[which(words_to_replace == original_list[i])]
    }
  }
  return(modified_list)
}

# Call the function to replace words
modified_col <- replace_words_in_list(original_words, words_to_replace, replacement_cols)

# Print the modified list of words
print(modified_col)

##substitute classes with Shapes####
replacement_shapes <- c("sphere",
                        "tetrahedron",
                        "cube",
                        "sphere",
                        "tetrahedron",
                        "cube",
                        "sphere",
                        "cube",
                        "tetrahedron",
                        "cube",
                        "sphere",
                        "tetrahedron",
                        "cube",
                        "sphere",
                        "tetrahedron",
                        "cube")

#Only run for ggplot graph
replacement_shapes <- c("16",
                        "17",
                        "15",
                        "16",
                        "17",
                        "15",
                        "16",
                        "15",
                        "17",
                        "15",
                        "16",
                        "17",
                        "15",
                        "16",
                        "17",
                        "15")
replace_words_in_list2 <- function(original_list, words_to_replace, replacement_shapes) {
  modified_list <- original_list
  for (i in seq_along(original_list)) {
    if (original_list[i] %in% words_to_replace) {
      modified_list[i] <- replacement_shapes[which(words_to_replace == original_list[i])]
    }
  }
  return(modified_list)
}

# Call the function to replace words
modified_shapes <- replace_words_in_list2(original_words, words_to_replace, replacement_shapes)

# Print the modified list of words
print(modified_shapes)


#### Generating 2D graph


pca2d(cond.pca,  components = 1:2, biplot=TRUE, group=cf_names, axes.color="black", show.plane=FALSE, radius=2,show.labels=TRUE,
      legend = TRUE,biplot.vars =2,
      col = modified_col ,
      shape = modified_shapes )


