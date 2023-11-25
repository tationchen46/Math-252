#1.Perform FAMD and retain only the principal components that are significant.
#2.Apply hierarchical clustering with different linkage methods and k-prototype clustering.
#3.Calculate the evaluation metrics for each clustering result.
#4.Determine which variables contribute the most to the clustering.

application <- read.csv("/Users/Maggie/Documents/GitHub/Math-252/credit_record.csv", header = TRUE)
View(application)
nrow(application)
credit_record <- read.csv("/Users/Maggie/Documents/GitHub/Math-252/application_record.csv", header = TRUE)
# Merging
merged_data <- merge(application, credit_record, by = "ID")
reduced_merged_data<-merged_data[1:1000,]
View(reduced_merged_data)
nrow(reduced_merged_data)
#Retired Dataset
retired <- reduced_merged_data[reduced_merged_data$NAME_INCOME_TYPE == "Pensioner", ]

library(dplyr)
library(FactoMineR)
library(cluster)
library(fpc)
library(mclust)
library(clustMixType)
library(factoextra)

# Check for missing values
any_na <- any(is.na(retired))
print(paste("Any NA values:", any_na))

# Get unique values of STATUS
unique_values <- unique(retired$STATUS)
print(unique_values)
print(paste("Number of unique STATUS values:", length(unique_values)))

# Data Cleaning: Remove columns with empty values for occupation type
retired_cleaned <- retired %>% select(-c(ID, NAME_INCOME_TYPE, OCCUPATION_TYPE))

# Perform FAMD
famd_result <- FAMD(retired_cleaned, graph = FALSE)
# Extract significant variables (top 10 principal components)
top_vars <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
subset_retired <- retired_cleaned[, names(top_vars)]
label_test <- retired_cleaned$STATUS

# Convert character columns to factors for clustering
for (col in colnames(subset_retired)) {
  if (is.character(subset_retired[[col]])) {
    subset_retired[[col]] <- as.factor(subset_retired[[col]])
  }
}

# Calculate Gower distance
dist_matrix <- daisy(subset_retired, metric = 'gower')

# Create a list to store clustering results
clustering_results <- list()
linkage_methods <- c("single", "complete", "average", "ward.D2")

# Apply hierarchical clustering with different linkage methods
for (method in linkage_methods) {
  hc <- hclust(dist_matrix, method = method)
  clustering_results[[method]] <- cutree(hc, k = 8)
  # Plot dendrogram
  plot(hc, main = paste("Dendrogram using", method, "linkage"))
}

# Apply k-prototype clustering
try_clusters <- min(nrow(subset_retired), 8)
km <- kproto(subset_retired, try_clusters)
clustering_results[["kproto"]] <- km$cluster

# Calculate evaluation metrics
metrics <- data.frame(Method = character(), ARI = double(), Dunn = double(), Cophenetic = double(), Silhouette = double(), stringsAsFactors = FALSE)

for (method in names(clustering_results)) {
  clusters <- clustering_results[[method]]
  ari <- adjustedRandIndex(clusters, label_test)
  dunn <- cluster.stats(dist_matrix, clusters)$dunn
  cophenetic_corr <- cor(cophenetic(hc), dist_matrix)
  silhouette_avg_width <- mean(silhouette(clusters, dist_matrix)[, "sil_width"])
  
  metrics <- rbind(metrics, data.frame(Method = method, ARI = ari, Dunn = dunn, Cophenetic = cophenetic_corr, Silhouette = silhouette_avg_width))
}

# Print the evaluation metrics
print(metrics)

# Identify which method performs best
best_method <- metrics$Method[which.max(metrics$ARI)]
print(paste("Best method based on ARI:", best_method))

# Identify which variables contribute most to the clustering
variable_contributions <- apply(subset_retired, 2, function(var) {
  ari_var <- adjustedRandIndex(km$cluster, var)
  return(ari_var)
})

# Print variable contributions
print(variable_contributions)
