application <- read.csv("/Users/tianxiangchen/Desktop/Math252/dataset/credit_record.csv", header = TRUE)
View(application)
nrow(application)
credit_record <- read.csv("/Users/tianxiangchen/Desktop/Math252/dataset/application_record.csv", header = TRUE)
# Merging
merged_data <- merge(application, credit_record, by = "ID")
reduced_merged_data<-merged_data[1:1000,]
View(reduced_merged_data)
nrow(reduced_merged_data)
#High school Dataset
highschool <- reduced_merged_data[reduced_merged_data$NAME_EDUCATION_TYPE == "Higher education", ]
# Write subset_data_frame to a CSV file
#write.csv(highschool, "highschool.csv", row.names = FALSE)
#Retired Dataset
retired <- reduced_merged_data[reduced_merged_data$NAME_INCOME_TYPE == "Pensioner", ]
#write.csv(retired, "retired.csv", row.names = FALSE)
#married Dataset
married <- reduced_merged_data[reduced_merged_data$NAME_FAMILY_STATUS == "Married", ]
#write.csv(married, "married.csv", row.names = FALSE)
#property Dataset
property <- reduced_merged_data[reduced_merged_data$NAME_HOUSING_TYPE == "House / apartment", ]
#write.csv(property, "property.csv", row.names = FALSE)
#manager Dataset
manager <- reduced_merged_data[reduced_merged_data$OCCUPATION_TYPE == "Managers", ]
#write.csv(manager, "manager.csv", row.names = FALSE)
#male dataset
male <- reduced_merged_data[reduced_merged_data$CODE_GENDER == "M", ]
#write.csv(male, "male.csv", row.names = FALSE)
#female
female <- reduced_merged_data[reduced_merged_data$CODE_GENDER == "F", ]
#write.csv(female, "female.csv", row.names = FALSE)
# person who older than 30yrs
yrs_above <- reduced_merged_data[reduced_merged_data$DAYS_BIRTH < -10957, ]
#write.csv(yrs_above, "yrs_above.csv", row.names = FALSE)
# person who younger than 30yrs
yrs_below <- reduced_merged_data[reduced_merged_data$DAYS_BIRTH > -10957, ]
#write.csv(yrs_below, "yrs_below.csv", row.names = FALSE)
#single dataset
single <- reduced_merged_data[reduced_merged_data$NAME_FAMILY_STATUS == "Single / not married", ]

#write.csv(single, "single.csv", row.names = FALSE)

########################################
### Dataset highschool####
########################################
# Step 2: Data Preparation
#Check is NA value in sub_data1
any(is.na(highschool))
#Return False means no missing value
#remove the "" value in the cells
highschool <- highschool[highschool$OCCUPATION_TYPE != "", ]
View(highschool)
#Remove some variable unnecessary
# Convert to matrix
library(dplyr)

# Check if all columns exist
if (all(c("ID", "NAME_EDUCATION_TYPE", "STATUS") %in% colnames(highschool))) {
  # If all columns exist, remove them
  highschool <- highschool %>% select(-ID, -NAME_EDUCATION_TYPE, -STATUS)
} else {
  # If not all columns exist, you can add your desired action here
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix1 <- as.data.frame(highschool)
#Step 3: Extract the significant variable 

# Assuming data_matrix1 is your dataset
column_lengths <- sapply(data_matrix1, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix1, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim1 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df <- as.data.frame(top_variables_dim1)
top_var_df$Variable <- rownames(top_var_df)
subset_data_matrix1 <- data_matrix1[, top_var_df$Variable]
#View(subset_data_matrix1)

# Recalculate the distance matrix
library(gower)
library(cluster) 
# Convert character columns to factors or numerics
for (col in colnames(subset_data_matrix1)) {
  if (is.character(subset_data_matrix1[[col]])) {
    # Assuming the column should be a factor
    subset_data_matrix1[[col]] <- as.factor(subset_data_matrix1[[col]])
    # Use as.numeric() instead if the column is numeric
  }
}


#subset_data_matrix1<-as.data.frame(subset_data_matrix1)
dist_matrix = daisy(subset_data_matrix1,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 3) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix1,metric = 'gower'))
plot(silhouette_score,col="blue")

#cophenetic
copheneticA<-cophenetic(hc)
cor(copheneticA,dist_matrix)

#using wss
fviz_nbclust(subset_data_matrix1,hcut,method = c("wss"))
#using silhouette
fviz_nbclust(subset_data_matrix1,hcut,method = c("silhouette"))
#dunn index
library(fpc)
dunn_index <- cluster.stats(dist_matrix, cutree_hc)$dunn
print(dunn_index)



#kprototype
library(klaR)
library(clustMixType)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix1), 3)
# Perform k-proto clustering on the cleaned matrix
# Adjust 'k' as necessary for your analysis
km <- kproto(subset_data_matrix1, try_clusters)
# Calculate Gower distance
gower_dist <- daisy(subset_data_matrix1, metric = 'gower')
# Calculate silhouette score
silhouette_score <- silhouette(km$cluster, gower_dist)
# Average silhouette width
avg_sil_width <- mean(silhouette_score[, "sil_width"])
print(avg_sil_width)
# Plot the silhouette plot (optional)
fviz_silhouette(silhouette_score)

#dunn index
library(fpc)
dunn_index <- cluster.stats(gower_dist, km$cluster)$dunn
print(dunn_index)



########################################
####Dataset retired####
########################################
#Return False means no missing value
#Check is NA value in sub_data2
any(is.na(retired))

# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(retired$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 8 cluster

#remove the "" value in the cells
#since the all the cells of occupation_type is "" then just remove the whole occupation_type column
#retired<-retired %>% select(-ID, -NAME_INCOME_TYPE,-STATUS,-OCCUPATION_TYPE)

library(dplyr)
# Check if all columns exist
if (all(c("ID", "NAME_INCOME_TYPE", "STATUS", "OCCUPATION_TYPE") %in% colnames(retired))) {
  # If all columns exist, remove them using dplyr's select
  retired <- dplyr::select(retired, -ID, -NAME_INCOME_TYPE, -STATUS, -OCCUPATION_TYPE)
} else {
  # If not all columns exist, you can add your desired action here
  print("One or more specified columns do not exist in the dataframe.")
}


data_matrix2 <- as.data.frame(retired)
#Step 3: Extract the significant variable 

# Assuming data_matrix1 is your dataset
column_lengths <- sapply(data_matrix2, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix2, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df2 <- as.data.frame(top_variables_dim2)
top_var_df2$Variable <- rownames(top_var_df2)
subset_data_matrix2 <- data_matrix2[, top_var_df2$Variable]

# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
for (col in colnames(subset_data_matrix2)) {
  if (is.character(subset_data_matrix2[[col]])) {
    # Convert to factor or numeric as appropriate
    subset_data_matrix2[[col]] <- as.factor(subset_data_matrix2[[col]])
  }
}

dist_matrix = daisy(subset_data_matrix2,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 8) # for example, 8 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix2,metric = 'gower'))
plot(silhouette_score,col="blue")
#cophenetic
copheneticA<-cophenetic(hc)
cor(copheneticA,dist_matrix)

#using wss
fviz_nbclust(subset_data_matrix2,hcut,method = c("wss"))
#using silhouette
fviz_nbclust(subset_data_matrix2,hcut,method = c("silhouette"))
#dunn index
library(fpc)
dunn_index <- cluster.stats(dist_matrix, cutree_hc)$dunn
print(dunn_index)

#kprototype
library(klaR)
library(clustMixType)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix2), 8)
# Perform k-proto clustering on the cleaned matrix
# Adjust 'k' as necessary for your analysis
km <- kproto(subset_data_matrix2, try_clusters)
# Calculate Gower distance
gower_dist <- daisy(subset_data_matrix2, metric = 'gower')
# Calculate silhouette score
silhouette_score <- silhouette(km$cluster, gower_dist)
# Average silhouette width
avg_sil_width <- mean(silhouette_score[, "sil_width"])
print(avg_sil_width)
# Plot the silhouette plot (optional)
fviz_silhouette(silhouette_score)

#dunn index
library(fpc)
dunn_index <- cluster.stats(gower_dist, km$cluster)$dunn
print(dunn_index)

#Distance is not work for is certain cluster only have 1 observation.

########################################
####Dataset married####
########################################
#Check is NA value in sub_data3
any(is.na(married))
#Return False means no missing value

# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(married$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 8 cluster

#remove the "" value in the cells
married <- married[married$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)

library(dplyr)

# Remove the "" value in the cells
married <- married[married$OCCUPATION_TYPE != "", ]

# Check if all columns exist
if (all(c("ID", "NAME_FAMILY_STATUS", "STATUS") %in% colnames(married))) {
  # If all columns exist, remove them using dplyr's select
  married <- dplyr::select(married, -ID, -NAME_FAMILY_STATUS, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}


data_matrix3 <- as.data.frame(married)
#Step 3: Extract the significant variable 

# Assuming data_matrix1 is your dataset
column_lengths <- sapply(data_matrix3, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix3, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix3 <- data_matrix3[, top_var_df3$Variable]
#View(subset_data_matrix3)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix3")) {
  for (col in colnames(subset_data_matrix3)) {
    if (is.character(subset_data_matrix3[[col]])) {
      subset_data_matrix3[[col]] <- as.factor(subset_data_matrix3[[col]])
    } else {
      subset_data_matrix3[[col]] <- as.numeric(subset_data_matrix3[[col]]) 
    }
  }
} else {
  print("subset_data_matrix3 is not a valid data frame.")
}


View(subset_data_matrix3)
dist_matrix = daisy(subset_data_matrix3,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 8) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix3,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix3), 8)
View(subset_data_matrix3)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix3,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix3,try_clusters)

#########################
####Dataset property####
########################

#Check is NA value in sub_data4
any(is.na(property))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(property$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 8 cluster

#Return False means no missing value
#remove the "" value in the cells
property <- property[property$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "NAME_HOUSING_TYPE", "STATUS") %in% colnames(property))) {
  # If all columns exist, remove them using dplyr's select
  property <- dplyr::select(property, -ID, -NAME_HOUSING_TYPE, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix4 <- as.data.frame(property)
#Step 3: Extract the significant variable 

# Assuming data_matrix1 is your dataset
column_lengths <- sapply(data_matrix4, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix4, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix4 <- data_matrix4[, top_var_df3$Variable]
#View(subset_data_matrix4)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix4")) {
  for (col in colnames(subset_data_matrix4)) {
    if (is.character(subset_data_matrix4[[col]])) {
      subset_data_matrix4[[col]] <- as.factor(subset_data_matrix4[[col]])
    }
  }
} else {
  print("subset_data_matrix4 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix4,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 8) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix4,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix4), 8)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix3,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix4,try_clusters)

#########################
####Dataset manager####
########################

#Check is NA value in sub_data4
any(is.na(manager))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(manager$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 3 cluster

#Return False means no missing value
#remove the "" value in the cells
manager <- manager[manager$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "OCCUPATION_TYPE", "STATUS") %in% colnames(manager))) {
  # If all columns exist, remove them using dplyr's select
  manager <- dplyr::select(manager, -ID, -OCCUPATION_TYPE, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix5 <- as.data.frame(manager)
#Step 3: Extract the significant variable 

# Assuming data_matrix1 is your dataset
column_lengths <- sapply(data_matrix5, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix5, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix5 <- data_matrix5[, top_var_df3$Variable]
#View(subset_data_matrix4)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix5")) {
  for (col in colnames(subset_data_matrix5)) {
    if (is.character(subset_data_matrix5[[col]])) {
      subset_data_matrix5[[col]] <- as.factor(subset_data_matrix5[[col]])
    }
  }
} else {
  print("subset_data_matrix5 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix5,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 3) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix5,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix5), 3)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix3,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix5,try_clusters)
km$cluster

####################
####Dataset male####
####################
#Check is NA value in sub_data4
any(is.na(male))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(male$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 8 cluster

#Return False means no missing value
#remove the "" value in the cells
male <- male[male$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "CODE_GENDER", "STATUS") %in% colnames(male))) {
  # If all columns exist, remove them using dplyr's select
  male <- dplyr::select(male, -ID, -CODE_GENDER, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix6 <- as.data.frame(male)
#Step 3: Extract the significant variable 

# Assuming data_matrix6 is your dataset
column_lengths <- sapply(data_matrix6, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix6, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix6 <- data_matrix6[, top_var_df3$Variable]
#View(subset_data_matrix4)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix6")) {
  for (col in colnames(subset_data_matrix6)) {
    if (is.character(subset_data_matrix6[[col]])) {
      subset_data_matrix6[[col]] <- as.factor(subset_data_matrix6[[col]])
    }
  }
} else {
  print("subset_data_matrix5 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix6,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 8) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix6,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix6), 8)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix6,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix6,try_clusters)
km$cluster

####################
####Dataset female##
####################
#Check is NA value in sub_data4
any(is.na(female))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(female$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 4 cluster

#Return False means no missing value
#remove the "" value in the cells
female <- female[female$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "CODE_GENDER", "STATUS") %in% colnames(female))) {
  # If all columns exist, remove them using dplyr's select
  female <- dplyr::select(female, -ID, -CODE_GENDER, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix7 <- as.data.frame(female)
#Step 3: Extract the significant variable 

# Assuming data_matrix6 is your dataset
column_lengths <- sapply(data_matrix7, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix7, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix7 <- data_matrix7[, top_var_df3$Variable]
#View(subset_data_matrix4)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix7")) {
  for (col in colnames(subset_data_matrix7)) {
    if (is.character(subset_data_matrix7[[col]])) {
      subset_data_matrix7[[col]] <- as.factor(subset_data_matrix7[[col]])
    }
  }
} else {
  print("subset_data_matrix7 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix7,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 4) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix7,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix7),4)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix6,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix7,try_clusters)
km$cluster

####################
####Dataset yrs_above##
####################
#Check is NA value in sub_data4
any(is.na(yrs_above))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(yrs_above$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 8 cluster

#Return False means no missing value
#remove the "" value in the cells
yrs_above <- yrs_above[yrs_above$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "DAYS_BIRTH", "STATUS") %in% colnames(yrs_above))) {
  # If all columns exist, remove them using dplyr's select
  yrs_above <- dplyr::select(yrs_above, -ID, -DAYS_BIRTH, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix8 <- as.data.frame(yrs_above)
#Step 3: Extract the significant variable 

# Assuming data_matrix8 is your dataset
column_lengths <- sapply(data_matrix8, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix8, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix8 <- data_matrix8[, top_var_df3$Variable]
#View(subset_data_matrix8)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix8")) {
  for (col in colnames(subset_data_matrix8)) {
    if (is.character(subset_data_matrix8[[col]])) {
      subset_data_matrix8[[col]] <- as.factor(subset_data_matrix8[[col]])
    }
  }
} else {
  print("subset_data_matrix8 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix8,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 8) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix8,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix8),8)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix6,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix8,try_clusters)
km$cluster
####################
####Dataset yrs_below##
####################
#Check is NA value in sub_data4
any(is.na(yrs_below))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(yrs_below$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 4 cluster

#Return False means no missing value
#remove the "" value in the cells
yrs_below <- yrs_below[yrs_below$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "DAYS_BIRTH", "STATUS") %in% colnames(yrs_below))) {
  # If all columns exist, remove them using dplyr's select
  yrs_below <- dplyr::select(yrs_below, -ID, -DAYS_BIRTH, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix9 <- as.data.frame(yrs_below)
#Step 3: Extract the significant variable 

# Assuming data_matrix9 is your dataset
column_lengths <- sapply(data_matrix9, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix9, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix9 <- data_matrix9[, top_var_df3$Variable]
#View(subset_data_matrix9)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix9")) {
  for (col in colnames(subset_data_matrix9)) {
    if (is.character(subset_data_matrix9[[col]])) {
      subset_data_matrix9[[col]] <- as.factor(subset_data_matrix9[[col]])
    }
  }
} else {
  print("subset_data_matrix9 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix9,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 4) # for example, 4 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix9,metric = 'gower'))
plot(silhouette_score,col="blue")

#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix9),4)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix6,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix8,try_clusters)
km$cluster

####################
####Dataset single##
####################
#Check is NA value in sub_data4
any(is.na(single))
# we want to know how many level does categorical variable status have.
# Get unique values of STATUS if it's not a factor
unique_values <- unique(single$STATUS)
# Print the unique values
print(unique_values)
# Print the number of unique values
print(paste("Number of unique values in STATUS:", length(unique_values)))
#so total should have 3 cluster

#Return False means no missing value
#remove the "" value in the cells
single <- single[single$OCCUPATION_TYPE != "", ]
#married<-married %>% select(-ID, -NAME_FAMILY_STATUS,-STATUS)
library(dplyr)
# Check if all columns exist
if (all(c("ID", "NAME_FAMILY_STATUS", "STATUS") %in% colnames(single))) {
  # If all columns exist, remove them using dplyr's select
  single <- dplyr::select(single, -ID, -NAME_FAMILY_STATUS, -STATUS)
} else {
  # If not all columns exist, take an alternative action
  print("One or more specified columns do not exist in the dataframe.")
}

data_matrix10 <- as.data.frame(single)
#Step 3: Extract the significant variable 

# Assuming data_matrix9 is your dataset
column_lengths <- sapply(data_matrix10, length)

# View the lengths
print(column_lengths)
# Check if all columns have the same length
all_equal <- all(column_lengths == column_lengths[1])

if (all_equal) {
  print("All columns have the same length.")
} else {
  print("Not all columns have the same length.")
}

# Assuming data_matrix1 is already loaded in your R environment
library(FactoMineR)

# Performing FAMD
famd_result <- FAMD(data_matrix10, graph = FALSE)
# Selecting top contributing variables for the first ten principal component
top_variables_dim2 <- sort(famd_result$var$contrib[, "Dim.1"], decreasing = TRUE)[1:10]
# for the first ten principal component name
top_var_df3 <- as.data.frame(top_variables_dim2)
top_var_df3$Variable <- rownames(top_var_df3)
subset_data_matrix10 <- data_matrix10[, top_var_df3$Variable]
#View(subset_data_matrix10)
# Recalculate the distance matrix
library(gower)
# Convert character columns to factors or numerics
if (exists("subset_data_matrix10")) {
  for (col in colnames(subset_data_matrix10)) {
    if (is.character(subset_data_matrix10[[col]])) {
      subset_data_matrix10[[col]] <- as.factor(subset_data_matrix10[[col]])
    }
  }
} else {
  print("subset_data_matrix9 is not a valid data frame.")
}
dist_matrix = daisy(subset_data_matrix10,metric = 'gower')

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")  # You can try different methods like "average", "single", etc.
print(hc)
plot(hc)

# Cutting the dendrogram to form clusters
cutree_hc <- cutree(hc, k = 3) # for example, 3 clusters
# Silhouette analysis
silhouette_score <- silhouette(cutree_hc, daisy(subset_data_matrix10,metric = 'gower'))
plot(silhouette_score,col="blue")

#transfrom variable
subset_data_matrix10[["CNT_CHILDREN"]] <- as.factor(subset_data_matrix10[["CNT_CHILDREN"]])
subset_data_matrix10[["FLAG_MOBIL"]] <- as.factor(subset_data_matrix10[["FLAG_MOBIL"]])
subset_data_matrix10[["FLAG_WORK_PHONE"]] <- as.factor(subset_data_matrix10[["FLAG_WORK_PHONE"]])
subset_data_matrix10[["FLAG_PHONE"]] <- as.factor(subset_data_matrix10[["FLAG_PHONE"]])
subset_data_matrix10[["FLAG_EMAIL"]] <- as.factor(subset_data_matrix10[["FLAG_EMAIL"]])
subset_data_matrix10[["CNT_FAM_MEMBERS"]] <- as.factor(subset_data_matrix10[["CNT_FAM_MEMBERS"]])
#kprototype
library(klaR)
# Set the number of clusters, ensuring it does not exceed the number of rows
try_clusters <- min(nrow(subset_data_matrix10),3)
#View(subset_data_matrix4)
# Perform k-proto clustering on the cleaned matrix
#clusters <- kmodes(subset_data_matrix6,try_clusters)
#clusters$cluster
km=kproto(subset_data_matrix10,try_clusters)
km$cluster



