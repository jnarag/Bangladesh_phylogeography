## Prepare data from the link tracing study - data provided by Jinnat 
## objective: measure the importance of similiraty of origins between two markets (Jaccard index)
# #The Jaccard index measures the similarity between two sets by dividing the size of their intersection 
#by the size of their union. 
## author: Marie-Cecile Dupas

## Load libraries 
library(readxl)
library(dplyr)

## set working directory -- Jinnat, you have to change this line with your working directory path
setwd("C:/Users/Admin/Dropbox/OneHealthPoultry/GravityModel")

## Load data --
EB_data <- read.table(file = "01_Data/Tracing_study/Exotic_broiler_data.csv", sep=";")

# Assign the first row as column names
colnames(EB_data) <- EB_data[1, ]

# Remove the first row since it is now used as column names
EB_data <- EB_data[-1, ]

# keep only flows for which we have the origin 
EB_data <- EB_data[which(!is.na(EB_data$Upazila_Farm)),]

# dataframe of edges from Upazila to market vendor
# Example network data
edges <- data.frame(
  origin =EB_data$Upazila_Farm ,
  destination = EB_data$`Market vendor`
)

# Create a vector of unique destinations
destinations <- unique(edges$destination)

# Initialize a matrix to store the results
num_destinations <- length(destinations)

# create an empty matrix to store the Jaccard Index 
jaccard_matrix <- matrix(0, nrow = num_destinations, ncol = num_destinations)


# Function to compute the Jaccard index for a pair of nodes
computeJaccardIndex <- function(edges, node1, node2) {
  origin1 <- unique(edges$origin[edges$destination == node1])
  origin2 <- unique(edges$origin[edges$destination == node2])
  
  intersection <- intersect(origin1, origin2)
  union <- union(origin1, origin2)
  
  jaccard_index <- length(intersection) / length(union)
  
  return(jaccard_index)
}

# Iterate over each pair of destinations
for (i in 1:num_destinations) {
  for (j in 1:num_destinations) {
    if (i != j) {
      node1 <- destinations[i]
      node2 <- destinations[j]
      jaccard_index <- computeJaccardIndex(edges, node1, node2)
      
      # Store the Jaccard index in the matrix
      jaccard_matrix[i, j] <- jaccard_index
      jaccard_matrix[j, i] <- jaccard_index
    }
  }
}

# Convert the matrix to a dataframe
jaccard_df <- as.data.frame(jaccard_matrix)

# Set row names and column names
rownames(jaccard_df) <- destinations
colnames(jaccard_df) <- destinations

jaccard_df$Destination <- rownames(jaccard_df)

# Print the resulting dataframe
write.table(jaccard_df, "01_Data/Tracing_study/Jaccard Index/JaccardIndes_by Market vendor.csv", sep=",", row.names = FALSE)

################## step 2 jaccard index for markets (not market vendors)
EB_data$site = substring(EB_data$`Market vendor`, first = 9, last = 11)  

# dataframe of edges from Upazila to market vendor
# Example network data
edges <- data.frame(
  origin =EB_data$Upazila_Farm ,
  destination = EB_data$site
)

# Create a vector of unique destinations
destinations <- unique(edges$destination)

# Initialize a matrix to store the results
num_destinations <- length(destinations)

# create an empty matrix to store the Jaccard Index 
jaccard_matrix <- matrix(0, nrow = num_destinations, ncol = num_destinations)

# Iterate over each pair of destinations
for (i in 1:num_destinations) {
  for (j in 1:num_destinations) {

      node1 <- destinations[i]
      node2 <- destinations[j]
      jaccard_index <- computeJaccardIndex(edges, node1, node2)
      
      # Store the Jaccard index in the matrix
      jaccard_matrix[i, j] <- jaccard_index
      jaccard_matrix[j, i] <- jaccard_index
    
  }
}

# Convert the matrix to a dataframe
jaccard_df <- as.data.frame(jaccard_matrix)

# Set row names and column names
rownames(jaccard_df) <- destinations
colnames(jaccard_df) <- destinations

jaccard_df$Destination <- rownames(jaccard_df)

# Print the resulting dataframe
write.table(jaccard_df, "01_Data/Tracing_study/Jaccard Index/JaccardIndex_by Market site.csv", sep=",", row.names = FALSE)


