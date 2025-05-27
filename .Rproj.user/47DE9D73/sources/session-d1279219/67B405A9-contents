# Memory-Efficient Approach for Clustering Nepali News
# This script handles large datasets with memory constraints

# Load necessary libraries
required_packages <- c("text2vec", "dplyr", "stringr", "irlba", "cluster", 
                       "Matrix", "ggplot2")

# Install missing packages
for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#' Apply dimensionality reduction to TF-IDF matrix
#'
#' @param tfidf TF-IDF matrix (sparse or dense)
#' @param n_components Number of components to keep
#' @return Reduced dimension matrix
#' @export
reduce_dimensions <- function(tfidf, n_components = 100) {
  cat("Reducing dimensions to", n_components, "components...\n")
  
  # Handle empty documents
  non_empty_docs <- which(rowSums(as.matrix(tfidf)) > 0)
  if (length(non_empty_docs) < nrow(tfidf)) {
    cat("Removing", nrow(tfidf) - length(non_empty_docs), "empty documents\n")
    tfidf <- tfidf[non_empty_docs, ]
  }
  
  # Convert to dense matrix for SVD
  tfidf_matrix <- as.matrix(tfidf)
  
  # Use prcomp for PCA (base R, more stable than irlba)
  cat("Performing PCA...\n")
  pca_result <- prcomp(tfidf_matrix, center = TRUE, scale. = FALSE, rank. = n_components)
  
  # Create reduced matrix (document-component)
  reduced_matrix <- pca_result$x
  
  # Return reduced matrix and document indices
  return(list(
    reduced_matrix = reduced_matrix,
    non_empty_docs = non_empty_docs
  ))
}

#' Perform k-means clustering on reduced dimensions
#'
#' @param reduced_matrix Matrix with reduced dimensions
#' @param k Number of clusters
#' @return A kmeans object
#' @export
perform_kmeans_on_reduced <- function(reduced_matrix, k) {
  cat("Performing k-means clustering with k =", k, "on reduced matrix...\n")
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Run k-means with multiple starts
  kmeans_result <- kmeans(reduced_matrix, centers = k, nstart = 25, iter.max = 100)
  
  return(kmeans_result)
}

#' Map clusters back to full dataset
#'
#' @param clusters Cluster assignments for non-empty documents
#' @param non_empty_docs Indices of non-empty documents
#' @param total_docs Total number of documents
#' @return A vector of cluster assignments for all documents
#' @export
map_clusters_to_full_dataset <- function(clusters, non_empty_docs, total_docs) {
  # Create a vector for all documents, default to NA
  full_clusters <- rep(NA, total_docs)
  
  # Assign clusters to non-empty documents
  full_clusters[non_empty_docs] <- clusters
  
  # For empty documents, assign to nearest non-empty cluster
  # (Here we just use the most common cluster as a fallback)
  most_common_cluster <- which.max(table(clusters))
  full_clusters[is.na(full_clusters)] <- most_common_cluster
  
  return(full_clusters)
}

#' Create vocabulary-limited DTM for memory efficiency
#'
#' @param text_data A vector of preprocessed text
#' @param max_features Maximum number of features to include
#' @return A Document-Term Matrix with limited vocabulary
#' @export
create_memory_efficient_dtm <- function(text_data, max_features = 5000) {
  # Create a corpus
  corpus <- Corpus(VectorSource(text_data))
  
  # Create DTM with limited features
  dtm <- DocumentTermMatrix(corpus, 
                            control = list(
                              wordLengths = c(1, Inf),
                              bounds = list(global = c(5, Inf)),  # Require terms to appear in at least 5 docs
                              weighting = weightTf,
                              removeNumbers = TRUE
                            ))
  
  # Find terms that appear in multiple documents
  # Use colSums instead of col_sums
  term_freq <- colSums(as.matrix(dtm))
  sorted_terms <- sort(term_freq, decreasing = TRUE)
  
  # Keep only the most frequent terms
  if (length(sorted_terms) > max_features) {
    terms_to_keep <- names(sorted_terms)[1:max_features]
    dtm <- dtm[, terms_to_keep]
  }
  
  cat("Created DTM with dimensions:", dim(dtm), "\n")
  return(dtm)
}

#' Process news data in batches to conserve memory
#'
#' @param news_data Data frame containing news articles
#' @param k Number of clusters
#' @param max_features Maximum number of features to use
#' @param n_components Number of components for dimension reduction
#' @return A list containing clustering results
#' @export
process_in_batches <- function(news_data, k = 20, max_features = 5000, n_components = 100) {
  # 1. Create memory-efficient DTM
  cat("Creating memory-efficient DTM...\n")
  dtm <- create_memory_efficient_dtm(news_data$processed_text, max_features)
  
  # 2. Create TF-IDF
  cat("Creating TF-IDF features...\n")
  tfidf <- weightTfIdf(dtm)
  
  # 3. Reduce dimensions
  cat("Reducing dimensions...\n")
  reduced_data <- reduce_dimensions(tfidf, n_components)
  
  # 4. Perform clustering
  cat("Clustering...\n")
  kmeans_result <- perform_kmeans_on_reduced(reduced_data$reduced_matrix, k)
  
  # 5. Map clusters back to full dataset
  cat("Mapping clusters to full dataset...\n")
  full_clusters <- map_clusters_to_full_dataset(
    kmeans_result$cluster, 
    reduced_data$non_empty_docs, 
    nrow(news_data)
  )
  
  # Return results
  return(list(
    clusters = full_clusters,
    centers = kmeans_result$centers,
    reduced_data = reduced_data,
    dtm = dtm
  ))
}

#' Find top words in each cluster using efficient methods
#'
#' @param dtm Document-Term Matrix
#' @param clusters Vector of cluster assignments
#' @param n Number of top words to return
#' @return A list with top n words for each cluster
#' @export
get_top_words_efficient <- function(dtm, clusters, n = 10) {
  # Get unique clusters
  unique_clusters <- sort(unique(clusters))
  
  # Create a list to store results
  top_words <- list()
  
  # Convert to matrix if needed for transposition
  if (inherits(dtm, "DocumentTermMatrix")) {
    # Get term names before converting
    terms <- colnames(dtm)
    # Convert to matrix and transpose
    tdm_matrix <- t(as.matrix(dtm))
    rownames(tdm_matrix) <- terms
  } else {
    # Already a matrix, just transpose
    tdm_matrix <- t(dtm)
  }
  
  for (cluster in unique_clusters) {
    # Find documents in this cluster
    docs_in_cluster <- which(clusters == cluster)
    
    if (length(docs_in_cluster) > 0) {
      # Sum the term frequencies for this cluster
      cluster_terms <- rowSums(tdm_matrix[, docs_in_cluster, drop = FALSE])
      
      # Sort terms by frequency
      sorted_indices <- order(cluster_terms, decreasing = TRUE)
      
      # Get top n terms
      top_n_indices <- head(sorted_indices, n)
      top_n_terms <- cluster_terms[top_n_indices]
      
      # Add to results
      top_words[[paste0("cluster_", cluster)]] <- top_n_terms
    } else {
      top_words[[paste0("cluster_", cluster)]] <- character(0)
    }
  }
  
  return(top_words)
}

#' Visualize clusters from reduced dimensions
#'
#' @param reduced_matrix Reduced dimension matrix 
#' @param clusters Cluster assignments
#' @param sample_size Number of points to sample for visualization
#' @return A ggplot object
#' @export
visualize_clusters_reduced <- function(reduced_matrix, clusters, sample_size = 1000) {
  # Sample data if needed
  n_docs <- nrow(reduced_matrix)
  
  if (n_docs > sample_size) {
    set.seed(42)
    sample_indices <- sample(1:n_docs, sample_size)
    reduced_sample <- reduced_matrix[sample_indices, ]
    clusters_sample <- clusters[sample_indices]
  } else {
    reduced_sample <- reduced_matrix
    clusters_sample <- clusters
  }
  
  # Use the first two components for visualization
  viz_data <- data.frame(
    x = reduced_sample[, 1],
    y = reduced_sample[, 2],
    cluster = as.factor(clusters_sample)
  )
  
  # Create plot
  p <- ggplot(viz_data, aes(x = x, y = y, color = cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = "Cluster Visualization",
         x = "Component 1", y = "Component 2") +
    theme_minimal() +
    theme(legend.position = "right")
  
  return(p)
}

#' Find representative documents for each cluster efficiently
#'
#' @param reduced_matrix Reduced dimension matrix
#' @param clusters Vector of cluster assignments
#' @param news_data Original news data frame
#' @param non_empty_docs Indices of non-empty documents
#' @param n Number of representative documents to return
#' @return A list with n representative documents for each cluster
#' @export
get_representative_docs_efficient <- function(reduced_matrix, clusters, news_data, 
                                              non_empty_docs, n = 3) {
  # Get unique clusters
  unique_clusters <- sort(unique(clusters))
  
  # Create a list to store results
  rep_docs <- list()
  
  for (cluster in unique_clusters) {
    # Find documents in this cluster
    docs_in_cluster_indices <- which(clusters == cluster)
    
    if (length(docs_in_cluster_indices) > 0) {
      # Get the cluster centroid
      centroid <- colMeans(reduced_matrix[docs_in_cluster_indices, , drop = FALSE])
      
      # Calculate distance from each document to centroid
      distances <- apply(reduced_matrix[docs_in_cluster_indices, , drop = FALSE], 1, 
                         function(x) sqrt(sum((x - centroid)^2)))
      
      # Get indices of documents closest to centroid
      closest_indices <- order(distances)[1:min(n, length(distances))]
      
      # Get the actual document indices in the original data
      doc_indices <- non_empty_docs[docs_in_cluster_indices[closest_indices]]
      
      # Get document info
      rep_docs[[paste0("cluster_", cluster)]] <- news_data[doc_indices, ]
    } else {
      rep_docs[[paste0("cluster_", cluster)]] <- data.frame()
    }
  }
  
  return(rep_docs)
}

#' Compare clustering results with original categories
#'
#' @param clusters Vector of cluster assignments
#' @param categories Vector of original category labels
#' @return A confusion matrix and accuracy
#' @export
compare_clusters_to_categories <- function(clusters, categories) {
  # Create a confusion matrix
  confusion <- table(categories, clusters)
  
  # Find the best matching category for each cluster
  cluster_to_category <- apply(confusion, 2, which.max)
  
  # Map clusters to their most likely category
  predicted_category <- sapply(clusters, function(c) rownames(confusion)[cluster_to_category[c]])
  
  # Calculate accuracy
  accuracy <- sum(predicted_category == categories) / length(categories)
  
  return(list(
    confusion = confusion,
    cluster_to_category = cluster_to_category,
    predicted_category = predicted_category,
    accuracy = accuracy
  ))
}







# Add this to your nepali-news-memory-efficient.R or create a new script
find_optimal_k_improved <- function(reduced_matrix, max_k = 30, method = "silhouette") {
  # Set seed for reproducibility
  set.seed(42)
  
  if (method == "silhouette") {
    # Calculate silhouette width for different k values
    silhouette_scores <- numeric(max_k - 1)
    
    for (k in 2:max_k) {
      cat("Testing k =", k, "\n")
      km <- kmeans(reduced_matrix, centers = k, nstart = 10)
      
      # Load required package for silhouette
      if (!require("cluster")) {
        install.packages("cluster")
        library(cluster)
      }
      
      ss <- silhouette(km$cluster, dist(reduced_matrix))
      silhouette_scores[k-1] <- mean(ss[, 3])
    }
    
    # Plot silhouette scores
    plot(2:max_k, silhouette_scores, type = "b", 
         xlab = "Number of clusters", ylab = "Average silhouette width",
         main = "Silhouette Method for Optimal k")
    abline(v = which.max(silhouette_scores) + 1, col = "red", lty = 2)
    
    optimal_k <- which.max(silhouette_scores) + 1
  } else if (method == "elbow") {
    # Elbow method using within-cluster sum of squares
    wss <- numeric(max_k)
    
    for (k in 1:max_k) {
      cat("Testing k =", k, "\n")
      km <- kmeans(reduced_matrix, centers = k, nstart = 10)
      wss[k] <- km$tot.withinss
    }
    
    # Plot elbow curve
    plot(1:max_k, wss, type = "b", 
         xlab = "Number of clusters", ylab = "Within-cluster sum of squares",
         main = "Elbow Method for Optimal k")
    
    # Identify the elbow point (this is a heuristic approach)
    # Calculate the angle for each point
    angles <- numeric(max_k-2)
    for (i in 2:(max_k-1)) {
      v1 <- c(1, wss[i] - wss[i-1])
      v2 <- c(1, wss[i+1] - wss[i])
      angles[i-1] <- acos(sum(v1*v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2))))
    }
    
    # The elbow is where the angle is maximum
    optimal_k <- which.max(angles) + 1
    abline(v = optimal_k, col = "red", lty = 2)
  }
  
  cat("Optimal number of clusters:", optimal_k, "\n")
  return(optimal_k)
}



