# Nepali News Clustering - Modular Functions
# This script contains modular functions for processing and clustering Nepali news

# Required libraries
required_packages <- c("text2vec", "dplyr", "stringr", "tm", "cluster", 
                       "factoextra", "ggplot2", "tidytext", "tibble")

# Install missing packages
for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#' Read all news articles from dataset
#' 
#' @param base_path Base path to the Nepali news dataset
#' @return A dataframe containing all news articles with their categories
#' @export
read_nepali_news <- function(base_path) {
  if (!dir.exists(base_path)) {
    stop("The specified path does not exist: ", base_path)
  }
  
  # Get all category folders
  categories <- list.files(base_path, full.names = FALSE)
  # Remove README.md if it exists
  categories <- categories[!grepl("README", categories)]
  
  news_data <- data.frame(
    category = character(),
    file_name = character(),
    text = character(),
    stringsAsFactors = FALSE
  )
  
  for (category in categories) {
    cat("Processing category:", category, "\n")
    category_path <- file.path(base_path, category)
    
    # Skip if not a directory
    if (!dir.exists(category_path)) {
      next
    }
    
    # Get all text files in this category
    files <- list.files(category_path, pattern = "\\.txt$", full.names = TRUE)
    
    # Read each file
    for (file_path in files) {
      tryCatch({
        # Read the file content
        content <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
        content <- paste(content, collapse = " ")
        
        # Add to dataframe
        news_data <- rbind(news_data, data.frame(
          category = category,
          file_name = basename(file_path),
          text = content,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        warning("Error reading file: ", file_path, " - ", e$message)
      })
    }
  }
  
  # Add an ID column
  news_data$id <- 1:nrow(news_data)
  
  return(news_data)
}

#' Preprocess Nepali text
#' 
#' @param text A character vector containing text to preprocess
#' @return A preprocessed character vector
#' @export
preprocess_nepali_text <- function(text) {
  # Remove URLs
  text <- str_replace_all(text, "http\\S+", "")
  
  # Remove HTML tags (if any)
  text <- str_replace_all(text, "<.*?>", "")
  
  # Normalize whitespace
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
  # Remove punctuation (keeping Nepali characters)
  # Note: This is a simplified approach and might need adjustment
  text <- str_replace_all(text, "[\\.,!\\?;:\"'\\(\\)\\[\\]{}]", " ")
  
  return(text)
}

#' Create Document-Term Matrix
#'
#' @param text_data A vector of preprocessed text
#' @return A Document-Term Matrix
#' @export
create_dtm <- function(text_data) {
  # Create a corpus
  corpus <- Corpus(VectorSource(text_data))
  
  # Create DTM
  dtm <- DocumentTermMatrix(corpus, 
                            control = list(
                              wordLengths = c(1, Inf),
                              bounds = list(global = c(2, Inf))
                            ))
  
  return(dtm)
}

#' Create TF-IDF features
#'
#' @param dtm Document-Term Matrix
#' @return A TF-IDF weighted Document-Term Matrix
#' @export
create_tfidf <- function(dtm) {
  tfidf <- weightTfIdf(dtm)
  return(tfidf)
}

#' Extract Named Entities (simplified approach)
#'
#' @param text A character vector containing text
#' @return A list of potential named entities
#' @export
extract_named_entities_simple <- function(text) {
  # This is a very simplified approach for Nepali
  # We'll look for capitalized words or words with certain patterns
  
  # For Nepali, this is challenging without NLP libraries
  # Here we'll just capture words that might be names based on length
  # A more sophisticated approach would use dedicated NER for Nepali
  
  words <- unlist(strsplit(text, "\\s+"))
  # Filter words that could be names (length between 3 and 20 characters)
  potential_names <- words[nchar(words) >= 3 & nchar(words) <= 20]
  
  return(unique(potential_names))
}

#' Perform K-means clustering
#'
#' @param feature_matrix A matrix or data frame of document features
#' @param k Number of clusters
#' @return A kmeans object
#' @export
perform_kmeans_clustering <- function(feature_matrix, k) {
  # Handle sparse matrix if needed
  if (class(feature_matrix)[1] == "dgCMatrix" || 
      class(feature_matrix)[1] == "dgTMatrix") {
    feature_matrix <- as.matrix(feature_matrix)
  }
  
  # Remove rows with NAs
  complete_cases <- complete.cases(feature_matrix)
  if (!all(complete_cases)) {
    warning("Removing ", sum(!complete_cases), " rows with missing values")
    feature_matrix <- feature_matrix[complete_cases, ]
  }
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Perform k-means
  kmeans_result <- kmeans(feature_matrix, centers = k, nstart = 25, iter.max = 100)
  
  return(kmeans_result)
}

#' Visualize clusters
#'
#' @param feature_matrix A matrix of document features
#' @param clusters A vector of cluster assignments
#' @param method Dimensionality reduction method ('pca' or 'tsne')
#' @return A ggplot object
#' @export
visualize_clusters <- function(feature_matrix, clusters, method = "pca") {
  # Reduce dimensions for visualization
  if (method == "pca") {
    # PCA for dimension reduction
    pca <- prcomp(feature_matrix, scale. = TRUE)
    viz_data <- data.frame(
      x = pca$x[, 1],
      y = pca$x[, 2],
      cluster = as.factor(clusters)
    )
    
    # Create plot
    p <- ggplot(viz_data, aes(x = x, y = y, color = cluster)) +
      geom_point(alpha = 0.7) +
      labs(title = "Cluster Visualization (PCA)",
           x = "PC1", y = "PC2") +
      theme_minimal()
    
    return(p)
  } else if (method == "tsne") {
    # t-SNE requires the Rtsne package
    if (!require("Rtsne")) {
      install.packages("Rtsne")
      library(Rtsne)
    }
    
    # Apply t-SNE
    tsne <- Rtsne(feature_matrix, dims = 2, perplexity = 30, 
                  verbose = TRUE, max_iter = 500)
    
    viz_data <- data.frame(
      x = tsne$Y[, 1],
      y = tsne$Y[, 2],
      cluster = as.factor(clusters)
    )
    
    # Create plot
    p <- ggplot(viz_data, aes(x = x, y = y, color = cluster)) +
      geom_point(alpha = 0.7) +
      labs(title = "Cluster Visualization (t-SNE)",
           x = "t-SNE1", y = "t-SNE2") +
      theme_minimal()
    
    return(p)
  } else {
    stop("Method must be either 'pca' or 'tsne'")
  }
}

#' Find top words per cluster
#'
#' @param dtm Document-Term Matrix
#' @param clusters Vector of cluster assignments
#' @param n Number of top words to return
#' @return A list with top n words for each cluster
#' @export
get_top_words_per_cluster <- function(dtm, clusters, n = 10) {
  # Convert DTM to a matrix
  dtm_matrix <- as.matrix(dtm)
  
  # Get unique clusters
  unique_clusters <- sort(unique(clusters))
  
  # Create a list to store results
  top_words <- list()
  
  for (cluster in unique_clusters) {
    # Find documents in this cluster
    docs_in_cluster <- which(clusters == cluster)
    
    # Sum the term frequencies for this cluster
    cluster_terms <- colSums(dtm_matrix[docs_in_cluster, ])
    
    # Sort terms by frequency
    sorted_terms <- sort(cluster_terms, decreasing = TRUE)
    
    # Get top n terms
    top_n_terms <- head(sorted_terms, n)
    
    # Add to results
    top_words[[paste0("cluster_", cluster)]] <- top_n_terms
  }
  
  return(top_words)
}

#' Find representative documents for each cluster
#'
#' @param feature_matrix Feature matrix used for clustering
#' @param clusters Vector of cluster assignments
#' @param news_data Original news data frame
#' @param n Number of representative documents to return
#' @return A list with n representative documents for each cluster
#' @export
get_representative_docs <- function(feature_matrix, clusters, news_data, n = 5) {
  # Get unique clusters
  unique_clusters <- sort(unique(clusters))
  
  # Create a list to store results
  rep_docs <- list()
  
  for (cluster in unique_clusters) {
    # Find documents in this cluster
    docs_in_cluster <- which(clusters == cluster)
    
    # Get the cluster centroid
    centroid <- colMeans(feature_matrix[docs_in_cluster, , drop = FALSE])
    
    # Calculate distance from each document to centroid
    distances <- apply(feature_matrix[docs_in_cluster, , drop = FALSE], 1, 
                       function(x) sqrt(sum((x - centroid)^2)))
    
    # Get indices of documents closest to centroid
    closest_indices <- order(distances)[1:min(n, length(distances))]
    
    # Get the actual document indices in the original data
    doc_indices <- docs_in_cluster[closest_indices]
    
    # Get document info
    rep_docs[[paste0("cluster_", cluster)]] <- news_data[doc_indices, ]
  }
  
  return(rep_docs)
}

#' Save clustering results
#'
#' @param news_data Original news data frame
#' @param clusters Vector of cluster assignments
#' @param output_file Output file path
#' @return None (writes to file)
#' @export
save_clustering_results <- function(news_data, clusters, output_file) {
  # Add cluster assignments to the news data
  results <- news_data
  results$cluster <- clusters
  
  # Write to CSV
  write.csv(results, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("Results saved to:", output_file, "\n")
}

#' Determine optimal number of clusters using silhouette method
#'
#' @param feature_matrix Feature matrix to cluster
#' @param max_k Maximum number of clusters to try
#' @return Optimal number of clusters
#' @export
find_optimal_k <- function(feature_matrix, max_k = 20) {
  # Handle sparse matrix if needed
  if (class(feature_matrix)[1] == "dgCMatrix" || 
      class(feature_matrix)[1] == "dgTMatrix") {
    feature_matrix <- as.matrix(feature_matrix)
  }
  
  # Sample if the dataset is too large
  max_rows <- 1000
  if (nrow(feature_matrix) > max_rows) {
    set.seed(42)
    sampled_indices <- sample(1:nrow(feature_matrix), max_rows)
    feature_matrix_sample <- feature_matrix[sampled_indices, ]
  } else {
    feature_matrix_sample <- feature_matrix
  }
  
  # Calculate silhouette width for different k values
  silhouette_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    cat("Testing k =", k, "\n")
    km <- kmeans(feature_matrix_sample, centers = k, nstart = 10)
    ss <- silhouette(km$cluster, dist(feature_matrix_sample))
    silhouette_scores[k-1] <- mean(ss[, 3])
  }
  
  # Find k with highest silhouette score
  optimal_k <- which.max(silhouette_scores) + 1
  
  # Plot silhouette scores
  plot(2:max_k, silhouette_scores, type = "b", 
       xlab = "Number of clusters", ylab = "Average silhouette width")
  abline(v = optimal_k, col = "red", lty = 2)
  
  cat("Optimal number of clusters:", optimal_k, "\n")
  
  return(optimal_k)
}

#' Analyze cluster completeness
#'
#' @param news_data News data frame with cluster assignments
#' @param rep_docs Representative documents per cluster
#' @return A data frame with completeness scores
#' @export
analyze_completeness <- function(news_data, rep_docs) {
  # This is a placeholder for future implementation
  # Will be developed in Phase 2 of the project
  
  cat("Completeness analysis will be implemented in Phase 2\n")
  
  # For now, return a simple structure
  return(data.frame(
    cluster = unique(news_data$cluster),
    completeness_score = NA
  ))
}

#' Analyze bias in clusters
#'
#' @param news_data News data frame with cluster assignments
#' @return A data frame with bias scores
#' @export
analyze_bias <- function(news_data) {
  # This is a placeholder for future implementation
  # Will be developed in Phase 3 of the project
  
  cat("Bias analysis will be implemented in Phase 3\n")
  
  # For now, return a simple structure
  return(data.frame(
    cluster = unique(news_data$cluster),
    bias_score = NA
  ))
}