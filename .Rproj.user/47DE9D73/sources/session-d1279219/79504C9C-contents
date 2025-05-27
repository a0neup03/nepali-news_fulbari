# Complete workflow for finding optimal clusters in Nepali news

# Source the necessary files
source("nepali-news-memory-efficient.R")
source("nepali-stemming.R")
source("nepali-news-stemming-example.R")
source("nepali-alluvial-plot.R")

# Function to find optimal clusters and create visualizations
find_optimal_clusters_and_visualize <- function(news_data, max_k = 25) {
  # 1. Apply improved preprocessing with stemming
  cat("Preprocessing with improved stemming...\n")
  news_data$processed_text <- sapply(news_data$text, preprocess_nepali_with_stemming)
  
  # Check for empty documents
  empty_docs <- sum(nchar(news_data$processed_text) == 0)
  cat("Number of empty documents after preprocessing:", empty_docs, "\n")
  
  # 2. Create DTM with better feature selection
  cat("Creating document-term matrix...\n")
  dtm <- DocumentTermMatrix(
    Corpus(VectorSource(news_data$processed_text)), 
    control = list(
      wordLengths = c(2, Inf),
      bounds = list(global = c(3, Inf)),
      weighting = weightTf
    )
  )
  cat("DTM dimensions:", dim(dtm), "\n")
  
  # 3. Apply additional feature filtering
  # Keep terms that appear in 3-50% of documents
  term_freq <- colSums(as.matrix(dtm) > 0)
  min_docs <- 3
  max_docs <- nrow(dtm) * 0.5
  distinctive_terms <- names(term_freq[term_freq >= min_docs & term_freq <= max_docs])
  
  if (length(distinctive_terms) > 0) {
    filtered_dtm <- dtm[, distinctive_terms]
    cat("Filtered DTM from", ncol(dtm), "to", ncol(filtered_dtm), "features\n")
  } else {
    filtered_dtm <- dtm
    cat("No terms matched the filtering criteria, using original DTM\n")
  }
  
  # 4. Create TF-IDF
  cat("Creating TF-IDF features...\n")
  tfidf <- weightTfIdf(filtered_dtm)
  
  # 5. Reduce dimensions
  cat("Reducing dimensions...\n")
  reduced_data <- reduce_dimensions(tfidf, n_components = 100)
  
  # 6. Visualize and find optimal number of clusters
  cat("Finding optimal number of clusters...\n")
  optimal_k_results <- visualize_optimal_clusters(
    reduced_data$reduced_matrix, 
    max_k = max_k,
    save_file = "nepali_news_optimal_k"
  )
  
  # 7. Use both optimal k values to create two clustering solutions
  results_list <- list()
  
  for (k_method in c("elbow", "silhouette")) {
    k <- optimal_k_results[[k_method]]
    cat("\nPerforming clustering with k =", k, "from", k_method, "method...\n")
    
    # Perform clustering
    clusters <- perform_kmeans_on_reduced(reduced_data$reduced_matrix, k)
    
    # Map clusters back to full dataset
    full_clusters <- rep(NA, nrow(news_data))
    full_clusters[reduced_data$non_empty_docs] <- clusters$cluster
    
    # Handle NA values (empty documents)
    if (any(is.na(full_clusters))) {
      most_common_cluster <- as.numeric(names(sort(table(clusters$cluster), decreasing = TRUE)[1]))
      full_clusters[is.na(full_clusters)] <- most_common_cluster
      cat("Assigned", sum(is.na(full_clusters)), "empty documents to the most common cluster\n")
    }
    
    # Create visualization
    cat("Creating alluvial plot...\n")
    alluvial_plot <- create_alluvial_plot(
      news_data, 
      full_clusters, 
      save_file = paste0("nepali_news_alluvial_", k_method, ".pdf")
    )
    
    # Create cluster composition plot
    cat("Creating cluster composition plot...\n")
    composition_plot <- create_cluster_composition_plot(
      news_data, 
      full_clusters, 
      top_n = 3, 
      save_file = paste0("nepali_news_composition_", k_method, ".pdf")
    )
    
    # Calculate accuracy
    cont_table <- table(news_data$category, full_clusters)
    cluster_to_category <- apply(cont_table, 2, which.max)
    predicted_category <- sapply(full_clusters, function(c) rownames(cont_table)[cluster_to_category[c]])
    accuracy <- sum(predicted_category == news_data$category) / nrow(news_data)
    cat("Clustering accuracy:", round(accuracy * 100, 2), "%\n")
    
    # Store results
    results_list[[k_method]] <- list(
      k = k,
      dtm = filtered_dtm,
      reduced_data = reduced_data,
      clusters = clusters,
      full_clusters = full_clusters,
      predicted_category = predicted_category,
      accuracy = accuracy,
      alluvial_plot = alluvial_plot,
      composition_plot = composition_plot
    )
  }
  
  # 8. Return all results
  return(list(
    optimal_k = optimal_k_results,
    results = results_list
  ))
}

# Example usage:
# results <- find_optimal_clusters_and_visualize(news_data)

# Analyze top words in each cluster (for the best method)
analyze_best_clusters <- function(results) {
  # Choose method with highest accuracy
  if (results$results$elbow$accuracy > results$results$silhouette$accuracy) {
    best_method <- "elbow"
  } else {
    best_method <- "silhouette"
  }
  
  best_results <- results$results[[best_method]]
  cat("Using clusters from", best_method, "method (k =", best_results$k, 
      ") with accuracy", round(best_results$accuracy * 100, 2), "%\n")
  
  # Analyze clusters
  stem_analysis <- analyze_clusters_with_stems(list(
    news_data = news_data,
    dtm = best_results$dtm,
    full_clusters = best_results$full_clusters
  ))
  
  return(stem_analysis)
}

# Example usage:
# stem_analysis <- analyze_best_clusters(results)