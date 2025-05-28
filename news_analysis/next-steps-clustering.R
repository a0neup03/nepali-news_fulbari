# Next Steps for Nepali News Clustering & Story Matching
# This script provides functions for identifying related news articles

# Function to calculate document similarity
calculate_document_similarity <- function(tfidf_matrix, threshold = 0.3) {
  # Convert to matrix if not already
  if (!is.matrix(tfidf_matrix)) {
    tfidf_matrix <- as.matrix(tfidf_matrix)
  }
  
  # Normalize the vectors for cosine similarity
  norm_tfidf <- tfidf_matrix / sqrt(rowSums(tfidf_matrix^2))
  
  # Replace NAs with 0s (for empty docs)
  norm_tfidf[is.na(norm_tfidf)] <- 0
  
  # Calculate similarity matrix (cosine similarity)
  sim_matrix <- norm_tfidf %*% t(norm_tfidf)
  
  # Create a similarity network
  n_docs <- nrow(sim_matrix)
  sim_network <- data.frame(
    doc1 = integer(),
    doc2 = integer(),
    similarity = numeric()
  )
  
  for (i in 1:(n_docs-1)) {
    for (j in (i+1):n_docs) {
      if (sim_matrix[i, j] >= threshold) {
        sim_network <- rbind(sim_network, data.frame(
          doc1 = i,
          doc2 = j,
          similarity = sim_matrix[i, j]
        ))
      }
    }
  }
  
  return(list(
    similarity_matrix = sim_matrix,
    similarity_network = sim_network
  ))
}

# Function to identify document clusters based on similarity
identify_document_clusters <- function(similarity_network, min_similarity = 0.3) {
  # Filter by minimum similarity
  network <- similarity_network[similarity_network$similarity >= min_similarity, ]
  
  if (nrow(network) == 0) {
    return(list())
  }
  
  # Create a graph
  if (!require("igraph")) {
    install.packages("igraph")
    library(igraph)
  }
  
  g <- graph_from_data_frame(network[, c("doc1", "doc2")], directed = FALSE)
  
  # Find connected components (clusters)
  components <- components(g)
  
  # Create a list of document IDs for each cluster
  clusters <- list()
  for (i in 1:components$no) {
    clusters[[i]] <- which(components$membership == i)
  }
  
  return(clusters)
}

# Function to extract potential named entities from a set of documents
extract_entities_from_documents <- function(texts, min_length = 3, min_freq = 2) {
  # Process each document
  all_entities <- lapply(texts, function(text) {
    extract_nepali_entities_improved(text, min_length)
  })
  
  # Flatten the list of entities
  all_entities_flat <- unlist(all_entities)
  
  # Count frequencies
  entity_counts <- table(all_entities_flat)
  
  # Filter by minimum frequency
  frequent_entities <- names(entity_counts[entity_counts >= min_freq])
  
  return(frequent_entities)
}

# Function to find related news articles
find_related_news <- function(news_data, similarity_threshold = 0.3) {
  # 1. Apply preprocessing with stemming
  cat("Preprocessing news data...\n")
  processed_texts <- sapply(news_data$text, preprocess_nepali_with_stemming)
  
  # 2. Create DTM
  cat("Creating document-term matrix...\n")
  dtm <- DocumentTermMatrix(
    Corpus(VectorSource(processed_texts)), 
    control = list(
      wordLengths = c(2, Inf),
      bounds = list(global = c(3, Inf)),
      weighting = weightTf
    )
  )
  
  # 3. Convert to TF-IDF
  cat("Creating TF-IDF matrix...\n")
  tfidf <- weightTfIdf(dtm)
  
  # 4. Calculate document similarity
  cat("Calculating document similarity...\n")
  similarity_data <- calculate_document_similarity(tfidf, similarity_threshold)
  
  # 5. Identify document clusters
  cat("Identifying document clusters...\n")
  doc_clusters <- identify_document_clusters(similarity_data$similarity_network, similarity_threshold)
  
  # 6. Extract entities for each cluster
  cat("Extracting entities for each cluster...\n")
  cluster_entities <- list()
  
  for (i in 1:length(doc_clusters)) {
    if (length(doc_clusters[[i]]) > 1) {
      cluster_texts <- news_data$text[doc_clusters[[i]]]
      cluster_entities[[i]] <- extract_entities_from_documents(cluster_texts)
    }
  }
  
  # 7. Create results
  results <- list()
  
  for (i in 1:length(doc_clusters)) {
    if (length(doc_clusters[[i]]) > 1) {
      # Get document info
      doc_indices <- doc_clusters[[i]]
      cluster_docs <- news_data[doc_indices, ]
      
      # Get entity info
      entities <- cluster_entities[[i]]
      
      # Add to results
      results[[i]] <- list(
        documents = cluster_docs,
        entities = entities
      )
    }
  }
  
  return(results)
}

# Function to compare news coverage across categories or sources
compare_news_coverage <- function(related_news_results, category_field = "category") {
  coverage_stats <- list()
  
  for (i in 1:length(related_news_results)) {
    cluster <- related_news_results[[i]]
    
    if (length(cluster$documents) > 1) {
      # Count documents by category/source
      category_counts <- table(cluster$documents[[category_field]])
      
      # Calculate percentages
      total_docs <- sum(category_counts)
      category_percentages <- 100 * category_counts / total_docs
      
      # Store in results
      coverage_stats[[i]] <- list(
        counts = category_counts,
        percentages = category_percentages,
        total = total_docs,
        entities = cluster$entities
      )
    }
  }
  
  return(coverage_stats)
}

# Function to identify potential blindspots (disproportionate coverage)
identify_blindspots <- function(coverage_stats, left_sources, right_sources, threshold = 33) {
  blindspots <- list()
  
  for (i in 1:length(coverage_stats)) {
    stats <- coverage_stats[[i]]
    
    # Skip if no source information
    if (length(stats$counts) == 0) {
      next
    }
    
    # Count coverage by political leaning
    source_names <- names(stats$counts)
    
    left_coverage <- sum(stats$counts[source_names %in% left_sources])
    right_coverage <- sum(stats$counts[source_names %in% right_sources])
    
    total_coverage <- left_coverage + right_coverage
    
    # Calculate percentages
    left_percent <- 100 * left_coverage / total_coverage
    right_percent <- 100 * right_coverage / total_coverage
    
    # Check if this is a left blindspot
    is_left_blindspot <- FALSE
    if (left_coverage < 10 && right_percent >= threshold) {
      threshold_calc <- (right_percent - left_percent - threshold) * (30/37)
      if (left_percent <= threshold_calc) {
        is_left_blindspot <- TRUE
      }
    }
    
    # Check if this is a right blindspot
    is_right_blindspot <- FALSE
    if (right_coverage < 10 && left_percent >= threshold) {
      threshold_calc <- (left_percent - right_percent - threshold) * (30/37)
      if (right_percent <= threshold_calc) {
        is_right_blindspot <- TRUE
      }
    }
    
    # Add to blindspots if applicable
    if (is_left_blindspot || is_right_blindspot) {
      blindspots[[length(blindspots) + 1]] <- list(
        cluster_id = i,
        stats = stats,
        left_coverage = left_coverage,
        right_coverage = right_coverage,
        left_percent = left_percent,
        right_percent = right_percent,
        is_left_blindspot = is_left_blindspot,
        is_right_blindspot = is_right_blindspot
      )
    }
  }
  
  return(blindspots)
}



hierarchical_clustering <- function(reduced_matrix, k = NULL, sample_size = 5000) {
  # Sample data if needed
  if (nrow(reduced_matrix) > sample_size) {
    set.seed(42)
    indices <- sample(1:nrow(reduced_matrix), sample_size)
    sampled_matrix <- reduced_matrix[indices, ]
  } else {
    sampled_matrix <- reduced_matrix
    indices <- 1:nrow(reduced_matrix)
  }
  
  # Calculate distance matrix
  dist_matrix <- dist(sampled_matrix)
  
  # Perform hierarchical clustering
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Plot dendrogram
  plot(hc, labels = FALSE, main = "Hierarchical Clustering Dendrogram",
       xlab = "Documents", ylab = "Distance", hang = -1)
  
  # Cut tree if k is provided
  if (!is.null(k)) {
    clusters <- cutree(hc, k = k)
    rect.hclust(hc, k = k, border = "red")
  } else {
    clusters <- NULL
  }
  
  return(list(
    hclust = hc,
    clusters = clusters,
    indices = indices
  ))
}

# Example workflow for the complete analysis pipeline:
# 
# # Step 1: Process news data with improved stemming
# results <- process_news_data_with_stemming(news_data)
# 
# # Step 2: Perform basic clustering analysis
# stem_analysis <- analyze_clusters_with_stems(results)
# 
# # Step 3: Find related news articles
# related_news <- find_related_news(news_data)
# 
# # Step 4: Compare coverage across categories
# coverage_stats <- compare_news_coverage(related_news)
# 
# # Step 5: Identify potential blindspots
# # (This would require a list of sources categorized by political leaning)
# left_sources <- c("source1", "source2", "source3")
# right_sources <- c("source4", "source5", "source6")
# blindspots <- identify_blindspots(coverage_stats, left_sources, right_sources)