# File: nepali-story-completeness.R
# Story completeness analysis system for Nepali news

# Required libraries
required_packages <- c("tm", "Matrix", "igraph", "stringr", "ggplot2", 
                       "reshape2", "parallel", "dplyr")

for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#' Identify similar articles using pairwise similarity
#'
#' @param news_data A data frame containing news articles
#' @param similarity_threshold Minimum similarity score to consider articles related
#' @param time_window_days Maximum days between articles to compare
#' @return A data frame of article pairs with similarity scores
#' @export
identify_similar_articles <- function(news_data, similarity_threshold = 0.3, time_window_days = 7) {
  # If needed, preprocess the text
  if(!"processed_text" %in% colnames(news_data)) {
    news_data$processed_text <- sapply(news_data$text, preprocess_nepali_with_stemming)
  }
  
  # Create DTM
  dtm <- create_memory_efficient_dtm(news_data$processed_text)
  
  # Create TF-IDF
  tfidf <- weightTfIdf(dtm)
  
  # Convert to matrix for calculations
  tfidf_matrix <- as.matrix(tfidf)
  
  # Add dates if not present (for simulation)
  if(!"date" %in% colnames(news_data)) {
    # Simulate dates spanning 30 days
    start_date <- as.Date("2023-01-01")
    dates <- start_date + sample(0:29, nrow(news_data), replace = TRUE)
    news_data$date <- dates
  }
  
  # Create similarity pairs
  n_articles <- nrow(news_data)
  similar_pairs <- data.frame(
    article1 = integer(),
    article2 = integer(),
    similarity = numeric(),
    stringsAsFactors = FALSE
  )
  
  # For efficiency, process in chunks
  chunk_size <- 100  # Adjust based on memory constraints
  
  for(i in 1:ceiling(n_articles/chunk_size)) {
    start_i <- (i-1)*chunk_size + 1
    end_i <- min(i*chunk_size, n_articles)
    
    # Normalize vectors for current chunk
    chunk_norms <- sqrt(rowSums(tfidf_matrix[start_i:end_i, ]^2))
    chunk_norms[chunk_norms == 0] <- 1  # Avoid division by zero
    chunk_normalized <- tfidf_matrix[start_i:end_i, ] / chunk_norms
    
    for(j in i:ceiling(n_articles/chunk_size)) {
      start_j <- (j-1)*chunk_size + 1
      end_j <- min(j*chunk_size, n_articles)
      
      # Only compare articles within time window
      date_matrix <- outer(news_data$date[start_i:end_i], 
                           news_data$date[start_j:end_j], 
                           FUN = function(x, y) abs(as.numeric(difftime(x, y, units = "days"))))
      
      # Skip pairs outside time window
      if(all(date_matrix > time_window_days)) {
        next
      }
      
      # Normalize vectors for comparison chunk if different from current
      if(i != j) {
        compare_norms <- sqrt(rowSums(tfidf_matrix[start_j:end_j, ]^2))
        compare_norms[compare_norms == 0] <- 1
        compare_normalized <- tfidf_matrix[start_j:end_j, ] / compare_norms
        
        # Calculate similarities
        sim_matrix <- chunk_normalized %*% t(compare_normalized)
      } else {
        # Same chunk, only calculate upper triangle
        sim_matrix <- chunk_normalized %*% t(chunk_normalized)
        sim_matrix[lower.tri(sim_matrix, diag = TRUE)] <- 0
      }
      
      # Apply time window filter
      sim_matrix[date_matrix > time_window_days] <- 0
      
      # Find pairs above threshold
      high_sim <- which(sim_matrix >= similarity_threshold, arr.ind = TRUE)
      
      if(nrow(high_sim) > 0) {
        # Adjust indices to global positions
        high_sim[, 1] <- high_sim[, 1] + start_i - 1
        if(i == j) {
          high_sim[, 2] <- high_sim[, 2] + start_i - 1
        } else {
          high_sim[, 2] <- high_sim[, 2] + start_j - 1
        }
        
        # Extract similarity values
        sim_values <- sapply(1:nrow(high_sim), function(k) {
          row_idx <- high_sim[k, 1] - start_i + 1
          col_idx <- if(i == j) high_sim[k, 2] - start_i + 1 else high_sim[k, 2] - start_j + 1
          return(sim_matrix[row_idx, col_idx])
        })
        
        # Add to result
        new_pairs <- data.frame(
          article1 = high_sim[, 1],
          article2 = high_sim[, 2],
          similarity = sim_values,
          stringsAsFactors = FALSE
        )
        similar_pairs <- rbind(similar_pairs, new_pairs)
      }
    }
  }
  
  return(similar_pairs)
}

#' Group similar articles into story clusters
#'
#' @param similar_pairs Data frame of article pairs with similarity scores
#' @param news_data News article data frame
#' @return List of story clusters
#' @export
form_story_clusters <- function(similar_pairs, news_data) {
  # Create graph from similar pairs
  g <- graph_from_data_frame(similar_pairs[, c("article1", "article2")], 
                             directed = FALSE)
  
  # Set edge weights to similarity scores
  E(g)$weight <- similar_pairs$similarity
  
  # Find communities (story clusters)
  communities <- cluster_louvain(g)
  
  # Extract clusters
  membership <- membership(communities)
  
  # Create list of story clusters
  story_clusters <- list()
  
  for(cluster_id in unique(membership)) {
    # Get articles in this cluster
    article_indices <- which(membership == cluster_id)
    
    if(length(article_indices) >= 3) {  # Only keep clusters with 3+ articles
      # Get article data
      cluster_articles <- news_data[article_indices, ]
      
      # Add to story clusters
      story_clusters[[length(story_clusters) + 1]] <- list(
        id = cluster_id,
        size = length(article_indices),
        articles = cluster_articles,
        article_indices = article_indices
      )
    }
  }
  
  # Sort by size
  sizes <- sapply(story_clusters, function(x) x$size)
  story_clusters <- story_clusters[order(sizes, decreasing = TRUE)]
  
  return(story_clusters)
}

#' Extract key elements from a story cluster
#'
#' @param story_cluster A story cluster object
#' @return List of extracted elements
#' @export
extract_story_elements <- function(story_cluster) {
  # Extract all text from cluster
  all_texts <- story_cluster$articles$text
  processed_texts <- story_cluster$articles$processed_text
  if(is.null(processed_texts)) {
    processed_texts <- sapply(all_texts, preprocess_nepali_with_stemming)
  }
  
  # 1. Extract entities (using frequency for now)
  words <- unlist(strsplit(paste(processed_texts, collapse = " "), " "))
  word_freq <- table(words)
  sorted_freq <- sort(word_freq, decreasing = TRUE)
  
  # Filter for potential entities (based on frequency and length)
  potential_entities <- names(sorted_freq)[nchar(names(sorted_freq)) >= 3 & sorted_freq >= 2]
  
  # 2. Extract key phrases (simplified)
  # Split each text into sentences
  sentences <- unlist(lapply(all_texts, function(text) {
    # Split on Nepali sentence markers
    sentences <- unlist(strsplit(text, "।|\\.|\\?|!"))
    # Clean and filter non-empty sentences
    sentences <- trimws(sentences)
    return(sentences[nchar(sentences) > 10])
  }))
  
  # Count sentence frequency across articles
  sentence_counts <- table(sentences)
  
  # Key phrases are those that appear in multiple articles
  key_phrases <- names(sentence_counts[sentence_counts > 1])
  
  # 3. Extract numerical information
  # Pattern for numbers in Nepali text
  number_pattern <- "\\d+\\.?\\d*"
  numbers <- unlist(lapply(all_texts, function(text) {
    matches <- str_extract_all(text, number_pattern)
    return(unlist(matches))
  }))
  
  # Return all extracted elements
  elements <- list(
    entities = potential_entities,
    key_phrases = key_phrases,
    numerical_info = numbers
  )
  
  return(elements)
}

#' Analyze completeness of story coverage across sources
#'
#' @param story_cluster A story cluster object
#' @param elements Extracted story elements
#' @return Completeness analysis results
#' @export
analyze_story_completeness <- function(story_cluster, elements) {
  # Get sources for each article
  sources <- story_cluster$articles$source
  
  if(is.null(sources)) {
    # Simulate sources if not present
    categories <- story_cluster$articles$category
    sources <- paste0("Source_", categories)
    story_cluster$articles$source <- sources
  }
  
  # Get unique sources
  unique_sources <- unique(sources)
  
  # Create presence matrix for elements across sources
  entity_matrix <- matrix(0, nrow = length(unique_sources), 
                          ncol = length(elements$entities))
  rownames(entity_matrix) <- unique_sources
  colnames(entity_matrix) <- elements$entities
  
  # Fill matrix based on presence of entities in each source's articles
  for(i in 1:length(unique_sources)) {
    source_articles <- story_cluster$articles[sources == unique_sources[i], ]
    source_text <- paste(source_articles$text, collapse = " ")
    
    for(j in 1:length(elements$entities)) {
      if(grepl(elements$entities[j], source_text, fixed = TRUE)) {
        entity_matrix[i, j] <- 1
      }
    }
  }
  
  # Calculate coverage metrics
  source_coverage <- rowSums(entity_matrix) / ncol(entity_matrix) * 100
  element_coverage <- colSums(entity_matrix) / nrow(entity_matrix) * 100
  
  # Identify elements missing from some sources
  missing_elements <- list()
  for(i in 1:length(unique_sources)) {
    missing <- elements$entities[entity_matrix[i, ] == 0]
    if(length(missing) > 0) {
      missing_elements[[unique_sources[i]]] <- missing
    }
  }
  
  # Calculate completeness score
  completeness_score <- sum(entity_matrix) / (nrow(entity_matrix) * ncol(entity_matrix)) * 100
  
  # Identify most and least comprehensive sources
  source_ranking <- sort(source_coverage, decreasing = TRUE)
  
  # Return results
  results <- list(
    entity_matrix = entity_matrix,
    source_coverage = source_coverage,
    element_coverage = element_coverage,
    missing_elements = missing_elements,
    completeness_score = completeness_score,
    source_ranking = source_ranking
  )
  
  return(results)
}

#' Visualize story completeness
#'
#' @param completeness_analysis Results from analyze_story_completeness
#' @param story_cluster A story cluster object
#' @param top_n Top N elements to display
#' @return ggplot object
#' @export
visualize_story_completeness <- function(completeness_analysis, story_cluster, top_n = 10) {
  # Create a data frame for the heatmap
  entity_matrix <- completeness_analysis$entity_matrix
  
  # Select top entities by coverage variation
  coverage_variation <- apply(entity_matrix, 2, var)
  top_entities <- names(sort(coverage_variation, decreasing = TRUE))[1:min(top_n, length(coverage_variation))]
  
  # Prepare data for plotting
  plot_data <- reshape2::melt(entity_matrix[, top_entities, drop = FALSE])
  names(plot_data) <- c("Source", "Element", "Present")
  
  # Create the heatmap
  p <- ggplot(plot_data, aes(x = Element, y = Source, fill = factor(Present))) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("0" = "white", "1" = "steelblue"), 
                      labels = c("0" = "Missing", "1" = "Present"),
                      name = "Coverage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Coverage Analysis for Story Cluster", story_cluster$id),
         subtitle = paste(story_cluster$size, "articles from", 
                          length(unique(story_cluster$articles$source)), "sources"))
  
  return(p)
}

#' Run complete story completeness analysis workflow
#'
#' @param news_data News article data frame
#' @param similarity_threshold Similarity threshold for article matching
#' @param min_cluster_size Minimum number of articles to form a story cluster
#' @return Complete analysis results
#' @export
analyze_news_completeness <- function(news_data, similarity_threshold = 0.3, min_cluster_size = 3) {
  # Step 1: Identify similar articles
  cat("Finding similar articles...\n")
  similar_pairs <- identify_similar_articles(news_data, similarity_threshold)
  
  # Step 2: Form story clusters
  cat("Forming story clusters...\n")
  story_clusters <- form_story_clusters(similar_pairs, news_data)
  
  cat("Found", length(story_clusters), "story clusters with", 
      min_cluster_size, "or more articles\n")
  
  # Step 3: Analyze each story cluster
  cat("Analyzing story completeness...\n")
  analysis_results <- list()
  
  for(i in 1:length(story_clusters)) {
    # Extract elements from the story
    elements <- extract_story_elements(story_clusters[[i]])
    
    # Analyze completeness
    completeness <- analyze_story_completeness(story_clusters[[i]], elements)
    
    # Store results
    analysis_results[[i]] <- list(
      story_cluster = story_clusters[[i]],
      elements = elements,
      completeness = completeness
    )
  }
  
  # Step 4: Return results
  return(list(
    story_clusters = story_clusters,
    analysis_results = analysis_results
  ))
}

# Example usage of the complete workflow:
# results <- analyze_news_completeness(news_data)