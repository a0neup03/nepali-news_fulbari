# Functions for comparing news articles across sources
# This implements the final goal of identifying related stories and analyzing coverage

# Function to identify similar news articles
identify_similar_articles <- function(news_data, similarity_threshold = 0.3) {
  # Apply preprocessing with stemming
  if (!"processed_text" %in% colnames(news_data)) {
    news_data$processed_text <- sapply(news_data$text, preprocess_nepali_with_stemming)
  }
  
  # Create TF-IDF matrix for document representation
  dtm <- DocumentTermMatrix(
    Corpus(VectorSource(news_data$processed_text)), 
    control = list(
      wordLengths = c(2, Inf),
      bounds = list(global = c(3, Inf)),
      weighting = weightTf
    )
  )
  
  tfidf <- weightTfIdf(dtm)
  
  # Convert to matrix for calculations
  tfidf_matrix <- as.matrix(tfidf)
  
  # Normalize vectors for cosine similarity
  norms <- sqrt(rowSums(tfidf_matrix^2))
  norms[norms == 0] <- 1  # Avoid division by zero
  normalized_matrix <- tfidf_matrix / norms
  
  # Calculate pairwise similarities (in batches to avoid memory issues)
  n_docs <- nrow(normalized_matrix)
  batch_size <- 1000  # Adjust based on available memory
  
  similar_pairs <- list()
  
  for (i in 1:ceiling(n_docs/batch_size)) {
    start_i <- (i-1)*batch_size + 1
    end_i <- min(i*batch_size, n_docs)
    
    for (j in i:ceiling(n_docs/batch_size)) {
      start_j <- (j-1)*batch_size + 1
      end_j <- min(j*batch_size, n_docs)
      
      # Only compute upper triangle if same batch
      if (i == j) {
        sim_batch <- tcrossprod(normalized_matrix[start_i:end_i, , drop = FALSE])
        # Zero out lower triangle including diagonal
        sim_batch[lower.tri(sim_batch, diag = TRUE)] <- 0
      } else {
        # Compute cross-batch similarities
        sim_batch <- tcrossprod(
          normalized_matrix[start_i:end_i, , drop = FALSE],
          normalized_matrix[start_j:end_j, , drop = FALSE]
        )
      }
      
      # Find similar pairs
      high_sim <- which(sim_batch >= similarity_threshold, arr.ind = TRUE)
      
      if (nrow(high_sim) > 0) {
        # Adjust indices to global positions
        high_sim[, 1] <- high_sim[, 1] + start_i - 1
        if (i == j) {
          high_sim[, 2] <- high_sim[, 2] + start_i - 1
        } else {
          high_sim[, 2] <- high_sim[, 2] + start_j - 1
        }
        
        # Store with similarity values
        for (k in 1:nrow(high_sim)) {
          doc1 <- high_sim[k, 1]
          doc2 <- high_sim[k, 2]
          sim_value <- sim_batch[high_sim[k, 1] - start_i + 1, high_sim[k, 2] - (if(i == j) start_i else start_j) + 1]
          
          similar_pairs[[length(similar_pairs) + 1]] <- list(
            doc1 = doc1,
            doc2 = doc2,
            similarity = sim_value
          )
        }
      }
    }
  }
  
  return(similar_pairs)
}

# Function to form story clusters from similar pairs
form_story_clusters <- function(similar_pairs, news_data) {
  if (!require("igraph")) {
    install.packages("igraph")
    library(igraph)
  }
  
  # Create graph from similar pairs
  edges <- data.frame(
    doc1 = sapply(similar_pairs, function(pair) pair$doc1),
    doc2 = sapply(similar_pairs, function(pair) pair$doc2),
    similarity = sapply(similar_pairs, function(pair) pair$similarity)
  )
  
  if (nrow(edges) == 0) {
    return(list())
  }
  
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  # Find connected components (story clusters)
  components <- components(g)
  
  # Create story clusters
  story_clusters <- list()
  
  for (i in 1:components$no) {
    member_indices <- which(components$membership == i)
    
    if (length(member_indices) > 1) {  # Only include multi-document stories
      # Get document metadata
      cluster_docs <- news_data[member_indices, ]
      
      # Extract key entities (if available)
      if ("source" %in% colnames(news_data)) {
        sources <- table(cluster_docs$source)
      } else {
        sources <- table(cluster_docs$category)
      }
      
      # Add to story clusters
      story_clusters[[length(story_clusters) + 1]] <- list(
        documents = cluster_docs,
        sources = sources,
        size = nrow(cluster_docs)
      )
    }
  }
  
  # Sort by cluster size
  sizes <- sapply(story_clusters, function(cluster) cluster$size)
  story_clusters <- story_clusters[order(sizes, decreasing = TRUE)]
  
  return(story_clusters)
}

# Function to summarize a story cluster
summarize_story_cluster <- function(story_cluster) {
  # Extract key information
  num_docs <- nrow(story_cluster$documents)
  
  # Get distribution by source or category
  if ("source" %in% colnames(story_cluster$documents)) {
    sources <- as.data.frame(table(story_cluster$documents$source))
    names(sources) <- c("Source", "Count")
    distribution_type <- "Sources"
  } else {
    sources <- as.data.frame(table(story_cluster$documents$category))
    names(sources) <- c("Category", "Count")
    distribution_type <- "Categories"
  }
  
  # Extract common text (simple approach)
  processed_texts <- story_cluster$documents$processed_text
  if (is.null(processed_texts)) {
    processed_texts <- sapply(story_cluster$documents$text, preprocess_nepali_with_stemming)
  }
  
  # Split into words and find common words
  word_lists <- strsplit(processed_texts, "\\s+")
  all_words <- unlist(word_lists)
  word_freq <- table(all_words)
  top_words <- names(sort(word_freq, decreasing = TRUE)[1:min(10, length(word_freq))])
  
  # Get publication dates if available
  if ("date" %in% colnames(story_cluster$documents)) {
    dates <- range(story_cluster$documents$date)
    date_info <- paste("Published between", dates[1], "and", dates[2])
  } else {
    date_info <- "Date information not available"
  }
  
  # Assemble summary
  summary <- list(
    num_documents = num_docs,
    distribution = sources,
    distribution_type = distribution_type,
    top_words = top_words,
    date_info = date_info,
    sample_headline = substr(story_cluster$documents$text[1], 1, 100)
  )
  
  return(summary)
}

# Function to identify blindspots based on your formula
identify_news_blindspots <- function(story_clusters, left_sources, right_sources, threshold = 33) {
  blindspots <- list()
  
  for (i in 1:length(story_clusters)) {
    cluster <- story_clusters[[i]]
    
    # Skip if no source information
    if (!"source" %in% colnames(cluster$documents)) {
      next
    }
    
    # Count coverage by political leaning
    sources <- table(cluster$documents$source)
    source_names <- names(sources)
    
    left_coverage <- sum(sources[source_names %in% left_sources])
    right_coverage <- sum(sources[source_names %in% right_sources])
    
    total_coverage <- left_coverage + right_coverage
    
    # Skip if no coverage from either side
    if (total_coverage == 0) {
      next
    }
    
    # Calculate percentages
    left_percent <- 100 * left_coverage / total_coverage
    right_percent <- 100 * right_coverage / total_coverage
    
    # Check if this is a left blindspot
    is_left_blindspot <- FALSE
    if (left_coverage < 10 && right_percent >= threshold) {
      threshold_calc <- (right_percent - threshold) * (30/37)
      if (left_percent <= threshold_calc) {
        is_left_blindspot <- TRUE
      }
    }
    
    # Check if this is a right blindspot
    is_right_blindspot <- FALSE
    if (right_coverage < 10 && left_percent >= threshold) {
      threshold_calc <- (left_percent - threshold) * (30/37)
      if (right_percent <= threshold_calc) {
        is_right_blindspot <- TRUE
      }
    }
    
    # Add to blindspots if applicable
    if (is_left_blindspot || is_right_blindspot) {
      # Get summary of this story
      summary <- summarize_story_cluster(cluster)
      
      blindspots[[length(blindspots) + 1]] <- list(
        summary = summary,
        left_coverage = left_coverage,
        right_coverage = right_coverage,
        left_percent = left_percent,
        right_percent = right_percent,
        is_left_blindspot = is_left_blindspot,
        is_right_blindspot = is_right_blindspot,
        story_cluster = cluster
      )
    }
  }
  
  return(blindspots)
}

# Function to analyze coverage across sources (simulated for the Nepali dataset)
analyze_coverage_across_sources <- function(news_data, story_threshold = 0.3) {
  # If source information doesn't exist, we'll simulate it for demonstration
  if (!"source" %in% colnames(news_data)) {
    # Assign each category to a simulated source
    source_mapping <- list(
      "Politics" = c("KantipurDaily", "HimalayaTimes"),
      "Sports" = c("GoalNepal", "AnnapurnaPost", "KantipurDaily"),
      "Economy" = c("NepalEconomy", "BusinessDaily", "HimalayaTimes"),
      "Entertainment" = c("CelebNepal", "AnnapurnaPost"),
      "Health" = c("SwasthyaNews", "HimalayaTimes"),
      "Technology" = c("TechNepal", "BusinessDaily"),
      "Education" = c("ShikshaNews", "KantipurDaily"),
      "Society" = c("SamajPost", "AnnapurnaPost", "NayaPatrika"),
      "Tourism" = c("TravelNepal", "HimalayaTimes", "NayaPatrika"),
      "Bank" = c("NepalEconomy", "BusinessDaily"),
      "Business" = c("BusinessDaily", "NepalEconomy", "KantipurDaily"),
      "Literature" = c("SahityaPost", "AnnapurnaPost"),
      "Agriculture" = c("KrishiNews", "NayaPatrika"),
      "Automobiles" = c("AutoNepal", "BusinessDaily"),
      "World" = c("GlobalNepal", "KantipurDaily", "HimalayaTimes"),
      "Blog" = c("BloggerNepal", "OnlineKhabar"),
      "Migration" = c("PrabasNews", "NayaPatrika"),
      "Interview" = c("AnnapurnaPost", "KantipurDaily"),
      "Opinion" = c("OnlineKhabar", "HimalayaTimes", "KantipurDaily"),
      "Employment" = c("JobPortal", "BusinessDaily")
    )
    
    # Add simulated source column