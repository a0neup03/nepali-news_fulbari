# Nepali News Clustering with Stemming
# This script implements stemming for better clustering of Nepali news

# Source the necessary functions
source("nepali-news-memory-efficient.R")
source("nepali-stemming.R")

preprocess_nepali_with_stemming <- function(text) {
  # Load necessary libraries
  if (!require("stringr")) {
    install.packages("stringr")
    library(stringr)
  }
  
  # Step 1: Enhanced cleaning - removing special characters and punctuation
  cleaned <- str_replace_all(text, "[\\x00-\\x1F\\x7F-\\x9F]", "")  # Control characters
  cleaned <- str_replace_all(cleaned, "[।॥,;:!\\?\\-\\+]", " ")     # Punctuation
  cleaned <- str_replace_all(cleaned, "\\s+", " ")                  # Normalize whitespace
  cleaned <- str_trim(cleaned)
  
  # Step 2: Remove extended list of stopwords
  nepali_stopwords <- c(
    # Basic conjunctions and particles
    "र", "पनि", "वा", "तथा", "अनि", "तर", "तरपनि", "किनकि", "किनभने", "यदि", "भने", 
    
    # Forms of "to be"
    "हो", "होइन", "थियो", "छ", "छन्", "छु", "छौं", "छौ", "थिए", "थिएन", "छैन", "हुन्", "हुने", "भएको", 
    
    # Pronouns
    "म", "मेरो", "हामी", "हाम्रो", "तिमी", "तिम्रो", "तपाईं", "तपाईंको", "उ", "उनी", "उनको", "उहाँ", "उहाँको", 
    "यो", "यी", "त्यो", "ती", "यस", "त्यस", "यसले", "त्यसले", "के", "को", "कसले", "कसरी", "कुन", "कुनै",
    
    # Prepositions and postpositions
    "मा", "लाई", "बाट", "देखि", "सम्म", "मध्ये", "भित्र", "बाहिर", "अगाडि", "पछाडि", "माथि", "तल",
    
    # Articles and determiners
    "एक", "दुई", "तीन", "केही", "सबै", "धेरै", "थोरै", "अरु", "अन्य",
    
    # Common verbs
    "गर्नु", "गर्ने", "गर्छ", "गर्छन्", "गरेको", "गरिएको", "गर्दै", "गर्नुहोस्", "गर्नुपर्छ", "गर्न", 
    "जानु", "जाने", "जान्छ", "जान्छन्", "गएको", "गईरहेको", "जाँदै", "जान",
    "भन", "भने", "लागि", "साथ", "बीच",
    
    # Additional stopwords from your results
    "गरे", "रहे", "भन्", "नै", "त", "छ।", "।", "मात्र", "बारे", "तर्फ",
    "रुप", "अघि", "पछि", "बीच", "अनुसार", "भए", "राखे", "दिए", "लिए",
    "सके", "हुँदै", "गर्दै", "रहेको", "भएको", "बताए", "जानकारी", "अवस्था",
    "प्रति", "तथा", "भएका", "गरेका", "रहेका", "लिएका", "दिएका", "गर्दा", 
    "भन्दा", "अब", "यता", "उता", "यसरी", "त्यसरी", "सम्म", "जस्तै",
    "आफ्नो", "आफु", "आफै", "उसको", "उनको", "उनी", "जसले", "जसलाई"
  )
  
  words <- unlist(strsplit(cleaned, "\\s+"))
  filtered_words <- words[!words %in% nepali_stopwords]
  
  # Step 3: Apply stemming
  if (length(filtered_words) > 0) {
    stemmed_words <- stem_nepali_words(filtered_words)
    
    # Additional filtering - remove very short words and numbers
    stemmed_words <- stemmed_words[nchar(stemmed_words) > 2 & !grepl("^[0-9]+$", stemmed_words)]
    
    stemmed_text <- paste(stemmed_words, collapse = " ")
    return(stemmed_text)
  } else {
    # If no words left after filtering, return cleaned text
    return(cleaned)
  }
}



# Process news data with stemming
process_news_data_with_stemming <- function(news_data, k = 20) {
  # 1. Apply preprocessing with stemming
  cat("Applying preprocessing with stemming...\n")
  news_data$processed_text <- sapply(news_data$text, preprocess_nepali_with_stemming)
  
  # Check for empty documents
  empty_docs <- sum(nchar(news_data$processed_text) == 0)
  cat("Number of empty documents after preprocessing:", empty_docs, "\n")
  
  # 2. Create DTM
  cat("Creating DTM...\n")
  dtm <- DocumentTermMatrix(
    Corpus(VectorSource(news_data$processed_text)), 
    control = list(
      wordLengths = c(2, Inf),
      bounds = list(global = c(3, Inf)),
      weighting = weightTf
    )
  )
  cat("DTM dimensions:", dim(dtm), "\n")
  
  # 3. Limit features
  max_features <- 8000
  if (ncol(dtm) > max_features) {
    term_freq <- colSums(as.matrix(dtm))
    sorted_terms <- sort(term_freq, decreasing = TRUE)
    terms_to_keep <- names(sorted_terms)[1:max_features]
    dtm <- dtm[, terms_to_keep]
    cat("Limited DTM to", max_features, "features\n")
  }
  
  # 4. Create TF-IDF
  cat("Creating TF-IDF...\n")
  tfidf <- weightTfIdf(dtm)
  
  # 5. Reduce dimensions
  cat("Reducing dimensions...\n")
  reduced_data <- reduce_dimensions(tfidf, n_components = 100)
  
  # 6. Clustering
  cat("Performing clustering with k =", k, "...\n")
  clusters <- perform_kmeans_on_reduced(reduced_data$reduced_matrix, k)
  
  # 7. Map clusters back to full dataset
  cat("Mapping clusters to full dataset...\n")
  full_clusters <- map_clusters_to_full_dataset(
    clusters$cluster,
    reduced_data$non_empty_docs,
    nrow(news_data)
  )
  
  # 8. Add cluster assignments to the data
  news_data$cluster <- full_clusters
  
  # 9. Return results
  return(list(
    news_data = news_data,
    dtm = dtm,
    tfidf = tfidf,
    reduced_data = reduced_data,
    clusters = clusters,
    full_clusters = full_clusters
  ))
}

# Analyze clusters with stems
analyze_clusters_with_stems <- function(results, n_words = 15, n_stems = 10) {
  # Extract components
  news_data <- results$news_data
  dtm <- results$dtm
  clusters <- results$full_clusters
  
  # 1. Find top words per cluster
  cat("Finding top words per cluster...\n")
  top_words <- get_top_words_efficient(dtm, clusters, n = n_words)
  
  # 2. Analyze word stems in each cluster
  cat("Analyzing word stems in clusters...\n")
  stem_analysis <- analyze_word_stems(top_words, n = n_stems)
  
  # Print top word stems for each cluster
  for (i in 1:length(stem_analysis)) {
    cat("\nCluster", i, "top word stems and variations:\n")
    cluster_stems <- stem_analysis[[i]]
    
    for (j in 1:length(cluster_stems)) {
      stem_name <- names(cluster_stems)[j]
      variations <- cluster_stems[[j]]
      cat("  Stem '", stem_name, "' words: ", paste(variations, collapse = ", "), "\n", sep = "")
    }
  }
  
  # 3. Compare clusters with original categories
  cat("\nComparing clusters with original categories...\n")
  comparison <- table(news_data$category, news_data$cluster)
  
  # Calculate accuracy
  cluster_to_category <- apply(comparison, 2, which.max)
  predicted_category <- sapply(news_data$cluster, function(c) rownames(comparison)[cluster_to_category[c]])
  accuracy <- sum(predicted_category == news_data$category) / nrow(news_data)
  cat("\nClustering accuracy:", round(accuracy * 100, 2), "%\n")
  
  # 4. Print the mapping between clusters and categories
  cat("\nCluster to category mapping:\n")
  for (i in 1:length(cluster_to_category)) {
    cat("Cluster", i, "->", rownames(comparison)[cluster_to_category[i]], "\n")
  }
  
  # 5. Save results
  cat("\nSaving clustering results...\n")
  output_file <- "nepali_news_stemming_results.csv"
  results_df <- data.frame(
    id = news_data$id,
    category = news_data$category,
    cluster = news_data$cluster,
    predicted_category = predicted_category
  )
  write.csv(results_df, file = output_file, row.names = FALSE)
  
  # 6. Return analysis results
  return(list(
    top_words = top_words,
    stem_analysis = stem_analysis,
    confusion = comparison,
    accuracy = accuracy,
    predicted_category = predicted_category
  ))
}

# Example usage
# results <- process_news_data_with_stemming(news_data)
# stem_analysis <- analyze_clusters_with_stems(results)