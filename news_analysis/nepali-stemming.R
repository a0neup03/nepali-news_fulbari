stem_nepali_words <- function(words) {
  # Load necessary libraries
  if (!require("stringr")) {
    install.packages("stringr")
    library(stringr)
  }
  
  # Common Nepali suffixes
  nepali_suffixes <- c(
    "ले", "को", "का", "की", "मा", "बाट", "लाई", "सँग", "द्वारा", 
    "हरु", "हरू", "सित", "देखि", "पछि", "अघि", "तिर", "तिरको", 
    "तिरका", "समेत", "लगायत", "।", "पूर्ण", "हरूले"
  )
  
  # Sort suffixes by length (longest first) to avoid partial matches
  nepali_suffixes <- nepali_suffixes[order(-nchar(nepali_suffixes))]
  
  # Function to remove suffixes from a word
  remove_suffixes <- function(word) {
    original_word <- word
    
    # Try removing each suffix
    for (suffix in nepali_suffixes) {
      if (nchar(word) > nchar(suffix) + 2) {  # Ensure word is long enough
        if (str_ends(word, suffix)) {
          word <- str_sub(word, 1, nchar(word) - nchar(suffix))
          break  # Remove only one suffix at a time
        }
      }
    }
    
    # Also remove postpositions that are separated by a dot
    word <- str_replace(word, "\\.$", "")
    
    # Handle special cases
    if (word == original_word) {
      # Try verb forms ending with common verb endings
      verb_endings <- c("छन्", "छ", "न्छ", "न्छन्", "ए", "एका", "एको", "ने", "नु", "र्दै", "दै")
      
      for (ending in verb_endings) {
        if (nchar(word) > nchar(ending) + 2) {
          if (str_ends(word, ending)) {
            word <- str_sub(word, 1, nchar(word) - nchar(ending))
            break
          }
        }
      }
    }
    
    return(word)
  }
  
  # Apply stemming to each word
  stemmed_words <- sapply(words, remove_suffixes)
  
  return(stemmed_words)
}




#' Apply stemming to Nepali text
#'
#' @param text Character vector of Nepali text
#' @return Character vector of text with stemmed words
#' @export
stem_nepali_text <- function(text) {
  # Split text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Apply stemming
  stemmed_words <- stem_nepali_words(words)
  
  # Recombine into text
  stemmed_text <- paste(stemmed_words, collapse = " ")
  
  return(stemmed_text)
}

#' Create a stemmed Document-Term Matrix for Nepali
#'
#' @param text_data A vector of Nepali text
#' @param max_features Maximum number of features
#' @return A Document-Term Matrix with stemmed terms
#' @export
create_stemmed_nepali_dtm <- function(text_data, max_features = 5000) {
  # First stem all the texts
  cat("Applying stemming to", length(text_data), "documents...\n")
  stemmed_texts <- sapply(text_data, stem_nepali_text)
  
  # Create corpus
  corpus <- Corpus(VectorSource(stemmed_texts))
  
  # Create DTM
  dtm <- DocumentTermMatrix(corpus, 
                            control = list(
                              wordLengths = c(2, Inf),
                              bounds = list(global = c(3, Inf)),
                              weighting = weightTf
                            ))
  
  # Limit features if needed
  if (ncol(dtm) > max_features) {
    # Get term frequencies
    term_freq <- colSums(as.matrix(dtm))
    sorted_terms <- sort(term_freq, decreasing = TRUE)
    terms_to_keep <- names(sorted_terms)[1:max_features]
    dtm <- dtm[, terms_to_keep]
  }
  
  cat("Created stemmed DTM with dimensions:", dim(dtm), "\n")
  return(dtm)
}

#' Group similar Nepali words
#'
#' @param words Character vector of Nepali words
#' @return A list of word groups
#' @export
group_similar_nepali_words <- function(words) {
  # First apply stemming
  stemmed <- stem_nepali_words(words)
  
  # Create mapping of stemmed to original
  word_groups <- list()
  
  for (i in 1:length(words)) {
    stem <- stemmed[i]
    word <- words[i]
    
    if (stem %in% names(word_groups)) {
      # Add to existing group
      word_groups[[stem]] <- c(word_groups[[stem]], word)
    } else {
      # Create new group
      word_groups[[stem]] <- word
    }
  }
  
  # Remove duplicates in each group
  word_groups <- lapply(word_groups, unique)
  
  return(word_groups)
}

#' Analyze frequent word stems in clusters
#'
#' @param top_words A list of top words per cluster
#' @param n Number of top stems to show
#' @return A list of top word stems per cluster
#' @export
analyze_word_stems <- function(top_words, n = 10) {
  # Process each cluster
  results <- list()
  
  for (i in 1:length(top_words)) {
    # Get words for this cluster
    cluster_words <- names(top_words[[i]])
    
    # Group similar words
    word_groups <- group_similar_nepali_words(cluster_words)
    
    # Sort groups by size
    group_sizes <- sapply(word_groups, length)
    sorted_indices <- order(group_sizes, decreasing = TRUE)
    
    # Get top n groups
    top_groups <- word_groups[sorted_indices[1:min(n, length(sorted_indices))]]
    
    # Add to results
    results[[paste0("cluster_", i)]] <- top_groups
  }
  
  return(results)
}
