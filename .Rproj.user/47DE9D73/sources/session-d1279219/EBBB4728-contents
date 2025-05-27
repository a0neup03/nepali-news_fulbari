# Improved Nepali Text Preprocessing Functions

#' Clean Nepali text by removing special characters and symbols
#'
#' @param text A character vector containing Nepali text
#' @return A cleaned character vector
#' @export
clean_nepali_text <- function(text) {
  # Load necessary libraries
  if (!require("stringr")) {
    install.packages("stringr")
    library(stringr)
  }
  
  # Function to remove various kinds of unwanted characters
  remove_special_chars <- function(text) {
    # Remove common control characters and unparsed unicode
    text <- str_replace_all(text, "[\\x00-\\x1F\\x7F-\\x9F]", "")
    
    # Remove non-Nepali scripts (Latin, Arabic, etc.)
    text <- str_replace_all(text, "[a-zA-Z\\u00C0-\\u024F\\u1E00-\\u1EFF]", "")
    
    # Remove various symbols, emojis, and other special characters
    text <- str_replace_all(text, "[~¡£¤¥¦§¨©ª«¬®¯±²³´µ¶·¸¹º»¼½¾¿]", "")
    text <- str_replace_all(text, "[ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ]", "")
    text <- str_replace_all(text, "[\\U0001F300-\\U0001F5FF\\U0001F600-\\U0001F64F\\U0001F680-\\U0001F6FF\\U0001F700-\\U0001F77F]", "")
    
    # Remove special punctuation that's not commonly used in Nepali
    text <- str_replace_all(text, "[\\(\\)\\[\\]\\{\\}\\<\\>\\|\\\\]", "")
    
    # Normalize whitespace
    text <- str_replace_all(text, "\\s+", " ")
    text <- str_trim(text)
    
    return(text)
  }
  
  # Clean the text
  cleaned_text <- remove_special_chars(text)
  
  return(cleaned_text)
}

#' Remove stopwords from Nepali text
#'
#' @param text A character vector containing Nepali text
#' @return Text with stopwords removed
#' @export
remove_nepali_stopwords <- function(text) {
  # Define common Nepali stopwords
  nepali_stopwords <- c(
    # Common Nepali stopwords
    "र", "छ", "छन्", "हो", "पनि", "भएको", "गरेको", "लागि", "भने", "गर्न", 
    "हुन्", "मा", "को", "का", "की", "ले", "छन", "छु", "थियो", "गरी", 
    "तथा", "हुने", "गर्ने", "भन्ने", "जस्तो", "नै", "एक", "यो", "यस", 
    "ती", "तर", "रहेको", "सँग", "बाट", "तीन", "यी", "केही", "उनी", "उनको", 
    "त्यो", "दिए", "जुन", "यसको", "भई", "अहिले", "छैन", "अन्य", "दुई", 
    "अब", "यस्तो", "त्यस्तो", "प्रति", "थिए", "सबै", "हुन", "अरु", "वा", 
    "भए", "साथै", "हुन्छ", "पछि", "त्यहाँ", "उनले", "आफ्नो", "हाल", "आज", 
    "यहाँ", "द्वारा", "अघि", "जसले", "समेत", "हामी", "तथापि", "जस्ता",
    "।", "छ।", "-", "&", "/", "$", "*", "@", "m", "g", "l", "h", "k", "a",
    "\b", ">", "\001", "\002", "\005", "\017", "\025", "\027", "\034", "\037", 
    "��", "��\025", "��\030", "��*", "��\005"
  )
  
  # Split text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Remove stopwords
  filtered_words <- words[!words %in% nepali_stopwords]
  
  # Recombine into text
  filtered_text <- paste(filtered_words, collapse = " ")
  
  return(filtered_text)
}

#' Advanced preprocessing for Nepali text
#'
#' @param text A character vector containing text to preprocess
#' @return A preprocessed character vector
#' @export
preprocess_nepali_text_advanced <- function(text) {
  # First apply basic cleaning
  cleaned_text <- clean_nepali_text(text)
  
  # Then remove stopwords
  filtered_text <- remove_nepali_stopwords(cleaned_text)
  
  # Final normalization of whitespace
  filtered_text <- str_replace_all(filtered_text, "\\s+", " ")
  filtered_text <- str_trim(filtered_text)
  
  return(filtered_text)
}

#' Create a more meaningful Document-Term Matrix for Nepali text
#'
#' @param text_data A vector of text
#' @param max_features Maximum number of features to include
#' @param min_word_length Minimum word length to include
#' @param min_doc_freq Minimum document frequency for terms
#' @return A Document-Term Matrix with meaningful terms
#' @export
create_meaningful_dtm <- function(text_data, max_features = 5000, 
                                  min_word_length = 3, min_doc_freq = 5) {
  # Create a corpus
  corpus <- Corpus(VectorSource(text_data))
  
  # Create DTM with more restrictive filtering
  dtm <- DocumentTermMatrix(corpus, 
                            control = list(
                              wordLengths = c(min_word_length, Inf),  # Only words with 3+ characters
                              bounds = list(global = c(min_doc_freq, Inf)),  # Must appear in at least 5 docs
                              weighting = weightTf,
                              removeNumbers = TRUE
                            ))
  
  # Find terms that appear in multiple documents
  term_freq <- colSums(as.matrix(dtm))
  sorted_terms <- sort(term_freq, decreasing = TRUE)
  
  # Keep only the most frequent terms
  if (length(sorted_terms) > max_features) {
    terms_to_keep <- names(sorted_terms)[1:max_features]
    dtm <- dtm[, terms_to_keep]
  }
  
  cat("Created meaningful DTM with dimensions:", dim(dtm), "\n")
  return(dtm)
}

#' Get named entities from Nepali text
#' 
#' @param text Nepali text
#' @param min_length Minimum length of potential entities
#' @return A vector of potential named entities
#' @export
extract_nepali_entities <- function(text, min_length = 3) {
  # This is a heuristic approach for Nepali text
  # Load required libraries
  if (!require("stringr")) {
    install.packages("stringr")
    library(stringr)
  }
  
  # Split text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Filter potential entities - using some heuristics for Nepali names
  # We'll assume proper nouns/names are often longer than common words
  potential_entities <- words[nchar(words) >= min_length]
  
  # Remove common words that aren't likely to be entities
  nepali_common_words <- c(
    "र", "छ", "छन्", "हो", "पनि", "भएको", "गरेको", "लागि", "भने", "गर्न", 
    "हुन्", "मा", "को", "का", "की", "ले", "छन", "छु", "थियो", "गरी", 
    "तथा", "हुने", "गर्ने", "भन्ने", "जस्तो", "नै", "एक", "यो", "यस" 
  )
  
  potential_entities <- potential_entities[!potential_entities %in% nepali_common_words]
  
  # Return unique potential entities
  return(unique(potential_entities))
}




# Add this to your preprocessing function to focus on more distinctive terms
improved_feature_selection <- function(dtm, min_freq = 5, max_freq_ratio = 0.5) {
  # Convert to matrix for calculations
  dtm_matrix <- as.matrix(dtm)
  
  # Calculate term frequency in documents
  term_freq <- colSums(dtm_matrix > 0)  # Number of documents containing term
  
  # Remove terms that appear too frequently (likely not distinctive)
  max_docs <- nrow(dtm_matrix) * max_freq_ratio
  distinctive_terms <- names(term_freq[term_freq >= min_freq & term_freq <= max_docs])
  
  # Filter DTM to keep only distinctive terms
  filtered_dtm <- dtm[, distinctive_terms]
  
  cat("Filtered DTM from", ncol(dtm), "to", ncol(filtered_dtm), "features\n")
  
  return(filtered_dtm)
}
