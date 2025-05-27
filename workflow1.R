# Complete Workflow for Nepali News Analysis
# This script includes the full pipeline from clustering to story comparison

# 1. Load all required libraries and source files
required_packages <- c("tm", "cluster", "ggplot2", "ggalluvial", "igraph")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Source all our custom functions
source("nepali-news-clustering.R")
source("nepali-news-memory-efficient.R")
source("nepali-stemming.R")
source("nepali-news-stemming-example.R")
source("nepali-alluvial-plot.R")
source("nepali-optimal-clusters.R")
source("nepali-comparison-report.R")

# Function to run the complete analysis pipeline
run_nepali_news_analysis <- function(base_path = NULL, news_data = NULL, 
                                     find_optimal = TRUE, k = 20,
                                     similarity_threshold = 0.3) {
  # Step 1: Load data if not provided
  if (is.null(news_data) && !is.null(base_path)) {
    cat("Loading news data from", base_path, "...\n")
    news_data <- read_nepali_news(base_path)
    cat("Loaded", nrow(news_data), "news articles from", 
        length(unique(news_data$category)), "categories\n")
  } else if (is.null(news_data) && is.null(base_path)) {
    stop("Either news_data or base_path must be provided")
  }
  
  # Step 2: Find optimal clusters if requested
  if (find_optimal) {
    cat("\n--- Finding Optimal Number of Clusters ---\n")
    optimal_results <- find_optimal_clusters_and_visualize(news_data)
    
    # Choose the method with higher accuracy
    if (optimal_results$results$elbow$accuracy > optimal_results$results$silhouette$accuracy) {
      best_method <- "elbow"
    } else {
      best_method <- "silhouette"
    }
    
    k <- optimal_results$results[[best_method]]$k
    cat("\nUsing optimal k =", k, "from", best_method, "method\n")
    
    # Use the clusters from the best method
    cluster_results <- optimal_results$results[[best_method]]
    full_clusters <- cluster_results$full_clusters
    
    # Analyze clusters
    cat("\n--- Analyzing Optimal Clusters ---\n")
    stem_analysis <- analyze_clusters_with_stems(list(
      news_data = news_data,
      dtm = cluster_results$dtm,
      full_clusters = full_clusters
    ))
  } else {
    # Use the provided k value
    cat("\n--- Clustering with k =", k, "---\n")
    
    # Process data with stemming
    results <- process_news_data_with_stemming(news_data, k)
    
    # Analyze clusters
    cat("\n--- Analyzing Clusters ---\n")
    stem_analysis <- analyze_clusters_with_stems(results)
    
    full_clusters <- results$full_clusters
  }
  
  # Step 3: Analyze coverage across sources (simulated)
  cat("\n--- Analyzing Coverage Across Sources ---\n")
  coverage_results <- analyze_coverage_across_sources(
    news_data, 
    story_threshold = similarity_threshold
  )
  
  # Step 4: Create an integrated report
  cat("\n--- Creating Integrated Report ---\n")
  report <- list(
    data_summary = list(
      num_articles = nrow(news_data),
      num_categories = length(unique(news_data$category)),
      category_distribution = table(news_data$category)
    ),
    clustering = list(
      k = k,
      cluster_sizes = table(full_clusters),
      accuracy = if (find_optimal) cluster_results$accuracy else results$accuracy,
      top_words = stem_analysis
    ),
    story_analysis = list(
      num_story_clusters = length(coverage_results$story_clusters),
      num_blindspots = length(coverage_results$blindspots),
      largest_story = coverage_results$cluster_summaries[[1]]
    )
  )
  
  # Print summary report
  cat("\n====== NEPALI NEWS ANALYSIS SUMMARY ======\n")
  cat("Total articles analyzed:", report$data_summary$num_articles, "\n")
  cat("Number of categories:", report$data_summary$num_categories, "\n")
  cat("Number of clusters:", k, "\n")
  cat("Clustering accuracy:", round(report$clustering$accuracy * 100, 2), "%\n")
  cat("Number of multi-article stories identified:", report$story_analysis$num_story_clusters, "\n")
  cat("Number of potential blindspots:", report$story_analysis$num_blindspots, "\n")
  
  # Return full results
  return(list(
    news_data = news_data,
    cluster_analysis = if (find_optimal) optimal_results else results,
    stem_analysis = stem_analysis,
    coverage_analysis = coverage_results,
    report = report
  ))
}

# Example usage:
# 1. Load and analyze data from scratch:
# base_path <- "./../archive/nepali_news_dataset_20_categories_large/nepali_news_dataset_20_categories_large"
# analysis_results <- run_nepali_news_analysis(base_path = base_path)

# 2. Use existing news_data:
# analysis_results <- run_nepali_news_analysis(news_data = news_data)

# 3. Skip optimal cluster finding and use a specific k:
# analysis_results <- run_nepali_news_analysis(news_data = news_data, find_optimal = FALSE, k = 15)

# Function to visualize results in a dashboard-like format
create_visualization_dashboard <- function(analysis_results, save_prefix = "nepali_dashboard") {
  # Create visualizations
  
  # 1. Category distribution
  cat("Creating category distribution plot...\n")
  cat_data <- as.data.frame(table(analysis_results$news_data$category))
  names(cat_data) <- c("Category", "Count")
  cat_data <- cat_data[order(-cat_data$Count), ]
  
  cat_plot <- ggplot(cat_data, aes(x = reorder(Category, -Count), y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution of News Articles by Category",
         x = "", y = "Number of Articles")
  
  # 2. Cluster composition
  if (!is.null(analysis_results$cluster_analysis$results)) {
    # For optimal clustering results
    best_method <- ifelse(
      analysis_results$cluster_analysis$results$elbow$accuracy > 
        analysis_results$cluster_analysis$results$silhouette$accuracy,
      "elbow", "silhouette"
    )
    full_clusters <- analysis_results$cluster_analysis$results[[best_method]]$full_clusters
  } else {
    # For direct clustering results
    full_clusters <- analysis_results$cluster_analysis$full_clusters
  }
  
  # 3. Story clusters visualization
  cat("Creating story clusters visualization...\n")
  
  # Get top 5 story clusters by size
  top_clusters <- head(
    analysis_results$coverage_analysis$story_clusters[
      order(sapply(analysis_results$coverage_analysis$story_clusters, function(x) x$size), 
            decreasing = TRUE)
    ], 5)
  
  # Create a data frame for source distribution in top stories
  source_dist <- data.frame()
  
  for (i in 1:length(top_clusters)) {
    cluster <- top_clusters[[i]]
    sources <- as.data.frame(table(cluster$documents$source))
    names(sources) <- c("Source", "Count")
    sources$StoryID <- paste0("Story ", i)
    source_dist <- rbind(source_dist, sources)
  }
  
  # Create source distribution plot
  source_plot <- ggplot(source_dist, aes(x = StoryID, y = Count, fill = Source)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(title = "Source Distribution in Top 5 Story Clusters",
         x = "", y = "Number of Articles")
  
  # 4. Save all plots
  if (!is.null(save_prefix)) {
    ggsave(paste0(save_prefix, "_categories.pdf"), cat_plot, width = 10, height = 6)
    ggsave(paste0(save_prefix, "_sources.pdf"), source_plot, width = 10, height = 6)
  }
  
  # 5. Return visualizations
  return(list(
    category_plot = cat_plot,
    source_plot = source_plot
  ))
}

# Example usage:
# visualizations <- create_visualization_dashboard(analysis_results)