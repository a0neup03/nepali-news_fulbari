






source("nepali-news-clustering.R")
source("nepali-news-memory-efficient.R")
source("nepali-stemming.R")
source("nepali-news-stemming-example.R")
source("nepali-story-completeness.R")

# Load your data
base_path <- "./../archive/nepali_news_dataset_20_categories_large/nepali_news_dataset_20_categories_large"
news_data <- read_nepali_news(base_path)

# Run the completeness analysis
results <- analyze_news_completeness(news_data)

# Visualize completeness for the first story cluster
p <- visualize_story_completeness(
  results$analysis_results[[1]]$completeness, 
  results$analysis_results[[1]]$story_cluster
)
print(p)