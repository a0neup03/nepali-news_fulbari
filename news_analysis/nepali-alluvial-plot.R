# Create an alluvial plot to visualize category-cluster relationships
# Using ggalluvial instead of riverplot

create_alluvial_plot <- function(news_data, cluster_assignments, save_file = NULL) {
  # Load required packages
  if (!require("ggalluvial")) {
    install.packages("ggalluvial")
    library(ggalluvial)
  }
  if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Category = news_data$category,
    Cluster = paste0("Cluster ", cluster_assignments),
    Count = 1
  )
  
  # Aggregate the data
  agg_data <- aggregate(Count ~ Category + Cluster, data = plot_data, FUN = sum)
  
  # Determine frequencies for sorting (largest categories at the top)
  category_freqs <- aggregate(Count ~ Category, data = agg_data, FUN = sum)
  category_freqs <- category_freqs[order(-category_freqs$Count), ]
  
  cluster_freqs <- aggregate(Count ~ Cluster, data = agg_data, FUN = sum)
  cluster_freqs <- cluster_freqs[order(-cluster_freqs$Count), ]
  
  # Order factors for plotting
  agg_data$Category <- factor(agg_data$Category, levels = category_freqs$Category)
  agg_data$Cluster <- factor(agg_data$Cluster, levels = cluster_freqs$Cluster)
  
  # Create the plot
  p <- ggplot(agg_data,
              aes(y = Count, axis1 = Category, axis2 = Cluster)) +
    geom_alluvium(aes(fill = Category), width = 0.4, alpha = 0.8) +
    geom_stratum(width = 0.4, fill = "white", color = "grey") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_x_discrete(limits = c("Category", "Cluster"), expand = c(0.05, 0.05)) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(title = "Nepali News Flow from Categories to Clusters",
         subtitle = paste("Showing how documents flow from original categories to", 
                          length(unique(agg_data$Cluster)), "clusters"))
  
  # Display the plot
  print(p)
  
  # Save plot if requested
  if (!is.null(save_file)) {
    ggsave(save_file, p, width = 12, height = 10)
    cat("Plot saved to:", save_file, "\n")
  }
  
  return(p)
}

# Create a more detailed cluster analysis plot
create_cluster_composition_plot <- function(news_data, cluster_assignments, top_n = 3, save_file = NULL) {
  # Load required packages
  if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require("reshape2")) {
    install.packages("reshape2")
    library(reshape2)
  }
  
  # Create a contingency table
  cont_table <- table(news_data$category, cluster_assignments)
  
  # Convert to data frame for plotting
  cont_df <- as.data.frame(cont_table)
  names(cont_df) <- c("Category", "Cluster", "Count")
  
  # Calculate percentages within each cluster
  cont_df$Cluster <- paste0("Cluster ", cont_df$Cluster)
  total_by_cluster <- aggregate(Count ~ Cluster, data = cont_df, FUN = sum)
  cont_df <- merge(cont_df, total_by_cluster, by = "Cluster", suffixes = c("", "_total"))
  cont_df$Percentage <- 100 * cont_df$Count / cont_df$Count_total
  
  # Keep only top N categories per cluster for readability
  top_categories <- data.frame()
  clusters <- unique(cont_df$Cluster)
  
  for (cluster in clusters) {
    cluster_data <- cont_df[cont_df$Cluster == cluster, ]
    cluster_data <- cluster_data[order(-cluster_data$Count), ]
    top_cluster_categories <- head(cluster_data, top_n)
    
    # Add "Other" category if needed
    if (nrow(cluster_data) > top_n) {
      other_count <- sum(cluster_data$Count[(top_n+1):nrow(cluster_data)])
      other_percentage <- 100 * other_count / cluster_data$Count_total[1]
      other_row <- data.frame(
        Cluster = cluster,
        Category = "Other",
        Count = other_count,
        Count_total = cluster_data$Count_total[1],
        Percentage = other_percentage
      )
      top_cluster_categories <- rbind(top_cluster_categories, other_row)
    }
    
    top_categories <- rbind(top_categories, top_cluster_categories)
  }
  
  # Create the plot
  p <- ggplot(top_categories, aes(x = Cluster, y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = ifelse(Percentage >= 5, 
                                 paste0(round(Percentage), "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 3) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right") +
    labs(title = "Cluster Composition by Top Categories",
         subtitle = paste("Showing top", top_n, "categories per cluster"),
         y = "Percentage (%)",
         x = "")
  
  # Display the plot
  print(p)
  
  # Save plot if requested
  if (!is.null(save_file)) {
    ggsave(save_file, p, width = 14, height = 8)
    cat("Plot saved to:", save_file, "\n")
  }
  
  return(p)
}

# Function to visualize optimal number of clusters
visualize_optimal_clusters <- function(reduced_matrix, max_k = 20, save_file = NULL) {
  # Load required packages
  if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require("factoextra")) {
    install.packages("factoextra")
    library(factoextra)
  }
  
  # Calculate within-cluster sum of squares for different k values
  set.seed(42)
  wss <- numeric(max_k)
  silhouette <- numeric(max_k - 1)
  
  for (k in 1:max_k) {
    cat("Testing k =", k, "\n")
    km <- kmeans(reduced_matrix, centers = k, nstart = 10, iter.max = 30)
    wss[k] <- km$tot.withinss
    
    # Calculate silhouette for k >= 2
    if (k >= 2) {
      sil <- cluster::silhouette(km$cluster, dist(reduced_matrix))
      silhouette[k-1] <- mean(sil[, 3])
    }
  }
  
  # Create data frames for plotting
  wss_df <- data.frame(k = 1:max_k, wss = wss)
  sil_df <- data.frame(k = 2:max_k, silhouette = silhouette)
  
  # Create elbow plot
  p1 <- ggplot(wss_df, aes(x = k, y = wss)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Elbow Method for Optimal k",
         x = "Number of clusters",
         y = "Within-cluster sum of squares")
  
  # Create silhouette plot
  p2 <- ggplot(sil_df, aes(x = k, y = silhouette)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Silhouette Method for Optimal k",
         x = "Number of clusters",
         y = "Average silhouette width")
  
  # Display plots
  print(p1)
  print(p2)
  
  # Find optimal k
  optimal_k_elbow <- findElbowPoint(wss)
  optimal_k_silhouette <- which.max(silhouette) + 1
  
  cat("Optimal k from elbow method:", optimal_k_elbow, "\n")
  cat("Optimal k from silhouette method:", optimal_k_silhouette, "\n")
  
  # Save plots if requested
  if (!is.null(save_file)) {
    pdf(paste0(save_file, "_elbow.pdf"), width = 8, height = 6)
    print(p1 + geom_vline(xintercept = optimal_k_elbow, linetype = "dashed", color = "red"))
    dev.off()
    
    pdf(paste0(save_file, "_silhouette.pdf"), width = 8, height = 6)
    print(p2 + geom_vline(xintercept = optimal_k_silhouette, linetype = "dashed", color = "red"))
    dev.off()
    
    cat("Plots saved with prefix:", save_file, "\n")
  }
  
  # Return optimal k values
  return(list(
    elbow = optimal_k_elbow,
    silhouette = optimal_k_silhouette,
    wss = wss,
    sil = silhouette
  ))
}

# Helper function to find the elbow point
findElbowPoint <- function(wss) {
  # Normalize values to 0-1 range
  x <- 1:length(wss)
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (wss - min(wss)) / (max(wss) - min(wss))
  
  # Find point furthest from line connecting first and last points
  # Line equation: y = mx + b
  m <- (y_norm[length(y_norm)] - y_norm[1]) / (x_norm[length(x_norm)] - x_norm[1])
  b <- y_norm[1] - m * x_norm[1]
  
  # Calculate distance from each point to the line
  distances <- abs(y_norm - (m * x_norm + b)) / sqrt(1 + m^2)
  
  # The elbow is the point with maximum distance
  return(which.max(distances))
}