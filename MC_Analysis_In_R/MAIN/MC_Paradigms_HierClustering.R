# Load the required libraries for data visualization, manipulation, and reading
library(dplyr)
library(readr)
library(ggplot2)
library(cluster)
library(xtable)

# Load the data from a CSV file into the 'data' dataframe
data <- read_csv("CollaborationMetric_Paradigms_Raw.csv")

# Convert 'TotalMC' from a raw score to a percentage for better visualization
data <- data %>%
  mutate(TotalMC_Percentage = as.numeric(TotalMC) * 100)

# Prepare data for hierarchical clustering
mc_percentage_matrix <- as.matrix(data$TotalMC_Percentage)

# Perform hierarchical clustering using the Ward's method
hclust_result <- hclust(dist(mc_percentage_matrix), method = "ward.D2")

# Cut the dendrogram to obtain a specific number of clusters
clusters <- cutree(hclust_result, h = 50, k = 5)

# Add cluster assignments back to the original data
data$Cluster <- as.factor(clusters)

# Calculate silhouette scores to evaluate clustering quality
silhouette_result <- silhouette(clusters, dist(mc_percentage_matrix))

# Convert silhouette results to a data frame for easier manipulation
silhouette_df <- as.data.frame(silhouette_result)
names(silhouette_df) <- c("Cluster", "Neighbor Cluster", "Silhouette Width")

# Calculate the average silhouette width for each cluster
avg_sil_width_per_cluster <- silhouette_df %>%
  group_by(Cluster) %>%
  summarise(Average_Silhouette_Width = mean(`Silhouette Width`))

# Create an xtable from the average silhouette widths by cluster
avg_silhouette_table <- xtable(avg_sil_width_per_cluster)

# First, save the scatter plot of clustering results to a PDF file
pdf("Clustering_Results.pdf", width = 11, height = 8.5)

# Visualize the data with cluster assignments and save the plot
ggplot(data, aes(x = TotalMC_Percentage, y = 1, color = Cluster)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Hierarchical clustering results of TotalMC_Percentage",
       x = "TotalMC percentage (%)",
       y = "") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white")) -> cluster_plot

print(cluster_plot)
dev.off()

# Second, save the table of average silhouette widths to another PDF file
pdf("Silhouette_Averages_Table.pdf", width = 11, height = 8.5)
grid.table(avg_silhouette_table)
dev.off()