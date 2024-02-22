# Load necessary libraries for data manipulation, visualization, and reading
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

# Read the data from a CSV file
data <- read_csv("language_collaboration_results.csv")

# Define a custom theme for ggplot2 charts for consistent appearance
adjusted_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
  axis.text.y = element_text(size = 6),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6)
)

# Convert the 'CollaborationScore' column to a percentage for easier interpretation
data <- data %>%
  mutate(Collaboration_Percentage = as.numeric(CollaborationScore) * 100)

# Sort the data by the first language
data_sorted <- data %>% arrange(Language1)
# Extract unique languages for plotting
unique_languages <- unique(data_sorted$Language1)
# Initialize a list to store individual graphs
graphs <- list()

# Define thresholds and corresponding colors for visualization
seuils <- c(25.7, 56.9, 74)
colors <- c("#d62728", "#ff7f0e", "#2ca02c")

# Loop through each unique language to create a bar plot
for (language in unique_languages) {
  # Filter data for the current language
  subset_data <- filter(data_sorted, Language1 == language)
  # Create the bar plot
  graph <- ggplot(subset_data, aes(x = Language2, y = Collaboration_Percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    adjusted_theme +
    labs(title = paste("Collaboration for", language), x = "Language 2", y = "Collaboration (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25))
  
  # Add horizontal lines at defined thresholds
  for(i in 1:length(seuils)) {
    graph <- graph + geom_hline(yintercept = seuils[i], linetype = "dashed", color = colors[i])
  }
  # Store the graph in the list using the language as the key
  graphs[[language]] <- graph
}

# Output the graphs to a PDF file
pdf("AllLanguageCollaborationReport.pdf")
marrangeGrob(grobs = lapply(graphs, ggplotGrob), ncol = 2, nrow = 2)
dev.off()