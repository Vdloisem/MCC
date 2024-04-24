# Load the required libraries for data visualization, manipulation, and reading
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

# Read the data from a CSV file
data <- read_csv("CollaborationMetric_Languages_Raw.csv")

# Convert 'CollaborationScore' to a percentage
data <- data %>%
  mutate(Collaboration_Percentage = as.numeric(CollaborationScore) * 100)

# Define a custom theme
adjusted_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
  axis.text.y = element_text(size = 6),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6)
)

seuils <- c(25.53, 59.07, 73.82)
colors <- c("#d62728", "#ff7f0e", "#2ca02c")
tolerance <- 5

data_sorted <- data %>% arrange(Language1)
unique_languages <- unique(data_sorted$Language1)
graphs <- list()

for (language in unique_languages) {
  subset_data <- filter(data_sorted, Language1 == language)
  graph <- ggplot(subset_data, aes(x = Language2, y = Collaboration_Percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() + adjusted_theme +
    labs(title = paste("Collaboration for", language), x = "Language 2", y = "Collaboration (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25))
  
  # Add horizontal lines for each threshold
  for(i in 1:length(seuils)) {
    graph <- graph +
      geom_hline(yintercept = seuils[i], linetype = "dashed", color = colors[i]) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = seuils[i] - tolerance, ymax = seuils[i] + tolerance, fill = colors[i], alpha = 0.2)
  }
  graphs[[language]] <- graph
}

# Export the graphs
ggsave("AllLanguageCollaborationReport.pdf", marrangeGrob(grobs = lapply(graphs, ggplotGrob), ncol = 2, nrow = 2), device = "pdf")
