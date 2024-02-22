# Load necessary libraries for data visualization, manipulation, and reading
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

# Read the data from a CSV file
data <- read_csv("mc_results.csv")

# Define a custom theme for ggplot2 charts for consistent appearance
adjusted_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
  axis.text.y = element_text(size = 6),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6)
)

# Preprocess the data by converting 'TotalMC' to a percentage for visualization
data <- data %>%
  mutate(TotalMC_Percentage = as.numeric(TotalMC) * 100)

# Sort the data by the first paradigm for plotting
data_sorted <- data %>% arrange(Paradigm1)
# Extract unique paradigms for plotting
unique_paradigms <- unique(data_sorted$Paradigm1)
# Initialize a list to store individual graphs
graphs <- list()

# Define thresholds and corresponding colors for visualization
seuils <- c(31.94, 52.89, 71.45)
colors <- c("#d62728", "#ff7f0e", "#2ca02c", "#1f77b4")

# Loop through each unique paradigm to create a bar plot
for (paradigm in unique_paradigms) {
  # Filter data for the current paradigm
  subset_data <- filter(data_sorted, Paradigm1 == paradigm)
  # Create the bar plot
  graph <- ggplot(subset_data, aes(x = Paradigm2, y = TotalMC_Percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    adjusted_theme +
    labs(title = paste("MC for", paradigm), x = "Paradigm 2", y = "MC (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25))
  
  # Add horizontal lines at defined thresholds
  for(i in 1:length(seuils)) {
    graph <- graph + geom_hline(yintercept = seuils[i], linetype = "dashed", color = colors[i])
  }
  # Store the graph in the list using the paradigm as the key
  graphs[[paradigm]] <- graph
}

# Output the graphs to a PDF file
pdf("AllParadigmMCReport.pdf")
marrangeGrob(grobs = lapply(graphs, ggplotGrob), ncol = 2, nrow = 2)
dev.off()