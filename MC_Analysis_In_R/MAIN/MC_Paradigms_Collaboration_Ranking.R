# Load the required libraries for data visualization, manipulation, and reading
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Chargement des données depuis un fichier CSV
data <- read_csv("CollaborationMetric_Paradigms_Raw.csv")

# Preprocess data to convert 'TotalMC' from a raw score to a percentage
data <- data %>%
  mutate(TotalMC_Percentage = as.numeric(TotalMC) * 100)

# Calculate summary statistics for each 'Paradigm1', sorted by the mean collaboration score
stats_collaboration <- data %>%
  group_by(Paradigm1) %>%
  summarise(
    MeanCollab = mean(TotalMC_Percentage, na.rm = TRUE),
    MedianCollab = median(TotalMC_Percentage, na.rm = TRUE),
    SDCollab = sd(TotalMC_Percentage, na.rm = TRUE)
  ) %>%
  arrange(desc(MeanCollab)) %>%
  # Crucially, convert 'Paradigm1' into a factor with levels in the order they are observed
  mutate(Paradigm1 = factor(Paradigm1, levels = unique(Paradigm1)))

# Reshape the data to a long format suitable for ggplot2 visualization
stats_long <- stats_collaboration %>%
  pivot_longer(cols = c(MeanCollab, MedianCollab, SDCollab),
               names_to = "Statistic",
               values_to = "Value")

# Create a bar chart with the reshaped data
ggplot(stats_long, aes(x = Paradigm1, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Collaboration statistics by paradigm",
       x = "Paradigm",
       y = "Value (%)") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())

# Save the plot to a PDF file
ggsave("Collaboration_Statistics_by_Paradigm.pdf", width = 8, height = 8, units = "in")
