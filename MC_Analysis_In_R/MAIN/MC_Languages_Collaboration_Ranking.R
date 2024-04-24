# Load the required libraries for data visualization, manipulation, and reading
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Read the data from a CSV file
data <- read_csv("CollaborationMetric_Languages_Raw.csv")

# Convert 'CollaborationScore' from a raw score to a percentage format for better visualization
data <- data %>%
  mutate(Collaboration_Percentage = as.numeric(CollaborationScore) * 100)

# Calculate summary statistics (mean, median, standard deviation) for each language, sorted by mean collaboration score
stats_collaboration_lang <- data %>%
  select(Language1, Collaboration_Percentage) %>%
  group_by(Language1) %>%
  summarise(
    MeanCollab = mean(Collaboration_Percentage, na.rm = TRUE),
    MedianCollab = median(Collaboration_Percentage, na.rm = TRUE),
    SDCollab = sd(Collaboration_Percentage, na.rm = TRUE)
  ) %>%
  arrange(desc(MeanCollab)) %>%
  # Convert 'Language1' into a factor with levels appearing in the order they are observed
  mutate(Language1 = factor(Language1, levels = unique(Language1)))

# Reshape the data to a long format suitable for ggplot2 visualization
stats_long_lang <- stats_collaboration_lang %>%
  pivot_longer(cols = c(MeanCollab, MedianCollab, SDCollab),
               names_to = "Statistic",
               values_to = "Value")

# Create a bar chart with the reshaped data
ggplot(stats_long_lang, aes(x = Language1, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Collaboration statistics by language",
       x = "Language",
       y = "Value (%)") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())

# Save the plot as a PDF file
ggsave("Collaboration_Statistics_by_Language.pdf", width = 8, height = 8, units = "in")
