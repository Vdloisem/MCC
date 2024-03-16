# Load the required libraries for data visualization, manipulation, and reading
library(ggplot2)
library(dplyr)
library(readr)

# Read the cleaned and preprocessed data from a CSV file
data <- read_csv2("CollaborationMetric_Languages_Cleaned.csv")

# Convert the 'CollaborationScore' to a percentage for easier interpretation and visualization
data <- data %>%
  mutate(CollaborationScore_Percentage = as.numeric(CollaborationScore) * 100)

# Calculate the density of the collaboration score percentages to find distribution
dens <- density(data$CollaborationScore_Percentage, adjust=0.77)

# Calculate the mean for the collaboration scores
mean_score <- mean(data$CollaborationScore_Percentage)

# Find the differences in density values to identify valleys in the density plot
dens_diff <- diff(dens$y)
# Identify points where the change in sign indicates a valley
creux <- which(diff(sign(dens_diff)) == 2) + 1
# Extract the x-values (collaboration score percentages) at these valley points
creux_x <- dens$x[creux]

# Define a function to find the nearest actual value in the data to specified thresholds
nearest <- function(vector, value) {
  idx <- which.min(abs(vector - value))
  return(vector[idx])
}

# Adjust symbiosis thresholds based on the nearest values found in the density plot
seuils_symbiose_ajustes <- c(nearest(creux_x, 31.94), nearest(creux_x, 52.89), nearest(creux_x, 71.45))

# Create a density plot of the collaboration score percentages with adjusted thresholds
plot_symbiose_ajuste <- ggplot(data, aes(x = CollaborationScore_Percentage)) +
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title = "Distribution of collaboration scores between languages with adjusted thresholds", x = "Collaboration score (%)", y = "Density") +
  theme_minimal()

# Add vertical lines and annotations for adjusted thresholds
for (i in seq_along(seuils_symbiose_ajustes)) {
  seuil <- seuils_symbiose_ajustes[i]
  plot_symbiose_ajuste <- plot_symbiose_ajuste +
    geom_vline(xintercept = seuil, color = "purple", linetype = "dashed", size = 1) +
    annotate("text", x = seuil, y = 0.01, label = paste(round(seuil, 2), "%"), vjust = -0.5,hjust = -0.2, color = "purple", size = 3)
}


# Output the plot to a PDF file with specified dimensions
pdf("LanguageCollaborationDistributionAdjustedReport.pdf", width = 11, height = 8)
print(plot_symbiose_ajuste)
dev.off()
