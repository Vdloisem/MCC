# Load necessary libraries for data visualization, manipulation, and reading
library(ggplot2)
library(dplyr)
library(readr)

# Read the data from a CSV file using read_csv2 which handles semicolon-separated values well
data <- read_csv2("mc_results_clean_csv.csv")

# Preprocess the data by converting 'TotalMC' to numeric and calculating the percentage of TotalMC
data <- data %>%
  mutate(TotalMC = as.numeric(TotalMC),
         TotalMC_Percentage = TotalMC * 100)

# Calculate the density of the TotalMC_Percentage to find its distribution
dens <- density(data$TotalMC_Percentage)

# Find the differences in density values to identify potential points of interest in the distribution
dens_diff <- diff(dens$y)

# Identify points where the change in sign in the differences indicates a valley (or creux)
creux <- which(diff(sign(dens_diff)) == 2) + 1
# Filter creux to select specific valleys of interest, here taking the 2nd, 3rd, and 4th found points
creux_f <- creux[c(2,3,4)]
# Extract the x-values (percentage) at these filtered valley points
creux_x <- dens$x[creux_f]

# Create a density plot of TotalMC_Percentage highlighting the identified valleys with vertical lines
distriPlot <- ggplot(data, aes(x = TotalMC_Percentage)) +
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_vline(xintercept = creux_x, color = "purple", linetype = "dashed", size = 1) +
  labs(title = "MC distribution", x = "MC (%)", y = "Density") +
  theme_minimal()

# Annotate the plot with the precise values of the valleys
for(i in 1:length(creux_x)) {
  distriPlot <- distriPlot + annotate("text", x = creux_x[i], y = -0.0015, label = round(creux_x[i], 2), vjust = -0.5, color = "purple")
}

# Output the plot to a PDF file with specified dimensions
pdf("AllParadigmMCDistributionReport.pdf", width = 11, height = 8)
print(distriPlot)
dev.off()