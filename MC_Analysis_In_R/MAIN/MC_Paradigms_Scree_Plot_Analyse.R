# Load necessary libraries for principal component analysis, data visualization, and manipulation
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Read the data from a CSV file
data <- read_csv2("CollaborationMetric_Paradigms_Cleaned.csv")

# Preprocess and normalize the data, converting all relevant columns to numeric and calculating TotalMC as a percentage
data <- data %>%
  mutate(
    I = as.numeric(I),
    R = as.numeric(R2),
    M = as.numeric(M),
    TC = as.numeric(TC),
    NDO = as.numeric(NDO),
    TotalMC = as.numeric(TotalMC),
    TotalMC_Percentage = TotalMC * 100
  )

# Normalize selected variables for PCA, excluding R1 due to analysis specifics (R1 always 0 in this context)
data_normalized <- scale(data[, c("I", "R", "M", "TC", "NDO")]) 
# Calculate variance of normalized data
var(data_normalized)

# Perform PCA on the normalized data without graph output
res_acp <- PCA(data_normalized, ncp = ncol(data_normalized), graph = FALSE)

# Create a scree plot visualizing the variance explained by each principal component
scree_plot <- fviz_eig(res_acp, addlabels = TRUE)
ylim_max <- max(res_acp$eig[, 2]) * 1.2
scree_plot <- scree_plot + ylim(c(0, ylim_max))

# Retrieve loadings of the PCA
loadings <- get_pca_var(res_acp)$coord

# Identify the top variable (main load) contributing to each dimension
top_variables <- apply(loadings, 2, function(x) {
  idx <- which.max(abs(x))
  list(Variable = names(x)[idx], Charge = x[idx])
})

# Convert the list of top variables to a data frame for easy visualization
top_variables_df <- do.call(rbind.data.frame, top_variables)
rownames(top_variables_df) <- paste("Dimension", seq_along(top_variables))

# Print the data frame containing the top variables for each dimension
print(top_variables_df)

# Customize the scree plot with the main load for each dimension annotated
scree_plot <- ggplot(data.frame(x = 1:length(res_acp$eig[, 2]), Variances = res_acp$eig[, 2]), aes(x = x, y = Variances)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Scree plot with main load", x = "Dimensions", y = "Variance Explained (%)") +
  geom_text(aes(label = paste("Main load : ", top_variables_df$Variable, "\n(", sprintf("%.2f%%", abs(top_variables_df$Charge) * 100), ")", sep="")),
            vjust = 2,
            size = 3)

# Output the scree plot to a PDF file with specified dimensions
pdf("AllParadigmMCScreePlotReport.pdf", width = 11, height = 8)
print(scree_plot)
dev.off()