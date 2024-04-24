# Load the required libraries for data visualization, manipulation, and reading
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Read the data from a CSV file
data <- read_csv("CollaborationMetric_Paradigms_Raw.csv")

# Normalize selected columns of data for PCA; adjust column names as necessary
data_normalized <- scale(data[, c("I", "R1", "R2", "M", "TC", "NDO")])

# Perform PCA without automatic plotting
res_acp <- PCA(data_normalized, graph = FALSE)

# Create a scree plot to display variance explained by each principal component
fviz_eig(res_acp, addlabels = TRUE, ylim = c(0, 100))

# Visualize individual scores on the principal components, colored by the quality of the representation on the components
fviz_pca_ind(res_acp, 
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Plot contributions of variables to the principal components, colored by their contributions
fviz_pca_var(res_acp, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
