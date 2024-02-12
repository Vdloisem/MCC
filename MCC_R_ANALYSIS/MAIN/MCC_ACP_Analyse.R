# Chargement des packages nécessaires
library(dplyr)
library(ggplot2)
library(FactoMineR) # Pour l'ACP
library(factoextra) # Pour la visualisation

data <- read_csv("mc_results.csv")

# Supposons que 'data' est votre dataframe et contient les variables pour l'ACP
# Normalisation des données (si non déjà fait)
data_normalized <- scale(data[, c("I", "R1", "R2", "M", "TC", "NDO")]) # Adaptez les noms de colonnes si nécessaire

# Application de l'ACP
res_acp <- PCA(data_normalized, graph = FALSE)

# Générer le scree plot
fviz_eig(res_acp, addlabels = TRUE, ylim = c(0, 100))

# Visualisation des résultats
# Scores des individus (observations)
fviz_pca_ind(res_acp, 
             col.ind = "cos2", # Colorer par la qualité de la représentation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Pour éviter le chevauchement des étiquettes

# Contribution des variables aux composantes principales
fviz_pca_var(res_acp, 
             col.var = "contrib", # Colorer par la contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Pour éviter le chevauchement des étiquettes
