# Chargement des packages nécessaires
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Lecture des données
data <- read_csv2("mc_results_clean_csv.csv")

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

# Normalisation des données
data_normalized <- scale(data[, c("I", "R", "M", "TC", "NDO")]) # Exclu R1 de l'analyse 
# car analyse descendente sur l'arbre des paradigme sans répétition donc R1 tjs 0
var(data_normalized)

# Application de l'ACP
res_acp <- PCA(data_normalized, ncp = ncol(data_normalized), graph = FALSE)

# Création du scree plot avec une limite Y plus élevée pour permettre plus d'espace pour les annotations
scree_plot <- fviz_eig(res_acp, addlabels = TRUE)
ylim_max <- max(res_acp$eig[, 2]) * 1.2 # 20% plus haut que la valeur max
scree_plot <- scree_plot + ylim(c(0, ylim_max))

# Obtenez les coordonnées des charges des variables
loadings <- get_pca_var(res_acp)$coord

# Trouver la variable la plus influente pour chaque dimension
top_variables <- apply(loadings, 2, function(x) {
  # Identifiez l'index de la charge maximale absolue pour la dimension actuelle
  idx <- which.max(abs(x))
  # Renvoyer le nom de la variable et sa charge
  list(Variable = names(x)[idx], Charge = x[idx])
})

# Convertissez la liste en dataframe pour une meilleure manipulation et visualisation
top_variables_df <- do.call(rbind.data.frame, top_variables)
# Ajustez les noms des lignes pour refléter les numéros de dimension
rownames(top_variables_df) <- paste("Dimension", seq_along(top_variables))

# Affichez le dataframe pour vérifier les résultats
print(top_variables_df)

# Utilisation de ggplot2 pour créer le scree plot avec annotations personnalisées plus élevées
scree_plot <- ggplot(data.frame(x = 1:length(res_acp$eig[, 2]), Variances = res_acp$eig[, 2]), aes(x = x, y = Variances)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Scree plot with main load", x = "Dimensions", y = "Variance Explained (%)") +
  geom_text(aes(label = paste("Main load : ", top_variables_df$Variable, "\n(", sprintf("%.2f%%", abs(top_variables_df$Charge) * 100), ")", sep="")),
            vjust = 2,  # Ajuster la position verticale des étiquettes
            size = 3)

# Ouvrir un nouveau fichier PDF pour le graphique
pdf("AllParadigmMCScreePlotReport.pdf", width = 11, height = 8)
print(scree_plot)
dev.off()





