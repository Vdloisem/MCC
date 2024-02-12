library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(gridExtra)
# Lire les données depuis le fichier CSV
data <- read_csv("mc_results.csv")

# Examiner les premières lignes pour vérifier la structure des données
head(data)

# Ajuster les éléments du thème pour diminuer la taille de la police
adjusted_theme <- theme(
  plot.title = element_text(size = 8), # Taille pour le titre du graphique
  axis.title = element_text(size = 6), # Taille pour les titres des axes
  axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6), # Taille et rotation pour le texte de l'axe des x
  axis.text.y = element_text(size = 6), # Taille pour le texte de l'axe des y
  legend.title = element_text(size = 6), # Taille pour le titre de la légende
  legend.text = element_text(size = 4) # Taille pour le texte de la légende
)


# Calculer le pourcentage de TotalMC si nécessaire
data <- data %>%
  mutate(
    I = as.numeric(I),
    R1 = as.numeric(R1),
    R2 = as.numeric(R2),
    M = as.numeric(M),
    TC = as.numeric(TC),
    NDO = as.numeric(NDO),
    TotalMC = as.numeric(TotalMC),
    TotalMC_Percentage = TotalMC * 100
  )

min_value <- min(data$TotalMC_Percentage, na.rm = TRUE)
max_value <- max(data$TotalMC_Percentage, na.rm = TRUE)

# Tri par Paradigm1
data_sorted <- data %>%
  arrange(Paradigm1)

# Identifier les valeurs uniques
unique_paradigms <- unique(data_sorted$Paradigm1)

# Diviser les données et créer des graphiques ou des analyses pour chaque paradigme
graphs <- list()

# Boucler sur chaque paradigme unique pour créer une heatmap
for (paradigm in unique_paradigms) {
  # Filtrer les données pour le paradigme actuel
  subset_data <- data %>% filter(Paradigm1 == paradigm)
  
  # Créer une matrice de corrélation pour la heatmap
  cor_matrix <- dcast(subset_data, Paradigm1 ~ Paradigm2, value.var = "TotalMC_Percentage")
  
  # Convertir les noms de lignes en une colonne
  cor_matrix$Paradigm1 <- rownames(cor_matrix)
  # Supprimer les anciens noms de lignes pour éviter la conversion en numérique
  rownames(cor_matrix) <- NULL
  
  # Faire fondre la matrice pour la visualisation de la heatmap
  # Assurez-vous que Var1 et Var2 sont traités comme des facteurs
  melted_cor_matrix <- melt(cor_matrix, id.vars = 'Paradigm1')
  melted_cor_matrix$Paradigm1 <- factor(melted_cor_matrix$Paradigm1)
  melted_cor_matrix$Paradigm2 <- factor(melted_cor_matrix$variable)
  
  # Créer la heatmap avec des limites d'échelle de couleur fixes
  heatmap_plot <- ggplot(melted_cor_matrix, aes(Paradigm2, Paradigm1, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red", limits = c(min_value, max_value)) +
    theme_minimal() +
    adjusted_theme +
    labs(title = paste("Heatmap de Corrélation pour", paradigm), x = "Paradigme 2", y = "Paradigme 1") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text.y = element_text(size = 5))
  
  # Ajouter le graphique à la liste
  graphs[[paradigm]] <- heatmap_plot
}


# Sauvegarder tous les graphiques dans un seul fichier PDF
pdf("AllParadigmMCHeatMapReport.pdf")
marrangeGrob(grobs = lapply(graphs, ggplotGrob), ncol = 2, nrow = 2)
dev.off()