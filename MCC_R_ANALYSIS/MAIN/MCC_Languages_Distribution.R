library(ggplot2)
library(dplyr)
library(readr)

# Lecture des données depuis un fichier CSV adapté aux langages
data <- read_csv2("language_collaboration_results_clean_clone_removed.csv")

# Convertir le score de collaboration en pourcentage
data <- data %>%
  mutate(CollaborationScore_Percentage = as.numeric(CollaborationScore) * 100)

# Calculer la densité
dens <- density(data$CollaborationScore_Percentage)

# Identifier les creux dans la distribution
dens_diff <- diff(dens$y)
creux <- which(diff(sign(dens_diff)) == 2) + 1
creux_x <- dens$x[creux]

# Fonction pour trouver la valeur la plus proche
nearest <- function(vector, value) {
  idx <- which.min(abs(vector - value))
  return(vector[idx])
}

# Seuils de symbiose ajustés en fonction des creux
seuils_symbiose_ajustes <- c(nearest(creux_x, 31.94), nearest(creux_x, 52.89), nearest(creux_x, 71.45))

# Créer le graphique de densité avec les seuils ajustés
plot_symbiose_ajuste <- ggplot(data, aes(x = CollaborationScore_Percentage)) +
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title = "Distribution of collaboration scores between languages with adjusted thresholds", x = "Collaboration score (%)", y = "Density") +
  theme_minimal()

for (i in seq_along(seuils_symbiose_ajustes)) {
  seuil <- seuils_symbiose_ajustes[i]
  plot_symbiose_ajuste <- plot_symbiose_ajuste +
    geom_vline(xintercept = seuil, color = "purple", linetype = "dashed", size = 1) +
    annotate("text", x = seuil, y = 0, label = paste(round(seuil, 2), "%"), vjust = 2.2, color = "purple", size = 3)
}

# Sauvegarder le graphique dans un fichier PDF
pdf("LanguageCollaborationDistributionAdjustedReport.pdf", width = 11, height = 8)
print(plot_symbiose_ajuste)
dev.off()
