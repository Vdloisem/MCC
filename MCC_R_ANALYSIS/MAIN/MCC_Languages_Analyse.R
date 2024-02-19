# Charger les packages nécessaires
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

# Lire les données depuis le fichier CSV adapté aux langages
data <- read_csv("language_collaboration_results.csv")

# Ajustements de thème
adjusted_theme <- theme(
  plot.title = element_text(size = 10),
  axis.title = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
  axis.text.y = element_text(size = 6),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6)
)

# Calculer le pourcentage de collaboration
data <- data %>%
  mutate(Collaboration_Percentage = as.numeric(CollaborationScore) * 100)

# Tri par Language1 et création des graphiques pour chaque langage
data_sorted <- data %>% arrange(Language1)
unique_languages <- unique(data_sorted$Language1)
graphs <- list()

seuils <- c(25.7, 56.9, 74) # Exemple de seuils de collaboration
colors <- c("#d62728", "#ff7f0e", "#2ca02c") # Couleurs pour les seuils

for (language in unique_languages) {
  subset_data <- filter(data_sorted, Language1 == language)
  graph <- ggplot(subset_data, aes(x = Language2, y = Collaboration_Percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    adjusted_theme +
    labs(title = paste("Collaboration for", language), x = "Language 2", y = "Collaboration (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25))
  
  # Ajouter des lignes horizontales pour les seuils de collaboration
  for(i in 1:length(seuils)) {
    graph <- graph + geom_hline(yintercept = seuils[i], linetype = "dashed", color = colors[i])
  }
  
  graphs[[language]] <- graph
}

# Sauvegarder tous les graphiques dans un seul fichier PDF
pdf("AllLanguageCollaborationReport.pdf")
marrangeGrob(grobs = lapply(graphs, ggplotGrob), ncol = 2, nrow = 2)
dev.off()
