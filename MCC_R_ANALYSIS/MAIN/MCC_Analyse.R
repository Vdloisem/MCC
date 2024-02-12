# Charger les packages nécessaires
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

# Lire les données depuis le fichier CSV
data <- read_csv("mc_results.csv")

# Examiner les premières lignes pour vérifier la structure des données
head(data)

# Ajuster les éléments du thème pour diminuer la taille de la police
adjusted_theme <- theme(
  plot.title = element_text(size = 10), # Taille pour le titre du graphique
  axis.title = element_text(size = 8), # Taille pour les titres des axes
  axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6), # Taille et rotation pour le texte de l'axe des x
  axis.text.y = element_text(size = 6), # Taille pour le texte de l'axe des y
  legend.title = element_text(size = 8), # Taille pour le titre de la légende
  legend.text = element_text(size = 6) # Taille pour le texte de la légende
)

# Couleurs pour représenter différentes symbioses
colors <- c("#d62728", "#ff7f0e", "#2ca02c","#1f77b4")


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

# Tri par Paradigm1
data_sorted <- data %>%
  arrange(Paradigm1)

# Identifier les valeurs uniques
unique_paradigms <- unique(data_sorted$Paradigm1)

# Diviser les données et créer des graphiques ou des analyses pour chaque paradigme
graphs <- list()
for (paradigm in unique_paradigms) {
  # Filtrer les données pour le paradigme actuel
  subset_data <- filter(data_sorted, Paradigm1 == paradigm)
  
  # Créer le graphique pour le paradigme actuel (exemple)
  # Créer le graphique pour le paradigme actuel avec une échelle y fixée entre 0 et 100
  graph <- ggplot(subset_data, aes(x = Paradigm2, y = TotalMC_Percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    adjusted_theme +
    labs(title = paste("MC for", paradigm), x = "Paradigm 2", y = "MC (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) # Fixer l'échelle de l'axe y entre 0 et 100 avec un pas de 20
  
  # Ajouter des lignes horizontales à chaque palier de 20% avec des couleurs spécifiques
  for(i in seq_along(colors)) {
    graph <- graph + geom_hline(yintercept = i*25, linetype = "dashed", color = colors[i])
  }
  
  # Stocker le graphique dans la liste
  graphs[[paradigm]] <- graph
  
}

# Sauvegarder tous les graphiques dans un seul fichier PDF
pdf("AllParadigmMCReport.pdf")
marrangeGrob(grobs = lapply(graphs, ggplotGrob), ncol = 2, nrow = 2)
dev.off()

