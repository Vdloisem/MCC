library(ggplot2)
library(dplyr)
library(readr)

# Lecture des données
data <- read_csv2("mc_results_clean_csv.csv")

# Convertir en numérique et calculer le pourcentage
data <- data %>%
  mutate(TotalMC = as.numeric(TotalMC),
         TotalMC_Percentage = TotalMC * 100)

# Calculer la densité
dens <- density(data$TotalMC_Percentage)

# Trouver les creux
# Calculer la première dérivée (changement dans la densité)
dens_diff <- diff(dens$y)
# Trouver les points où la dérivée change de signe (creux)
creux <- which(diff(sign(dens_diff)) == 2) + 1  # +1 car la diff décale l'indice
creux_f <- creux[c(2,3,4)]
creux_x <- dens$x[creux_f]

# Créer le graphique
distriPlot <- ggplot(data, aes(x = TotalMC_Percentage)) +
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_vline(xintercept = creux_x, color = "purple", linetype = "dashed", size = 1) +
  #geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", size = 1) +
  #geom_vline(xintercept = median_value, color = "green", linetype = "dashed", size = 1) +
  labs(title = "MC distribution", x = "MC (%)", y = "Density") +
  theme_minimal()
  #annotate("text", x = mean_value, y = 0.020, label = paste("Moyenne:", round(mean_value, 2)), color = "red") +
  #annotate("text", x = median_value, y = 0.015, label = paste("Médiane:", round(median_value, 2)), color = "green")

for(i in 1:length(creux_x)) {
  distriPlot <- distriPlot + annotate("text", x = creux_x[i], y = -0.0015, label = round(creux_x[i], 2), vjust = -0.5, color = "purple")
}
pdf("AllParadigmMCDistributionReport.pdf", width = 11, height = 8)
print(distriPlot)
dev.off()

