#Bibliothèques
library(ggplot2)

#Données
data <- read.csv("C:/Users/aleth/Downloads/Deuxième année/Research Project/Data/Ranef/Ranef_Tot.csv", sep=";")


# Tracer le graphique
ggplot(data, aes(x = Scenario, y = Ranef, color = Genotype)) +
  geom_point(size = 3) + # Points pour les valeurs
  geom_line(aes(group = Genotype), linetype = "solid", alpha = 0.7) + # Lignes pour relier les points par génotype
  labs(
    title = "Random Effects of Genotypes Estimated Under Different Scenarios",
    x = "Scenario",
    y = "The estimated random effect of genotype:scenario (Ranef)",
    color = "Genotype"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


