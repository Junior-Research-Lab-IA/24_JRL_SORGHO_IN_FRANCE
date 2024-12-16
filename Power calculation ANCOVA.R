#Power
library(pwr)

# Définir les paramètres de l'ANCOVA
alpha <- 0.05                 # Niveau de significativité
f2 <- 0.1                     # Taille de l'effet (On suppose un petit effet : 0,1)
groups <- 33                  # Nombre de niveaux du facteur
covariates <- 6               # Nombre de covariables
total_sample_size <- 330      # Taille totale de l'échantillon

# Calcul des degrés de liberté
numerator_df <- groups - 1
denominator_df <- total_sample_size - groups - covariates - 1

# Calcul de la puissance à l'aide du test de puissance F
power_result <- pwr.f2.test(u = numerator_df,          # Degrés de liberté du numérateur
                            v = denominator_df,        # Degrés de liberté du dénominateur
                            f2 = f2,                   # Taille de l'effet
                            sig.level = alpha,         # Niveau alpha
                            power = NULL)              # Puissance (à calculer)

# Afficher les résultats
cat("Résultat de la puissance de l'ANCOVA :\n")
print(power_result)


