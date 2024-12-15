library(magrittr)
library(GGally)
library(ggplot2)
library(dplyr)
#Open data as CSV UTF-8 (Comma delimited)
data <- Scenario_1 
data2 <- Scenario_2

#Fv.Fm + Min_P is integer but needs to be numeric to model
#Change to numeric
data$Fv.Fm <- as.numeric(as.character(data$Fv.Fm))
data$Min_P <- as.numeric(as.character(data$Min_P))
data$X <- as.numeric(as.character(data$X))
data$Y <- as.numeric(as.character(data$Y))
data$PI <- as.numeric(as.character(data$PI))
data$Yield <- as.numeric(as.character(data$Yield))

data2$Fv.Fm <- as.numeric(as.character(data2$Fv.Fm))
data2$Min_P <- as.numeric(as.character(data2$Min_P))
data2$X <- as.numeric(as.character(data2$X))
data2$Y <- as.numeric(as.character(data2$Y))
data2$PI <- as.numeric(as.character(data2$PI))
data2$Yield <- as.numeric(as.character(data2$Yield))

#Correction des données 

data <- data[data$Fv.Fm > quantile(data$Fv.Fm, 0.01, na.rm = TRUE), ]
data <- data[data$PI > quantile(data$PI, 0.01, na.rm = TRUE), ]
data <- data[data$Yield > quantile(data$Yield, 0.01, na.rm = TRUE), ]

data2<- data2[data2$Fv.Fm > quantile(data2$Fv.Fm, 0.01, na.rm = TRUE), ]
data2<- data2[data2$PI > quantile(data2$PI, 0.01, na.rm = TRUE), ]
data2<- data2[data2$Yield > quantile(data2$Yield, 0.01, na.rm = TRUE), ]

#Graphique des données
ggpairs(data, column=c(7,14,15,19,20), cardinality_threshold = 40)
ggpairs(data, column=c(7,14,15,19,20), cardinality_threshold = 40)

# Densité
ggplot(data_reduced_FvFm, aes(x = Fv.Fm)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_minimal()


# Personnalisation des couleurs
custom_theme <- theme_minimal(base_size = 14) + 
  theme(panel.grid.major = element_line(color = "gray85"), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "lightblue", color = "darkblue"),
        strip.text = element_text(color = "black", face = "bold"))

# Création d'un ggpairs avec seulement les densités et corrélations
ggpairs(data, 
        columns = c(14, 15, 19, 20), 
        cardinality_threshold = 40,
        lower = list(continuous = wrap("points", alpha = 0.7, color = "darkblue")),
        diag = list(continuous = wrap("densityDiag", fill = "lightblue", color = "darkblue")),
        upper = list(continuous = wrap("cor", size = 5, color = "darkred"))) +
  custom_theme

ggpairs(data2, 
        columns = c(14, 15, 19, 20), 
        cardinality_threshold = 40,
        lower = list(continuous = wrap("points", alpha = 0.7, color = "darkblue")),
        diag = list(continuous = wrap("densityDiag", fill = "lightblue", color = "darkblue")),
        upper = list(continuous = wrap("cor", size = 5, color = "darkred"))) +
  custom_theme



