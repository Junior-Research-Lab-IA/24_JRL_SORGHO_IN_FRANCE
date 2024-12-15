# Chargement des bibliothèques nécessaires
library(lme4)
library(performance)
library(ggplot2)

# Chargement des données
data_s1 <- read.csv("C:/Users/aleth/Downloads/Deuxième année/Research Project/Data/DFS1.csv", sep=";")
data_s2 <- read.csv("C:/Users/aleth/Downloads/Deuxième année/Research Project/Data/DFS2.csv", sep=";")
data_s3 <- read.csv("C:/Users/aleth/Downloads/Deuxième année/Research Project/Data/DFS3.csv",sep=";")
# Fusion des deux jeux de données
data <- rbind(data_s1, data_s2, data_s3)

# Conversion des colonnes nécessaires en facteurs
data$Scenario <- as.factor(data$Scenario)
data$PI <- as.numeric(data$PI) 

#enlever point aberrants
data <- data[data$PI > quantile(data$PI, 0.01, na.rm = TRUE), ]

# Ajustement du modèle mixte linéaire avec interaction
mixed_model_PI <- lmer(PI ~ Scenario + (1 | Genotype)+(1|Genotype:Scenario), data = data) #Explications : Le terme suit une loi normal, centrée en zéro, variance sigma carré. 

# Affichage des résultats du modèle
summary(mixed_model_PI)
#Hypothèses 
check_model(mixed_model_PI)

# Héritabilité
heritabilite_sl=0.6615/(0.6615+0.2898/3+1.7192) #compare two individuals : le caractère est peu héritable -> très environnemental)
heritabilite_ss=0.6615/(0.6615+0.2898/3+1.7192/10) #genetic value _ dispositif : on réobtiendrait dans le même dispotif globalement les mêmes valeurs)

#Ranef : the random effect estimate 
ranef(mixed_model_PI) #The random effect estimate : Genotype : 
#Une fois enlevé les effets fixes : part due à la génétique -> on regarde le ranef du genotype (on corrige l'effet du scénario, valeurs positives car génotype plus résistant) et ensuite on regarde dans l'intéraction G:S pour vérifier qu'il n'est pas seulement le meilleur pour un scénario.
blupG<-as.data.frame(ranef(mixed_model_PI)$Genotype)
blupGS<-as.data.frame(ranef(mixed_model_PI)$'Genotype:Scenario')


hist(as.numeric(blupG[,1]))
hist(as.numeric(blupGS[,1]))


PI_S1 <- as.numeric(data_s1$PI)
PI_S2 <- as.numeric(data_s2$PI)
#PI_S3 <- as.numeric(data_s3$PI)

cfi <- log(PI_S2/PI_S1) #+2log(PI_S3/PI_S1)

data_s1 <- data_s1[data_s1$PI > quantile(data_s1$PI, 0.01, na.rm = TRUE), ]
df_cfi <- data_s1[,c("Genotype","cfi")]
cfi.anova <- lm(cfi ~ Genotype, data = df_cfi)
summary(cfi.anova)
anova(cfi.anova)
check_model(cfi.anova)
