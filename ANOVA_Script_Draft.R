#Open data as CSV UTF-8 (Comma delimited)
Mesures.de.fluorescences_S1 <- read.csv("~/1 - University/Year 3/JRL-AT/Project/Mesures de fluorescences_S1.csv")

Mesures.de.fluorescences_S2 <- read.csv("~/1 - University/Year 3/JRL-AT/Project/Mesures de fluorescences_S2.csv")

Mesures.de.fluorescences.S3 <- read.csv("~/1 - University/Year 3/JRL-AT/Project/Mesures de fluorescences S3.csv")

data <- rbind(Mesures.de.fluorescences_S1, Mesures.de.fluorescences_S2, Mesures.de.fluorescences.S3)

#Fv.Fm + Min_P is integer but needs to be numeric to model
#Change to numeric
data$Fv.Fm <- as.numeric(as.character(data$Fv.Fm))
data$Min_P <- as.numeric(as.character(data$Min_P))
data$X <- as.numeric(as.character(data$X))
data$Y <- as.numeric(as.character(data$Y))
data$PI <- as.numeric(as.character(data$PI))
data$Yield <- as.numeric(as.character(data$Yield))
data$Min_M <- as.numeric(as.character(data$Min_M))

#Its taking time as a categorical variable
#Add minutes column to data
#Time starting from 0 --> How long have the plants been exposed to the stable temperature which starts when we start measuring


hist(data_reduced_FvFm$Fv.Fm)
##############################################
#Fv.Fm
#Scenario at the start, Scenario-Genotype at the end
#Start with environmental + end with genotype
data_reduced_FvFm=data[data$Fv.Fm>quantile(data$Fv.Fm,0.01,na.rm = T),]
modelF <- lm(Fv.Fm ~ Scenario + Min_P + Day + Table+ Table:X + Table:Y + Genotype + Scenario:Genotype, data = data_reduced_FvFm)
summary(modelF)
anova(modelF)

#Assumptions
resFv <- residuals(modelF)
hist(resFv)
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(modelF)
mtext("Fv/Fm",
      side = 3,
      outer = TRUE,
      line = -1,
      cex = 1.5)  

#Combine ANCOVA summary with emmeans plot
#Significance + stable, high emmeans
library(emmeans)
library(ggplot2)
emmeans_results_Fv <- emmeans(modelF, ~ Scenario|Genotype)
print(emmeans_results_Fv)
compare_Fv <- contrast(emmeans_results_Fv, method = "pairwise")
print(compare_Fv)
compare_Fv_Tukey <- contrast(emmeans_results_Fv, method = "pairwise", adjust = "tukey")
summary(compare_Fv_Tukey)

#Visualisation
emmeans_Fv_df <- as.data.frame(emmeans_results_Fv)
y_limit <- range(emmeans_Fv_df$emmean, na.rm = TRUE)  # This will calculate the minimum and maximum across all data points

ggplot(emmeans_Fv_df, aes(x = Scenario, y = emmean, group = Genotype, color = Genotype)) +
  geom_point(size = 2) + # Adds points at each scenario
  facet_wrap(~ Genotype, scales = "free_y", ncol = 4) + # Each genotype in its own plot with free y scales
  scale_y_continuous(limits = y_limit) + # Set a consistent y-axis limit across all plots
  labs(
    title = "Performance of Genotypes Across Climate Scenarios (Estimated Fv/Fm)",
    x = "Climate Scenario",
    y = "Estimated Fv/Fm",
    color = "Genotype"
  ) +
  theme_minimal() + # Clean minimal theme
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "cm") # Increase plot margins to space plots
  )

###########################################
#PIabs
data_reduced_PI=data[data$PI>quantile(data$PI,0.01,na.rm = T),]
hist(data_reduced_PI$PI)
modelPI <- lm(PI ~ Scenario + Min_P + Day + Table+ Table:X + Table:Y + Genotype + Scenario:Genotype, data = data_reduced_PI)
summary(modelPI)
anova(modelPI)

resPI <- residuals(modelPI)
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(modelPI)
mtext("PIabs",
      side = 3,
      outer = TRUE,
      line = -1,
      cex = 1.5) 


library(emmeans)
library(ggplot2)
emmeans_results_PI <- emmeans(modelPI, ~ Genotype|Scenario)
print(emmeans_results_PI)
compare_PI <- contrast(emmeans_results_PI, method = "pairwise")
print(compare_PI)
compare_PI_Tukey <- contrast(emmeans_results_PI, method = "pairwise", adjust = "tukey")
summary(compare_PI_Tukey)


emmeans_PI_df <- as.data.frame(emmeans_results_PI)
y_limit <- range(emmeans_PI_df$emmean, na.rm = TRUE)  # This will calculate the minimum and maximum across all data points

ggplot(emmeans_PI_df, aes(x = Scenario, y = emmean, group = Genotype, color = Genotype)) +
  geom_point(size = 2) + # Adds points at each scenario
  facet_wrap(~ Genotype, scales = "free_y", ncol = 4) + # Each genotype in its own plot with free y scales
  scale_y_continuous(limits = y_limit) + # Set a consistent y-axis limit across all plots
  labs(
    title = "Performance of Genotypes Across Climate Scenarios (Estimated PIabs)",
    x = "Climate Scenario",
    y = "Estimated PIabs",
    color = "Genotype"
  ) +
  theme_minimal() + # Clean minimal theme
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "cm") # Increase plot margins to space plots
  )




###########################################
#PhiPSII
data_reduced_Phi=data[data$Yield>quantile(data$Yield,0.01,na.rm = T),]
modelPhi <- lm(Yield ~ Scenario + Min_M + Day + Table+ Table:X + Table:Y + Genotype + Scenario:Genotype, data = data_reduced_Phi)
summary(modelPhi)
anova(modelPhi)

resPhi <- residuals(modelPhi)
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(modelPhi)
mtext("PhiPSII",
      side = 3,
      outer = TRUE,
      line = -1,
      cex = 1.5) 



library(emmeans)
emmeans_results_Phi <- emmeans(modelPhi, ~ Genotype|Scenario)
print(emmeans_results_Phi)
compare_Phi <- contrast(emmeans_results_Phi, method = "pairwise")
print(compare_Phi)
compare_Phi_Tukey <- contrast(emmeans_results_Phi, method = "pairwise", adjust = "tukey")
summary(compare_Phi_Tukey)

emmeans_Phi_df <- as.data.frame(emmeans_results_Phi)
y_limit <- range(emmeans_Phi_df$emmean, na.rm = TRUE)  # This will calculate the minimum and maximum across all data points

ggplot(emmeans_Phi_df, aes(x = Scenario, y = emmean, group = Genotype, color = Genotype)) +
  geom_point(size = 2) + # Adds points at each scenario
  facet_wrap(~ Genotype, scales = "free_y", ncol = 4) + # Each genotype in its own plot with free y scales
  scale_y_continuous(limits = y_limit) + # Set a consistent y-axis limit across all plots
  labs(
    title = "Performance of Genotypes Across Climate Scenarios (Estimated PIabs)",
    x = "Climate Scenario",
    y = "Estimated PhiPSII",
    color = "Genotype"
  ) +
  theme_minimal() + # Clean minimal theme
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "cm") # Increase plot margins to space plots
  )

#BoxPlots
library(ggplot2)
library(tidyr)

box_data <- data[, c("Genotype", "Fv.Fm", "PI", "Yield")]
colnames(box_data)[colnames(box_data) == "Yield"] <- "PhiPSII"

data_long <- gather(box_data, key = "Parameter", value = "Value", Fv.Fm, PI, PhiPSII)

ggplot(data_long, aes(x = Parameter, y = Value, fill = Parameter)) + 
  geom_boxplot() + 
  facet_wrap(~Genotype, scales = "free_y", ncol = 6) + 
  theme_minimal() + 
  labs(title = "Fv/Fm, PIabs and PhiPSII by Genotype",
       x = "Parameter", 
       y = "Value") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
facet_wrap(~Genotype, scales = "free_y", ncol = 4)


#Heritability
0.0002064/(0.002064 + 0.0009794/30)

#Genotype 17, 29 + 31 are significant in Fv/Fm + PIabs


hist(data_reduced_PI$PI)


#CFI

Mesures.de.fluorescences_S1$PI <- as.numeric(as.character(Mesures.de.fluorescences_S1$PI))
Mesures.de.fluorescences_S2$PI <- as.numeric(as.character(Mesures.de.fluorescences_S2$PI))
Mesures.de.fluorescences.S3$PI <- as.numeric(as.character(Mesures.de.fluorescences.S3$PI))

library(dplyr)
library(tidyr)

mean_PI <- data %>%
  group_by(Genotype, Scenario) %>%
  summarize(mean_PI = mean(`PI`, na.rm = TRUE), .groups = "drop")
mean_PI_wide <- mean_PI %>%
  pivot_wider(names_from = Scenario, values_from = mean_PI, names_prefix = "PI_")

mean_PI_wide <- mean_PI_wide %>%
  mutate(
    CFI = log(PI_S2 / PI_S1) + 2 * log(PI_S3 / PI_S1)
  )
ranked_genotypes <- mean_PI_wide %>%
  arrange(desc(CFI))

write.csv(ranked_genotypes, "Ranked_Genotypes.csv", row.names = FALSE)

genotype_of_interest <- "Sb03"

mean_pi <- Mesures.de.fluorescences_S1 %>%
  filter(Genotype == genotype_of_interest) %>%
  summarise(mean_PI = mean(PI, na.rm = TRUE)) %>%
  pull(mean_PI)
print(mean_pi)

mean_pi_2 <- Mesures.de.fluorescences_S2 %>%
  filter(Genotype == genotype_of_interest) %>%
  summarise(mean_PI = mean(PI, na.rm = TRUE)) %>%
  pull(mean_PI)
print(mean_pi_2)

mean_pi_3 <- Mesures.de.fluorescences.S3 %>%
  filter(Genotype == genotype_of_interest) %>%
  summarise(mean_PI = mean(PI, na.rm = TRUE)) %>%
  pull(mean_PI)
print(mean_pi_3)

log(5.0156/6.2033) + 2*log(3.779/6.2033)

#PI
#Scenario
1441.14 / (1291.04 + 1441.4)
#Genotype
712.71 / (1291.04 + 712.71)
#Scenario:Genotype
342.81 / (342.81 + 1291.04)

#PhiPSII
#Scenario
4.3604 / (4.3604 + 1.2147)
#Genotype
0.2659 / (0.2659 + 1.2147)
#Scenario:Genotype
0.3476 / (0.3476 + 1.2147)
