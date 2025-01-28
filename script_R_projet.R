
#################################Partie 1 du projet#############################################

library(haven)
library(VIM)

library(car)
library(nnet)

Ecmoss_2006 <- read_dta("C:/Users/monte/Downloads/Ecmoss_2006.dta(1)/Ecmoss_2006.dta")
View(Ecmoss_2006)


######Rendons disponible les variables de notre base de données,

attach(Ecmoss_2006)
dim(Ecmoss_2006)

colSums(is.na(Ecmoss_2006)) # Pour afficher le nombre de valeurs manquantes 
aggr(Ecmoss_2006, numbers = TRUE, prop = TRUE, sortVars = TRUE, only.miss = TRUE)

#####Test de corrélation : réduire la multicolinéarité des variables explicatives (quantitatives)

quantitative_vars <- c("age_r", "duree", "nbheur", "qs24_r", "qs3_3_r")
colSums(is.na(Ecmoss_2006[ , quantitative_vars]))

clean_data <- na.omit(Ecmoss_2006[ , quantitative_vars])
cor_matrix <- cor(clean_data)
cor_matrix

corrplot(cor_matrix, method = "circle", type = "upper", 
         title = "Matrice des corrélations - Variables quantitatives", mar = c(0, 0, 2, 0))

#####Test de corrélation pour réduire la multicolinéarité des variables explicatives (qualitatives)

####Nous utilisons model.matrix pour créer les variables indicatrices
dummy_sexe <- model.matrix(~ sexe_r - 1, data = Ecmoss_2006)
dummy_statut <- model.matrix(~ statut - 1, data = Ecmoss_2006)
dummy_cs_r <- model.matrix(~ cs_r - 1, data = Ecmoss_2006)
dummy_cs2_r <- model.matrix(~ cs2_r - 1, data = Ecmoss_2006)

####Combinons toutes les variables dummy dans un data frame
dummy_data <- cbind(dummy_sexe, dummy_statut, dummy_cs_r, dummy_cs2_r)

####Test de Cramer's V pour mesurer l'association entre les variables qualitatives
cramer_test <- function(var1, var2) {
  contingency_table <- table(var1, var2)
  chi_square <- chisq.test(contingency_table)
  n <- sum(contingency_table)
  min_dim <- min(nrow(contingency_table), ncol(contingency_table))
  cramer <- sqrt(chi_square$statistic / (n * (min_dim - 1)))
  return(cramer)
}

####Calcul du Cramer's V pour toutes les paires de variables
variables <- c("sexe_r", "statut", "cs_r", "cs2_r")
n_vars <- length(variables)
cramer_matrix <- matrix(NA, n_vars, n_vars)
rownames(cramer_matrix) <- colnames(cramer_matrix) <- variables

for(i in 1:(n_vars-1)) {
  for(j in (i+1):n_vars) {
    cramer_val <- cramer_test(Ecmoss_2006[[variables[i]]], 
                              Ecmoss_2006[[variables[j]]])
    cramer_matrix[i,j] <- cramer_matrix[j,i] <- cramer_val
  }
}
diag(cramer_matrix) <- 1

####Afficher la matrice des Cramer's V
print("Matrice des coefficients de Cramer's V :")
print(round(cramer_matrix, 3))

####Test du Chi-deux pour l'indépendance
chi_square_results <- matrix(NA, n_vars, n_vars)
rownames(chi_square_results) <- colnames(chi_square_results) <- variables

for(i in 1:(n_vars-1)) {
  for(j in (i+1):n_vars) {
    chi_test <- chisq.test(table(Ecmoss_2006[[variables[i]]], 
                                 Ecmoss_2006[[variables[j]]])) # Ajout d'une parenthèse ici
    chi_square_results[i,j] <- chi_square_results[j,i] <- chi_test$p.value
  }
}
diag(chi_square_results) <- 1

print("\nP-values des tests d'indépendance du Chi-deux :")
print(round(chi_square_results, 4))

####Visualisation : Transformation de la matrice des coefficients de Cramer
cramer_df <- as.data.frame(as.table(cramer_matrix))
colnames(cramer_df) <- c("Variable1", "Variable2", "CramerV")

####Heatmap des coefficients de Cramer avec ggplot
ggplot(cramer_df, aes(x = Variable1, y = Variable2, fill = CramerV)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(fill = "Cramer's V") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Transformation de la matrice des p-values en format long pour ggplot
chi_square_df <- as.data.frame(as.table(chi_square_results))
colnames(chi_square_df) <- c("Variable1", "Variable2", "PValue")

####Heatmap des p-values avec ggplot
ggplot(chi_square_df, aes(x = Variable1, y = Variable2, fill = PValue)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = "P-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Analyse des variables qualitatives
##Tableaux de fréquences
freq_sexe <- table(Ecmoss_2006$sexe_r)
prop_sexe <- prop.table(freq_sexe) * 100
tab_sexe <- data.frame(
  Effectifs = as.numeric(freq_sexe),
  Pourcentage = as.numeric(prop_sexe)
)
row.names(tab_sexe) <- names(freq_sexe)

freq_statut <- table(Ecmoss_2006$statut)
prop_statut <- prop.table(freq_statut) * 100
tab_statut <- data.frame(
  Effectifs = as.numeric(freq_statut),
  Pourcentage = as.numeric(prop_statut)
)
row.names(tab_statut) <- names(freq_statut)


## Graphiques pour variables qualitatives
p1 <- ggplot(Ecmoss_2006, aes(x = sexe_r, fill = sexe_r)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution du sexe", x = "Sexe", y = "Effectifs") +
  theme(legend.position = "none")

p2 <- ggplot(Ecmoss_2006, aes(x = statut, fill = statut)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution du statut", x = "Statut", y = "Effectifs") +
  theme(legend.position = "none")


############Analyse des variables quantitatives########################
## Statistiques descriptives
stats_quanti <- data.frame(
  Variable = c("age_r", "qs3_3_r", "qs24_r", "nbheur"),
  Minimum = sapply(Ecmoss_2006[c("age_r", "qs3_3_r", "qs24_r", "nbheur")], min, na.rm = TRUE),
  Maximum = sapply(Ecmoss_2006[c("age_r", "qs3_3_r", "qs24_r", "nbheur")], max, na.rm = TRUE),
  Moyenne = sapply(Ecmoss_2006[c("age_r", "qs3_3_r", "qs24_r", "nbheur")], mean, na.rm = TRUE),
  Mediane = sapply(Ecmoss_2006[c("age_r", "qs3_3_r", "qs24_r", "nbheur")], median, na.rm = TRUE),
  Ecart_type = sapply(Ecmoss_2006[c("age_r", "qs3_3_r", "qs24_r", "nbheur")], sd, na.rm = TRUE)
)

## Graphiques pour variables quantitatives
p3 <- ggplot(Ecmoss_2006, aes(x = age_r)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  theme_minimal() +
  labs(title = "Distribution de l'âge", x = "Age", y = "Densité")

p4 <- ggplot(Ecmoss_2006, aes(x = qs3_3_r)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  theme_minimal() +
  labs(title = "Distribution des primes", x = "Primes", y = "Densité")

p5 <- ggplot(Ecmoss_2006, aes(x = qs24_r)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  theme_minimal() +
  labs(title = "Distribution de l'ancienneté", x = "Ancienneté", y = "Densité")

p6 <- ggplot(Ecmoss_2006, aes(x = nbheur)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  theme_minimal() +
  labs(title = "Distribution des heures travaillées", x = "Heures travaillées", y = "Densité")

# Affichage des résultats
print("Analyse des variables qualitatives :")
print("Table de fréquence pour sexe_r :")
print(tab_sexe)
print("\nTable de fréquence pour statut :")
print(tab_statut)

print("\nStatistiques descriptives des variables quantitatives :")
print(stats_quanti)

# Affichage des graphiques
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, p5, p6, ncol = 2)

# #########Vérifier les doublons

nrow(Ecmoss_2006)
sum(duplicated(Ecmoss_2006))

#  Tableau de contingence 
tab <- table(Ecmoss_2006$sexe_r, Ecmoss_2006$statut)
print(tab)

#pourcentage de cadres/non-cadres DANS chaque sexe
tab_pct <- prop.table(tab, margin = 1) * 100  
print(round(tab_pct, 2))  

tab_complet <- ftable(addmargins(tab))  
print(tab_complet)

#Test du chi-deux pour tester l'indépendance
chi_test <- chisq.test(tab)
print(chi_test)

# Statistiques descriptives des primes par sexe
aggregate(qs3_3_r ~ sexe_r, data = Ecmoss_2006, 
          FUN = function(x) c(moyenne = mean(x, na.rm = TRUE),
                              mediane = median(x, na.rm = TRUE),
                              ecart_type = sd(x, na.rm = TRUE)))
library(dplyr)

Ecmoss_2006
  group_by(sexe_r)
  summarise(
    moyenne_prime = mean(qs3_3_r, na.rm = TRUE),
    mediane_prime = median(qs3_3_r, na.rm = TRUE),
    ecart_type_prime = sd(qs3_3_r, na.rm = TRUE)
  )

t.test(qs3_3_r ~ sexe_r, data = Ecmoss_2006)

library(ggplot2)
ggplot(Ecmoss_2006, aes(x = sexe_r, y = qs3_3_r, fill = sexe_r)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution des primes selon le sexe",
       x = "Sexe",
       y = "Montant des primes") +
  theme(legend.position = "none")

## Statistiques descriptives du salaire net
summary(Ecmoss_2006$s_net)
describe(Ecmoss_2006$s_net)
hist(Ecmoss_2006$s_net, 
     main = "Distribution du salaire net",
     xlab = "Salaire net",
     breaks = 50)

boxplot(Ecmoss_2006$s_net,
        main = "Boîte à moustaches du salaire net",
        ylab = "Salaire net")

boxplot(s_net ~ statut, 
        data = Ecmoss_2006,
        main = "Salaire net selon le statut",
        ylab = "Salaire net")

boxplot(s_net ~ sexe_r, 
        data = Ecmoss_2006,
        main = "Salaire net selon le sexe",
        ylab = "Salaire net")

# Coefficient de variation
cv <- sd(Ecmoss_2006$s_net, na.rm = TRUE) / mean(Ecmoss_2006$s_net, na.rm = TRUE) * 100
print(paste("Coefficient de variation:", round(cv, 2), "%"))


####### Estimation de notre modèle ##########

colSums(is.na(Ecmoss_2006[c("s_net", "age_r", "nbheur", "qs24_r", "qs3_3_r", "statut", "sexe_r")]))


na_proportions <- colMeans(is.na(Ecmoss_2006[c("s_net", "age_r", "nbheur", "qs24_r", "qs3_3_r", "statut", "sexe_r")])) * 100
print("Pourcentage de valeurs manquantes par variable :")
print(round(na_proportions, 2))

aggr(Ecmoss_2006[c("s_net", "age_r", "nbheur", "qs24_r", "qs3_3_r", "statut", "sexe_r")],
     plot = TRUE)

ecmoss_clean <- na.omit(Ecmoss_2006[c("s_net", "age_r", "nbheur", "qs24_r", "qs3_3_r", "statut", "sexe_r")])

#Vérifier de la taille du jeu de données avant et après
print("Nombre d'observations avant nettoyage:")
nrow(Ecmoss_2006)
print("Nombre d'observations après nettoyage:")
nrow(ecmoss_clean)

# Estimation de notre modèle

model_net <- lm(log(s_net + 1) ~ age_r + log(nbheur+ 1) + log(qs24_r + 1) + log(qs3_3_r + 1) + statut + sexe_r, 
                data = Ecmoss_2006)

summary(model_net)


### Tests des hypothèses fondamentales des MCO, Normalité des résidus
install.packages("nortest")
library(nortest)

ad.test(residuals(model_net))

# Homoscédasticité
library(lmtest)
bptest(model_net)

# Absence de multicolinéarité
vif(model_net)

# Graphiques de diagnostic
par(mfrow=c(2,2))
plot(model_net)

# Intervalles de confiance
confint(model_net)

# Prédictions : même si pas demander dans le projet

predictions <- predict(model_net, Ecmoss_2006)

predictions_salaire <- exp(predictions)

rmse <- sqrt(mean((Ecmoss_2006$s_net - predictions_salaire)^2, na.rm = TRUE))
mae <- mean(abs(Ecmoss_2006$s_net - predictions_salaire), na.rm = TRUE)

print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))

pred_int <- predict(model_net, Ecmoss_2006, interval = "prediction", level = 0.95)
pred_int_salaire <- exp(pred_int)

resultats_pred <- data.frame(
  salaire_reel = Ecmoss_2006$s_net,
  prediction = predictions_salaire,
  borne_inf = pred_int_salaire[,"lwr"],
  borne_sup = pred_int_salaire[,"upr"]
)

head(resultats_pred)

ggplot(resultats_pred, aes(x = salaire_reel, y = prediction)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Salaires réels", y = "Salaires prédits") +
  theme_minimal()



###################################################################################################################################################################################


##########Partie 2 du projet : CONSTRUCTION D’UNE RELATION BINAIRE ET FERMETURE TRANSITIVE#########

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)

arbres <- read.csv("sgl-arbres-urbains-wgs84.csv", header = TRUE, sep = ",", 
                   stringsAsFactors = FALSE)

str(arbres)
head(arbres)
attach(arbres)
any(is.na(hauteur)) # False, donc pas besoin de remplacer les valeur manquantes
any(is.na(diametre)) # False, donc pas besoin de remplacer les valeur manquantes

# Analyse descriptive
analyse_descriptive <- function(data) {
  # Statistiques descriptives
  cat("Résumé statistique des variables numériques :\n")
  print(summary(data[c("hauteur", "diametre")]))
  
  # Écarts-types
  cat("\nÉcarts-types :\n")
  cat("Hauteur :", sd(data$hauteur), "mètres\n")
  cat("Diamètre :", sd(data$diametre), "cm\n")
}

# Visualisation des distributions
visualiser_distributions <- function(data) {
  # Distribution de la hauteur
  p1 <- ggplot(data, aes(x = hauteur)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = "Distribution des hauteurs",
         x = "Hauteur (m)",
         y = "Fréquence") +
    theme_minimal()
  
  # Distribution du diamètre
  p2 <- ggplot(data, aes(x = diametre)) +
    geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
    labs(title = "Distribution des diamètres",
         x = "Diamètre (cm)",
         y = "Fréquence") +
    theme_minimal()
  
  # Corrélation hauteur-diamètre
  p3 <- ggplot(data, aes(x = hauteur, y = diametre)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = "Corrélation Hauteur-Diamètre",
         x = "Hauteur (m)",
         y = "Diamètre (cm)") +
    theme_minimal()
  
  # Affichage des trois graphiques
  grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)
}

# Exécution des analyses
analyse_descriptive(arbres)
visualiser_distributions(arbres)

# Les seuils
analyser_differences <- function(data) {
  # Calcul des différences directionnelles (non absolues)
  differences_hauteur <- outer(data$hauteur, data$hauteur, `-`)
  differences_diametre <- outer(data$diametre, data$diametre, `-`)
  
  # Pour la relation de proximité I (différences absolues)
  diff_h_abs <- abs(differences_hauteur[upper.tri(differences_hauteur)])
  diff_d_abs <- abs(differences_diametre[upper.tri(differences_diametre)])
  
  # Pour la relation d'éloignement P (différences positives uniquement)
  diff_h_pos <- differences_hauteur[differences_hauteur > 0]
  diff_d_pos <- differences_diametre[differences_diametre > 0]
  
  # Calcul des quantiles pour les deux types de relations
  cat("Analyse pour la relation de proximité I :\n")
  cat("Quartiles des différences absolues de hauteur (m):\n")
  print(quantile(diff_h_abs, probs = c(0.25, 0.5, 0.75)))
  cat("\nQuartiles des différences absolues de diamètre (cm):\n")
  print(quantile(diff_d_abs, probs = c(0.25, 0.5, 0.75)))
  
  cat("\nAnalyse pour la relation d'éloignement P :\n")
  cat("Quartiles des différences positives de hauteur (m):\n")
  print(quantile(diff_h_pos, probs = c(0.25, 0.5, 0.75)))
  cat("\nQuartiles des différences positives de diamètre (cm):\n")
  print(quantile(diff_d_pos, probs = c(0.25, 0.5, 0.75)))
  
  return(list(
    proximite = list(hauteur = quantile(diff_h_abs, probs = c(0.25, 0.5, 0.75)),
                     diametre = quantile(diff_d_abs, probs = c(0.25, 0.5, 0.75))),
    eloignement = list(hauteur = quantile(diff_h_pos, probs = c(0.25, 0.5, 0.75)),
                       diametre = quantile(diff_d_pos, probs = c(0.25, 0.5, 0.75)))
  ))
}

# Exécution de l'analyse
resultats <- analyser_differences(arbres)

###########

valider_relations_modifiee <- function(data, seuils_prox, seuils_eloign) {
  # Décomposer les seuils
  s1_I <- seuils_prox$hauteur
  s2_I <- seuils_prox$diametre
  s1_P <- seuils_eloign$hauteur
  s2_P <- seuils_eloign$diametre
  
  # Calculer les matrices de différences
  differences_hauteur <- outer(data$hauteur, data$hauteur, `-`)
  differences_diametre <- outer(data$diametre, data$diametre, `-`)
  
  # Identifier les relations de proximité (I)
  proximite <- (abs(differences_hauteur) <= s1_I & 
                  abs(differences_diametre) <= s2_I)
  
  # Identifier les relations d'éloignement (P)
  eloignement <- (differences_hauteur >= s1_P & 
                    differences_diametre >= s2_P)
  
  # Calculer les statistiques
  n_arbres <- nrow(data)
  n_paires <- (n_arbres * (n_arbres - 1)) / 2
  
  # Compter les relations
  n_proximite <- sum(proximite) / 2
  n_eloignement <- sum(eloignement) / 2
  
  # Afficher les résultats détaillés
  cat("Validation des seuils choisis :\n\n")
  
  cat("Seuils de proximité (I) :\n")
  cat("s₁_I (hauteur) =", s1_I, "mètres\n")
  cat("s₂_I (diamètre) =", s2_I, "cm\n\n")
  
  cat("Seuils d'éloignement (P) :\n")
  cat("s₁_P (hauteur) =", s1_P, "mètres\n")
  cat("s₂_P (diamètre) =", s2_P, "cm\n\n")
  
  cat("Statistiques globales :\n")
  cat("Nombre total de paires d'arbres :", n_paires, "\n\n")
  
  cat("Relations de proximité (I) :\n")
  cat("Nombre de paires proches :", n_proximite, "\n")
  cat("Pourcentage :", round((n_proximite/n_paires)*100, 2), "%\n\n")
  
  cat("Relations d'éloignement (P) :\n")
  cat("Nombre de paires éloignées :", n_eloignement, "\n")
  cat("Pourcentage :", round((n_eloignement/n_paires)*100, 2), "%\n\n")
  
  return(list(proximite = proximite, eloignement = eloignement))
}

# Définir les seuils
seuils_proximite <- list(hauteur = 100, diametre = 6.37)
seuils_eloignement <- list(hauteur = 400, diametre = 35.01)

# Exécuter la validation
resultats_validation <- valider_relations_modifiee(arbres, seuils_proximite, seuils_eloignement)

########
construire_relation_Q <- function(data, seuils_prox, seuils_eloign) {
  # Calcul des matrices de différences
  differences_hauteur <- outer(data$hauteur, data$hauteur, `-`)
  differences_diametre <- outer(data$diametre, data$diametre, `-`)
  
  # Construction de la relation IQ (proximité)
  IQ <- (abs(differences_hauteur) <= seuils_prox$hauteur & 
           abs(differences_diametre) <= seuils_prox$diametre)
  
  # Construction de la relation PQ (éloignement)
  PQ <- (differences_hauteur >= seuils_eloign$hauteur & 
           differences_diametre >= seuils_eloign$diametre)
  
  # Construction de la relation Q = IQ + PQ
  Q <- IQ + PQ
  
  # Création d'une liste contenant toutes les matrices
  relations <- list(
    Q = Q,
    IQ = IQ,
    PQ = PQ
  )
  
  # Analyse des propriétés de la relation Q
  n_arbres <- nrow(data)
  cat("Propriétés de la relation binaire Q :\n")
  cat("Dimension de la matrice :", n_arbres, "x", n_arbres, "\n")
  cat("Nombre total de relations (IQ + PQ) :", sum(Q), "\n")
  cat("Nombre de relations de proximité (IQ) :", sum(IQ), "\n")
  cat("Nombre de relations d'éloignement (PQ) :", sum(PQ), "\n")
  
  return(relations)
}

# Application avec nos seuils validés
relation_Q <- construire_relation_Q(arbres, seuils_proximite, seuils_eloignement)


visualiser_relation_binaire <- function(relation, titre) {
  # Conversion de la matrice en format long pour ggplot
  n <- nrow(relation)
  donnees_plot <- expand.grid(x = 1:n, y = 1:n)
  donnees_plot$valeur <- as.numeric(as.vector(relation))  # Conversion en numérique
  
  # Création du graphique
  ggplot(donnees_plot, aes(x = x, y = y, fill = factor(valeur))) +
    geom_tile() +
    scale_fill_manual(values = c("blue", "green"),
                      labels = c("Absence de relation", "Présence de relation")) +
    labs(title = titre,
         x = " Ai",
         y = "Aj",
         fill = "Relation") +
    theme_minimal() +
    theme(legend.position = "right")
}

# Visualisation des différentes relations
p1 <- visualiser_relation_binaire(relation_Q$Q, "Relation binaire Q")
p2 <- visualiser_relation_binaire(relation_Q$IQ, "Relation de proximité IQ")
p3 <- visualiser_relation_binaire(relation_Q$PQ, "Relation d'éloignement PQ")

# Affichage des trois visualisations
grid.arrange(p1, p2, p3, ncol = 2)


# Fermeture
calculer_fermeture_transitive <- function(relation) {
  # Copie de la matrice initiale
  n <- nrow(relation)
  fermeture <- relation
  
  # Algorithme de Warshall
  for(k in 1:n) {
    for(i in 1:n) {
      for(j in 1:n) {
        fermeture[i,j] <- fermeture[i,j] || 
          (fermeture[i,k] && fermeture[k,j])
      }
    }
  }
  
  # Analyse des propriétés de la fermeture transitive
  cat("Propriétés de la fermeture transitive :\n")
  cat("Nombre de relations dans Q initiale :", sum(relation), "\n")
  cat("Nombre de relations dans la fermeture transitive :", sum(fermeture), "\n")
  cat("Nombre de nouvelles relations ajoutées :", 
      sum(fermeture) - sum(relation), "\n")
  
  return(fermeture)
}

# Application à notre relation Q
Q_star <- calculer_fermeture_transitive(relation_Q$Q)

# Si nécessaire, conversion préalable en matrice logique
Q_logique <- as.matrix(relation_Q$Q) 
Q_star <- calculer_fermeture_transitive(Q_logique)

#verifier_proprietes_fermeture <- function(Q_initiale, Q_star) {
  n <- nrow(Q_initiale)
  
# Vérification de la réflexivité
reflexive <- all(diag(Q_star) == TRUE)
  
# Vérification de la transitivité
transitive <- TRUE
 
#######

# Vérification et traitement des données avant visualisation
visualiser_fermeture <- function(Q_initiale, Q_star) {
  # Vérification des données
  if(any(is.na(Q_initiale)) || any(is.na(Q_star))) {
    cat("Détection de valeurs NA dans les matrices. Nettoyage des données...\n")
    Q_initiale[is.na(Q_initiale)] <- FALSE
    Q_star[is.na(Q_star)] <- FALSE
  }
  
  # Conversion en matrices numériques
  Q_initiale <- matrix(as.numeric(Q_initiale), nrow = nrow(Q_initiale))
  Q_star <- matrix(as.numeric(Q_star), nrow = nrow(Q_star))
  
  # Préparation des données pour la visualisation
  n <- nrow(Q_initiale)
  donnees_Q <- expand.grid(x = 1:n, y = 1:n)
  donnees_Q$valeur <- as.vector(Q_initiale)
  
  donnees_Qstar <- expand.grid(x = 1:n, y = 1:n)
  donnees_Qstar$valeur <- as.vector(Q_star)
  
  # Création des deux graphiques
  p1 <- ggplot(donnees_Q, aes(x = x, y = y, fill = factor(valeur))) +
    geom_tile() +
    scale_fill_manual(values = c("blue", "green")) +
    labs(title = "Relation binaire initiale Q",
         x = "Ai",
         y = "Aj") +
    theme_minimal()
  
  p2 <- ggplot(donnees_Qstar, aes(x = x, y = y, fill = factor(valeur))) +
    geom_tile() +
    scale_fill_manual(values = c("blue", "green")) +
    labs(title = "Fermeture transitive Q*",
         x = "Ai",
         y = "Aj") +
    theme_minimal()
  
  # Affichage côte à côte
  grid.arrange(p1, p2, ncol = 2)
}


# Application de la visualisation avec vérification
visualiser_fermeture(relation_Q$Q, Q_star)


################################################################################################

###############Partie 3 du projet : Optimisation sous R#####################

#Package principal pour l'optimisation non linéaire
install.packages("quadprog")
library(quadprog)

#Méthodes d'optimisation additionnelles
install.packages("optimx")
library(optimx)

#Calculs matriciels
install.packages("Matrix")
library(Matrix)

#graphiques 3D interactifs
install.packages("rgl")           
install.packages("plot3D")      
install.packages("plotly")      

library(rgl)
library(plot3D)
library(plotly)


# Définition de la matrice quadratique Q <=> D
D <- matrix(c(2, 0, 0,
              0, 2, 0,
              0, 0, 2), nrow=3, ncol=3)

# Définition du vecteur d (termes linéaires avec signes inversés)
dvec <- c(2, 3, -1)

# Définition de la matrice des contraintes Amat <=> A^T
Amat <- matrix(c(-1,  1,  1,  0,  0,  
                 -2,  1,  0,  1,  0,   
                 -1,  0,  0,  0,  1),   
               nrow=3, ncol=5, byrow=TRUE)

# Définition du vecteur des bornes
bvec <- c(-4, 1, 0, 0, 0)

######test de faisabilité#######
# Fonction de vérification du problème d'optimisation
verify_optimization_problem <- function(D, dvec, Amat, bvec) {
  # Test de convexité via les valeurs propres
  eigenvalues <- eigen(D)$values
  convexity_status <- all(eigenvalues > 0)
  
  # Affichage du résultat de convexité
  if (convexity_status) {
    cat("Test de convexité :\n")
    cat("- La fonction objectif est convexe (matrice D définie positive)\n")
    cat("- Valeurs propres :", eigenvalues, "\n\n")
  } else {
    cat("Test de convexité :\n")
    cat("- La fonction objectif n'est pas convexe\n")
    cat("- Valeurs propres problématiques :", eigenvalues[eigenvalues <= 0], "\n\n")
    return(FALSE)
  }
  
  # Test de faisabilité avec une matrice définie positive minimale
  D_test <- diag(1e-6, nrow(D))  # Matrice diagonale avec de petites valeurs positives
  dvec_test <- rep(0, length(dvec))
  
  # Tentative de résolution pour tester la faisabilité
  tryCatch({
    test_solution <- solve.QP(D_test, dvec_test, Amat, bvec, meq=0)
    if (!is.null(test_solution$solution)) {
      cat("Test de faisabilité :\n")
      cat("- Les contraintes sont faisables\n")
      cat("- Point admissible trouvé :", test_solution$solution, "\n\n")
      
      cat("Conclusion : Le problème est bien posé et peut être résolu.\n")
      return(TRUE)
    }
  }, error = function(e) {
    cat("Test de faisabilité :\n")
    cat("- Les contraintes sont infaisables\n")
    cat("- Erreur détectée :", e$message, "\n\n")
    
    cat("Conclusion : Le problème ne peut pas être résolu.\n")
    return(FALSE)
  })
}

# Utilisation de la fonction avec nos matrices définies précédemment
problem_status <- verify_optimization_problem(D, dvec, Amat, bvec)


#######################################
# Résolution du problème d'optimisation
solution <- solve.QP(Dmat=D,
                     dvec=dvec,
                     Amat=Amat,
                     bvec=bvec,
                     meq=0)  # meq=0 car toutes nos contraintes sont des inégalités

# Extraction et affichage des résultats
x_optimal <- solution$solution
valeur_optimale <- solution$value

# Affichage des résultats de manière structurée
cat("Solution optimale :\n")
cat("x =", x_optimal[1], "\n")
cat("y =", x_optimal[2], "\n")
cat("z =", x_optimal[3], "\n")
cat("\nValeur optimale de la fonction objectif :", valeur_optimale, "\n")

#################
# Vérification des conditions KKT
check_KKT <- function(x_optimal, lambda) {
  # Gradient de la fonction objectif au point optimal
  grad_f <- c(2*x_optimal[1] - 2,  # dérivée par rapport à x
              2*x_optimal[2] - 3,  # dérivée par rapport à y
              2*x_optimal[3] + 1)  # dérivée par rapport à z
  
  # Vérification des conditions d'optimalité
  print("Gradient de la fonction objectif:")
  print(grad_f)
  
  # Vérification des contraintes
  c1 <- x_optimal[1] + 2*x_optimal[2] + x_optimal[3] <= 4
  c2 <- x_optimal[1] + x_optimal[2] >= 1
  c3 <- all(x_optimal >= 0)
  
  print("Vérification des contraintes:")
  print(paste("Contrainte 1:", c1))
  print(paste("Contrainte 2:", c2))
  print(paste("Non-négativité:", c3))
}

# Application de la vérification
check_KKT(c(1.00, 1.50, 0.00), NULL)


# Vérification par perturbation
check_perturbation <- function(x_optimal, epsilon = 1e-6) {
  f_optimal <- x_optimal[1]^2 + x_optimal[2]^2 + x_optimal[3]^2 - 
    2*x_optimal[1] - 3*x_optimal[2] + x_optimal[3]
  
  # Test des points voisins
  for(i in 1:3) {
    x_test <- x_optimal
    x_test[i] <- x_test[i] + epsilon
    
    f_test <- x_test[1]^2 + x_test[2]^2 + x_test[3]^2 - 
      2*x_test[1] - 3*x_test[2] + x_test[3]
    
    if(f_test < f_optimal) {
      print(paste("Point non optimal: meilleure valeur trouvée en perturbant x", i))
      return(FALSE)
    }
  }
  print("Le point semble être un minimum local")
  return(TRUE)
}

# Application de la vérification
check_perturbation(c(1.00, 1.50, 0.00))


##################Visualisation
library(RColorBrewer)


# Paramètres de la grille avec une résolution plus élevée
x_grid <- seq(0, 2, length=150)  # Augmentation de la résolution
y_grid <- seq(0, 2, length=150)

# Matrice des valeurs avec une fonction vectorisée optimisée
z_matrix <- outer(x_grid, y_grid, function(x, y) {
  x^2 + y^2 - 2*x - 3*y
})

# Création du graphique amélioré
fig <- plot_ly() %>%
  # Surface avec un rendu plus sophistiqué
  add_surface(
    x = x_grid,
    y = y_grid,
    z = z_matrix,
    opacity = 0.85,
    colorscale = "Viridis",  # Palette de couleurs professionnelle
    name = "Surface d'optimisation",
    showscale = TRUE,
    contours = list(
      x = list(show = TRUE, width = 1.5, color = "white", usecolormap = TRUE),
      y = list(show = TRUE, width = 1.5, color = "white", usecolormap = TRUE),
      z = list(show = TRUE, width = 1.5, color = "white", usecolormap = TRUE)
    ),
    lighting = list(
      ambient = 0.8,
      diffuse = 0.9,
      specular = 0.8,
      roughness = 0.4
    )
  ) %>%
  # Point optimal avec un design plus visible
  add_trace(
    type = "scatter3d",
    x = c(x_optimal[1]),
    y = c(x_optimal[2]),
    z = c(x_optimal[3]),
    mode = "markers+text",
    marker = list(
      size = 12,
      color = "red",
      symbol = "diamond",
      line = list(color = "white", width = 2)
    ),
    name = "Point optimal",
    text = sprintf("(%.2f, %.2f, %.2f)", x_optimal[1], x_optimal[2], x_optimal[3]),
    textposition = "top center"
  ) %>%
  # Configuration avancée du layout
  layout(
    title = list(
      text = "Visualisation de l'Optimisation Quadratique",
      font = list(size = 20, family = "Arial", color = "darkblue")
    ),
    scene = list(
      camera = list(
        eye = list(x = 2, y = 2, z = 1.5),
        center = list(x = 0.5, y = 0.5, z = 0)
      ),
      xaxis = list(
        title = list(text = "x", font = list(size = 14)),
        gridcolor = "lightgray",
        showgrid = TRUE,
        zeroline = TRUE,
        zerolinecolor = "darkgray"
      ),
      yaxis = list(
        title = list(text = "y", font = list(size = 14)),
        gridcolor = "lightgray",
        showgrid = TRUE,
        zeroline = TRUE
      ),
      zaxis = list(
        title = list(text = "Valeur de la fonction", font = list(size = 14)),
        gridcolor = "lightgray",
        showgrid = TRUE,
        zeroline = TRUE
      ),
      aspectmode = "cube",
      bgcolor = "white"
    ),
    showlegend = TRUE,
    legend = list(
      x = 0.85,
      y = 0.95,
      bgcolor = "rgba(255, 255, 255, 0.9)",
      bordercolor = "darkgray",
      borderwidth = 1,
      font = list(size = 12)
    )
  )

# Affichage du graphique
fig


#################################################################################################################################
setwd("C:/Users/monte/OneDrive/Desktop/projet_math_R")
list.files()

##############Partie 4 du projet : SOUS-GRAPHE RECOUVRANT OPTIMAL DU METRO PARISIEN############

library(igraph)
library(tidygraph)
library(ggraph)     
library(ggplot2)
library(viridis)

##chargeons nos données

metro_matrix <- as.matrix(read.csv("metro_distance_matrix_updated.csv", row.names = 1))
View(metro_matrix)

# Vérifications
dim(metro_matrix)
sum(is.na(metro_matrix))
is_symmetric <- all(metro_matrix == t(metro_matrix))
print(paste("La matrice est-elle symétrique ?", is_symmetric))

unique_values <- sort(unique(as.vector(as.matrix(metro_matrix))))
print("Valeurs uniques dans la matrice :")
print(unique_values)

diag_values <- diag(as.matrix(metro_matrix))
print("Vérification des valeurs de la diagonale :")
print(table(diag_values))

##CONSTRUCTION DU GRAPHE INITIAL (PONDÉRÉ)

num_stations  <- nrow(metro_matrix)
station_names <- rownames(metro_matrix)

edges   <- c()
weights <- c()

for (i in 1:(num_stations - 1)) {
  for (j in (i + 1):num_stations) {
    edges   <- rbind(edges, c(station_names[i], station_names[j]))
    weights <- c(weights, metro_matrix[i, j])
  }
}

# Graphe pondéré
metro_graph <- graph_from_edgelist(as.matrix(edges), directed = FALSE)
E(metro_graph)$weight <- weights

if (is.connected(metro_graph)) {
  print("Le graphe est connecté.")
} else {
  print("Le graphe n'est pas connecté. Certaines stations sont isolées.")
}

################################################################################
## 1- GRAPHE PONDÉRÉ COMPLET

g_tbl_full <- as_tbl_graph(metro_graph)

plot_pondere <- ggraph(g_tbl_full, layout = "fr") +
  geom_edge_link(aes(color = weight), alpha = 0.8) +
  scale_edge_color_viridis(option = "plasma", begin = 0.1, end = 0.9, name = "Poids") +
  geom_node_point(size = 3, color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5, color = "black") +
  theme_graph(base_family = "Helvetica") +
  labs(
    title = "1) Graphe pondéré complet du métro parisien",
    subtitle = "Visualisation ggraph (Fruchterman-Reingold)"
  ) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10),
    legend.position = "right"
  )

print(plot_pondere)
ggsave(
  filename = "01_graphe_pondere_complet.pdf",
  plot     = plot_pondere,
  width    = 12,
  height   = 8,
  dpi      = 300
)

################################################################################
##CALCUL DU MST (COMPLET)

mst_graph <- mst(metro_graph)

num_nodes <- vcount(mst_graph)
num_edges <- ecount(mst_graph)

print(paste("Nombre de nœuds MST :", num_nodes))
print(paste("Nombre d'arêtes MST :", num_edges))

if (num_edges == num_nodes - 1) {
  print("MST valide : connexe et acyclique.")
} else {
  print("Le MST peut contenir des cycles ou être incomplet.")
}

if (is.connected(mst_graph)) {
  print("Le MST est connecté.")
} else {
  print("Le MST n'est pas connecté.")
}

################################################################################
## 2- MST COMPLET (AVEC JUSSIEU ET LA DÉFENSE)

mst_tbl <- as_tbl_graph(mst_graph)

# Mise en évidence de "Jussieu" et "La Défense (Grande Arche)"
station_focus <- c("Jussieu", "La Défense (Grande Arche)")

mst_tbl <- mst_tbl %>%
  activate(nodes) %>%
  mutate(
    focus_station = ifelse(name %in% station_focus, TRUE, FALSE),
    color_node    = ifelse(focus_station, "red", "orange"),
    size_node     = ifelse(focus_station, 5, 3)
  )

plot_mst <- ggraph(mst_tbl, layout = "fr") +
  geom_edge_link(aes(color = weight), alpha = 0.8) +
  scale_edge_color_viridis(option = "plasma", begin = 0.1, end = 0.9, name = "Poids") +
  geom_node_point(
    aes(color = color_node, size = size_node),
    shape  = 21,
    fill   = "white",
    stroke = 1
  ) +
  scale_size_continuous(range = c(2, 6), guide = FALSE) +
  scale_color_identity(guide = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5, color = "black") +
  theme_graph(base_family = "Helvetica") +
  labs(
    title    = "2) MST du métro parisien (complet)",
    subtitle = "Mise en évidence de Jussieu et La Défense (Grande Arche)"
  ) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10),
    legend.position = "right"
  )

print(plot_mst)
ggsave(
  filename = "02_mst_complet.pdf",
  plot     = plot_mst,
  width    = 12,
  height   = 8,
  dpi      = 300
)

################################################################################
## 3- MST SANS "JUSSIEU"

if ("Jussieu" %in% V(metro_graph)$name) {
  
  graph_without_jussieu <- delete_vertices(metro_graph, "Jussieu")
  mst_without_jussieu   <- mst(graph_without_jussieu)
  
  mst_tbl_jussieu <- as_tbl_graph(mst_without_jussieu) %>%
    activate(nodes) %>%
    mutate(name = as.character(name))  # Évite d'éventuels soucis de factor
  
  plot_mst_jussieu <- ggraph(mst_tbl_jussieu, layout = "fr") +
    geom_edge_link(aes(color = weight), alpha = 0.8) +
    scale_edge_color_viridis(option = "plasma", begin = 0.1, end = 0.9, name = "Poids") +
    geom_node_point(size = 3, color = "darkgreen") +
    geom_node_text(aes(label = name), repel = TRUE, size = 2.5, color = "black") +
    theme_graph(base_family = "Helvetica") +
    labs(
      title    = "3) MST du métro SANS 'Jussieu'",
      subtitle = "Impact de la suppression de Jussieu"
    ) +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size = 10),
      legend.position = "right"
    )
  
  print(plot_mst_jussieu)
  ggsave(
    filename = "03_mst_sans_jussieu.pdf",
    plot     = plot_mst_jussieu,
    width    = 12,
    height   = 8,
    dpi      = 300
  )
  
} else {
  print("La station 'Jussieu' n'existe pas dans ce graphe.")
}

################################################################################
## 4- MST SANS "LA DÉFENSE (GRANDE ARCHE)"

if ("La Défense (Grande Arche)" %in% V(metro_graph)$name) {
  
  graph_without_defense <- delete_vertices(metro_graph, "La Défense (Grande Arche)")
  mst_without_defense   <- mst(graph_without_defense)
  
  mst_tbl_defense <- as_tbl_graph(mst_without_defense) %>%
    activate(nodes) %>%
    mutate(name = as.character(name))
  
  plot_mst_defense <- ggraph(mst_tbl_defense, layout = "fr") +
    geom_edge_link(aes(color = weight), alpha = 0.8) +
    scale_edge_color_viridis(option = "plasma", begin = 0.1, end = 0.9, name = "Poids") +
    geom_node_point(size = 3, color = "purple") +
    geom_node_text(aes(label = name), repel = TRUE, size = 2.5, color = "black") +
    theme_graph(base_family = "Helvetica") +
    labs(
      title    = "4) MST du métro SANS 'La Défense (Grande Arche)'",
      subtitle = "Impact de la suppression de La Défense"
    ) +
    theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size = 10),
      legend.position = "right"
    )
  
  print(plot_mst_defense)
  ggsave(
    filename = "04_mst_sans_defense.pdf",
    plot     = plot_mst_defense,
    width    = 12,
    height   = 8,
    dpi      = 300
  )
  
} else {
  print("La station 'La Défense (Grande Arche)' n'existe pas dans ce graphe.")
}


##Importance relative de jussieu et de la défense


#Calcul de la centralité d’intermédiarité (betweenness) sur le MST complet
bc_mst <- betweenness(mst_graph, normalized = TRUE)

val_jussieu <- bc_mst["Jussieu"]
val_defense <- bc_mst["La Défense (Grande Arche)"]

cat("Centralité d’intermédiarité (MST) :\n")
cat(" - Jussieu                 :", val_jussieu, "\n")
cat(" - La Défense (Grande Arche) :", val_defense, "\n\n")

#Verdict
if (is.na(val_jussieu) || is.na(val_defense)) {
  cat("Impossible de comparer : 'Jussieu' ou 'La Défense' n'existe pas dans le MST.\n")
} else if (val_jussieu > val_defense) {
  cat("=> Jussieu présente une intermédiarité plus élevée,",
      "elle est donc potentiellement plus critique.\n")
} else if (val_jussieu < val_defense) {
  cat("=> La Défense (Grande Arche) présente une intermédiarité plus élevée,",
      "elle est donc potentiellement plus critique.\n")
} else {
  cat("=> Les deux stations ont la même intermédiarité",
      "et semblent donc jouer un rôle similaire dans le MST.\n")
}


#######################################
##COMPLEMENTAIRE : le closeness

##Calcul de la centralité de proximité sur le MST
closeness_scores <- closeness(mst_graph, normalized = TRUE)

##Mettre les résultats dans un data.frame et trier
stations_importance_closeness <- data.frame(
  Station   = V(mst_graph)$name,
  Closeness = closeness_scores
)

# Tri par ordre décroissant de la proximité
stations_importance_closeness <- stations_importance_closeness[
  order(-stations_importance_closeness$Closeness),
]

##Extraire les scores pour Jussieu et La Défense
jussieu_close <- stations_importance_closeness$Closeness[
  stations_importance_closeness$Station == "Jussieu"
]
defense_close <- stations_importance_closeness$Closeness[
  stations_importance_closeness$Station == "La Défense (Grande Arche)"
]

##Comparaison et écart relatif
difference_relative <- (jussieu_close - defense_close) / 
  max(jussieu_close, defense_close) * 100

##verdict
verdict_closeness <- sprintf("
ANALYSE COMPLEMENTAIRE : CENTRALITE DE PROXIMITE (CLOSENESS)

1. Scores de closeness :
   - Jussieu : %.4f (Rang %d sur %d stations)
   - La Défense : %.4f (Rang %d sur %d stations)

2. Différence relative : %.1f%%

VERDICT : %s

EXPLICATION :
La centralité de proximité mesure la distance moyenne d'une station
au reste du réseau. Plus elle est élevée, plus la station est 'centrale'
dans le sens où elle est rapidement accessible depuis toutes les autres.
",
                             jussieu_close, jussieu_rank, nrow(stations_importance_closeness),
                             defense_close, defense_rank, nrow(stations_importance_closeness),
                             abs(difference_relative),
                             ifelse(is.na(jussieu_close) || is.na(defense_close),
                                    "Impossible de comparer (l'une des deux n'existe pas).",
                                    ifelse(jussieu_close > defense_close,
                                           "Jussieu est plus centrale que La Défense selon la closeness.",
                                           "La Défense est plus centrale que Jussieu selon la closeness."))
)

cat(verdict_closeness)

#Visualisation comparative
# Sélectionner uniquement Jussieu et La Défense
comparison_close <- stations_importance_closeness[
  stations_importance_closeness$Station %in% c("Jussieu","La Défense (Grande Arche)"), 
]

comparison_close$Pourcentage_Closeness <- comparison_close$Closeness * 100

comparison_plot_close <- ggplot(comparison_close, 
                                aes(x = reorder(Station, -Closeness),
                                    y = Pourcentage_Closeness)) +
  geom_bar(stat = "identity", fill = c("#F08A5D", "#B83B5E")) +
  geom_text(aes(label = sprintf("%.2f%%", Pourcentage_Closeness)),
            vjust = -0.5, size = 4) +
  labs(
    title    = "Comparaison Closeness : Jussieu vs. La Défense",
    subtitle = "Centralité de Proximité (pourcentage normalisé)",
    x        = "Station",
    y        = "Closeness (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", size = 14),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )

print(comparison_plot_close)

#Sauve PDF
ggsave(
  filename = "05_importance_closeness_comparaison.pdf",
  plot     = comparison_plot_close,
  width    = 10,
  height   = 6,
  dpi      = 300
)


################################################################################
## FIN DU PROJET

