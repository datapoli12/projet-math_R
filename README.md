# Projet Mathématiques Avancées - Modélisation et Optimisation

Ce dépôt contient le projet réalisé dans le cadre du cours de mathématiques avancées de la Licence 3 Économie et Mineure Informatique à l'Université Paris 1 Panthéon-Sorbonne, supervisé par le professeur Marc-Arthur Diaye. Ce projet explore des problématiques de nature mathématique, statistique et économétrique.

## Structure du projet

### Partie 1 : Déterminants des Salaires (MCO)
- Analyse des variables explicatives influençant le salaire net des salariés.
- Utilisation des Moindres Carrés Ordinaires (OLS) pour modéliser les relations entre les variables.
* Étapes clés :
  - Sélection et analyse des variables explicatives.
  - Vérification des hypothèses des MCO.
  - Estimation de notre modèle de régression
  - Analyse et interprétation des résultats (coefficients, R²).
  - Prédiction de notre modèle, en ce qui concerne le salaire net
### Méthodologie
- Régression linéaire multiple
- Variables explicatives : âge, heures travaillées, ancienneté, primes, statut, sexe
- Tests de multicolinéarité et validation MCO
- Transformation logarithmique des variables dispersées
- R² ajusté : 0.7639
### Résultats clés
- Impact significatif du statut cadre/non-cadre (-48.8% pour non-cadres)
- Écart salarial hommes/femmes de 9.67%
- Effet positif mais modéré de l'âge (+0.7% pour une année supplémentaire)

### Partie 2 : Construction d'une Relation Binaire et Fermeture Transitive
- Création d'une relation binaire entre 709 arbres de Saint-Germain-en-Laye.
- Définition des notions de proximité et d’éloignement basées sur la hauteur et le diamètre des arbres.
* Étapes clés :
  - Détermination des seuils de proximité et d’éloignement.
  - Construction de notre relation binaire
  - Validation de la fermeture transitive de la relation.
### Caractéristiques
- Relation Q = IQ + PQ sur 709 arbres
- Seuils s1 (hauteur) et s2 (diamètre) définis par quartiles
- 15.93% de paires considérées proches
- 5.58% de paires considérées éloignées


### Partie 3 : Optimisation sous R
- Résolution d’un problème d’optimisation (minimisation) sous contraintes à la main, puis via R.
* Étapes clés :
  - Modélisation mathématique de la fonction objectif et des contraintes.
  - Vérification des conditions d’admissibilité et de convexité.
  - Analyse du point optimal.
### Problème
- Minimisation f(x,y,z) = x²+y²+z²-2x-3y+z
- Contraintes : x+2y+z≤4, x+y≥1, x,y,z≥0
- Solution optimale : P=(1,3/2,0)
### Validation
- Convexité vérifiée (valeurs propres positives)
- Faisabilité des contraintes confirmée
- Point optimal vérifié par KKT


### Partie 4 : Sous-graphe Recouvrant Optimal du Métro Parisien
- Modélisation du réseau du métro parisien en tant que graphe pondéré.
- Utilisation de l’algorithme de Kruskal pour obtenir un sous-graphe couvrant minimal (MST).
- Étapes clés :
  - Construction du graphe pondéré
  - Extraction du MST
  - Analyse de deux hubs en particulier (centralité) : Jussieu et défense
  - Visualisation et interprétation des résultat
### Analyse Comparative Jussieu vs La Défense
- Betweenness : Jussieu (48.44%) > La Défense (0%)
- Closeness : Jussieu (3.80%) > La Défense (1.97%)
- Jussieu plus centrale et stratégique dans le réseau
  

## Contenu du dépôt

- **rapport.pdf** : Rapport complet détaillant les méthodologies, analyses et résultats.
- **scripts/** : Script R utilisé pour ce projet.
- **Les outputs en pdf/** : Facilement consultable, patienter 10 seconde pour visualisation :blush:.
- **README.md** : Présentation synthétique du projet.


## Auteurs 
* Apollinaire Eyitayo Monteiro
* Phédeline François

## Contacts
apollinaire-eyitayo.monteiro@etu.univ-paris1.fr
phedeline.francois@etu.univ-paris1.fr


