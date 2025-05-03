## Description

Ce projet a été réalisé dans le cadre du cours **MATH60633** à HEC Montréal.  
L’objectif principal est d’analyser la **gestion d’actifs** ou la **gestion des risques** selon le projet choisi, en implémentant une analyse de sensibilité, une évaluation de portefeuille, et des méthodes avancées comme les modèles gaussiens, copules et surfaces de volatilité.

## Objectifs du projet

- Réaliser une analyse de sensibilité sur des portefeuilles long-only.
- Étudier l’impact de l’incertitude des paramètres (volatilité, corrélation).
- Implémenter des modèles gaussiens et copules pour estimer le risque.
- Calculer la VaR et l’ES d’un portefeuille d’options.
- Ajuster et utiliser une surface de volatilité.
- Automatiser l’exécution du projet avec une structure claire et reproductible.

## Structure du projet

- `Code/` → Contient le script R principal du projet `Main.R`  
- `Data/` → Contient les données financières (ex. FTSE 100, options, taux)  
- `Function/` → Contient les scripts R des fonctions utilisées
- `README/` → Consignes et énoncé du projet  
- `Presentation.Rmd` → RMarkdown pour présenter le travail et les résultats

## Méthodologie

1. Télécharger les données (actions FTSE 100, options S&P 500, VIX, taux).
2. Calculer les rendements hebdomadaires et/ou quotidiens.
3. Réaliser une analyse de sensibilité pour différentes tailles de portefeuille.
4. Étudier l’impact de l’incertitude des paramètres via des modèles gaussiens, Student-t et copules.
5. Simuler les prix et volatilités pour calculer la distribution P&L.
6. Estimer la VaR95 et l’ES95 du portefeuille.
7. Ajuster une surface paramétrique de volatilité.
8. Générer un fichier RMarkdown pour présenter les résultats.

## Auteurs

- Faycal Berrada  
- Jérémy Lagacé
- Louis Gengembre
- Maxime Levasseur-Vandal
