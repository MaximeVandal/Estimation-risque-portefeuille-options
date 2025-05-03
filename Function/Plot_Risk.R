### Fonction de tracé de la distribution du P&L avec VaR et ES
#  INPUTS
#   PL_distribution   : [vecteur] Simulation des profits et pertes du portefeuille sur l'horizon choisi
#   option_book_price  : [scalaire] Prix initial du portefeuille (base pour le calcul des ratios relatifs)
#   title              : [texte] Titre du graphique à afficher
#  OUTPUTS
#   Aucun objet retourné
#  NOTES
#   o Calcule la VaR 95% et l'ES 95% en valeurs absolues et relatives.
#   o Trace un histogramme de la distribution du P&L avec les mesures de risque indiquées.

f_PlotPLWithRiskMeasures <- function(PL_distribution, option_book_price, title = "Distribution P&L du portefeuille (5 jours)") {
  
  ### Étape 1 : Calcul des mesures de risque
  
  # VaR 95% absolue (quantile 5%)
  VaR_95_abs <- quantile(PL_distribution, 0.05)
  
  # ES 95% absolue (moyenne des pertes au-delà de la VaR)
  ES_95_abs <- mean(PL_distribution[PL_distribution <= VaR_95_abs])
  
  # VaR et ES relatives au prix initial
  VaR_95_rel <- VaR_95_abs / option_book_price
  ES_95_rel <- ES_95_abs / option_book_price
  
  ### Étape 2 : Tracé de l'histogramme
  
  hist(PL_distribution,
       xlim = c(-500, 500),
       breaks = 50,
       col = "lightgray",
       border = "black",
       main = title,
       xlab = "P&L en $")
  
  # Ajout des lignes verticales pour VaR et ES
  abline(v = VaR_95_abs, col = "red", lwd = 2)    # VaR en rouge
  abline(v = ES_95_abs, col = "blue", lwd = 2)    # ES en bleu
  
  # Ajout de la légende
  legend("topright",
         legend = c(
           paste0("VaR 95% = ", round(VaR_95_abs, 2), " $"),
           paste0("ES 95%  = ", round(ES_95_abs, 2), " $")
         ),
         col = c("red", "blue"),
         lwd = 2,
         box.lty = 0)
}
