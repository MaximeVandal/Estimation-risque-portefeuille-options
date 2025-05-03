Int_lin <- function(Time, maturities, rf_rates) {
  ### Fonction qui calcule le taux d'intérêt interpolé linéairement
  #  INPUTS
  #   Time        : [scalaire] Maturité cible (en fraction d'année, par exemple 20/360)
  #   maturities  : [vecteur] Maturités connues (ordonnées croissantes, en fraction d'année)
  #   rf_rates    : [vecteur] Taux sans risque associés aux maturités connues
  #  OUTPUTS
  #   r_T         : [scalaire] Taux d'intérêt interpolé à la maturité cible
  #  NOTE
  #   o L'interpolation linéaire est effectuée entre les deux maturités encadrant la maturité cible
  
  # Identifier l'indice de la dernière maturité inférieure ou égale à Time
  idx <- max(which(maturities <= Time))
  
  # Extraire les deux points d'interpolation
  T1 <- maturities[idx]
  T2 <- maturities[idx + 1]
  r1 <- rf_rates[idx]
  r2 <- rf_rates[idx + 1]
  
  # Appliquer la formule d'interpolation linéaire
  r_T <- r1 + ((Time - T1) / (T2 - T1)) * (r2 - r1)
  
  # Retourner le taux interpolé
  return(r_T)
}
