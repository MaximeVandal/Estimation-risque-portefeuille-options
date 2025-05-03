# ==== Fonctions de surface de volatilité ====

sigma_model <- function(m, tau, alpha) {
  ### Fonction qui calcule la volatilité implicite paramétrique en fonction du moneyness et de la maturité
  #  INPUTS
  #   m     : [scalaire] Moneyness (K/S)
  #   tau   : [scalaire] Temps à maturité (en années)
  #   alpha : [vecteur] Paramètres de la surface de volatilité
  #  OUTPUTS
  #   sigma : [scalaire] Volatilité implicite modélisée
  #  NOTE
  #   o Modèle polynomial ajusté sur le moneyness et la racine de la maturité
  
  sigma <- alpha[1] + alpha[2] * (m - 1)^2 + alpha[3] * (m - 1)^3 + alpha[4] * sqrt(tau)
  return(sigma)
}

loss_function <- function(alpha, m, tau, sigma_obs) {
  ### Fonction qui calcule la perte absolue totale entre volatilité observée et modélisée
  #  INPUTS
  #   alpha      : [vecteur] Paramètres de la surface de volatilité
  #   m          : [vecteur] Moneyness (K/S) des options observées
  #   tau        : [vecteur] Maturités des options observées
  #   sigma_obs  : [vecteur] Volatilités implicites observées
  #  OUTPUTS
  #   loss       : [scalaire] Somme des erreurs absolues
  #  NOTE
  #   o Sert d'objectif pour l'optimisation du calibrage de la surface
  
  sigma_pred <- sigma_model(m, tau, alpha)
  loss <- sum(abs(sigma_obs - sigma_pred))
  return(loss)
}

sigma_model_w <- function(m, tau, alpha, vix) {
  ### Fonction qui ajuste la volatilité modélisée pour être cohérente avec le niveau du VIX
  #  INPUTS
  #   m     : [scalaire] Moneyness (K/S)
  #   tau   : [scalaire] Temps à maturité (en années)
  #   alpha : [vecteur] Paramètres de la surface de volatilité
  #   vix   : [scalaire] Niveau du VIX observé
  #  OUTPUTS
  #   sigma_corrigée : [scalaire] Volatilité ajustée
  #  NOTE
  #   o Correction pour que la volatilité modélisée soit alignée avec le VIX
  
  sig_mod <- sigma_model(m, tau, alpha)
  sig_mod_w <- sig_mod - (alpha[1] + alpha[4]) + vix
  return(sig_mod_w)
}

Prix_PF_alpha <- function(S, vix, K, tau, alpha, R) {
  ### Fonction qui calcule le prix d'un portefeuille d'options avec une surface de volatilité ajustée
  #  INPUTS
  #   S     : [scalaire] Prix du sous-jacent
  #   vix   : [scalaire] Niveau du VIX utilisé pour ajustement
  #   K     : [vecteur] Strikes des options
  #   tau   : [vecteur] Temps à maturité des options
  #   alpha : [vecteur] Paramètres de la surface de volatilité
  #   R     : [vecteur] Taux sans risque associés aux options
  #  OUTPUTS
  #   P     : [scalaire] Prix total du portefeuille d'options
  #  NOTE
  #   o Utilise Black-Scholes avec volatilité ajustée scénario par scénario
  
  prix_options <- mapply(function(k, t, r) {
    vol_ajustee <- sigma_model_w(k/S, t, alpha, vix)
    Black_Scholes(0, S, r, vol_ajustee, k, t, "call")
  }, K, tau, R)
  
  P <- sum(prix_options)
  return(P)
}