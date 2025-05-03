Prix_PF <- function(S, vol, K, tau, R) {
  ### Fonction qui calcule le prix d'un portefeuille composé de 4 options d'achat européennes
  #  INPUTS
  #   S    : [scalaire] Prix du sous-jacent (S&P 500)
  #   vol  : [scalaire] Volatilité implicite (VIX utilisé comme approximation constante)
  #   K    : [vecteur] Strikes des options
  #   tau  : [vecteur] Temps à maturité des options (en années)
  #   R    : [vecteur] Taux sans risque interpolés associés à chaque option
  #  OUTPUTS
  #   P    : [scalaire] Prix total du portefeuille d'options
  #  NOTE
  #   o Chaque option est évaluée individuellement avec le modèle de Black-Scholes puis sommée
  
  # Évaluation du prix de chaque option avec Black-Scholes
  P1 <- Black_Scholes(0, S, R[1], vol, K[1], tau[1], "call")
  P2 <- Black_Scholes(0, S, R[2], vol, K[2], tau[2], "call")
  P3 <- Black_Scholes(0, S, R[3], vol, K[3], tau[3], "call")
  P4 <- Black_Scholes(0, S, R[4], vol, K[4], tau[4], "call")
  
  # Somme des prix des 4 options
  P <- P1 + P2 + P3 + P4
  
  # Retourner le prix total du portefeuille
  return(P)
}
