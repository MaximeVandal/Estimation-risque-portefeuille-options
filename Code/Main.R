# ==== Initialisation ====

# Chargement des librairies nécessaires
library(here)
library(PerformanceAnalytics)
library(mvtnorm)
library(copula)
library(MASS)
library(fGarch)
library(plotly)
library(qrmtools)
library(xts)


# Chargement des fonctions personnalisées
source(here("Function", "Int_Lin.R"))
source(here("Function", "Prix_PF.R"))
source(here("Function", "Func_Vol_Surface.R"))
source(here("Function", "Plot_Risk.R"))

# Seed pour la reproductibilité
seedSimul <- 123

# ==== Données ====

# Chargement des données de marché
load(here("Data", "Market.rda"))

# Préparation des données
Rf <- data.frame(Maturity = as.numeric(names(Market[["rf"]])), Rate = Market[["rf"]])
SP500 <- Market$sp500
VIX <- Market$vix
Calls <- as.data.frame(Market$calls)
Puts <- as.data.frame(Market$puts)

# ==== Fixation du prix du portefeuille d'options ====

# Interpolation des taux
r_20 <- Int_lin(20/360, Rf$Maturity, Rf$Rate)
r_40 <- Int_lin(40/360, Rf$Maturity, Rf$Rate)

# Paramètres
T_20 <- 20/250
T_40 <- 40/250
K <- c(1600, 1650, 1750, 1800)
tau <- c(T_20, T_20, T_40, T_40)
R <- c(r_20, r_20, r_40, r_40)

# Prix spot
SP500_T <- last(SP500)
VIX_T <- last(VIX)

# Prix initial du portefeuille
Prix_PF1 <- Prix_PF(SP500_T, VIX_T, K, tau, R)

cat("Prix initial du portefeuille :", round(Prix_PF1,2), "$\n")

# ==== Modèle univarié gaussien (1 facteur de risque) ====

# Calcul des rendements logarithmiques du SP500
rets_SP500_log <- CalculateReturns(SP500, method = "log")[-1,]
mu_SP500 <- mean(rets_SP500_log)
sigma_SP500 <- sd(rets_SP500_log)

# Simulation
set.seed(seedSimul)
n_scenarios <- 10000
n_jours <- 5
scenarios <- matrix(rnorm(n_scenarios * n_jours, mu_SP500, sigma_SP500), nrow = n_scenarios)
SP500_sim <- as.numeric(SP500_T) * exp(rowSums(scenarios))

# Réévaluation du portefeuille
r_15 <- Int_lin(15/360, Rf$Maturity, Rf$Rate)
r_35 <- Int_lin(35/360, Rf$Maturity, Rf$Rate)
T_15 <- 15/250
T_35 <- 35/250
tau <- c(T_15, T_15, T_35, T_35)
R <- c(r_15, r_15, r_35, r_35)

PF_SIM_1 <- Prix_PF(SP500_sim, vol = as.numeric(VIX_T), K = K, tau = tau, R = R)
PnL <- PF_SIM_1 - as.numeric(Prix_PF1)

# Calcul VaR et ES
VaR_95 <- quantile(PnL, 0.05)
ES_95 <- mean(PnL[PnL <= VaR_95])

# Distribution P&L du portefeuille d’options
hist_PnL <- f_PlotPLWithRiskMeasures(PnL, Prix_PF1, title = "P&L - Modèle Univarié")
cat("VaR 95% : ", round(VaR_95, 2), "$")
cat("ES  95% : ", round(ES_95, 2), "$")


# ==== Modèle bivarié gaussien (2 facteurs de risque) ====

# Calcul des rendements logarithmiques quotidiens du VIX
rets_VIX_log <- CalculateReturns(VIX, method = "log")[-1,]

# Estimation conjointe
rets <- cbind(rets_SP500_log, rets_VIX_log)
mu_biv <- colMeans(rets)
Sigma_biv <- cov(rets) * (nrow(rets) - 1) / nrow(rets)

# Simulation
set.seed(seedSimul)
rets_sim_biv <- matrix(0, nrow = n_scenarios, ncol = 2)

for (i in 1:5) {
  rets_sim_biv <- rets_sim_biv + rmvnorm(n = n_scenarios, mean = mu_biv, sigma = Sigma_biv)
}

P_SP500_sim <- as.numeric(SP500_T) * exp(rets_sim_biv[,1])
P_VIX_sim <- as.numeric(VIX_T) * exp(rets_sim_biv[,2])

PF_SIM_2 <- Prix_PF(P_SP500_sim, P_VIX_sim, K, tau, R)
PnL_Biv <- PF_SIM_2 - as.numeric(Prix_PF1)

# Calcul VaR et ES
VaR_95_Biv <- quantile(PnL_Biv, 0.05)
ES_95_Biv <- mean(PnL_Biv[PnL_Biv <= VaR_95_Biv])

# Distribution P&L du portefeuille d’options
hist_PnL_Biv <- f_PlotPLWithRiskMeasures(PnL_Biv, Prix_PF1, title = "P&L - Modèle Bivarié")
cat("VaR 95% : ", round(VaR_95_Biv, 2), "$")
cat("ES  95% : ", round(ES_95_Biv, 2), "$")

# ==== Modèle copule-marginal (Student-t) ====

# Ajustement des marginales
fit1 <- suppressWarnings(fitdistr(rets_SP500_log, densfun = dstd, start = list(mean = 0, sd = 1, nu = 10)))
fit2 <- suppressWarnings(fitdistr(rets_VIX_log, densfun = dstd, start = list(mean = 0, sd = 1, nu = 5)))

theta1 <- fit1$estimate
theta2 <- fit2$estimate

# Construction des pseudo-observations
U1 <- pstd(rets_SP500_log, mean = theta1[1], sd = theta1[2], nu = theta1[3]) 
U2 <- pstd(rets_VIX_log, mean = theta2[1], sd = theta2[2], nu = theta2[3]) 
U <- cbind(U1, U2)

# Ajustement de la copule normale
C <- normalCopula(dim = 2)
fit <- fitCopula(C, data = matrix(U, ncol=2), method = "ml")

# Simulation avec boucle for sur 5 jours
rets1_sim <- numeric(n_scenarios)
rets2_sim <- numeric(n_scenarios)
set.seed(seedSimul)

for (day in 1:5) {
  U_sim <- rCopula(n_scenarios, fit@copula)
  
  rets1_day <- qstd(U_sim[,1], mean = theta1[1], sd = theta1[2], nu = theta1[3])
  rets2_day <- qstd(U_sim[,2], mean = theta2[1], sd = theta2[2], nu = theta2[3])
  
  rets1_sim <- rets1_sim + rets1_day
  rets2_sim <- rets2_sim + rets2_day
}

# Prix simulés
P_SP500_sim_2 <- as.numeric(SP500_T) * exp(rets1_sim)
P_VIX_sim_2 <- as.numeric(VIX_T) * exp(rets2_sim)

# Simulation du portefeuille et distribution des pertes et profits
PF_SIM_3 <- Prix_PF(P_SP500_sim_2, P_VIX_sim_2, K, tau, R)
PnL_Cop <- PF_SIM_3 - as.numeric(Prix_PF1)

# Calcul VaR et ES
VaR_95_Cop <- quantile(PnL_Cop, 0.05)
ES_95_Cop <- mean(PnL_Cop[PnL_Cop <= VaR_95_Cop])

# Affichage
par(mfrow = c(1, 2))

hist(rets1_sim, breaks = 100, main = "S&P 500 (Student-t)", 
     xlab = "Rendement sur 5 jours")
hist(rets2_sim, breaks = 100, main = "VIX (Student-t)", 
     xlab = "Rendement sur 5 jours")

mtext("Rendements simulés", outer = TRUE, cex = 1.5, line = -1.1)

par(mfrow = c(1, 1))

hist_PnL_Cop <- f_PlotPLWithRiskMeasures(PnL_Cop, Prix_PF1, title = "P&L - Copule-Marginal (Boucle For)")
cat("VaR 95% : ", round(VaR_95_Cop, 2), "$")
cat("ES  95% : ", round(ES_95_Cop, 2), "$")

# ==== Surface de volatilité ====

# Préparation des données pour la calibration
Call_Put <- rbind(Calls, Puts)
m_CP <- Call_Put$K / as.numeric(SP500_T)
tau_CP <- Call_Put$tau
sigma_obs_CP <- Call_Put$IV

# Estimation des paramètres alpha
alpha0 <- c(0.2, 0.1, 0, 0.05)
res_Call_Put <- optim(par = alpha0, fn = loss_function, m = m_CP, tau = tau_CP, sigma_obs = sigma_obs_CP)
alpha_opt_call_put <- res_Call_Put$par

cat("Paramètres optimisés de la surface de volatilité :",alpha_opt_call_put,"\n")

# Visualisation de la surface calibrée
moneyness <- seq(min(m_CP), max(m_CP), by = 0.01)
maturity <- seq(min(tau_CP), max(tau_CP), by = 0.01)

volatility_surface <- outer(moneyness, maturity, Vectorize(function(m, t) sigma_model(m, t, alpha_opt_call_put)))

fig <- plot_ly(x = moneyness, y = maturity, z = volatility_surface) %>%
  add_surface() %>%
  layout(
    title = "Surface de Volatilité Implicite",
    scene = list(
      xaxis = list(title = "Moneyness (K/S)"),
      yaxis = list(title = "Maturité"),
      zaxis = list(title = "Volatilité implicite")
    )
  )
fig

# === Ajustement par VIX et recalcul des prix ===

# Prix du portefeuille initial avec la surface calibrée
tau <- c(T_20, T_20, T_40, T_40)
R <- c(r_20, r_20, r_40, r_40)

P_VOL_T <- Prix_PF_alpha(SP500_T, VIX_T, K, tau, alpha_opt_call_put, R)

# Simulation du portefeuille à T+5 jours pour chaque scénario simulé
tau <- c(T_15, T_15, T_35, T_35)
R <- c(r_15, r_15, r_35, r_35)

PF_VOL_T5 <- sapply(1:n_scenarios, function(i) {
  Prix_PF_alpha(P_SP500_sim_2[i], P_VIX_sim_2[i], K, tau, alpha_opt_call_put, R)
})

# Distribution du portefeuille
hist(PF_VOL_T5, breaks = 200,
     main = "Distribution du prix du portefeuille (surface ajustée)",
     xlab = "Prix ($)", col = "lightgray",xlim = c(0,1000))

abline(v = mean(PF_VOL_T5), col = "red", lwd = 2)
abline(v = median(PF_VOL_T5), col = "blue", lwd = 2)
abline(v = P_VOL_T, col = "darkgreen", lwd = 2)

legend("topright",
       legend = c(
         paste("Mean :", round(mean(PF_VOL_T5), 2), "$"),
         paste("Median :", round(median(PF_VOL_T5), 2), "$"),
         paste("Initial :", round(P_VOL_T, 2), "$")),
       col = c("red", "blue", "darkgreen"),
       lty = 1, cex = 0.8)

# Calcul du P&L
PnL_VOL <- PF_VOL_T5 - as.numeric(P_VOL_T)

# Calcul de la VaR et de l'ES
VaR_95_VOL <- quantile(PnL_VOL, 0.05)
ES_95_VOL <- mean(PnL_VOL[PnL_VOL <= VaR_95_VOL])

# Affichage des résultats
hist_PnL_VOL <- f_PlotPLWithRiskMeasures(PnL_VOL, Prix_PF1, title = "P&L - Surface de Volatilité Ajustée")
cat("VaR 95% : ", round(VaR_95_VOL, 2), "$")
cat("ES  95% : ", round(ES_95_VOL, 2), "$")

# ==== Résumé final ====

resultats_risque <- data.frame(
  Modele = c(
    "Modèle univarié (SP500)",
    "Modèle bivarié (SP500 + VIX)",
    "Modèle copule-marginal",
    "Surface de volatilité"
  ),
  VaR_95 = c(
    VaR_95,
    VaR_95_Biv,
    VaR_95_Cop,
    VaR_95_VOL
  ),
  ES_95 = c(
    ES_95,
    ES_95_Biv,
    ES_95_Cop,
    ES_95_VOL
  )
)
print(resultats_risque, row.names = FALSE)
