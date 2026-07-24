# ------------------------------------------------------------
# BVAR with Pandemic Priors and Cholesky identification
# ------------------------------------------------------------
# This script keeps the same structure as your BVAR.R file, but
# replaces the bsvars estimation step with the Pandemic Priors
# implementation of Cascaldi-Garcia.
#
# Required objects in the environment before running this script:
#   - ts_Data   : monthly ts object containing the VAR data
#                 Example:
#                 ts_Data <- ts(cbind(ilr_result$balances, Data[, names_macro]),
#                               start = 2001, frequency = 12)
#                 colnames(ts_Data) <- names_VAR
#   - names_VAR : character vector with the variables included in the VAR
#   - nSteps    : IRF horizon, e.g. nSteps <- 24
#
# Output objects:
#   - fit_bvar_pp      : list with posterior draws and estimation objects
#   - irfs             : array [response, shock, horizon, draw]
#   - irf_df           : tidy posterior IRFs
#   - irf_summary      : posterior median and credible bands
# ------------------------------------------------------------

# ------------------------------------------------------------
# 0. Packages and auxiliary functions
# ------------------------------------------------------------

required_pkgs <- c("vars", "Matrix", "LaplacesDemon", "dplyr", "tidyr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install the following packages before running the script: ",
       paste(missing_pkgs, collapse = ", "))
}

library(vars)
library(Matrix)
library(LaplacesDemon)
library(dplyr)
library(tidyr)

# Source the Pandemic Priors functions. Adjust this path if needed.
# The file should be the one from Cascaldi-Garcia's replication files.
#source("D:/Disco C/Repositorios Git/Labor-Market-and-Compositional-Analysis/Compositional Data and Labor MArket/TOT/R/Programs/functions_Pandemic_Priors.R")
source('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVBVAR/Programs/functions_Pandemic_Priors.R')

# ------------------------------------------------------------
# 1. Prepare data
# ------------------------------------------------------------

# Your original data construction is assumed to be something like:
#   ts_Data <- ts(cbind(ilr_result$balances, Data[, names_macro]),
#                 start = 2001, frequency = 12)
#   colnames(ts_Data) <- names_VAR
# Hence, ts_Data is a monthly time-series object starting in January 2001.

if (!is.ts(ts_Data)) {
  stop("ts_Data must be a ts object. Please construct it with ts(..., start = 2001, frequency = 12).")
}

if (!all(names_VAR %in% colnames(ts_Data))) {
  stop("Some variables in names_VAR are not column names of ts_Data.")
}

# Build a monthly Date vector directly from the ts attributes.
# For ts_Data <- ts(..., start = 2001, frequency = 12), start(ts_Data) is c(2001, 1),
# so the first observation is interpreted as January 2001.
ts_start <- start(ts_Data)
start_year <- as.integer(ts_start[1])
start_month <- as.integer(ts_start[2])

freq = c("month")
if(frequency(ts_Data)==4) freq = c("quarter")
full_date_vec <- seq.Date(
  from = as.Date(sprintf("%04d-%02d-01", start_year, start_month)),
  by = freq,
  length.out = NROW(ts_Data)
)

# Keep only complete observations for the variables included in the VAR.
# This avoids dropping observations because of unrelated columns in ts_Data.
complete_VAR <- complete.cases(ts_Data[, names_VAR, drop = FALSE])
merged_df1 <- ts_Data[complete_VAR, names_VAR, drop = FALSE]

# Date vector aligned with the estimation sample after dropping incomplete rows.
date_vec_use <- full_date_vec[complete_VAR]

# Use only the variables included in the VAR.
Yraw <- as.matrix(merged_df1)
storage.mode(Yraw) <- "double"

# Number of variables in the VAR
N <- ncol(Yraw)

# Variable names
var_names <- colnames(Yraw)

if (anyNA(date_vec_use)) {
  stop("The inferred date vector contains NA values. Please verify start(ts_Data) and frequency(ts_Data).")
}

# ------------------------------------------------------------
# 2. Lag-length selection
# ------------------------------------------------------------

lag_selection <- VARselect(Yraw, lag.max = 12, type = "const")

# View lag-selection results
lag_selection

# Choose lag length according to HQ criterion, as in your original BVAR.R
p <-ceiling(mean(lag_selection$selection))
p = 12
if(frequency(ts_Data)==4) p = 4
# ------------------------------------------------------------
# 3. Pandemic-prior specification settings
# ------------------------------------------------------------

set.seed(1234)

# VAR settings
constant <- 1L              # 0 = no intercept; 1 = intercept
nAR <- p                    # keep notation used by Cascaldi-Garcia's functions
nimp <- nSteps + 1L         # include impact horizon, so output horizons are 0:nSteps
rps <- 5000L               # posterior draws; analogous to main posterior S in BVAR.R
burnin_draws <- 0L        # not needed here because draws are direct NIW posterior draws
covid_periods <- Cov_per   # January 2020 onward; set to 0 for no pandemic dummies
if(frequency(ts_Data)==4)   covid_periods <- 4L
test_stab <- 1L             # 1 = keep only stable posterior draws, 0 = all draws
nshocks <- N                # Cholesky shocks for all variables
bands <- c(50, 16, 84, 16, 84)

# Prior hyperparameters
lambda <- 0.2               # overall prior tightness
epsilon <- 0.001            # prior tightness for the constant
phi <- 999                  # 999 = select phi by marginal likelihood grid; otherwise set numeric value

diff_or_lv <- 1             # 1 = variables in levels, 0 = variables in differences
if (diff_or_lv == 0) {
  delta <- 0
} else if (diff_or_lv == 1) {
  delta <- 1
} else {
  stop("diff_or_lv must be either 0 or 1.")
}

if(ident == "IV"){
  
  Data$Date <- as.Date(Data$Fecha)
  Z_aligned <- rep(NA_real_, length(date_vec_use)) # NA_real_ fuerza a que sea numérico
  
  for (i in seq_along(date_vec_use)) {
    fecha_i <- date_vec_use[i]
    
    # as.numeric fuerza a que el tibble entregue un escalar
    valor_instrumento <- as.numeric(Data[Data$Date == fecha_i, instrument_name])
    
    # Comprobamos que no esté vacío y no sea NA
    if (length(valor_instrumento) > 0 && !is.na(valor_instrumento[1])) {
      Z_aligned[i] <- valor_instrumento[1]
    }
  }
  
  Z_final <- Z_aligned[(nAR + 1):length(Z_aligned)]
  
  # Por seguridad absoluta, garantizamos que sea vector numérico
  Z_final <- as.numeric(unlist(Z_final)) 
}

tau <- 10 * lambda          # sum-of-coefficients prior tightness, following the replication script

# ------------------------------------------------------------
# 4. Construct VAR matrices and COVID dummies
# ------------------------------------------------------------

# If you want logs/differences, transform Yraw before this block.
# This version assumes Yraw is already transformed as in your empirical work.
Traw <- nrow(Yraw)

if (Traw <= nAR) {
  stop("The number of observations must be larger than the selected lag length.")
}

x <- mlag2(Yraw, nAR)
if (constant == 1L) {
  X1 <- cbind(x[(nAR + 1):Traw, , drop = FALSE], rep(1, Traw - nAR))
} else {
  X1 <- x[(nAR + 1):Traw, , drop = FALSE]
}

# COVID-19 time dummies: one dummy for each pandemic month starting in March 2020.
# These are treated as exogenous regressors, but they are excluded from the companion matrix.
if (covid_periods > 0L) {
  X1 <- cbind(X1, matrix(0, nrow = nrow(X1), ncol = covid_periods))
  colnames(X1) <- c(
    paste0(rep(var_names, times = nAR), "_L", rep(seq_len(nAR), each = N)),
    if (constant == 1L) "const" else NULL,
    paste0("D_covid_", seq_len(covid_periods))
  )

  covid_start <- as.Date("2020-03-01")
  if(frequency(ts_Data)==4)  covid_start <- as.Date("2020-01-01")
  covid_ind_raw <- which(date_vec_use == covid_start)
  if (length(covid_ind_raw) != 1L) {
    stop("Could not find exactly one observation dated 2020-03-01. Adjust covid_start if your data use another date convention.")
  }
  covid_ind <- covid_ind_raw - nAR

  if (covid_ind < 1L || (covid_ind + covid_periods - 1L) > nrow(X1)) {
    stop("COVID dummy positions are outside the estimation sample. Check nAR, covid_periods, and date_vec_use.")
  }

  covid_cols <- (ncol(X1) - covid_periods + 1L):ncol(X1)
  X1[covid_ind:(covid_ind + covid_periods - 1L), covid_cols] <- diag(covid_periods)
} else {
  colnames(X1) <- c(
    paste0(rep(var_names, times = nAR), "_L", rep(seq_len(nAR), each = N)),
    if (constant == 1L) "const" else NULL
  )
}

Y1 <- Yraw[(nAR + 1):Traw, , drop = FALSE]
T <- Traw - nAR
Y <- Y1
X <- X1
K <- ncol(X)

# ------------------------------------------------------------
# 5. OLS benchmark with COVID dummies
# ------------------------------------------------------------

A_OLS <- solve(crossprod(X), crossprod(X, Y))
RESID_OLS <- Y - X %*% A_OLS
SSE_OLS <- crossprod(RESID_OLS)
SIGMA_OLS <- SSE_OLS / (T - K)
VCV_OLS <- cov(RESID_OLS)
A0_OLS <- t(chol(VCV_OLS))

# ------------------------------------------------------------
# 6. Choose phi for the pandemic prior
# ------------------------------------------------------------

lambda <- as.numeric(lambda)
delta <- as.numeric(delta)
epsilon <- as.numeric(epsilon)
phi <- as.numeric(phi)
tau <- as.numeric(tau)

if (phi == 999) {
  phi_grid <- c(0.001, 0.01, 0.025, 0.050, 0.075, 0.10,
                0.15, 0.20, 0.25, 0.30, 0.35, 0.40,
                0.45, 0.50, 0.75, 1, 2, 5)
  phi_density <- numeric(length(phi_grid))

  for (jj in seq_along(phi_grid)) {
    phi_density[jj] <- OptimalPhi(
      X = X, Y = Y, Yraw = Yraw, nAR = nAR, constant = constant,
      δ = delta, λ = lambda, τ = tau, ϵ = epsilon,
      ϕ = phi_grid[jj], covid_periods = covid_periods
    )
    message("Density(", phi_grid[jj], ") = ", round(phi_density[jj], 4))
  }

  phi_use <- phi_grid[which.max(phi_density)]
  message("Optimal phi for the model is: ", phi_use)
} else {
  phi_grid <- NA_real_
  phi_density <- NA_real_
  phi_use <- phi
}

# ------------------------------------------------------------
# 7. Pandemic priors and posterior moments
# ------------------------------------------------------------

pp_result <- pandemicpriors(
  X = X, Y = Y, Yraw = Yraw, nAR = nAR, constant = constant,
  delta = delta, lambda = lambda, tau = tau,
  ϵ = epsilon, phi = phi_use, covid_periods = covid_periods
)

Xst <- pp_result$Xst
Yst <- pp_result$Yst
xd <- pp_result$xd
yd <- pp_result$yd

XXst <- crossprod(xd) + crossprod(X)
invXXst <- solve(XXst)
XYst <- crossprod(xd, yd) + crossprod(X, Y)
A_post <- invXXst %*% XYst

RESID_post <- Yst - Xst %*% A_post
SSE_post <- crossprod(RESID_post)

#F-test for instrument
if(ident == "IV"){
  policy_var_name <- policy_var_name
  policy_idx <- which(var_names == policy_var_name)
  model <- lm(RESID_post[1:nrow(Y),policy_idx]~Z_final)
  s = summary(model)
  print(s$fstatistic)
}

# Posterior degrees of freedom and covariance of coefficients
v1 <- nrow(Xst) + 2 - ncol(Xst)
xx <- crossprod(Xst)
ixx <- solve(xx)
chol_ixx <- chol(ixx)

# ------------------------------------------------------------
# 8. Posterior draws 
# ------------------------------------------------------------

A_companion_T <- array(0, dim = c(rps, N * nAR, N * nAR))
A0hat_T <- array(0, dim = c(rps, N, N))
IMP_T   <- array(0, dim = c(rps, N, N)) # Array para la matriz de impacto final


pb <- txtProgressBar(min = 1, max = rps, initial = 1, style = 3)
message("Drawing posterior coefficients and identifying shocks...")
for (iii in seq_len(rps)) {
  setTxtProgressBar(pb, iii)
  
  # Extraer parámetros (ya asumo que tu función devuelve nbeta_dr)
  if (test_stab == 0L) {
    draw_i <- draw_coef_pandemic_priors(
      SSE_post = SSE_post, v1 = v1, chol_ixx = chol_ixx,
      A_post = A_post, n = N, nAR = nAR, covid_periods = covid_periods
    )
  } else {
    draw_i <- draw_coef_pandemic_priors_stab(
      SSE_post = SSE_post, v1 = v1, chol_ixx = chol_ixx,
      A_post = A_post, n = N, nAR = nAR, covid_periods = covid_periods
    )
  }
  
  A_companion_T[iii, , ] <- draw_i$A_companion_dr
  A0hat_T[iii, , ] <- draw_i$A0hat
  
  # ---- IDENTIFICACIÓN ----
  if (ident == "IV") {
    # Definir el índice de la variable de política (Asegúrate de que policy_var_name exista, ej: "TIB")
    policy_var_name <- policy_var_name
    policy_idx <- which(var_names == policy_var_name)
    if (length(policy_idx) == 0) stop("La variable de política no se encuentra en var_names")
    # Usamos la función IV inyectando la columna correcta
    IMP_T[iii, , ] <- get_IMP_IV_draw(
      nbeta_draw = draw_i$nbeta_dr, 
      A0hat_chol = draw_i$A0hat, 
      Y = Y, 
      X = X, 
      Z_instrument = Z_final, 
      policy_idx = policy_idx
    )
  } else {
    # Si es "chol", usamos la matriz estándar
    IMP_T[iii, , ] <- draw_i$A0hat
  }
}
close(pb)

# ------------------------------------------------------------
# 9. Compute impulse responses
# ------------------------------------------------------------

irfs <- array(
  NA_real_,
  dim = c(N, nshocks, nimp, rps),
  dimnames = list(
    response = var_names,
    shock = var_names,          
    horizon = 0:nSteps,
    draw = seq_len(rps)
  )
)

pb <- txtProgressBar(min = 1, max = rps, initial = 1, style = 3)
message("Computing IRFs...")
for (iii in seq_len(rps)) {
  setTxtProgressBar(pb, iii)
  irf_i <- do_irfs(
    A_companion_dr = A_companion_T[iii, , ],
    IMP_dr = IMP_T[iii, , ],    # <--- AHORA PASAMOS LA MATRIZ IMP_T
    n = N,
    nshocks = nshocks,
    nimp = nimp
  )
  irfs[, , , iii] <- aperm(irf_i, c(3, 1, 2))
}
close(pb)

# ------------------------------------------------------------
# 10. Convert IRFs into tidy format
# ------------------------------------------------------------

irf_df <- as.data.frame.table(
  irfs,
  responseName = "irf"
) |>
  mutate(
    horizon = as.integer(as.character(horizon)),
    draw = as.integer(as.character(draw))
  )

# ------------------------------------------------------------
# 11. Compute posterior summaries
# ------------------------------------------------------------
# These are Bayesian credible bands, not frequentist confidence intervals.

irf_summary <- irf_df |>
  group_by(response, shock, horizon) |>
  summarise(
    median  = median(irf, na.rm = TRUE),
    lower68 = quantile(irf, probs = 0.16, na.rm = TRUE),
    upper68 = quantile(irf, probs = 0.84, na.rm = TRUE),
    lower90 = quantile(irf, probs = 0.16, na.rm = TRUE),
    upper90 = quantile(irf, probs = 0.84, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 12. Store estimation output
# ------------------------------------------------------------

fit_bvar_pp <- list(
  call = match.call(),
  Yraw = Yraw,
  X = X,
  Y = Y,
  p = p,
  nAR = nAR,
  N = N,
  var_names = var_names,
  covid_periods = covid_periods,
  covid_start = as.Date("2020-03-01"),
  hyperparameters = list(
    lambda = lambda,
    tau = tau,
    epsilon = epsilon,
    phi = phi,
    phi_use = phi_use,
    delta = delta
  ),
  phi_grid = phi_grid,
  phi_density = phi_density,
  A_OLS = A_OLS,
  SIGMA_OLS = SIGMA_OLS,
  A_post = A_post,
  SSE_post = SSE_post,
  A_companion_draws = A_companion_T,
  A0hat_draws = A0hat_T,
  irfs = irfs,
  irf_df = irf_df,
  irf_summary = irf_summary
)

# ------------------------------------------------------------
# 13. Example: extract a response
# ------------------------------------------------------------
# Example:
# irfs["GDP", "TIB", , ]
# irf_summary |> filter(response == "GDP", shock == "TIB")
