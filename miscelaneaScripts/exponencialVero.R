library(readr)
library(nleqslv)
library(dplyr)

baseIne = read_csv(file ="/Users/shoshenskoe/Documents/muestreo/Tesis/bases/baseVerosi.csv",
                   col_types = cols(
                     Stratum = col_character(),
                     ID_ENTIDAD = col_character(),
                     totalPan = col_integer(),
                     totalMorena = col_integer(),
                     totalMC = col_integer(),
                     total = col_integer(),
                     totalEstrato = col_integer(),
                     pi = col_double(),
                     id = col_integer()
                   ) )

porcentaje = 0.15
muestra = baseIne %>% 
  group_by(Stratum) %>% 
  slice_sample(prop = porcentaje) %>%
  ungroup() %>% as.data.frame() 

# Set theta
theta <- 0.6122974

y = muestra$totalMorena
w = muestra$total
pi_i = muestra$pi
stratum = as.factor(muestra$Stratum)
n = length(y)


# construimos matriz de disenio
H <- length(unique(stratum))
n <- length(y)
strata_levels <- sort(unique(stratum))
Z <- matrix(0, nrow = n, ncol = H)
for (h in 1:H) {
  Z[, h] <- as.integer(stratum == strata_levels[h])
}

# Compute n_h and vector of proportions
n_h <- colSums(Z)
n_vec <- n_h / sum(n_h)

# Function to compute p_i from lambda and gamma
compute_p <- function(lambda, gamma) {
  exponent <- lambda * (y - theta * w) / pi_i + Z %*% gamma
  weights <- exp(exponent)
  p <- weights / sum(weights)
  return(p)
}

# System of equations to solve
moment_conditions <- function(par) {
  lambda <- par[1]
  gamma <- par[-1]
  p <- compute_p(lambda, gamma)
  
  eq_moment <- sum((p / pi_i) * (y - theta * w))  # scalar
  eq_strata <- as.vector(t(Z) %*% p - n_vec)      # vector of H equations
  
  return(c(eq_moment, eq_strata))
}

# Solve system
init <- c(0, rep(0, H))  # initial values for lambda and gamma
sol <- nleqslv(init, moment_conditions, method = "Newton")

# Extract solution
lambda_hat <- sol$x[1]
gamma_hat <- sol$x[-1]
p_hat <- compute_p(lambda_hat, gamma_hat)

# Empirical log-likelihood
log_likelihood <- sum(log(p_hat))

cat("Log-likelihood:", log_likelihood, "\n")

