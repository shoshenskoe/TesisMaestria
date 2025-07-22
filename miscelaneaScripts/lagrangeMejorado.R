setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
library(nloptr)
library(readr)
library(nleqslv)
set.seed(1)

baseVero = read_csv(file ="/Users/shoshenskoe/Documents/muestreo/Tesis/bases/baseVerosi.csv",
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


porcentaje = 0.20
muestra = baseVero %>% 
  group_by(Stratum) %>% 
  slice_sample(prop = porcentaje) %>%
  ungroup() %>% as.data.frame() 

y = muestra$totalMorena
w = muestra$total
pi_i = muestra$pi
estratos = as.factor(muestra$Stratum)
n = length(y)

# 4. Matriz Z: indicadores de estrato
Z = stats::model.matrix(~ estratos - 1)
n_vec = colSums(Z)
H = ncol(Z)


solve_system <- function(par) {
  lambda <- par[1]
  mu <- par[2]
  gamma <- par[-c(1,2)]
  
  denom <- mu + lambda * (y - theta * w) / pi_i + Z %*% gamma
  p <- 1 / as.vector(denom)
  
  eq1 <- sum(p) - 1 # asociado a la suma 
  eq2 <- sum((p / pi_i) * (y - theta * w)) #momento
  eq3 <- as.vector(t(Z) %*% p - n_vec / length(y)) #estratos
  
  return(c(eq2, eq3, eq1))
}

theta = muestra %>% 
  summarise( estimacion = sum(totalMorena)/ sum(total) ) %>%
  select( estimacion) %>% pull()

# Initial values: small lambda, mu, zeros for gamma
init <- c(10, 10, rep(10, ncol(Z)))
sol <- nleqslv::nleqslv(init, solve_system, method = "Broyden", 
               control = list(allowSingular = TRUE) )

# Recover p_i
lambda <- sol$x[1]
mu <- sol$x[2]
gamma <- sol$x[-c(1,2)]
denom <- mu + lambda * (y - theta * w) / pi_i + Z %*% gamma

denomValido = denom [ denom > 0 ]
p_hat <- 1 / as.vector(denom)
# Calculate log-likelihood
log_likelihood <- sum(log(p_hat))



