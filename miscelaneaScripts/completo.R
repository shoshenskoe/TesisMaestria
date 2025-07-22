
library(nleqslv)
setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
library(nloptr)
library(readr)
library(ggplot2)
set.seed(1)

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

y = muestra$totalMorena
w = muestra$total
pi_i = muestra$pi
stratum <- muestra$Stratum
n= length(y)

H <- length(unique(stratum))
n <- length(y)
strata_levels <- sort(unique(stratum))
Z <- matrix(0, nrow = n, ncol = H)
for (h in 1:H) {
  Z[, h] <- as.integer(stratum == strata_levels[h])
}

n_h <- colSums(Z)
n_vec <- n_h / sum(n_h)

compute_p <- function(lambda, gamma, theta) {
  exponent <- lambda * (y - theta * w) / pi_i + Z %*% gamma
  weights <- exp(exponent)
  p <- weights / sum(weights)
  return(p)
}

compute_loglik_theta <- function(theta_val) {
  moment_conditions <- function(par) {
    lambda <- par[1]
    gamma <- par[c(-1)]
    p <- compute_p(lambda, gamma, theta_val)
    eq_moment <- sum((p / pi_i) * (y - theta_val * w))
    eq_strata <- as.vector(t(Z) %*% p - n_vec)
    return(c(eq_moment, eq_strata))
  }
  init <- as.numeric ( rep(  1/1000 , H+1) )
  sol <- tryCatch(
    nleqslv(init, fn= moment_conditions, method = "Newton",
            control= list(allowSingular = TRUE)) ,
    error = function(e) return(NULL)
  )
  if (is.null(sol) || sol$termcd != 1) return(NA)
  p_hat <- compute_p(sol$x[1], sol$x[-1], theta_val)
  return(sum(log(p_hat)))
}

compute_loglik_theta(0.60123)

# Grid search
theta_grid <- seq(0.58, 0.64, length.out = 60)
loglik_vals <- sapply(theta_grid, compute_loglik_theta)
plot_df <- data.frame(theta = theta_grid, loglik = loglik_vals)

# Find theta hat and cutoff
max_idx <- which.max(plot_df$loglik)
theta_hat <- plot_df$theta[max_idx]
loglik_max <- plot_df$loglik[max_idx]
delta <- 1.92
lower_ci <- min(plot_df$theta[plot_df$loglik >= loglik_max - delta])
upper_ci <- max(plot_df$theta[plot_df$loglik >= loglik_max - delta])

# Plot with CI
ggplot(plot_df, aes(x = theta, y = loglik)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "darkblue") +
  geom_vline(xintercept = theta_hat, linetype = "dashed", color = "red") +
  geom_hline(yintercept = loglik_max - delta, linetype = "dotted", color = "gray40") +
  annotate("rect", xmin = lower_ci, xmax = upper_ci,
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  labs(title = "Profile Empirical Log-Likelihood with 95% CI",
       subtitle = paste0("thetâ = ", round(theta_hat, 5),
                         ", CI: [", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
       x = expression(theta),
       y = expression(l[max](theta))) +
  theme_minimal()
