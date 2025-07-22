setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
library(nloptr)
library(readr)
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
baseVero = baseVero %>% select(-pi)

N_h = baseVero %>% count(Stratum, name= "N")


porcentaje = 0.15
muestra = baseVero %>% 
  group_by(Stratum) %>% 
  slice_sample(prop = porcentaje) %>%
  ungroup() %>% as.data.frame()

n_h = muestra %>% count( Stratum, name = "n")

muestra = dplyr::inner_join(N_h, n_h, by = "Stratum") %>% 
  mutate(pi= n/N) %>% select (Stratum, pi) %>% 
  dplyr::inner_join(muestra,  by = "Stratum")
  
y =  muestra$totalMorena
w = muestra$total
pi_i = muestra$pi
estratos = as.factor(muestra$Stratum)
n = length(y)

# 4. Matriz Z: indicadores de estrato
Z =  stats::model.matrix(~ estratos - 1)
n_vec = colSums(Z)
H = ncol(Z)

# sum log likelihood funcion a maximizar (minimizar segun nloptr)
sumLikelihood = function(p) {
  -sum(log(p))
}

# Restricciones 
restricciones = function(p, theta) {
  c(
    sum((p / pi_i) * (y - theta * w)),   # restriccion de momento
    colSums(p * Z) - n_vec / n,          # restriccion de estrato
    sum(p) - 1                           # sum(p) = 1
  )
}



# Gradiente de sumLoglikelihood
gradienteLikelihood = function(p) {
  -1 / p
}

# Jacobiano de restricciones
jacobianoM = function(p, theta) {
  grad_moment <- (1 / pi_i) * (y - theta * w)
  grad_strata <- t(Z)
  grad_sum <- rep(1, n)
  rbind(grad_moment, grad_strata, grad_sum)
}

#funciones closure
restriccionClosure = function( theta ) {
  return( function(p) { restricciones(p,theta)} )
}

jacobianoClosure = function( theta ) {
  return( function(p) { jacobianoM(p,theta)} )
}


# parametros para el optimizador
opts1 <- list(
  "algorithm" = "NLOPT_LD_AUGLAG", # Global/outer algorithm for constraints
  "local_opts" = list(
    "algorithm" = "NLOPT_LD_LBFGS", # Local/inner algorithm for subproblems
    "xtol_rel" = 1.0e-4 # Tolerance for local solver
  ),
  "xtol_rel" = 1.0e-4, # Tolerance for global solver
  "maxeval" = 1000 ,   # Maximum number of evaluations
  print_level = 0
)

opts2 = list(
  algorithm = "NLOPT_LD_SLSQP",
  xtol_rel = 1e-8,
  maxeval = 500
)


#  Restricciones de caja: p_i > 0
cotaInf = rep(1e-10, n)
cotaSup = rep(1, n)
#vectorInicial = runif(n= n, min=0, max=1)
vectorInicial = rep(1/n, n)
#vectorInicial = rep(1, n)

## funcion que calcula log_{max} ----------
lMax = function(theta) {
  
  #creamos las funciones que seran usadas en el optimizador
  restriccion = restriccionClosure(theta)
  jacobiano = jacobianoClosure(theta)
  
  #vectorInicial = rep(1/n, n)
  
  #inicial = Sys.time()
  #usamos el optimizador
  #Inicio = Sys.time()
  resultado = nloptr::nloptr(
    x0 = vectorInicial,
    eval_f = sumLikelihood,
    eval_grad_f = gradienteLikelihood,
    eval_g_eq = restriccion,
    eval_jac_g_eq = jacobiano,
    lb = cotaInf,
    ub = cotaSup,
    opts = opts1
  )
  #Final = Sys.time()
  #print(Final-Inicio)

  
  #sum(optimal_params)
  return( -resultado$objective )
}


thetaHat = muestra %>% 
  summarise(estimacion = sum(totalMorena) / sum(total) ) %>%
  select(estimacion ) %>% pull()

#cuantil 05% de una xi cuadrada  con un grado de libertad
cuantil = qchisq(0.95, df = 1)

#funcion ratio 
#calculamos l_{max} en el estimador de razon por eficiencia
#lMAXreal = sum(  log( 1/muestra$pi ) )

lMaxThetaHat =  lMax(thetaHat)

ratioStatistic = function( theta ) {
  
  return( 2*(  lMaxThetaHat - lMax(theta) )  )
}

malla = seq( from = thetaHat-0.01 , to = thetaHat+0.01, length.out= 15  )
length(malla)
#funcion ratio en una malla -----


tiempoInicial = Sys.time()
valoresr = sapply(malla, ratioStatistic)
valoresr = -1*valoresr
tiempoFinal = Sys.time()
print(tiempoFinal-tiempoInicial)

valoresValidos = valoresr[ valoresr >=0  & valoresr < (cuantil + 2 )]

mallaValida = malla [valoresr >=0  & valoresr < (cuantil + 2 ) ]

valoresMenoresCuantil = mallaValida[ valoresValidos <= cuantil ]


intervalo = range(valoresMenoresCuantil)

thetaReal = baseVero %>% 
  summarise( porcentajeReal = sum(totalMorena) / sum(total)) %>%
  pull()

acierto = intervalo[1] <= thetaReal & thetaReal <= intervalo[2]

#grafica

plot(mallaValida, valoresValidos, xlab  = "theta", ylab = "likelihood ratio", 
     main = "intervalo" )
abline(h = cuantil, col = "red", lty = 1)
abline(v= thetaHat, col = "blue", lty= 2)
legend("topright", legend=c("Cuantil Xi cuadrada"),
       col=c("red"), lty=1, cex=0.5, text.font=4)


##libreria rootsolve----
library(rootSolve)

tiempoIn1 = Sys.time()
raiz1 = multiroot(f = ratioStatistic, start = thetaHat-0.01)
tiempoFin1 = Sys.time()
print(tiempoFin1 - tiempoIn1)

tiempoIn2 = Sys.time()
raiz2 = multiroot(f = ratioStatistic, start = thetaHat+0.01)
tiempoFin2 = Sys.time()
print(tiempoFin2 - tiempoIn2)

acierto2 = raiz1$root <= thetaReal & thetaReal<= raiz2$root

intervalo2 = c(raiz1$root, raiz2$root)
print(intervalo2)
print(acierto2)


plot(mallaValida, valoresValidos, xlab  = "theta", ylab = "likelihood ratio", 
     main = "intervalo" )
abline(h = cuantil, col = "red", lty = 1)
abline(v= thetaHat, col = "blue", lty= 2)
legend("topright", legend=c("Cuantil Xi cuadrada"),
       col=c("red"), lty=1, cex=0.5, text.font=4)







library(parallel)
# Get the number of available cores
num_cores <- detectCores()

# Create a cluster
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, c("ratioStatistic", "lMax", "lMaxThetaHat", "cuantil", "n", "pi_i", "y", "w", "Z", "n_vec", "H", "cotaInf", "cotaSup", "vectorInicial", "opts1", "restriccionClosure", "jacobianoClosure", "sumLikelihood", "gradienteLikelihood", "restricciones", "jacobianoM"))


# Parallelize sapply
valoresr_parallel <- parSapply(cl, malla, ratioStatistic)

# Stop the cluster
stopCluster(cl)

indices <- valoresr_parallel > -10
valoresValidos = valoresr_parallel[indices]
mallaValida = malla[indices]
plot(mallaValida, valoresValidos)
