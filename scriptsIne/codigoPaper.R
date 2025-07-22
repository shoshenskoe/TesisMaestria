setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
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


#cuantil 05% de una xi cuadrada  con un grado de libertad
cuantil = qchisq(0.95, df = 1)

##----Funciones que dependen de la muestra

#Creamos una funcion que nos calcule la matriz c* (cada renglon es un c_i*)
#Depende de theta y de la muestra 
#(la matriz Z que indica el estrato multiplicado por pi)
matrizCistar = function (theta, Z, y, w ) {
  
  CiStar = as.matrix( cbind(Z, y - theta*w ) )
  
  return ( CiStar )
}

##----Funciones auxiliares 


#funcion mi_star depende de etaVector pagina 34 ecuacion 3.2.5
miStar = function( etaVec, CiStar, pis) {
  productoEtaCiStar = CiStar %*% etaVec
  
  denominador = pis + productoEtaCiStar
  
  return( as.vector ( 1/ denominador ) )
}

#pagina 34 ecuacion 3.2.7
deltaEta = function( etaVec , CiStar,  pis ) {
  
  #almcenamos resultado de la suma de las matrices c_i %*% c_i
  #resultado = matrix(0, nrow = Q, ncol = Q )
  #factor de la ec (3.2.7) que son los mi
  
  mi = ( CiStar %*% etaVec )  + pis 
  
  escalar = 1/ ( mi^2)
  
  escalar = as.vector( escalar) #sera multiplicado por c_i* %*& c_i*
  
  resultado= t( CiStar*escalar )  %*% ( CiStar ) #ecuacion 3.2.7 pagina 35
  #resultado tiene dimension 301x301
  
  #for (renglon in 1:nrow(CiStar) ) {
  #tcrossprod calcula t(x) %*% x mas rapido
  #productoMatricial =  tcrossprod( CiStar[renglon,], CiStar[renglon,] )
  #escalar =  ( divisores[renglon] )^2
  #multiplicacion =  productoMatricial / escalar
  #resultado = resultado + multiplicacion
  #}
  resultado = -1*resultado
  
  return( resultado )
  
}#terminadeltaEta


#pagina 34 ecuacion (3.2.5) en forma de vector
miStarHatVector = function( etaVector, CiStar, pis) {
  producto = as.vector( CiStar %*% etaVector )
  mi = as.vector( producto + pis )
  
  return( 1/mi )
}

#pagina 34
funcionF = function( etaVector , CiStar, pis ) {
  #es el escalar que multiplica los vectores c_i*, calculado como vector
  vectorMiHat = miStarHatVector (etaVector, CiStar, pis )
  
  #multiplicamos cada renglon de la matriz CiStar por un mi*
  #cada renglon de CiStar es un c_i*
  sumandos = sweep( CiStar,vectorMiHat, MARGIN = 1, FUN = "*" )
  
  return( as.vector ( colSums(sumandos) ) )
}


#suma de logaritmos
lmax = function(mi) {
  return ( sum( log(mi) ) )
}


##-----Metodo Newton
#algoritmo  6.1 pagina 99
#vector etaVector es dimension de C* , es decir 301
NewtonModified = function( etaVector = rep(0,301), epsilon = 1e-10, CiStar, pis, Cstar){
  # CiStar es matriz de nxQ
  
  k=1
  
  while( k < 1000 ) {
    
    #paso 2
    #calculamos el delta que depende de etaVector, es una matriz de QxQ
    # Q=301
    
    delta = deltaEta(etaVec =  etaVector, CiStar= CiStar, pis= pis)
    
    #pagina 34 , funcion f(eta) = sum mic_star_i
    f = funcionF(etaVector = etaVector, CiStar= CiStar, pis = pis )

    #se usa en la ecuacion 3.2.6 de la pagina 34
    v = f - Cstar
    
    #paso 4, revisamos condicion de norma y lo anticipamos un paso por
    #no necesitar el paso 3
    #vectorCondicion =  (delta %*% v)
    #norma =  as.numeric( sqrt ( t( vectorCondicion ) %*%  vectorCondicion ) )
    norma = as.numeric( t(v)%*% v )
    #print(norma)
    if (  norma < epsilon ) {
      return( etaVector )
    }
    
    #Paso 3 , despejamos del algoritmo 6.1 pagina 99
    #a delta(eta_0) + C- f(eta_k) = delta(eta)
    #vectorEcuacion = as.vector( delta %*% etaVector ) + v
    #resolvemos el sistema de ecuaciones , a su solucion lo llamamos iota
    #iota =  solve(delta,  vectorEcuacion, tol=1e-14 )
    iota =  solve(delta,  v, tol=1e-17 )
    
    
    #paso 5, obtenemos el minimo y revisamos condicion
    condicion = min ( pis +  as.vector( CiStar %*% ( etaVector - iota ) )  )
    #toleranciaWhile = 0
    
    while( condicion <= 0  ) {
      
      iota = iota/2 #ajustamos iota como se indica
      condicion = min ( pis +  as.vector( CiStar %*% ( etaVector - iota ) )  )
      #toleranciaWhile = toleranciaWhile+1
      #print( paste0("tolerancia iteracion: ",toleranciaWhile) )
    }
    
    #paso 6
    etaVector = etaVector - iota
    #print( paste0("iteracion: ", k) )
    k = k +1
  }#termina while
  
}#termina el metodo modificado de Newton-Raphson




##-----funcion ratio

#pagina 35 ecuacion 3.3.1
funcionRatio = function (  theta, lMaxThetaEstimada, Z, y, w , pis, Cstar ) {
  
  #creamos la matriz c_i* con theta
  CiStarTheta = matrizCistar(theta = theta, Z = Z, y = y, w = w )
  
  #newton para la matriz de c_i* para theta
  etaEstimadoTheta = NewtonModified( CiStar = CiStarTheta, pis= pis, Cstar = Cstar)
  
  #funcion para calcular mi estimadas de theta
  miTheta = miStar( eta= etaEstimadoTheta , CiStar= CiStarTheta, pis = pis)

  #con las mi calculadas calculamos la suma de los logaritmos de mi estimadas
  lMaxTheta = lmax( miTheta )
  
  return ( 2* (  lMaxThetaEstimada - lMaxTheta ) )
}

ratioMenosCuantil = function ( theta, lMaxThetaEstimada, Z, y, w , pis, Cstar ) {
  return( abs( funcionRatio( theta, lMaxThetaEstimada, 
                             Z, y, w , pis,Cstar ) - cuantil ) )
}

##metodo para resolver las raices con libreria rootSolve

if (!requireNamespace("rootSolve", quietly = TRUE)) {
  install.packages("rootSolve", dependencies = TRUE) }

library(rootSolve)



##------Funciones condensadoras

generadorMuestra = function(porcentaje) {
  
  muestra = baseVero %>%
    group_by(Stratum) %>%
    slice_sample(prop = porcentaje) %>%
    ungroup() %>% as.data.frame()
  
  n_h = muestra %>% count( Stratum, name = "n")
  
  #anadimos la columna asociada al pi_i = n_i / N_i
  #anadimos la columna asociada al pi_i = n_i / N_i
  muestra = dplyr::inner_join(N_h, n_h, by = "Stratum") %>%
    mutate(pi = n/N ) %>% select (Stratum, pi) %>%
    dplyr::inner_join(muestra,  by = "Stratum")
  
  return (muestra)
}

listaMuestras = function (porcentaje, cantMuestras) {
  #listaDeMuestras = vector(mode = "list", length = cantMuestras )
  vectorPorcentajes = rep(porcentaje, cantMuestras)
  listaDeMuestras = lapply( vectorPorcentajes,generadorMuestra )
  
  return( listaDeMuestras  )
}

generadorIntervalo = function ( muestra, thetaReal , partido) {
  
  y = muestra[, partido]
  w = muestra$total
  pi_i = muestra$pi
  estratos = as.factor(muestra$Stratum)
  n = length(y)
  
  # 4. Matriz matrizEstratos: indicadores de estrato
  matrizEstratos =  stats::model.matrix(~ estratos - 1)
  
  n_vec = colSums(matrizEstratos) #cantidad de elementos en cada estrato
  H = ncol(matrizEstratos) #numero de estratos
  
  Z =  sweep( matrizEstratos, pi_i, MARGIN = 1, FUN = "*" ) #matriz para generar
  #los z_{i} de la pagina 46 ecuacion 3.6.1
  
  Cstar = as.vector( c(n_vec,0) ) #al numero de estatos anadimos momentos
  
  Q= length(Cstar)  #dimension de c_{i}* y C*
  
  #procedemos al calculado de lmaxThetaEstimada que nos sirve en la funcion
  #r(theta) pues es una constante.
  
  estRazon = as.numeric( sum(y) / sum( w ) )
  
  #creamos la matriz c_i* con thetaHat = estRazon
  CiStarThetaHat = matrizCistar(estRazon, Z, y, w)
  
  
  #newton para la matriz de c_i* para theta estimada = estRazon
  etaEstimadoThetaHat = NewtonModified(CiStar = CiStarThetaHat, 
                                       pis = pi_i, Cstar = Cstar )
  

  #funcion para calcular mi estimadas de theta estimada
  miThetaHat = miStar(etaVec= etaEstimadoThetaHat, 
                      CiStar= CiStarThetaHat, pis= pi_i)

  #con las mi calculadas calculamos la suma de los logaritmos de mi estimadas
  lmaxThetaEstimada = lmax(miThetaHat)
  
  #usamos la funcion multiroot busca la raiz de la funcion ratioMenosCuantil
  #la cual utiliza como parametro maxThetaHat (theta estimada). Ese parametro
  #lo podemos pasar nombrando explicitamente el nombre de su argumento
  solucion1 = rootSolve::multiroot(f = ratioMenosCuantil,
                                   start = estRazon-0.01 , 
                                   lMaxThetaEstimada = lmaxThetaEstimada, 
                                   Z = Z, , y = y, w = w , 
                                   pis = pi_i, Cstar = Cstar )
  
  
  #usamos la funcion multiroot busca la raiz de la funcion ratioMenosCuantil
  #la cual utiliza como parametro maxThetaHat (theta estimada). Ese parametro
  #lo podemos pasar nombrando explicitamente el nombre de su argumento
  solucion2 = rootSolve::multiroot(f = ratioMenosCuantil,
                                   start = estRazon+0.01 , 
                                   lMaxThetaEstimada = lmaxThetaEstimada, 
                                   Z = Z, , y = y, w = w , 
                                   pis = pi_i, Cstar = Cstar  )
  
  izquierdo = solucion1$root
  derecho = solucion2$root
  
  
  #revisamos si atrapa el valor
  
  
  
  exito = ( izquierdo <= thetaReal ) & ( thetaReal <= derecho )

  return ( list( izquierdo, derecho, derecho-izquierdo ,as.integer(exito) ) )
  
} #termina generadorIntervalo


##------Ejecucion

#library(parallel)


# Detect the number of cores available
#numCores <- detectCores() - 1  # use one less than total  avoid freezing 

# Parallel version of lapply
#tiempoInicial = Sys.time()
#intervalos <- parallel::mclapply(lista, generadorIntervalo,
#                                 thetaReal= thetaReal,
#                                 mc.cores = numCores)
#tiempoFinal = Sys.time()
#print( tiempoFinal - tiempoInicial)

#lista = listaMuestras(0.05, 1)



#introducimos parametros

partido = "totalPan"
porcentaje = 0.10
numMuestras = 10
lista <- listaMuestras(porcentaje, numMuestras)


#con estos parametros obtenemos la lista de intervalos

thetaReal = sum( baseVero[,partido] ) / sum( baseVero$total )

tiempoInicial = Sys.time()
intervalos = lapply( lista, generadorIntervalo, 
                     thetaReal= thetaReal, partido = partido )
tiempoFinal = Sys.time()
print( tiempoFinal - tiempoInicial)

matrizDeIntervalos = do.call(rbind, intervalos)

write.csv(matrizDeIntervalos, file= "/Users/shoshenskoe/Documents/muestreo/Intervalos/INE/tamanio10/Pan/int1_1.csv")

