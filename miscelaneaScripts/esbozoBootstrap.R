

library(dplyr)
library(readr)
set.seed(1)
library(survey)


#-----leemos la base
archivo = "https://raw.githubusercontent.com/shoshenskoe/TesisMaestria/refs/heads/main/bases/baseVerosi.csv"
base = readr::read_csv(file =archivo,
                       col_types = cols(
                         Stratum = col_character(),
                         ID_ENTIDAD = col_character(),
                         totalPan = col_integer(),
                         totalMorena = col_integer(),
                         totalMC = col_integer(),
                         total = col_integer(),
                         totalEstrato = col_integer(),
                         id = col_integer()
                       ) ) 


#obtenemos la muestra principal
muestraPrincipal = base %>% 
  dplyr::group_by(Stratum) %>% 
  dplyr::sample_n(size= 3) %>% 
  dplyr::ungroup()


muestraBoots = muestraPrincipal %>% 
  dplyr::group_by(Stratum) %>% 
  dplyr::sample_n(size= 2, replace = TRUE) %>% 
  dplyr::ungroup()

posiblesResultados = as.factor( muestraPrincipal$CLAVE_ACTA )
valoresMuestraBots = factor( muestraBoots$CLAVE_ACTA , 
                             levels = posiblesResultados )

tablaFrecuencias = dplyr::as_tibble( table(valoresMuestraBots) )





valoresBoot = as.factor( muestraBoots$CLAVE_ACTA, 
                         levels = as.factor(muestraPrincipal$CLAVE_ACTA) )

valoresPrincipal = as.factor(muestraPrincipal$CLAVE_ACTA)

table ( muestraBoots$CLAVE_ACTA, 
        levels=   as.factor(muestraPrincipal$CLAVE_ACTA) )




##------ codigo usando solo biblioteca dplyr

tamanioMuestraEstrato = 3
tamanioBoots= tamanioMuestraEstrato - 1


#funcion que genera una estimacion bootstrap utilizando el algoritmo . Como argumento recibe una muestra originada por muestreo
#estratificado m.a.s.
Bootstrap = function( muestraPrin , tamanioBoots, tamanioMuestraEstrato  ) {
  
  estimadorBootstrap = muestraPrin %>% 
    dplyr::group_by(estrato) %>% #volvemos a agrupar por estrato
    dplyr::sample_n(size=tamanioBoots, replace = TRUE)%>%  #muestreo reemplazo
    dplyr::ungroup() %>% #tenemos la muestra bootstrap
    dplyr::group_by( id) %>%  #agrupamos por id
    dplyr::mutate( conteoBots = n() ) %>% #contamos veces aparecio cada clave
    dplyr::ungroup() %>% #desagrupamos y almacenamos el conteo en cada regristro
    dplyr::mutate( replicateWeight = 
                     pi*conteoBots*
                     (tamanioMuestraEstrato/ (tamanioMuestraEstrato-1) ) ) %>% 
    dplyr::summarise( estTotalPartido = sum( replicateWeight*y ), 
                      estTotal = sum(replicateWeight*z) ) %>% #est de totales
    dplyr::summarise( estimadorBoots = estTotalPartido / estTotal ) %>% #est bots
    dplyr::pull() #convertimos a un vector o numerico
  
  return( estimadorBootstrap )
  
} #termina funcion

GeneradorIntervalo = function( base, tamanioMuestraEstrato, R=500 ){
  
  tamanioBoots= tamanioMuestraEstrato - 1
  
  muestraPrincipal = base %>% 
    dplyr::group_by( estrato ) %>% #agrupamos por estrato
    dplyr::sample_n(size= tamanioMuestraEstrato, replace = FALSE) %>% # m.a.s.
    dplyr::ungroup() %>% #hasta este punto tenemos la muestra principal
    dplyr::mutate( pi = tamanioMuestraEstrato/totalEstrato )  #anade pesos pi
  
  estTotalPartido = sum(muestraPrincipal$y/ muestraPrincipal$pi)
  estTotal = sum( muestraPrincipal$z / muestraPrincipal$pi )
  estRazon = estTotalPartido / estTotal
  
  
  R=500
  #estimadoresBoots = rep(0, times= R)
  
  estimadoresBootstrap = replicate( n= R,
                                    Bootstrap( muestraPrin = muestraPrincipal , 
                                               tamanioBoots = tamanioBoots, 
                                               tamanioMuestraEstrato = tamanioMuestraEstrato ) )
  
  
  varianzaBootstrap = mean( (estimadoresBootstrap- estRazon)^2 )
  
  intervaloIzq = estRazon - varianzaBootstrap
  intervaloDer = estRazon + varianzaBootstrap
  
  return( c(intervaloIzq, intervaloDerm , varianzaBootstrap) )
  
} #termina funcion generadora de intervalos

GeneraIntervalos = function(numIntervalos, base, tamMuestraEst, R=500) {
  matriz = replicate( n=numIntervalos, GeneraIntervalos(base= base, 
                                               tamanioMuestraEstrato =tamMuestraEst,
                                               R= R) )
  matriz = t( matriz )
  
  return( matriz )
  
}

##---------------- echamos a correr la base



datos =  dplyr::tibble(
  id = base$CLAVE_ACTA,
  y = base$totalPan,
  z = base$total,
  estrato = base$Stratum,
  totalEstrato = base$totalEstrato
)

generadorIntervalo( base=datos, tamanioMuestraEstrato = 25 )
