

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


##------ codigo usando solo biblioteca dplyr


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

Intervalo = function( base, tamanioMuestraEstrato, R=500, thetaReal ){
  
  tamanioBoots= tamanioMuestraEstrato - 1
  
  muestraPrincipal = base %>% 
    dplyr::group_by( estrato ) %>% #agrupamos por estrato
    dplyr::sample_n(size= tamanioMuestraEstrato, replace = FALSE) %>% # m.a.s.
    dplyr::ungroup() %>% #hasta este punto tenemos la muestra principal
    dplyr::mutate( pi = tamanioMuestraEstrato/totalEstrato )  #anade pesos pi
  
  estTotalPartido = sum(muestraPrincipal$y/ muestraPrincipal$pi)
  estTotal = sum( muestraPrincipal$z / muestraPrincipal$pi )
  estRazon = estTotalPartido / estTotal
  
  #estimadoresBoots = rep(0, times= R)
  
  estimadoresBootstrap = replicate( n= R,
                                    Bootstrap( muestraPrin = muestraPrincipal , 
                                               tamanioBoots = tamanioBoots, 
                                               tamanioMuestraEstrato = tamanioMuestraEstrato ) )
  
  
  varianzaBootstrap = mean( (estimadoresBootstrap- estRazon)^2 )
  
  intervaloIzq = estRazon -  cuantil*sqrt( varianzaBootstrap )
  intervaloDer = estRazon + cuantil*sqrt( varianzaBootstrap )
  
  #verificamos que se atrapo al verdadero valor
  exito = ( intervaloIzq <= thetaReal & thetaReal <= intervaloDer )
  
  longitud = intervaloDer - intervaloIzq
  
  return( c(intervaloIzq, intervaloDer , varianzaBootstrap, longitud, exito ) )
  
  
} #termina funcion generadora de intervalos

GeneraIntervalos = function(numIntervalos,base,tamMuestraEst,R=500,parametro) {
  
  matriz = replicate( n=numIntervalos, 
                      Intervalo(base= base, 
                                tamanioMuestraEstrato =tamMuestraEst,
                                R= R,
                                thetaReal = parametro) )
  matriz = t( matriz )
  
  return( matriz )
  
}

##---------------- echamos a correr la base

partidos = c("totalMorena", "totalPan", "totalMC")
tamanios = c(3,15,25)

cuantil = qnorm(0.95, mean = 0, sd = 1)

for (partido in partidos) {
  for (tamanio in tamanios) {
    
    nombreArchivo = paste( as.character(partido) ,as.character(tamanio), 
                           sep = "_")
    nombreArchivo = paste(nombreArchivo, ".csv", sep = "")
    
    print(paste("procesando: ", nombreArchivo) )
    
    ruta = "/Users/shoshenskoe/Documents/muestreo/Intervalos/INE/intervalos_bootstrap/"
    nombreArchivo = paste(ruta, nombreArchivo , sep = "")
    
    
    datos =  dplyr::tibble(
      id = base$CLAVE_ACTA,
      y = base[[ partido ]],
      z = base$total,
      estrato = base$Stratum,
      totalEstrato = base$totalEstrato
    )
    
    paramReal  = sum(datos$y) / sum(datos$z)
    
    valores = GeneraIntervalos(numIntervalos = 2, base = datos, 
                               tamMuestraEst = tamanio, R=500, 
                               parametro = paramReal)
    
    dataframe = as.data.frame(valores)
    readr::write_csv(dataframe, nombreArchivo)
  }
}






