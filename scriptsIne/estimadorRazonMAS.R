setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
set.seed(1)

baseIne = read.csv("~/Documents/muestreo/Tesis/bases/PRES_2024.csv", 
                   encoding="UTF-8")
colnames(baseIne)


##------ ajuste de la base
# Transformamos la base del INE
listaColumnasNecesarias = c( colnames(baseIne)[1:5],
                             colnames(baseIne)[13:30] )

#vemos que hay nas en la columna de los distritos
nas = which( baseIne$ID_DISTRITO_FEDERAL == "N/A" )
length(nas)

baseIne[nas,]  %>% group_by( ENTIDAD  ) %>% 
  summarise( conteo = n() ) %>%  arrange(desc(conteo) , .by_group=TRUE)

#baseIne[nas,] %>% summarise( totalVotosna =  sum(total) )

#baseIne[nas,] %>% group_by( ID_ENTIDAD  ) %>% 
#  summarise( conteo = n(), porcentajeVotos = sum(total)/totalVotos ) %>%  
#  arrange(desc(conteo) , .by_group=TRUE)


#eliminamos los renglones que no posean un distrito federal
baseIne = baseIne %>%
  filter(!row_number() %in% nas )

baseIne = baseIne %>% 
  select( all_of( listaColumnasNecesarias ) )  %>% 
  rename( candicatoNoRegistrado = CANDIDATO.A.NO.REGISTRADO.A ,
          votosNulos = VOTOS.NULOS ) 

#cambiamos todas las columnas del tipo caracter por numerico
baseIne[,5:22] = baseIne[,5:22] %>% mutate_if(is.character, as.integer)

#creamos un codigo de estrato y usamos biblioteca stringr para ajustar
#la longitud de la cadena
#library(stringr)

#convertimos a string los id de las entidad y el distrito electoral
baseIne$ID_ENTIDAD =  as.character(baseIne$ID_ENTIDAD)
baseIne$ID_DISTRITO_FEDERAL = as.character(baseIne$ID_DISTRITO_FEDERAL)

#establecemos una longitud determinada y colocamos cero al comienzo
col1 = stringr::str_pad( baseIne$ID_ENTIDAD , width =2, pad = "0" )
col2= stringr::str_pad( baseIne$ID_DISTRITO_FEDERAL , width =2, pad = "0" )

#creamos la nueva columna en la base

baseIne$idEstrato =  paste(col1,col2, sep = "")
baseIne$idEstrato = as.factor( baseIne$idEstrato )
#baseIne = baseIne %>% unite( col1, col2, col = "idEstrato", sep = "" )


#calculamos los totales
baseIne = baseIne %>% mutate(total = TOTAL_VOTOS_CALCULADOS - votosNulos - candicatoNoRegistrado)
baseIne = baseIne %>% mutate( totalPan= PAN+ PRD+ PRI +PAN_PRI_PRD + PAN_PRI 
                              + PRI_PRD 
                              + PAN_PRD )
baseIne = baseIne %>% mutate( totalMorena= MORENA + PT + PVEM +  PVEM_PT+ 
                                PT_MORENA + PVEM_PT_MORENA + PVEM_MORENA )
baseIne = baseIne %>% mutate( totalMC = MC )

baseIne = baseIne %>% 
  select( CLAVE_ACTA, ID_ENTIDAD, idEstrato, totalPan, totalMorena, totalMC, total )
str(baseIne)


##-----funciones auxiliares

#estimador HT para un total m.a.s.  formula 42 pagina 20
EstimadorHTMAS = function( y, tamPob) {
  tamMuestra = length(y)
  pik =  rep( tamMuestra/ tamPob , times = length(y) )
  
  return( tamPob* mean( y /pik) )
}

#funccion que calcula estimador de razon ecuacion 72 pagina 34
EstimadorRazonMAS = function( y, z, tamPob) {
  tEstimado = EstimadorHTMAS(y, tamPob)
  zEstimado = EstimadorHTMAS(z, tamPob)
  return( tEstimado / zEstimado )
}

#funcion que calcula variable de linearizacion teorica
#formula 75 pagina 34
VLinTeorica = function( varInteres, variableTotal, razonTeorica) {
  tz = sum( variableTotal )
  variablesLinerazacion = (varInteres-razonTeorica * variableTotal )/tz
  return(variablesLinerazacion)
}

#funcion que calcula la variable de linearizacion estimada
#formula 76 pagina 34
VLinEstimada = function( y, z, tamPob ) {
  zEstMAS = EstimadorHTMAS(z,tamPob)
  
  estimador = EstimadorHTMAS(y,tamPob) / zEstMAS
  
  resultado = ( y - ( estimador* z ) ) / zEstMAS
  
}

#matriz de segundo orden pi_kl para m.a.s.

GenMatrizSegOrden = function( tamMuestra, tamPoblacion ) {
  valorKL_distintos = ( (tamMuestra -1)*tamMuestra ) / 
    ( (tamPoblacion-1)*tamPoblacion)
  
  valorKL_iguales = tamMuestra / tamPoblacion
    
  matriz = matrix( valorKL_distintos, 
                   nrow = tamPoblacion, ncol = tamPoblacion)
  
  diag(matriz) = rep(valorKL_iguales , times = tamPoblacion)
  
  return( matriz )
}

#matriz delta para m.a.s 

MatrizDeltaMAS = function( matrizSegOrden, tamMuestra, tamPob ) {
  pk_x_pl =  (tamMuestra /tamPob)*(tamMuestra /tamPob)
  return( matrizSegOrden - pk_x_pl )
}

#matriz Delta estimada para m.a.s.
#ecuacion 70 pag 34
MatrizDeltaMasEst = function( tamMuestra, tamPoblacion ) {
  
  #formula de recuadro azul pagina 34
  valorKL_distintos = ( (tamMuestra -1)*tamMuestra ) / 
    ( (tamPoblacion-1)*tamPoblacion)
  
  #formula de recuadro azul pagina 34
  valorKL_iguales = tamMuestra / tamPoblacion

  pik_x_pil = (tamMuestra / tamPoblacion)* (tamMuestra / tamPoblacion)
  
  #creamos matriz a la cual modificaremos la diagonal
  matriz = matrix( (valorKL_distintos - pik_x_pil )/ pik_x_pil , 
                   nrow = tamMuestra, ncol = tamMuestra)
  #establecemos el valor de la diagonal
  valorDiagonal = ( valorKL_iguales - pik_x_pil ) / valorKL_iguales
  
  #colocamos el valor de la diagonal
  diag(matriz) = rep( valorDiagonal , times = tamMuestra )
  
 
  return ( matriz )
  
}

#ec 69 pagina 34 
EstVarPoblacional  = function( tamMuestra, varInteres, varTotal, razonTeorica ) {
  
  cantPoblacion = length(varInteres)
  
  vPobl = VLinTeorica(varInteres, varTotal, razonTeorica )
  
  pis = rep( tamMuestra/cantPoblacion , times = cantPoblacion )
  
  matrizSegundoOrden = GenMatrizSegOrden( tamMuestra , cantPoblacion)
  
  matrizDelta = MatrizDeltaMAS( matrizSegundoOrden, tamMuestra, cantPoblacion )
  
  resultado = t( vPobl / pis ) %*% matrizDelta %*% ( vPobl / pis )
  
  return( resultado )
}


#ec 70 pagina 34 
EstVarMAS  = function(  varInteres, varTotal, tamPob ) {
  
  tamMuestra = length(varInteres)
  
  vEstimada = VLinEstimada(y= varInteres, z= varTotal, tamPob = tamPob)
  
  pis = rep( tamMuestra/tamPob , times = tamMuestra )
  
  deltaEstimada = MatrizDeltaMasEst(tamMuestra=tamMuestra,tamPoblacion= tamPob)
  
  resultado = t( vEstimada / pis ) %*% deltaEstimada %*% ( vEstimada / pis )
  
  return( resultado )
}

#intervalo de confianza pagina 42 ecuacion 82

IntervaloRazonMAS = function(alfa, y, z, tamPob){
  
  n = length(y)
  
  cuantil = qt(1 -(alfa/2), df= n-1 )
  
  estimadorMAS = EstimadorRazonMAS(y=y, z=z, tamPob=tamPob)
  
  estimacionVarianza = EstVarMAS(varInteres = y,varTotal= z, tamPob = tamPob )
  
  izquierdo = estimadorMAS - cuantil* sqrt(estimacionVarianza)
  
  derecho = estimadorMAS + cuantil* sqrt(estimacionVarianza)
  
  return( c( izquierdo, derecho ) )
}

#incorporamos integramos la funcion que nos arroje un 
#intervalo dependiendo de la muestra

generadorIntervaloRazonMAS = function( muestra,partido, alfa= 0.05, tamPob, thetaReal){
  y = muestra[ , partido]
  z = muestra[ , "total"]
  
  #usamos el estimador de razon para m.a.s. con funcion EstimadorRazonMAS
  estimador = EstimadorRazonMAS(y= y, z= z, tamPob = tamPob)
  estimador = round( estimador, 5)
  
  #obtenemos el intervalo con la funcion IntervaloRazonMAS
  intervalo = IntervaloRazonMAS(alfa = alfa, y = y , z= z, tamPob = tamPob)
  intervalo = round(intervalo, 5)
  
  longitud = round( intervalo[2] - intervalo[1], 5)
  
  exito = ( intervalo[1] <= thetaReal) & ( thetaReal <= intervalo[2] )
  
  resultado = list( intervalo[1], intervalo[2],  intervalo[2]-intervalo[1], 
                    estimador,  as.integer(exito) )
  
  return( resultado  )
  
}


#generamos una muestra con un porcentaje

generadorMuestra = function(porcentaje) {
  
  muestra = baseIne %>%
    slice_sample(prop = porcentaje) 
  
  return ( as.data.frame(muestra) )
}


#funcion para generar LISTA de muestras
listaMuestras = function (porcentaje, cantMuestras) {
  #listaDeMuestras = vector(mode = "list", length = cantMuestras )
  vectorPorcentajes = rep(porcentaje, cantMuestras)
  listaDeMuestras = lapply( vectorPorcentajes,generadorMuestra )
  
  return( listaDeMuestras  )
}




##------ valores

#en partido puede incluirse "totalMorena", "totalPan", "totalMC"
partido = "totalPan"
porcentaje = 0.05
cantidadMuestras = 1000

N = dim(baseIne)[1]

totalPartido = sum(baseIne[partido])
totalVotos = sum(baseIne$total)

#valor poblacional o theta real

thetaPoblacional  = totalPartido / totalVotos



##----Corremos codigo

#con los parametros indicados procedemos
#a obtener la lista de intervalos

#creamos una lista de muestras
lista = listaMuestras(porcentaje, cantidadMuestras)


#realizamos el calculo
tiempoInicial = Sys.time()
intervalos = lapply( lista, generadorIntervaloRazonMAS ,
                     thetaReal= thetaPoblacional , 
                     partido= partido, tamPob= N, )
tiempoFinal = Sys.time()
print( tiempoFinal - tiempoInicial)

#juntamos los intervalos
matrizDeIntervalos = do.call(rbind, intervalos)

#le damos nombre a las columnas para facilidad de lectura e identificacion
colnames(matrizDeIntervalos) = c("Izq", "Der", "longitud", "estimador", "exito")

print(matrizDeIntervalos)
