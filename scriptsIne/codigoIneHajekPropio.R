##Estimador de razon o Hajek

## Tesis

setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
set.seed(1)

baseIne = read.csv("~/Documents/muestreo/Tesis/bases/PRES_2024.csv", 
                   encoding="UTF-8")
colnames(baseIne)

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


#valores poblacionales

numeroActas = length( baseIne$CLAVE_ACTA )
totalVotos = sum( baseIne$total )
thetaPoblacionalMor = sum( baseIne$totalMorena ) / totalVotos
thetaPoblacionalPan = sum( baseIne$totalPan ) / totalVotos
thetaPoblacionalMC = sum( baseIne$totalMC ) / totalVotos



RMorHajek= rep(0,1000)

RPanHajek =  rep(0,1000)

RMCHajek = rep(0,1000)

#anadimos columna vk teorico

razonTeoricaMor = sum( baseIne$totalMorena ) / sum( baseIne$total)
razonTeoricaPan = sum( baseIne$totalPan ) / sum( baseIne$total)
razonTeoricaMC = sum( baseIne$totalMC ) / sum( baseIne$total)

#calculamos las variables de linealizacion
tz = sum( baseIne$total)
v_Mor = ( baseIne$totalMorena - razonTeoricaMor*baseIne$total) / tz
v_Pan = ( baseIne$totalPan - razonTeoricaPan*baseIne$total) / tz
v_MC= ( baseIne$totalMC - razonTeoricaMC*baseIne$total) / tz


#inicializamos las varianzas teoricas en 0
varTeoMorena = 0
varTeoPan = 0
varTeoMC = 0

#definimos el tamanio de muestra
porcentaje = 0.4
n = round( numeroActas*porcentaje)


#obtenemos algunos valores necesarios para usar la expresion de la varianza
pi_kl = ( n*(n-1) ) / ( numeroActas* (numeroActas-1) ) #pi_kl para mas
pi_k = ( n / numeroActas ) #pi_k para el mas
deltakl = pi_kl - (n/numeroActas)*(n/numeroActas) #cuando k =! l
deltall = (n/numeroActas) - (n/numeroActas)*(n/numeroActas) #cuando k=l

#obtenemos las varianzas teoricas (sumando)
for (k in 1:numeroActas) {
  for (l in 1:numeroActas) {
    if (k != l) {
      
      varTeoMorena =  varTeoMorena + 
        deltakl* ( v_Mor[k] / pi_k) * ( v_Mor[l] /pi_k )
      
      varTeoPan = varTeoPan + 
        deltakl* ( v_Pan[k] / pi_k) * ( v_Pan[l] /pi_k )
      
      varTeoMC = varTeoMC + 
        deltakl* ( v_MC[k] / pi_k) * ( v_MC[l] /pi_k ) 
    }
    else {
      
      varTeoMorena =  varTeoMorena + 
        deltall* ( v_Mor[k] / pi_k) * ( v_Mor[l] /pi_k )
      
      varTeoPan = varTeoPan + 
        deltall* ( v_Pan[k] / pi_k) * ( v_Pan[l] /pi_k )
      
      varTeoMC = varTeoMC + 
        deltall* ( v_MC[k] / pi_k) * ( v_MC[l] /pi_k )
    } #termina else
    
  } #termina segundo for
  
} #termina primer for

print(paste("Varianza teorica morena: " , varTeoMorena))
print(paste("Varianza teorica morena: " , varTeoPan))
print(paste("Varianza teorica morena: " , varTeoMC))




#inicializamos el vector que almacena las varianzas
varianzasMorHajek = rep(0,1000)
varianzasPanHajek = rep(0,1000)
varianzasMCHajek = rep(0,1000)


for (iteracion in 1:1000) {
  
  indicesSeleccionados = sample(1:numeroActas, 
                                size = n , 
                                replace = FALSE)
  #calculamos la estimacion
  totalEstimacion = ( mean( baseIne$total[indicesSeleccionados]) )
  
  #estimamos la razon
  RMorHajek[iteracion] =  ( mean( baseIne$totalMorena[indicesSeleccionados] ) )
  / totalEstimacion
  
  RPanHajek[iteracion] = ( mean( baseIne$totalPan[indicesSeleccionados] ) )
  / totalEstimacion
  
  RMCHajek[iteracion] = ( mean( baseIne$totalMC[indicesSeleccionados] ) )
  / totalEstimacion
  
  
  #calculamos las variables de linealizacion
  vEst_mor = rep(0 , n)
  vEst_pan = rep(0 , n)
  vEst_mc = rep(0 , n)
  
  vEst_mor = (1/ totalEstimacion) * 
    ( baseIne$totalMorena[indicesSeleccionados] - 
        RMorHajek[iteracion]*  baseIne$total[indicesSeleccionados] )
  
  vEst_pan = (1/ totalEstimacion) * 
    ( baseIne$totalPan[indicesSeleccionados] - 
        RPanHajek[iteracion]*  baseIne$total[indicesSeleccionados] )
  
  vEst_mc = (1/ totalEstimacion) * 
    ( baseIne$totalMC[indicesSeleccionados] - 
        RMCHajek[iteracion]*  baseIne$total[indicesSeleccionados] )
  
  #calculamos la varianza estimada
  
  
  pi_kl = ( n*(n-1) ) / ( numeroActas* (numeroActas-1)) #pi_kl para mas
  pi_k = ( n / numeroActas ) #pi_k para el mas
  deltaklEst = 1 -  ( ( (n/numeroActas)*(n/numeroActas) ) / pi_kl ) #cuando k=!l
  deltallEst = 1 -  ( (n/numeroActas)*(n/numeroActas) / pi_k ) #cuando k=l
  
  for (k in 1:n) {
    for (l in 1:n) {
      if ( k != l) {
        
        varianzasMorHajek[iteracion] = varianzasMorHajek[iteracion] +
          deltaklEst* (vEst_mor[k] / pi_k ) * ( vEst_mor[l] / pi_k )
        
        varianzasPanHajek[iteracion] = varianzasPanHajek[iteracion] +
          deltaklEst* (vEst_pan[k] / pi_k ) * ( vEst_pan[l] / pi_k )
        
        varianzasMCHajek[iteracion] = varianzasMCHajek[iteracion] +
          deltaklEst* (vEst_mc[k] / pi_k ) * ( vEst_mc[l] / pi_k )
        
        
      }else{
        
        varianzasMorHajek[iteracion] = varianzasMorHajek[iteracion] +
          deltallEst* (vEst_mor[k] / pi_k ) * ( vEst_mor[l] / pi_k )
        
        varianzasPanHajek[iteracion] = varianzasPanHajek[iteracion] +
          deltallEst* (vEst_pan[k] / pi_k ) * ( vEst_pan[l] / pi_k )
        
        varianzasMCHajek[iteracion] = varianzasMCHajek[iteracion] +
          deltallEst* (vEst_mc[k] / pi_k ) * ( vEst_mc[l] / pi_k )
        
      }#termina else
      
    }#termina primer for
  }#termina segundo for
  
} #termina ciclo for para elaborar estimaciones



