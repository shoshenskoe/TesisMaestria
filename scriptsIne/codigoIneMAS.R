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


#Muestreo aleatorio simple 

#ajustamos la base para calcular un estimador HT de un total sencillo
baseIne = baseIne%>% mutate( totalMorena = totalMorena/ totalVotos,
                             totalPan = totalPan / totalVotos ,
                             totalMC = totalMC / totalVotos )


estimadoresMor= rep(0,1000)
varianzasMor = rep(0,1000)

estimadoresPan =  rep(0,1000)
varianzasPan = rep(0,1000)

estimadoresMC = rep(0,1000)
varianzasMC = rep(0,1000)


#realizamos el muestreo mas

porcentaje = 0.4
n = round( numeroActas*porcentaje)

for (iteracion in 1:3000) {
  
  indicesSeleccionados = sample(1:numeroActas, 
                                size = n , 
                                replace = FALSE)
  
  estimadoresMor[iteracion] = ( numeroActas* 
    mean( baseIne$totalMorena[indicesSeleccionados]  ) )
  varianzasMor[iteracion] = ( ( (numeroActas^2) / n )* 
    ( 1- (n / numeroActas) ) ) *var( baseIne$totalMorena[indicesSeleccionados] )
  
  estimadoresPan[iteracion] = ( numeroActas*
    mean( baseIne$totalPan[indicesSeleccionados] ) )
  varianzasPan[iteracion] = ( ( (numeroActas^2) / n )* 
    ( 1- (n / numeroActas) ) )*var( baseIne$totalPan[indicesSeleccionados] )
  
  estimadoresMC[iteracion] = ( numeroActas*
    mean( baseIne$totalMC[indicesSeleccionados] ) )
  varianzasMC[iteracion] = ( ( (numeroActas^2) / n )* 
    ( 1- (n / numeroActas) ) )*var( baseIne$totalMC[indicesSeleccionados] )
  
} #termina ciclo for para elaborar estimaciones


#graficamos los estimadores
par(mfrow = c(2, 2))
par(mar = c(2, 2, 2, 2) )
par("mar")

tituloMorena = paste ( "Morena  ", porcentaje*100, "%" ) 
tituloPan = paste ( "Pan n =  ", porcentaje*100, "%" ) 
tituloMc = paste ( "MC n =  ", porcentaje*100, "%" ) 

hist(estimadoresMor, breaks= 50, main = tituloMorena,
     xlab = "Estimador", ylab = "Frecuencia")
abline(v= thetaPoblacionalMor, col = "red" )


hist(estimadoresPan, 
     xlab = "Estimador",  breaks= 50, ylab = "Frecuencia", main = tituloPan)
abline(v= thetaPoblacionalPan, col = "red" )

hist(estimadoresMC,
     xlab = "Estimador",  breaks= 50, ylab = "Frecuencia", main= tituloMc)
abline(v= thetaPoblacionalMC, col = "red" )

#evaluamos cuantos intervalos capturan el verdadero valor

cuantilNormal = qnorm(0.975, mean = 0, sd=1 )

interInferior = estimadoresMor - ( cuantilNormal* sqrt( varianzasMor ) )
interSuperior = estimadoresMor + ( cuantilNormal* sqrt( varianzasMor ) )

intervalosExitosos = rep( 0,  length(estimadoresMor))

intervalosExitosos = (interInferior <= thetaPoblacionalMor) & 
  (thetaPoblacionalMor <= interSuperior)

sum(intervalosExitosos) / length(intervalosExitosos)

longitudIntervalos = interSuperior - interInferior

par(mfrow = c(1, 1))
dev.off() 
summary(longitudIntervalos)
hist(longitudIntervalos, main = "Longitud de intervalos de confianza", 
     xlab = "Lontigud", ylab = "Frecuencia")

#calculo de varianza poblacional (usando estimador insesesgado)

varianzasMorPoblacional = ( (numeroActas^2) / n )*
  ( 1- (n / numeroActas) )*var( baseIne$totalMorena )

hist( varianzasMor, main = "Varianzas estimadas morena" , 
      xlab = "estimaciones", ylab = "Frecuencias")
abline(v=varianzasMorPoblacional,  col = "red")

