#estimador razon o hajek

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


library(survey)

#numero de simulaciones
numSimulaciones = 5

#fijamos la cantidad de muestra
porcentaje = 0.4
n = round( numeroActas*porcentaje)

#inicializamos estimadores 
RMorHajek= rep(0,numSimulaciones)
RPanHajek =  rep(0,numSimulaciones)
RMCHajek = rep(0,numSimulaciones)

#inicializamos vector que almanacena varianzas estimadas
varianzasMorHajek = rep(0,numSimulaciones)
varianzasPanHajek = rep(0,numSimulaciones)
varianzasMCHajek = rep(0,numSimulaciones)

#inicializamos vector que almacena la longitud de intervalos
longitudIntervalosMor =  rep(0,numSimulaciones)
longitudIntervalosPan =  rep(0,numSimulaciones)
longitudIntervalosMC =  rep(0,numSimulaciones)

#inicializamos el contador de los casos de exito 
exitosMorena = 0
exitosPan = 0
exitosMC = 0

for (iteracion in 1:numSimulaciones) {
  print(iteracion)
  indicesSeleccionados = sample(1:numeroActas, 
                                size = n , 
                                replace = FALSE)
  disenio = svydesign( 
    id = ~1 , 
    data = baseIne[indicesSeleccionados,] ,
    weights = ~rep( (numeroActas/n), n) , 
    fpc = ~rep(numeroActas, n)
  )
  
  #summary(disenio)
  
  #realiamos la estimacion de razon
  razon = survey::svyratio(numerator = ~totalMorena + totalPan + totalMC, 
                           denominator =  ~total, design =  disenio ,vartype= "se")
  
  #vaciamos la info de los estimadores en los vectores designados para ello
  RMorHajek [ iteracion] = razon$ratio[1]
  RPanHajek[ iteracion ] = razon$ratio[2]
  RMCHajek [ iteracion ] = razon$ratio [3]
  
  varianzasMorHajek[iteracion] = razon$var[1]
  varianzasPanHajek[iteracion] = razon$var[2]
  varianzasMCHajek[iteracion] = razon$var[3]
  
  intervalos = confint(object = razon, level = 0.95)
  
  #introducimos la longitud de los intervalos
  longitudIntervalosMor[ iteracion ] = intervalos[1,2 ] - intervalos[1,1 ]
  longitudIntervalosPan[iteracion ] = intervalos[2,2] - intervalos[2,1 ]
  longitudIntervalosMC[iteracion ] = intervalos[ 3,2] - intervalos[ 3,1]
  
  #revisamos si el intervalo contiene el valor poblacional real 
  if ( ( intervalos[1,1] <=  thetaPoblacionalMor ) & 
       (thetaPoblacionalMor <= intervalos[1,2 ]  )  ) {
    exitosMorena = exitosMorena +1 
  }
  
  if ( ( intervalos[2,1] <=  thetaPoblacionalPan ) & 
       (thetaPoblacionalPan <= intervalos[2,2 ]  )  ) {
    exitosPan = exitosPan +1 
  }
  
  if ( ( intervalos[3,1] <=  thetaPoblacionalMC ) & 
       (thetaPoblacionalMC <= intervalos[3,2 ]  )  ) {
    exitosMC = exitosMC +1 
  }
  
}#termina for de simulaciones

par(mfrow = c(1, 1))
dev.off() 
hist(longitudIntervalosMor, main = "Longitud intervalos Morena", 
     xlab = "Longitud")
print(exitosMorena/numSimulaciones)
print(exitosPan/numSimulaciones)
print(exitosMC/numSimulaciones)



#graficamos los estimadores
par(mfrow = c(2, 2))
par(mar = c(2, 2, 2, 2) )
par("mar")

tituloMorena = paste ( "Morena  ", porcentaje*100, "%" ) 
tituloPan = paste ( "Pan n =  ", porcentaje*100, "%" ) 
tituloMc = paste ( "MC n =  ", porcentaje*100, "%" ) 

hist(RMorHajek, breaks= 30, main = tituloMorena,
     xlab = "Estimador", ylab = "Frecuencia")
abline(v= thetaPoblacionalMor, col = "red" )


hist(RPanHajek, 
     xlab = "Estimador",  breaks= 30, ylab = "Frecuencia", main = tituloPan)
abline(v= thetaPoblacionalPan, col = "red" )

hist(RMCHajek,
     xlab = "Estimador",  breaks= 30, ylab = "Frecuencia", main= tituloMc)
abline(v= thetaPoblacionalMC, col = "red" )


#graficamos las varianzas
par(mfrow = c(2, 2))
par(mar = c(2, 2, 2, 2) )
par("mar")

hist( varianzasMorHajek, main = "Varianzas morena" , 
      xlab = "estimaciones", ylab = "Frecuencias")
hist( varianzasPanHajek, main = "Varianzas pan" , 
      xlab = "estimaciones", ylab = "Frecuencias")
hist( varianzasMCHajek, main = "Varianzas mc" , 
      xlab = "estimaciones", ylab = "Frecuencias")

#vemos la longitud de los intervalos
summary( longitudIntervalosMor )
summary( longitudIntervalosPan )
summary( longitudIntervalosMC )

library("plotrix")
x <- c(1)
F <- c(thetaPoblacionalMor)
L <- intervalos[1,1]
U <- intervalos[1,2]

# install.packages("plotrix")
plotCI(x, F, ui=U, li=L)
