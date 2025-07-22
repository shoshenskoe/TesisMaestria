## muestreo estratificado

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



#tamanio de muestra
porcentaje = 0.15
tamanioMuestra = round( numeroActas*porcentaje)

#visualizamos 
#baseIne %>% group_by(idEstrato) %>% summarise( conteoActas =  n() ) %>%
#  arrange(  conteoActas , .by_group = TRUE) %>% 
#  mutate( conteoEstrato =  conteoActas)%>% ungroup()

baseIne =  baseIne %>% group_by(idEstrato) %>% 
  mutate( totalEstrato =  n(), peso = numeroActas / tamanioMuestra ) %>% 
  ungroup() 



#obtenemos una muestra de los indices por estrato
muestraIndices = baseIne %>% 
  group_by( idEstrato ) %>%  #agrupamos por estrato
  slice_sample( replace = FALSE, prop = 0.15) %>% #obtemeos registros
  ungroup() %>% #desagrupamos 
  select(CLAVE_ACTA) %>%  #nos quedamos con la columna que contiene las claves
  pull() #lo convertimos en un vector de strings

muestra = baseIne %>% filter( CLAVE_ACTA %in%  muestraIndices ) #filtramos

disenioEstratificado = survey::svydesign(id = ~1, strata = ~idEstrato, 
                                         data = muestra, fpc = ~totalEstrato, weights = ~peso)

survey::svyratio( numerator = ~totalMorena+ totalPan+totalMC , 
                  denominator = ~total,
                  design =  disenioEstratificado, object = muestra )

#survey::svyby( ~totalMorena,denominator=~total, design =  disenioEstratificado,
#                by = ~idEstrato, 
#                FUN = svyratio )



