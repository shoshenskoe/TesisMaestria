#bootrap Rao and Wu (1988)

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

#valores poblacionales

numeroActas = length( baseIne$CLAVE_ACTA )
totalVotos = sum( baseIne$total )


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
library(stringr)

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

#buscamos los indices de las actas que no contienes ningun voto
indicesActasSinVotos = which( baseIne$total==0 )
#eliminamos esos indices del dataframe
baseIne = baseIne[-indicesActasSinVotos,]

#convertimos a factor la columna de las claves del acta pues se usara para
#conteos
baseIne$CLAVE_ACTA = as.factor( baseIne$CLAVE_ACTA )

#creamos los estratos y fijamos el tamanio de muestra
#tamanio de muestra
porcentaje = 0.15
tamanioMuestra = round( numeroActas*porcentaje)

#visualizamos 
#baseIne %>% group_by(idEstrato) %>% summarise( conteoActas =  n() ) %>%
#  arrange(  conteoActas , .by_group = TRUE) %>% 
#  mutate( conteoEstrato =  conteoActas)%>% ungroup()

#anadimos una columna que identifique el peso de la observacion y 
#la cantidad de elementos que hay en el estrato
baseIne =  baseIne %>% group_by(idEstrato) %>% 
  mutate( totalEstrato =  n(), peso = numeroActas / tamanioMuestra ) %>% 
  ungroup() 






## Preparamos la muestra principal

muestraPrincipal = baseIne %>% 
  group_by (idEstrato) %>% slice_sample( prop = porcentaje ) %>% ungroup()


#conteosMuestraPrincipal = muestraPrincipal %>% 
#  group_by(idEstrato) %>% summarise( nh = n() )


#tible de apariciones
tablaApariciones= muestraPrincipal %>% dplyr::count(idEstrato , 
                                               name = "nh" )
#repeticiones bootstrap
R = 50

#creamos el vector que almacenara las repeticiones bootstrap
thetaStar = rep(0 , R)

numEstratosMuestra = length(tablaApariciones$idEstrato)


#library(devtools)
#source_gist("https://gist.github.com/mrdwab/6424112")
#vector1 = setNames(object = tablaApariciones$nh-1, nm= tablaApariciones$idEstrato)
#tiempo1 = Sys.time()
#muestraFinal = stratified(muestraPrincipal, 
#                          group = "idEstrato",
#                          size =vector1, 
#                          replace = TRUE)


for (i in 1:1) {
  tamanioBootstrap = tablaApariciones$nh[1] -1 
  nombreEstrato = tablaApariciones$idEstrato[1]
  
  muestraRepetida = muestraPrincipal %>% 
    filter( idEstrato == nombreEstrato  ) %>% 
    slice_sample(n = tamanioBootstrap  , replace = TRUE) %>%
    select(CLAVE_ACTA)
  
  
  for (estrato in 2:numEstratosMuestra ) {
    tamanioBootstrap = tablaApariciones$nh[estrato] -1 
    nombreEstrato = tablaApariciones$idEstrato[estrato]
    
    muestraBootstrap = muestraPrincipal %>% 
      filter( idEstrato == nombreEstrato  ) %>% 
      slice_sample(n = tamanioBootstrap  , replace = TRUE) %>%
      select(CLAVE_ACTA)
    muestraRepetida = dplyr::bind_rows(muestraRepetida,muestraBootstrap  )
    
  }
  
  #tible de apariciones en las repeticiones bootstrap
  
  muestraConteos = muestraRepetida %>% dplyr::count(CLAVE_ACTA , 
                                                    name = "mhjr", 
                                                    .drop = FALSE )
  #join la tabla de contetos bootstrap con la tabla de la muestra principal
  #anadimos una columna llamada repWeight 
  #que nos indica el nuevo peso de la observacion bajo
  #el algoritimo bootstrap de Rao-Wu
  
  tablaBootstrap = muestraPrincipal%>%
    dplyr::inner_join( muestraConteos, by = "CLAVE_ACTA" ) %>% 
    mutate( repWeight = mhjr* peso *(totalEstrato / (totalEstrato-1) ) )
  
  #calculamos el estimador de Razon Hajek y lo almacenamos en la entrada
  #i del vector thetaStar
  
  thetaStar[i] = tablaBootstrap %>% 
    mutate( sumNum =  repWeight*totalMorena , sumDen = repWeight*total  ) %>% 
    summarise( numerador = sum ( sumNum  ), denominador = sum(sumDen) ) %>%
    summarise( estimacionHT =  numerador/ denominador ) %>% pull()
  print(i)
}
print(tiempoFinal - tiempoInicial)

thetaEstPrincipal = muestraPrincipal %>% 
  mutate( sumNum =  peso*totalMorena , sumDen = peso*total  ) %>% 
  summarise( numerador = sum ( sumNum  ), denominador = sum(sumDen) ) %>%
  summarise( estimacionHT =  numerador/ denominador ) %>% pull()
hist(thetaStar)
summary(thetaStar)
varianza = mean( ( thetaStar - thetaEstPrincipal )^2  )





disen = svydesign( data = muestraPrincipal,
                   ids = ~CLAVE_ACTA,
                   fpc = ~totalEstrato,
                   strata = ~idEstrato,
                   weights = ~peso)
library(svrep)

boots = svrep::as_bootstrap_design(
  disen,
  type = "Rao-Wu",
  replicates = 10
)

algo = survey::svyratio( numerator = ~totalMorena+ totalPan+totalMC, 
                  denominator = ~total , 
                  design = boots )
confint(algo)

library("survey")
disenioBootstrap = svydesign( data = tablaBootstrap,
                              ids = ~CLAVE_ACTA,
                              fpc = ~totalEstrato,
                             strata = ~idEstrato,
                             weights = ~repWeight)

tablaBootstrap$repWeight %>% head(5)
tablaBootstrap$peso %>% head(5)

estimacionMorena = survey::svyratio( numerator = ~totalMorena, 
                  denominator = ~total , 
                  design =disenioBootstrap )

print(paste( "Una estimacion bootstrap del porcentaje a favor de Morena :  ",
             estimacionMorena$ratio) )


#analizamos las coberturas

#fijamos el numero de simulaciones
simulaciones = 500

#inicializamos el vector que contiene los exitos en las coberturas
ensayoBootstrapMor = rep(0, simulaciones)
ensayoBootstrapPan = rep(0, simulaciones)
ensayoBootstrapMC = rep(0, simulaciones)

longMor = rep(0, simulaciones)
longPan = rep(0, simulaciones)
longMC = rep(0, simulaciones)

estimacionesMor = rep(0, simulaciones)
estimacionesPan = rep(0, simulaciones)
estimacionesMC = rep(0, simulaciones)

#install.packages("svrep")
#library("svrep")

for (ensayo in 1:simulaciones) {
  
  #obtenemos una muestra de los indices por estrato
  muestra = baseIne %>% 
    group_by( idEstrato ) %>%  #agrupamos por estrato
    slice_sample( replace = FALSE, prop = 0.004) %>% #obtemeos registros
    ungroup()
  
  disenio = survey::svydesign(id = ~1, strata = ~idEstrato, 
                              fpc = ~totalEstrato, 
                              weights = ~peso, 
                              data  = muestra ) #realizamos un disenio estratificado 
  
  disenioBootstrap = svrep::as_bootstrap_design( disenio ,
                                                 type = "Rao-Wu",
                                                 replicates = 100) #disenio bootstrap
  
  ratios = survey::svyratio( numerator = ~totalMorena+totalPan+totalMC, 
                             denominator = ~total , 
                             design = disenioBootstrap , 
                             data = muestra ,
                             vartype= "ci") #calculamos ratios con bootstrap
  
  #calculamos intervalo de confianza
  intervalos = confint(ratios, level = 0.95)
  
  #almacenamos el estimador
  estimacionesMor[ensayo] = ratios$ratio[1]
  estimacionesPan[ensayo] = ratios$ratio[2]
  estimacionesMC[ensayo] = ratios$ratio[3]
  
  #almacenamos la longitud de los intervalos
  longMor[ensayo] =  intervalos[1,2] - intervalos[1,1] 
  longPan[ensayo] = intervalos[2,2] -intervalos[2,1]
  longMC[ensayo] = intervalos[3,2] - intervalos[3,1]
  
  
  #revisamos si se realiza con exito la cobertura
  ensayoBootstrapMor[ensayo] = as.integer( 
    ( intervalos[1,1] <= thetaPoblacionalMor ) & 
      ( thetaPoblacionalMor <= intervalos[1,2] ) )
  
  ensayoBootstrapPan[ensayo] = as.integer( 
    ( intervalos[2,1] <= thetaPoblacionalPan ) & 
      ( thetaPoblacionalPan <= intervalos[2,2] ) )
  
  ensayoBootstrapMC[ensayo] = as.integer( 
    ( intervalos[3,1] <= thetaPoblacionalMC ) & 
      ( thetaPoblacionalMC <= intervalos[3,2]  ) )
  
  print(ensayo)
} #termina for para coberturas

#graficamos los estimadores
par(mfrow = c(2, 2))
par(mar = c(2, 2, 2, 2) )
par("mar")

tituloMorena = paste ( "Morena  ", porcentaje*100, "%" ) 
tituloPan = paste ( "Pan n =  ", porcentaje*100, "%" ) 
tituloMc = paste ( "MC n =  ", porcentaje*100, "%" ) 

hist(estimacionesMor, breaks= 50, main = tituloMorena,
     xlab = "Estimador", ylab = "Frecuencia")
abline(v= thetaPoblacionalMor, col = "red" )


hist(estimacionesPan, 
     xlab = "Estimador",  breaks= 50, ylab = "Frecuencia", main = tituloPan)
abline(v= thetaPoblacionalPan, col = "red" )

hist(estimacionesMC,
     xlab = "Estimador",  breaks= 50, ylab = "Frecuencia", main= tituloMc)
abline(v= thetaPoblacionalMC, col = "red" )

max(longMor)
max(longPan)
max(longMC)

mean(ensayoBootstrapMor)
mean(ensayoBootstrapPan)
mean(ensayoBootstrapMC)


