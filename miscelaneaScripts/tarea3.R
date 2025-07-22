library(tidyverse)

##----- preparacion de la base
setwd("/Users/shoshenskoe/Documents/muestreo/tarea3")
base0 = read.csv("~/Documents/muestreo/tarea3/diputaciones.csv", encoding="UTF-8")

colnames(base0)

base = base0 %>% 
  select(-CLAVE_CASILLA, -NOMBRE_ESTADO,-ID_CASILLA,
                        -TIPO_CASILLA,-CASILLA, -MECANISMOS_TRASLADO, 
                -NOMBRE_DISTRITO, -EXT_CONTIGUA, -NUM_ACTA_IMPRESO,
                -OBSERVACIONES, -FECHA_HORA, - SECCION, 
                -LISTA_NOMINAL_CASILLA) %>% #quitamos algunas columnas
mutate(total = TOTAL_VOTOS_CALCULADOS - VOTOS.NULOS ,
                    total_coalicion= PAN+ PRD+ PRI + PAN.PRI.PRD + PAN.PRD +
                      PAN.PRI + PRI.PRD) #calculamos nuevas columnas

#revisamos las columnas de la nueva base
nombresColumnas = colnames(base)

#una vez obtenidas los totales de la coalicion y sus combinaciones procedemos
#a quitar de la base los coneteos de otros partidos. Nos quedamos con total
#y total_coalicion
columnasNoNecesarias = nombresColumnas[4:25]

base = base %>% 
  select( -any_of(columnasNoNecesarias) ) #quitamos columnas


#vemos la estructura de la base
str(base)

#write.csv(base,"/Users/shoshenskoe/Documents/muestreo/tarea3/base_final.csv",
#row.names=FALSE)

#Creamos un nuevo id combinando el id_estado y id_distrito llamado 
#idDistrito
base = base %>% mutate( ID_ESTADO = as.integer(ID_ESTADO), 
                        ID_DISTRITO = as.integer(ID_DISTRITO) ) %>%
mutate(ID_ESTADO = ID_ESTADO+10, ID_DISTRITO = ID_DISTRITO + 10 ) %>% 
mutate( ID_ESTADO = as.character(ID_ESTADO), 
            ID_DISTRITO = as.character(ID_DISTRITO) ) %>% 
mutate ( idDistrito = paste(ID_ESTADO, ID_DISTRITO, sep = "" )  )

#Verificamos que hay 300 distritos electorales 
length( unique( base$idDistrito ) ) == 300

colnames(base)


set.seed(1)
  
###------ Ejercicios

#establecemos el numero de simulaciones que queremos
numSimulaciones = 500

#obtenemos el valor R poblacional (es un tibble)
valorRPoblacional =  base %>% 
  summarise( votosCoalicion = sum( total_coalicion ), 
             votosTotal = sum(total)) %>% #hacemos un resumen
  summarise ( R= votosCoalicion/votosTotal )   %>% #estimamos R
  dplyr::pull( )


#contamos el numero de casillas que hay por cada uno de los 300 distritos
casillasPorDistrito = base %>% group_by( idDistrito) %>% 
  summarise(  conteo = n() )


#calculamos las pi_k de I
base1 = base  %>% inner_join(casillasPorDistrito) %>% 
  mutate( pi_k = (30/300)*(20/conteo) )

#creamos una funcion que seleccione cierto numero de casillas
#dado un distrito
#seleccionCasillas = function(distrito, numCasillas) {
  
#  seleccion = base1 %>% filter( idDistrito %in%  distrito) %>% 
#    sample_n(size = 20, replace = FALSE) %>% select(CLAVE_ACTA)
  
#  return(seleccion)
  
#}

simulaciones1 = rep(0, numSimulaciones)
listadoDistritos = unique( base1$idDistrito)
for (entrada in (1:numSimulaciones) ) {
  
  muestraDistritos = sample( listadoDistritos , 30, replace = FALSE)
  
  
  #casillasSeleccionadas = sapply(muestraDistritos, seleccionCasillas , 20) 
  #casillasSeleccionadas = list_c(casillasSeleccionadas)   
  
 
  
  #### prueba
  
  simulacion1 = base1 %>% 
    filter( idDistrito %in% muestraDistritos ) %>%
    group_by(idDistrito ) %>% 
    sample_n(size = 20, replace = FALSE) %>% ungroup(idDistrito) %>%
    select(CLAVE_ACTA,total_coalicion, total, pi_k ) %>% 
    summarise(estNum = sum( total_coalicion / pi_k), 
              estDen = sum(total/ pi_k)) %>%
    summarise(R = estNum/estDen) %>% pull()
  simulaciones1[entrada] = simulacion1
  
  
  ####fin de prueba
  
  #simulacion1 = base1 %>% filter( CLAVE_ACTA %in%  casillasSeleccionadas) %>% 
  #  summarise(estNum = sum( total_coalicion / pi_k),estDen = sum(total/ pi_k))%>%
  #  summarise(R = estNum/estDen) %>% pull()
  #simulaciones1[entrada] = simulacion1
  #print(paste("Entrada: ", entrada))
  
}

#calculamos el ECM
ECM1 = mean(  (simulaciones1 - valorRPoblacional)^2 )
print(paste("Error cuadratico medio del disenio I: " ,ECM1 ))

#graficamos
hist(simulaciones1,  main = "Simulaciones disenio I")
abline(v= valorRPoblacional, col = "red")


### disenio II

numCasillasMuestra = 19

casillasPorEstado =  base %>% 
  group_by(ID_ESTADO) %>%  #agrupamos por estado
  summarise(conteo = n()) #contamos el numero de actas/casillas por estado

#aqui guardamos las simulaciones
simulaciones2 = rep(0,numSimulaciones)

base2 = base %>% 
  inner_join(casillasPorEstado)  %>%  #cruzamos informacion tabla casillasxEdo
  mutate (pi_k = numCasillasMuestra/ conteo ) #creamos una nueva columna 
  #que nos da el valor pi_k de cada renglon


#simulamos
for (entrada in 1:numSimulaciones) {
  rEstimada2 = base2 %>% group_by(ID_ESTADO) %>% #agrupamos por edo
    sample_n(size = numCasillasMuestra, replace = FALSE) %>% #de cada edo 
    #muestreamos el numero de casillas requerido sin reemplazo
    summarise( numEdo = sum(total_coalicion / pi_k ) , 
               denEdo = sum( total / pi_k ) ) %>% #resumimos info
    summarise( num = sum(numEdo) , den = sum(denEdo) )  %>% 
    summarise( R = num/den )
  
  #convertimos el tibble a un numeric de R usual
  simulaciones2[entrada] = dplyr::pull( rEstimada2)
}

hist(simulaciones2)
mean(simulaciones2)

#calculamos el ECM
ECM2 = mean(  (simulaciones2 - valorRPoblacional)^2 )
print(paste("Error cuadratico medio del disenio II: " ,ECM2 ))

#graficamos
hist(simulaciones2,  main = "Simulaciones disenio I")
abline(v= valorRPoblacional, col = "red")

#### disenio III

#leemos la base con la que obtendremos las circunscripciones
circunscripciones = read_csv("circunscripciones.csv", 
                             col_types = cols(ID_ESTADO = col_character(), 
                                          circunscripcion = col_character()) )

#cruzamos  las bases
base3 = base %>% inner_join(circunscripciones)
#verificamos que se anadio la columna
colnames(base3)

#calculamos una tabla que nos indique los distritos por circunscripcion
distritosPorCircunscripcion = base3%>% 
  select(circunscripcion, idDistrito) %>% distinct() %>%
  group_by(circunscripcion) %>% summarise( totalDistritos = n())

#tabla de actas por distrito electoral
actasPorDistrito = base3%>% group_by(idDistrito)  %>% 
  summarise(numActasDistrito = n() ) 

base3 = base3 %>% 
  inner_join(distritosPorCircunscripcion) %>% #anadimos numero de distrxcir
  inner_join(actasPorDistrito) %>% #anadimos num de actas por distrito
  mutate(pi_k = (8/totalDistritos )*(15/numActasDistrito)  ) %>% # pi_k
  select( CLAVE_ACTA, idDistrito, total, total_coalicion,circunscripcion, pi_k )



simulaciones3 = rep(0,numSimulaciones)

for (i in 1:numSimulaciones) {
  #tabla con muestra de 8  distritos de las 5 circunscripciones
  distritosSeleccionados = base3 %>% 
    select(circunscripcion, idDistrito) %>% #seleccionamos estas columnas
    group_by(circunscripcion)  %>% #agrupamos por circunscripcion
    distinct() %>% # queremos los pares distintos de idDistrito -circunscripcion
    sample_n(size=8)  #por cada elemento de la agrupacion (circunscripcion)
  #seleccionamos 8 elementos (idDistritos)
  
  #tenemos 40 distritos diferentes seleccionados
  #length(unique(distritosSeleccionados$idDistrito)) == 40
  
  #creamos una tabla con la muestra. Como columnas contiene las clave_acta,
  #total de votos validos, el total de votos por la coalicion y los respectivos
  #pi_k
  
  muestra3 = base3 %>%
    filter( idDistrito %in% distritosSeleccionados$idDistrito ) %>% 
    # seleccionamos renglones que esten en distritosSeleccionados
    group_nest(idDistrito) %>% #agrupamos en idDistrito 
    rowwise() %>% #por cada renglon..
    mutate( muestra2= list ( slice_sample(data, n = 15) )  ) %>%  #creamos 
    # una muestra
    select(-data) %>% #eliminamos la columna data
    unnest(muestra2) %>% #expandimos las tablas incluidas en la columna muestra2
    select (CLAVE_ACTA, total, total_coalicion,pi_k ) #las columnas finales
  
  simulaciones3[i] = muestra3 %>% 
    summarise( numerador = sum(total_coalicion / pi_k),
               denominador = sum (total/ pi_k) ) %>% #creamos resumen
    summarise( REstimada3 = numerador/denominador) %>%#otro resumen
    dplyr::pull() #convierte a un numeric usual de R en vez de una tabla (tibble)
}

print(mean(simulaciones3))

#graficamos
hist(simulaciones3, main = "simulaciones disenio III")
abline(v= valorRPoblacional, col = "red")


#calculamos el ECM
ECM3 = mean(  (simulaciones3 - valorRPoblacional)^2 )

print(paste("Error cuadratico medio del disenio III: " , ECM3 ))

print(paste( "Disenio con mejor ECM", which.min( c(ECM1, ECM2, ECM3) ) ) )

##b

#asociamos la variable de linealización por cada renglon
base1 = base1 %>% 
  mutate( v_k = ( total_coalicion - valorRPoblacional* total)/ sum(total) )

#creamos el vector de variables de linealización 
Vs = base1 %>% group_nest(idDistrito) %>% rowwise() %>% 
  mutate( sumaV = sum(data$v_k ) )  %>% select( sumaV) %>% dplyr::pull()


matrizDeltaUPM = matrix(0, nrow= 300, ncol = 300)
val1= (30/300)*(29/299) - ( (30^2)/(300^2) ) #valor i != j
val2 = (30/300) - ( (30^2)/(300^2) )  #valor i = j

#llenamos matrizDeltaUPM
for (i in 1:300) {
  for (j in 1:300) {
    
    if (i == j) {
      matrizDeltaUPM[i,j] = val2 #valor i = j
    }
    else{
      matrizDeltaUPM[i,j] = val1  #valor i != j
    }#terma if-else
    
  }#termina segundo for
  
}#termina primer for

#multiplicamos matrices para obtener VPSU
VPSU =  as.numeric( t(Vs / (30/300) ) %*% matrizDeltaUPM %*%  (Vs/(30/300) )  )


#Empezamos el calculo de VSSU

#creamos una lista de distritos
listaDistritos = base1 %>% group_nest(idDistrito) %>%
  select(idDistrito)  %>% dplyr::pull()

#creamos el vector V*
VEstrella = rep(0, length(listaDistritos))

#le ponemos nombre a cada entrada de VEstrella (su idDistrito)
names(VEstrella) = listaDistritos

#llenamos el vector V*
for (distrito in listaDistritos) {
  
  y = base1 %>% filter(idDistrito == distrito) %>% 
    select( v_k) %>% dplyr::pull()
  
  tamanioDistrito = length(y)
  
  matrizDeltaCasillas = matrix(0, nrow= tamanioDistrito, ncol = tamanioDistrito)
  
  val1= (20/tamanioDistrito)*(19/(tamanioDistrito-1)) - 
    ( (20^2)/(tamanioDistrito^2) ) #valor i != j
  val2 = (20/tamanioDistrito) - ( (20^2)/(tamanioDistrito^2) )  #valor i = j
  
  #llenamos matrizDeltaCasillas
  for (i in 1:tamanioDistrito) {
    for (j in 1:tamanioDistrito) {
      
      if (i == j) {
        matrizDeltaCasillas[i,j] = val2 #valor i = j
      }
      else{
        matrizDeltaCasillas[i,j] = val1  #valor i != j
      }#terma if-else
      
    }#termina segundo for
    
  }#termina primer for
  
  #multiplicamos matrices para obtener V*[distrito] formula 157
  VEstrella[distrito] = t(y/ (20/tamanioDistrito) )  %*% 
    matrizDeltaCasillas  %*% (y/ (20/tamanioDistrito) )
  
} #termina el llenado de V*

#Finalmente calculamos VSSU
#formula 156
VSSU = sum(VEstrella / (30/300) )

varianzaI = VPSU + VSSU

### c3

#se usa formula 128


#la base1 ya tiene calculados los vk. Veremos las varianzas por estado
#para calcular la formula 100 de la varianza por cada estado
tablaVarianzas = base1 %>%  group_by( ID_ESTADO ) %>%
  summarise( varianza =  var( v_k )  ) 

#Veremos los coeficientes de ponderacion de las varianzas por estado
tablaPonderacion = base1 %>% group_nest(ID_ESTADO) %>% rowwise() %>% 
  mutate( numElementos =  nrow(data), 
          ponderacion = ( (numElementos^2) /19 )*(1 - (19/numElementos) ) ) %>%
  select(ID_ESTADO, ponderacion)

#juntamos las tablas de las varianzas y su ponderacion para posterioremente
#calcular la varianza de la formula 128
varianzaII = tablaVarianzas %>% 
  inner_join(tablaPonderacion) %>% 
  summarise( varianza = sum( varianza*ponderacion ) ) %>% dplyr::pull()
  
print( paste( "Disenio con menor varianza:", which.min( c(varianzaI, 
                                                          varianzaII) ) ) )
print(paste( "Disenio con mejor ECM", which.min(c( ECM1, ECM2) ) ) )
        


### HAY COINCIDENCIA CON EL MEJOR DISENIO OBTENIDO VIA SIMULACIONES    





## 2

#los incisos i y ii se hacen por escrito.

#inciso iii

renglon1 = c( ( (2/300) - (2/300)*(2/300) ) / (2/300) ,  
          ( (2/300) - (2/300)*(2/300) ) / (2/300)*(1/299) )
renglon2 = rev(renglon1)

deltaI = rbind(renglon1, renglon2)

reng1 = c( ( (2/100) - (2/100)*(2/100) ) / (2/100) ,  
           ( (2/100) - (1/300)*(2/299) ) / (2/100)*(1/99) )
reng2 = rev( reng1 )
deltai = rbind(reng1, reng2)

t= c( (20+20)/ (2/100), (23+25)/(2/100) )

y1 = c( 20,20)

y2 = c( 23,25 )

v1 = as.numeric ( t( y1 / (2/100) ) %*% deltai %*% ( y1 / (2/100) ) )
v2 = as.numeric ( t( y2 / (2/100) ) %*% deltai %*% ( y2 / (2/100) ) )

varianzaInsesgada2 = as.numeric( t( t/(2/300) ) %*% deltaI %*%  t/(2/300) )

varianzaInsesgada2 = varianzaInsesgada2 + (300/2)*(v1+v2)

print(paste("Varianza insesgada : ", varianzaInsesgada2))



#inciso iv

nI = 2

pik = (2/300)*(2/100)

varianzaConExpansion = ( ( (20+20)/pik ) - ( (20+20+23+25 /pik ) / 2 ) )^2
varianzaConExpansion = varianzaConExpansion + 
  ( ( (23+25)/pik ) - ( (20+20+23+25 /pik ) / 2 ) )^2

varianzaConExpansion = 2*varianzaConExpansion

print(paste("Varianza utilizando solo factores de expansion: ", 
            varianzaConExpansion))




## 3

baseHogar <- read.csv("~/Documents/muestreo/tarea3/THOGAR.csv")

#cambiamos a categorico la variable P3A1_1 y el nombre de la columa
#P3A1_1 por respuesta
baseHogar = baseHogar %>% mutate( P3A1_1= as.factor(P3A1_1) )%>% 
  rename( respuesta = P3A1_1 )


library(survey)


options(survey.lonely.psu="adjust")

#creamos el modelo con survey
hogar <-svydesign(id=~UPM_DIS,strat=~EST_DIS,weight=~FACTOR,data=baseHogar,
                  nest=TRUE)

#revisamos el modelo
summary(hogar)

#revisamos cuantos registros hay de las opciones posibles
# 1- quiere vivienda nueva , 2- no quiere , 3-no sabe
svytable(~respuesta, hogar)
#en porcentajes
prop.table(svytable(~respuesta,design=hogar))*100 



#estimacion del total (del todo el pais)
total = svytotal(~respuesta, hogar)
print(total)

#estimacion porcentaje
porcentaje = svymean(~respuesta, hogar)
print(porcentaje)

#intervalo de confianza del total (de todo el pais)
intervalo = confint(svytotal(~respuesta, hogar), df=degf(hogar))
print(intervalo)

#intervalo de confianza del porcentaje (de todo el pais)
intervalo = confint(svymean(~respuesta, hogar), df=degf(hogar) , level = 0.90)
print(intervalo)


#estimamos el total por estados (incluye errores estandar)
totalPorEstados = svyby(~respuesta,~ENT,design=hogar, svytotal)
print(totalPorEstados)

#estimamos el porcentaje por estados (incluye errores estandar)
porcentajePorEstados = svyby(~respuesta,~ENT,design=hogar, svymean)
print(porcentajePorEstados)


#  intervalos de confianza (no se piden)
svyby(~respuesta,~ENT,design=hogar, svymean,vartype="ci")


#punto extra

tablaPorcentajes = as_tibble( porcentajePorEstados %>% select( ENT, respuesta1) )
tablaPorcentajes= tablaPorcentajes %>% mutate( ENT = as.character (ENT) ) %>%
  rename( region = ENT, value= respuesta1 )  %>% 
  mutate( value = (value/ sum(value) )*100 ) 

tablaPorcentajes %>% summarise(conteo = sum(value))

#entidades=c("AGU", "BCN", "BCS", "CAM", "COA", "COL", "CHP", 
#            "CHH", "CMX", "DUR", "GUA", "GRO", "HID", "JAL", 
#            "MEX", "MIC", "MOR", "NAY", "NLE", "OAX", "PUE", 
#            "QUE", "ROO", "SLP", "SIN", "SON", "TAB", "TAM", 
#            "TLA", "VER", "YUC", "ZAC")


#devtools::install_github('diegovalle/mxmaps')

library("mxmaps")

mxstate_choropleth( tablaPorcentajes,  
                    num_colors = 1, title = "Grafica",legend = "%")


## 4

basePobreza = read.csv("~/Documents/muestreo/tarea3/pobreza_20.csv")
totalObservaciones = basePobreza %>% count() %>% pull()
basePobreza = basePobreza %>% select(folioviv,upm, foliohog, 
                                     numren, ictpc, ent, factor, )

#numero de upms
N= length( unique(basePobreza$upm) )

cantUPMxEnt = basePobreza %>% select(ent, upm ) %>% distinct() %>% group_by(ent) %>%
  summarise( cantUPM = n())

#listado de las upm y su entidad
UPMEntidad = basePobreza %>% select(ent, upm ) %>% distinct()

#vector de upms
vectorUPM = unique( basePobreza$upm )

#matriz de numUpm x numUPM
pi_I = matrix(0, nrow = N, ncol = N)


#suma deltas
VPSU4 = 0
entidades = unique( basePobreza$ent)

#calculamos VPSU4 iterando pore entidades
for (entidad in entidades) {
  
  cantUPM = basePobreza %>% filter( ent == entidad ) %>% count() %>% pull() 
  
  #calculamos los totales por upm (que ademas ya contempla su ponderizacion 
  # piI_k).
  t = basePobreza %>% filter( ent == entidad ) %>% 
    select(ictpc,upm )  %>% group_by(upm) %>%
    summarise( total = sum(ictpc) / (11/cantUPM ) )
  
  productoCartesianoT = t %>% cross_join(t) 
  
  sumaIguales = productoCartesianoT  %>% filter( upm.x == upm.y) %>%
    mutate( delta = (11/cantUPM) - ( (11^2) / (cantUPM^2) ) ) %>%
    summarise( suma =  sum( (total.x) * (total.y)*delta ) ) %>% pull()

  sumaDiferentes = productoCartesianoT  %>% filter( upm.x != upm.y) %>%
    mutate( delta = (11/cantUPM)*(10/(cantUPM-1) ) - 
              ( (11^2) / (cantUPM^2) ) ) %>%
    summarise( suma =  sum( (total.x) * (total.y)*delta ) ) %>%
    pull()
  
  
  VPSU4 = VPSU4 + sumaDiferentes + sumaIguales
}#cierra for

print(paste("Vpsu = ", VPSU4))

#almacenamos las V de la formula 156
V4 = rep(0, N)

#recorremos el vector V4
for (numUPM in 1:N) {
  
  #print(paste("upm numero : ", numUPM))
  
  tamanoUPM = basePobreza %>% filter(upm == numUPM) %>% count() %>% pull()
  
  if ( tamanoUPM == 0) {
    next
  }
  
  y = basePobreza %>% filter(upm == numUPM) %>% select( ictpc ) %>% 
    add_column(identificador = 1:tamanoUPM)
  
  productoCartesianoT = y %>% cross_join(y) 
  
  minimo = min(3, tamanoUPM)
  
  sumaIguales = productoCartesianoT %>% 
    filter( identificador.x == identificador.y ) %>% 
    mutate( delta = ( minimo /tamanoUPM ) -  
              ( minimo /tamanoUPM )^2  ) %>%
    summarise( suma = sum( (ictpc.x)*(ictpc.y)*
                             delta )*( ( tamanoUPM / minimo )^2 ) ) %>%
    pull()
  
  sumaDiferentes = productoCartesianoT %>% 
    filter( identificador.x != identificador.y ) %>% 
    mutate( delta = (minimo / tamanoUPM )*( (minimo-1) / (tamanoUPM-1) ) -  
              ( minimo /tamanoUPM )^2  ) %>%
    summarise( suma = sum( (ictpc.x)*(ictpc.y)*
                             delta )*( ( tamanoUPM / minimo )^2 ) ) %>%
    pull()
  
  V4[numUPM] = sumaDiferentes + sumaIguales
  
}#termina for

#calculamos los pi_I de cada upm
upmPI = UPMEntidad %>% inner_join(cantUPMxEnt) %>% mutate( piI = (11/cantUPM) ) %>% 
  select(upm, piI)

#calculamos Vssu
VSSU4 = upmPI %>% add_column( V= V4) %>% 
  summarise( Vssu = sum( V/piI )  ) %>% pull()

library("scales")

#calculamos varianza teorica
varTeorica = ( VSSU4 + VPSU4)/ (totalObservaciones^2)

print(paste("La varianza teorica es : ", 
            prettyNum(varTeorica, big.mark = ",") ) )



## ii Estimaciones MC

tPoblacional = sum( basePobreza$ictpc ) / (totalObservaciones)
numSimulacionesMC = 2000

simulacionesMC = rep(0, numSimulacionesMC)

for (indice in 1:numSimulacionesMC) {
  
  #print(indice)
  
  
  muestraUPM  = basePobreza %>% group_by( ent ) %>% 
    select( ent, upm ) %>% 
    distinct() %>% sample_n(11, replace = FALSE) %>% ungroup() %>%
    select( upm ) %>% pull()
  
  #procedemos a la estimacion
  
  simulacionesMC[indice] = basePobreza %>% 
    filter( upm %in% muestraUPM ) %>% 
    group_nest( upm ) %>% 
    rowwise() %>% 
    mutate(
      # calculamos el tamanio min(3, elementos en la upm)
      tamanioMuestra = min(3, nrow(data) ) , 
      # creamos la variable muestraElementos dentro de la upm
      muestraElementos = list( sample_n(data, size = tamanioMuestra ) ) ) %>% 
    select( -c(data, tamanioMuestra) ) %>% 
    unnest(muestraElementos) %>% select( ictpc, factor ) %>% 
    summarise( estimacion = sum( ictpc*factor )  ) %>% 
    pull()
  
}#cierra for de simulaciones


basePobreza %>% add_column( id = 1:totalObservaciones )

ECM4 = mean( ( ( simulacionesMC/ totalObservaciones) - tPoblacional )^2 )


## bootstrap

#seleccionamos una muestra 

#primero usamos una muestra de los upm en cada estrato (entidad)
muestraUPM  = basePobreza %>% group_by( ent ) %>% 
  select( ent, upm ) %>% 
  distinct() %>% sample_n(11, replace = FALSE) %>% ungroup() %>%
  select( upm ) %>% pull()

#de cada upm seleccionamos 3 o el minimo de elementos que contenga cada
#upm . Juntamos todos los elementos para construir la muestra
muestra = basePobreza %>% 
  filter( upm %in% muestraUPM ) %>% 
  group_nest( upm ) %>% 
  rowwise() %>% 
  mutate(
    # calculamos el tamanio min(3, elementos en la upm)
    tamanioMuestra = min(3, nrow(data) ) , 
    # creamos la variable muestraElementos dentro de la upm
    muestraElementos = list( sample_n(data, size = tamanioMuestra ) ) ) %>% 
  select( -c(data, tamanioMuestra) ) %>% 
  unnest(muestraElementos)

