library(dplyr)
library(readr)
set.seed(1)
library(survey)


#-----leemos la base
archivo = "/Users/shoshenskoe/Documents/muestreo/TesisMaestria/bases/baseVerosi.csv"
baseVero = read_csv(file =archivo,
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


##----- funciones auxiliares


GeneradorIntervalosMAS = function( muestra , numeroActas, partido, thetaPob ) {
  
  n = dim(muestra)[1]
  
  disenio = survey::svydesign( 
    id = ~1 , 
    data = muestra ,
    weights = ~rep( (numeroActas/n), n) , 
    fpc = ~rep(numeroActas, n)
  )
  
  #realiamos la estimacion de razon
  razon = survey::svyratio(numerator = muestra[,partido]  , denominator =  ~total , 
                           design =  disenio ,
                           vartype= "se")
  
  estimador = as.numeric ( razon$ratio[1] )
  varianzaEstimada = as.numeric( razon$var[1] )
  
  intervalo = confint(object = razon, level = 0.95)
  
  exito = ( intervalo[1] <=  thetaPob ) & 
    ( thetaPob <=  intervalo[1]  )
  
  
  resultado = list( intervalo[1],  intervalo[2], estimador, 
                    varianzaEstimada ,  intervalo[2] -  intervalo[1]  ,
                    as.integer(exito) )
  return( resultado )
  
}

# tamMuestra es la cantidad de elementos por cada estrato 
generadorMuestra = function(tamMuestraEstrato) {
  
  muestra = baseVero %>% dplyr::group_by(Stratum) %>%
    dplyr::slice_sample(n = tamMuestraEstrato) %>% 
    dplyr::ungroup() %>%
    as.data.frame()
  
  return (muestra)
}

listaMuestras = function (tamMuestraEstrato, cantMuestras) {
  #listaDeMuestras = vector(mode = "list", length = cantMuestras )
  vectorTamanios = rep(tamMuestraEstrato, cantMuestras)
  listaDeMuestras = lapply( vectorTamanios, generadorMuestra )
  
  return( listaDeMuestras  )
}



##---- generacion intervalos 


#numero de simulaciones
numSimulaciones = 1000

#tamanio de muestra
n = 15

#en partido puede incluirse "totalMorena", "totalPan", "totalMC"
partido = "totalPan"

#calculamos el theta real
thetaReal = sum(baseVero[,partido]) / sum(baseVero$total)



#con los parametros indicados procedemos
#a obtener la lista de intervalos

#creamos una lista de muestras
lista = listaMuestras(tamMuestraEstrato= n, 
                      cantMuestras = numSimulaciones)
numeroActas = dim(baseVero)[1]

#realizamos el calculo
tiempoInicial = Sys.time()
intervalos = lapply( lista, GeneradorIntervalosMAS , partido= partido,
                     numeroActas = numeroActas, thetaPob = thetaReal )
tiempoFinal = Sys.time()
print( tiempoFinal - tiempoInicial)

matrizDeIntervalos = do.call(rbind, intervalos)

matrizDeIntervalos = cbind( as.numeric ( unlist( matrizDeIntervalos[1] )  ) ,
                             as.numeric ( unlist( matrizDeIntervalos[2] ) ) , 
                             as.numeric ( unlist( matrizDeIntervalos[3] ) ) ,
                             as.numeric ( unlist( matrizDeIntervalos[4] ) ) , 
                             as.numeric ( unlist( matrizDeIntervalos[5] ) ) , 
                             as.numeric ( unlist( matrizDeIntervalos[6] ) )  )

matrizDeIntervalos = as.data.frame(matrizDeIntervalos)
colnames(matrizDeIntervalos) = c("Izq", "Der", "estimador", 
                                 "varEst", "longitud","exito")

ruta = "/Users/shoshenskoe/Documents/muestreo/TesisMaestria/intervalos/INE/intervaloEstrMAS/ineEst15/Pan/estMAS15.csv"
write_csv(matrizDeIntervalos , 
          file = ruta)

#par(mfrow = c(1, 1))
#dev.off() 
#hist(longitudIntervalosMor, main = "Longitud intervalos Morena", 
#     xlab = "Longitud")
#print(exitosMorena/numSimulaciones)
#print(exitosPan/numSimulaciones)
#print(exitosMC/numSimulaciones)



#graficamos los estimadores
#par(mfrow = c(2, 2))
#par(mar = c(2, 2, 2, 2) )
#par("mar")

#tituloMorena = paste ( "Morena  ", porcentaje*100, "%" ) 
#tituloPan = paste ( "Pan n =  ", porcentaje*100, "%" ) 
#tituloMc = paste ( "MC n =  ", porcentaje*100, "%" ) 

#hist(RMorHajek, breaks= 30, main = tituloMorena,
#     xlab = "Estimador", ylab = "Frecuencia")
#abline(v= thetaPoblacionalMor, col = "red" )


#hist(RPanHajek, 
#     xlab = "Estimador",  breaks= 30, ylab = "Frecuencia", main = tituloPan)
#abline(v= thetaPoblacionalPan, col = "red" )

#hist(RMCHajek,
#     xlab = "Estimador",  breaks= 30, ylab = "Frecuencia", main= tituloMc)
#abline(v= thetaPoblacionalMC, col = "red" )


#graficamos las varianzas
#par(mfrow = c(2, 2))
#par(mar = c(2, 2, 2, 2) )
#par("mar")

#hist( varianzasMorHajek, main = "Varianzas morena" , 
#      xlab = "estimaciones", ylab = "Frecuencias")
#hist( varianzasPanHajek, main = "Varianzas pan" , 
#      xlab = "estimaciones", ylab = "Frecuencias")
#hist( varianzasMCHajek, main = "Varianzas mc" , 
#      xlab = "estimaciones", ylab = "Frecuencias")

#vemos la longitud de los intervalos
#summary( longitudIntervalosMor )
#summary( longitudIntervalosPan )
#summary( longitudIntervalosMC )

#library("plotrix")
#x <- c(1)
#F <- c(thetaPoblacionalMor)
#L <- intervalos[1,1]
#U <- intervalos[1,2]

# install.packages("plotrix")
#plotCI(x, F, ui=U, li=L)

