

library(dplyr)
library(readr)
set.seed(1)
library(survey)


#-----leemos la base
archivo = "/Users/shoshenskoe/Documents/muestreo/TesisMaestria/bases/baseVerosi.csv"
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

base %>% 
  dplyr::group_by(Stratum) %>% #agrupamos por estrato
  dplyr::sample_n(size= 3, replace = FALSE) %>% #muestreo m.a.s.
  dplyr::ungroup() %>% #hasta este punto tenemos la muestra principal
  dplyr::group_by(Stratum) %>% #volvemos a agrupar por estrato
  dplyr::sample_n(size=2, replace = TRUE)%>%  #muestreo con reemplazo
  dplyr::ungroup() %>% #tenemos la muestra bootstrap
  dplyr::group_by(CLAVE_ACTA) %>%  #agrupamos por clave de acta
  dplyr::mutate( conteoBots = n() ) %>% #contamos veces aparecio cada clave
  dplyr::ungroup() #desagrupamos y almacenamos el conteo en cada regristro