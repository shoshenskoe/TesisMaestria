

library(dplyr)
library(readr)
set.seed(1)
library(survey)


#-----leemos la base
archivo = "/Users/shoshenskoe/Documents/muestreo/TesisMaestria/bases/baseVerosi.csv"
base = read_csv(file =archivo,
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
  dplyr::slice_sample(n= 3) %>% 
  dplyr::ungroup()

#numero de repeticiones bootstrap 
R = 500

#cantidad de muestra por estrato
n_h = 3

# muestra pan
y = muestraPrincipal$totalPan
z = muestraPrincipal$total
canMuestra = length(y)

#vector de pi_i originales
pis = n_h / muestraPrincipal$totalEstrato

#lista de estratos de la muestra
listaEstrat = muestraPrincipal$Stratum

#lista de indices
listaIndices = muestraPrincipal$id

BootstrapEstrato = function( estrato, cantMuestra ) {
  indicesSeleccionados = listaIndices[ listaEstrat == estrato ]
  
  
  
  
}


