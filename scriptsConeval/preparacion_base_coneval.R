#elaboracion base

enlace= "https://raw.githubusercontent.com/shoshenskoe/TesisMaestria/refs/heads/main/bases/pobreza_22.csv"

library(readr)
library(dplyr)
pobreza_22 <- read_csv(enlace, 
                       col_types = cols(foliohog = col_character(), 
                                        est_dis = col_character(), upm = col_character(), 
                                        factor = col_double()))
colnames(pobreza_22)

pobreza_22 = pobreza_22 %>% 
  dplyr::select("folioviv", "foliohog", "numren"  ,"est_dis", "upm", "factor", "ictpc")


head(pobreza_22, 5)


pobreza_22["id"] =  paste( pobreza_22$folioviv, 
                           pobreza_22$foliohog, 
                           pobreza_22$numren, sep = "")

base = pobreza_22 %>% dplyr::select(id, est_dis, upm, factor, ictpc)

vector_ictpc = sort( base$ictpc)
summary(vector_ictpc)
hist(vector_ictpc, breaks = 500)

#write.csv(base,"base_coneval.csv", row.names = FALSE)



