

setwd("/Users/shoshenskoe/Documents/muestreo/Tesis/bases")
library(dplyr)
library(tidyverse)
library(stringr)
set.seed(1)


basePobreza = read.csv("~/Documents/muestreo/Tesis/bases/pobreza_22.csv")

# Transformamos la base de coneval

colnames(basePobreza)
str(basePobreza)
length( basePobreza$folioviv )