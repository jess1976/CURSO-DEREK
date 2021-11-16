#### CURSO R
#### Derek Corcoran Barrios
#### 
#### https://derek-corcoran-barrios.github.io/Libro/_book/index.html

################################################################################# CLASE 1 

# install.packages("pacman", "rmarkdown", "tidyverse", "tinytex")
library(swirl)

# You can exit swirl and return to the R prompt (>) at any time by pressing the Esc key. If you
# | are already at the prompt, type bye() to exit and save your progress. When you exit properly,
# | you'll see a short message letting you know you've done so.
# 
# | When you are at the R prompt (>):
#   | -- Typing skip() allows you to skip the current question.
# | -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
# | -- UNTIL you type nxt() which will regain swirl's attention.
# | -- Typing bye() causes swirl to exit. Your progress will be saved.
# | -- Typing main() returns you to swirl's main menu.
# | -- Typing info() displays these options again.

# # | If at any point you'd like more information on a particular topic related to R, you can type
# | help.start() at the prompt, which will open a menu of resources (either within RStudio or your
#                                                                    | default web browser, depending on your setup). Alternatively, a simple web search often yields
# | the answer you're looking for.

## CLASE 1: tidy data y manipulación de datos
## INVESTIGACION REPRODUCIBLE
## 
data("uspop")
View("uspop")

### ORDENAR LAS TABLAS - UNTIDY a TIDY!

library(tidyverse) # META-PAQUETE Q CONTIENE VARIOS PAQUETES
library(dplyr)  # SUB-PAQUETE DE tidyverse

Summary.Petal <- summarize(iris, Mean.Petal.Length = mean(Petal.Length), 
                           SD.Petal.Length = sd(Petal.Length))
Summary.Petal2 <- summarize(iris, Mean.Petal.Length = mean(Petal.Length), 
                           SD.Petal.Length = sd(Petal.Length), suma=sum(Petal.Length))

##3 puedo poner todos los descriptivos q quiero, primero va el nombre de la BD y desp
##las columnas q quiero resumir y la funcion

### Si quiero q me resuma dividido en las especies

IrisPorEspecie=group_by(iris, Species) # el mismo q iris. pero sabiendo q hay grupos
 ## es un DATA FRAME AGRUPADO

### summarize - especifico de a una cada columna q voy a generar y con q valores

Summary.Petal <- summarize(IrisPorEspecie, Mean.Petal.Length = mean(Petal.Length), 
                           SD.Petal.Length = sd(Petal.Length))
Summary.Petal

# class(IrisPorEspecie)
# [1] "grouped_df" "tbl_df"     "tbl"        "data.frame"
# class(iris)
# [1] "data.frame"

 ## AGRUPAR POR MAS DE UNA CATERGORIA

data("mtcars")

carsAGRUPADO=group_by(mtcars, cyl, am)
resumenMPG= summarize(carsAGRUPADO, consumoProm=mean(mpg), desvio=sd(mpg))
resumenMPG

c2=mutate(mtcars, nuevaV=hp/wt, vuevaV2=2, wt=0) # modifica, crea o elimina columnas

c3=cbind(mtcars, nuevaV=mtcars$hp/mtcars$wt) .# solo agrega, y tengo q especificar el nombre
                                              # entero de la variable

# ejemplo MIO
# starwars
data("starwars")

# Newly created variables are available immediately
starwars %>%
  select(name, mass) %>%
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )

# As well as adding new variables, you can use mutate() to
# remove variables and modify existing variables.
starwars %>%
  select(name, height, mass, homeworld) %>%
  mutate(
    mass = NULL,
    height = height * 0.0328084 # convert to feet
  )


# By default, new columns are placed on the far right.
# Experimental: you can override with `.before` or `.after`
df <- tibble(x = 1, y = 2)
df %>% mutate(z = x + y)
df %>% mutate(z = x + y, .before = 1)
df %>% mutate(z = x + y, .before = x) 
df %>% mutate(z = x + y, .after = x)

## PIPELINE   %>% 
## COMMAND + SHIFT + M
## CONTROL + SHIFT + M

abs(round(sqrt(log(4)),2))

log(4) %>% sqrt %>% round(2) %>% abs

c(1,4,2) %>% sum %>% sqrt

## cuando uso un nombre de función, el resultado anterior a %>%  se usa como primer
## argumento de la función

### summarize_all - lo hace para todas las columnas - genera cada funcion para todas las columns
DF= iris %>% group_by(Species) %>% 
  summarize_all(.funs=list(media=mean, desv=sd, mediana=median)) # las funs sin parentesis
### summarize_at  - especifico varias columns y varias func y hace todas las combinaciones
### filter - mantiene solo los datos que cumplen el criterio

starwars   %>% 
      filter(!is.na(height)&!is.na(mass)) %>% 
      summarize_at(.vars = c("height", "mass"), .funs = list(promedio=mean))

starwars %>% 
      summarize_at(.vars = c("height", "mass"), .funs = list(promedio=mean), na.rm=TRUE)

filter(mtcars, cyl==8) ## tidyverse
subset(mtcars, cyl==8) ## base

iris %>% dplyr::filter(Species!="virginica" & Sepal.Width <= 3)
mtcars %>% dplyr::filter(cyl %in% c(6,8) )

mtcars %>% dplyr::filter(cyl %in% c(6,8) ) %>% group_by(am) %>% 
  summarise(consumo=mean(mpg))
iris %>% group_by(Species) %>% summarise(cantidad=n())

# Ejercicio 1 clase 1.
# Usando la base de datos storms del paquete dplyr, calcular la velocidad promedio y diámetro promedio (hu_diameter) de las tormentas que han sido declaradas huracanes para cada año.
storms  %>% filter(status == "hurricane") %>% group_by(year) %>% summarize(viento.prom =mean(wind), presion_prom=mean(pressure))
# en veloc y diam tiene todo NA
# otra forma
storms  %>% filter(status == "hurricane") %>% select(year, wind, pressure) %>%group_by(year) %>% summarize_all(mean)

################################################################## CLASE 2
################################################################## 
library(tidyverse)
library(knitr)
library(kableExtra)

## crear NUEVO PROYECTO Version Control


