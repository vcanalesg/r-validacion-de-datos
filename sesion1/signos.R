# Crear base de datos primera sesión --------------------------------------

# Este script tiene por objetivo mostrarte como fueron creadas las bases de
# datos utilizadas en la primera sesión

# Paquetes ----------------------------------------------------------------

# Correr solo una vez
# install.packages("haven")
# install.packages("tidyverse")

library(haven)
library(tidyverse)

# Carguemos los datos a nuestra sesión ------------------------------------
# Estos datos fueron descargados directamente de la página del Observatorio Social: 
# http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/basedatos.php

# cambia la ruta en función de donde descargaste los datos de casen

casen13 <- read_dta("data/casen2013.dta")
casen15 <- read_dta("data/casen2015.dta")
casen17 <- read_dta("data/casen2017.dta")

# Cuál es tu signo zodiacal? ----------------------------------------------

# Para conocer el signo zodiacal chino es necesario calcular el año de
# nacimiento a partir de la edad de las personas Este cálculo tendrá un error
# que por ahora no abordaremos. El calendario chino es lunisolar y no coincide
# con el calendario gregoriano (solar).

casen13$agno_nacimiento <- as.integer(2013 - casen13$edad)
casen15$agno_nacimiento <- as.integer(2015 - casen15$edad)
casen17$agno_nacimiento <- as.integer(2017 - casen17$edad)

# Los signos zodiacales chinos corresponden a 12 animales que representan la
# personalidad de las personas. Creemos un vector para tenerlos a la mano.

signos <- c("Rata", "Buey", "Tigre", "Conejo", "Dragón", "Serpiente", "Caballo", "Cabra", "Mono", "Gallo", "Perro", "Cerdo")

# Ahora necesito conocer la correspondencia entre los signos chinos y el año de
# nacimiento. Sabiendo que los signos se repiten cada 12 años y tomando un
# período de 12 repeticiones (144 años), repito el objeto signos 12 veces. Para
# comenzar con la repetición necesito tener los signos en orden de aparición.
# Fijaré el comienzo del calendario en 1876 que corresponde al año de la Rata
# (primer signo en el calendario).

calendario_chino <- data.frame(agno_nacimiento = 1876:2019, 
                               signo_zodiacal = rep(signos, 12))

# Con esta información, ahora puedo unir los datos de casen con este calendario
# utilizando como variable de identificación para la unión el año de nacimiento.
# Utilizaremos para esto la función left_join(). Se parece al merge en stata en
# que el sentido de la unión es el de las columnas. Pero en todo lo demás, es
# muy distinta.

casen13 <- left_join(casen13, calendario_chino, by = "agno_nacimiento")
casen15 <- left_join(casen15, calendario_chino, by = "agno_nacimiento")
casen17 <- left_join(casen17, calendario_chino, by = "agno_nacimiento")

# Al tabular los datos, observamos que a nivel muestral los signos se
# distribuyen más o menos de forma equitativa. Esto es de esperarse considerando
# que se repiten cada 12 años. Miremos estos resultados para casen 2013:

casen13 %>% group_by(signo_zodiacal) %>% summarise(n = n())

# ¿Cuáles signos se emparejan más veces? ----------------------------------
# Ahora nos interesa conocer que signos se emparejan más veces. Nos enfocaremos
# solo en la información del núcleo principal (jefatura de hogar y su pareja).
# ¿Existirán signos que tienden a juntarse más que otros?

# Primero uniremos todas las casen que analizaremos y trabajaremos desde ahí.
# Seleccionaremos primero las columnas que nos interesan de cada conjunto de
# datos. Veamos que al asignar a los objetos el mismo nombre, estos se
# sobreescriben

casen13 <- casen13 %>% select(folio, pco1, sexo, edad, region, signo_zodiacal)
casen15 <- casen15 %>% select(folio, pco1, sexo, edad, region, signo_zodiacal)
casen17 <- casen17 %>% select(folio, pco1, sexo, edad, region, signo_zodiacal)

# Usaremos la función bind_rows para unir las filas. Por ahora no nos interesará
# la advertencia que arroja el programa. Lo único importante que ocurrió ahí es
# que se perdieron las etiquetas de las categorías de respuesta

casen <- bind_rows("Casen 2013" = casen13, "Casen 2015" = casen15, "Casen 2017" = casen17, .id = "casen")

# Necesitamos identificar a los hogares que tienen en su interior un núcleo
# principal. La información del resto de los integrantes no nos interesa para
# este ejercicio.

# Podemos partir quedándonos solo con las filas que corresponden a las parejas
# de la jefatura del hogar. Nos quedaremos además solo con las columnas que nos
# interesan para este análisis para simplificar el manejo de los datos.

pareja_jh <- casen %>% filter(pco1 == 2) 

# Luego, nos quedamos con las observaciones correspondientes a la jefatura

jh <- casen %>% filter(pco1 == 1) 

# Como pueden ver en la ventana environment (ambiente), hay menos parejas que
# jefaturas de hogar. Esta diferencia ocurre porque, por construcción, todos los
# hogares deben identificar una sola jefatura. Además, no todas las personas
# conviven con sus parejas, ni tampoco todas tienen una pareja.

# Juntamos ambos conjuntos de datos en el sentido de las columnas. Utilizando la
# función left_join() solo nos quedaremos con las filas para las que se
# encuentra un par en el conjunto de datos de la derecha. Usaremos los datos de
# las parejas porque contiene únicamente los folios que cumplen con esa
# condición.

parejas_casen <- left_join(pareja_jh, jh, by = c("casen", "folio", "region"), suffix = c("_par","_jh"))

# Ahora nos interesa saber qué signos se emparejan más seguido. Una alternativa
# para conocer esto es concatenar las columnas que tienen la información del
# signo de la jefatura y su pareja (haremo esto con la función paste). Sin
# embargo, esto tiene un problema. ¿Sospechas cuál puede ser?

parejas_casen <- parejas_casen %>% mutate(comb_signos1 = paste(signo_zodiacal_par, "-", signo_zodiacal_jh))

# Tabulemos los datos para observar el problema. Agreguemos la función view al
# final para utilizar un visor de datos similar a una hoja de excel. ¿De cuántas
# formas está escrita la combinación Buey-Caballo?

parejas_casen %>% group_by(comb_signos1) %>% summarise(n = n()) %>% view()

# El problema de esta forma de construir las combinaciones es que se crean
# combinaciones repetidas. Para el análisis que queremos hacer no importa si la
# combinación es Serpiente-Buey o Buey-Serpiente.

# Un camino para solucionar esto es asignar números a los animales, tales que al
# sumar el resultado de esta suma sea único para cada combinación.

# Identifiquemos primero todas las combinaciones sin repeticiones. Le
# colocaremos comb_signos2 para que al finalizar y unir las bases, mantengamos
# tanto esta columna como comb_signos1 en la tabla de datos.

comb_signos_sin_rep <- data.frame(comb_signos2 = combn(signos, 2, paste, collapse = " - ")) 

# Luego creemos un vector con 12 números cuyo orden será correlativo al orden de los signos.
numeros <- 10^(0:11) 
options(scipen = 999) # para quitar la notación científica

# Si no te queda tan claro que está pasando detrás de esta operación, corre el
# código después de la función potencia (^). El vector que contiene al número 10
# se recicla de acuerdo al largo del segundo vector de números

# Ahora podemos usar la misma función que usamos para combinar para obtener
# todas las sumas posibles.

suma_numeros <- data.frame(suma_numeros = combn(numeros, 2, sum))

# Listo. Armamos una sola tabla con ambos conjuntos de datos y tenemos la suma
# única para cada combinación.

comb_signos_numeros <- data.frame(comb_signos_sin_rep, suma_numeros)

# Bueno, pero no hemos terminado. Aún no sabemos que combinaciones son las más
# frecuentes. Tenemos que agregar los números a cada signo para que luego, al
# hacer la suma de ambos signos, podamos usar la columna con la suma como
# variable de identificación para la unión.

# Volvamos a la tabla con los datos de parejas. Tenemos que agregar el número a
# cada signo para hacer la suma. No obstante, no tenemos una tabla que contenga
# la información de los signos y de sus números al mismo tiempo. Creémosla.

signos_numeros <- data.frame(signos, numeros)

# Ahora usemos esta tabla para agregar esta información dependiendo del signo
# zodiacal de la jefatura del hogar y de su pareja. Son dos columnas que debemos
# agregar a nuestros datos (una para la pareja y otro para la jefatura)

parejas_casen <- left_join(parejas_casen, signos_numeros, by = "signos")

# Bueno, esto no ha funcionado. ¿Por qué? Necesitamos que las columas se llamen
# igual para que sirvan como variable de identificación Tenemos dos columnas con
# el signo zodiacal (_par, _jh). Peguemos primero el número asociado al signo de
# la pareja, renombrando la columna con la función rename(nombre_nuevo =
# nombre_actual)

signos_numeros <- signos_numeros %>% rename(signo_zodiacal_par = signos)

# Ahora sí debería funcionar

parejas_casen <- left_join(parejas_casen, signos_numeros, by = "signo_zodiacal_par")

# Hacemos lo mismo ahora para la jefatura del hogar. Renombraremos primero el
# número del signo zodiacal de la pareja.

parejas_casen <- parejas_casen %>% rename(numeros_par = numeros)
signos_numeros <- signos_numeros %>% rename(signo_zodiacal_jh = signo_zodiacal_par)
parejas_casen <- left_join(parejas_casen, signos_numeros, by = "signo_zodiacal_jh")

parejas_casen <- parejas_casen %>% rename(numeros_jh = numeros) # hacemos lo mismo para el número del signo de la jefatura

# Sumamos ambos signos, para crear la columna que nos permitirá identificar a
# que combinación de signos corresponde

parejas_casen <- parejas_casen %>% mutate(suma_numeros = numeros_par + numeros_jh)

# Bueno, ahora sí podemos juntar la información de la tabla que tiene la
# información de la suma y las combinaciones sin repetición

parejas_casen <- left_join(parejas_casen, comb_signos_numeros, by = "suma_numeros")

# Si revisamos los datos veremos que tenemos algunos datos con NA (not
# available, corresponde a los datos perdidos). ¿A qué combinaciones de signo
# corresponden? Démosle un vistazo a los datos

parejas_casen %>% filter(is.na(comb_signos2)) %>% select(comb_signos1) %>% unique()

# No están las combinaciones en las que se repite el signo (ambas personas
# tienen el mismo signo). No obstante, como estas solo tienen una manera de
# escribirse, podemos utilizar la información de la columna que inicialmente
# creamos para identificar la unión de signos (comb_signos1), reemplazando en la
# nueva columna en aquellos casos en la información falta

parejas_casen <- parejas_casen %>% mutate(comb_signos = ifelse(is.na(comb_signos2), comb_signos1, comb_signos2))

# Ahora sí! Tabulemos las uniones más y observemos si hay algunas que son más
# frecuentes que otras

parejas_casen %>% group_by(comb_signos) %>% summarise(n = n()) %>% view()

# A primera vista vemos que todas las combinaciones de signos existen. ¿Cuáles
# serán las más frecuentes? Podríamos ordenar la tabla anterior con arrange

parejas_casen %>% group_by(comb_signos) %>% summarise(n = n()) %>% arrange(desc(n)) %>% view()

# Al parecer no hay mucha diferencia...

# Guardaremos las bases de datos que usaremos en la primera sesión
# Borramos los datos que no nos interesa guardar
rm(casen, casen13, casen15, comb_signos_numeros, comb_signos_sin_rep, jh, pareja_jh, signos_numeros, suma_numeros, numeros, signos, calendario_chino)

# Nos quedamos solo con las variables relevantes para el taller (este código borra las variables que no usaremos)
parejas_casen <- parejas_casen %>% select(-c(suma_numeros,numeros_par, numeros_jh, comb_signos1, comb_signos2))

save(casen17, parejas_casen, file = "data/datos_taller1.RData")
