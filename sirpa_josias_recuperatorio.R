##Primer examen parcial - Recuperatorio
#Josias Milan Sirpa Pinto

##PREGUNTA 1
#A partir del análisis de comentarios de una tienda en línea, se extrajo la siguiente regla de asociación:
#• Regla: {'entrega'} → {'tarde'}
#• Soporte: 0.10
#• Confianza: 0.80
#• Lift: 2.5
#Se pide:
#• 1.Interpreta el significado de esta regla en el contexto de los comentarios de los usuarios.
#• 2.Explica lo que nos dicen el soporte, la confianza y el lift.
#• 3.Sugiere al menos una acción concreta que podría tomar el equipo de servicio al cliente o logística
#con base en esta información

##RESPUESTA
#1. La regla entrega a tarde indica que cada vez que los usuarios comentan la palabra "entrega", es muy probable que también comenten la palabra "tarde". Esto nos dice que los clientes asocian las entregas principalmente con demoras. por lo tanto, los usuarios tienen una percepción negativa respecto a las entregas, lo que puede significar que los pedidos llegan mas tarde de lo planificado.
#2. a. Soporte (0.10): mide la frecuencia de la combinación "entrega" y "tarde" dentro de todos los comentarios analizados, por lo tanto, el 10% de todos los comentarios hacen referencia tanto a "entrega" como a "tarde".
#   b. Confianza (0.80): mide la probabilidad de que, si se menciona "entrega", también se mencione "tarde", por lo tanto, el 80% de las veces que se menciona "entrega", también aparece "tarde", lo que demuestra una relación muy fuerte entre ambos.
#   c. Lift (2.5): mide la fuerza de la asociación, comparando la frecuencia observada de "entrega" y "tarde" juntos con la frecuencia que esperaríamos si fueran independientes cada uno, por lo tanto, significa que la mencion en conjunto de "entrega" y "tarde" es 2.5 veces mas alta de lo que ocurriría por casualidad, indicando una asociación positiva y fuerte
#3. Dado que se evidencia una fuerte relacion entre "entregas" y "tarde", se sugiere que se optimice los tiempos de entrega se puede revisar los procesos logisticos, identificar cuellos de botella y buscar forma de acelerar, tambien para reducir el descontento de los clientes, implementar alertas cuando un pedido tenga fuerte probabilidad de llegar tarde.


##PREGUNTA 2
#A partir de un corpus de reseñas de estudiantes sobre cursos universitarios, se generó una nube de palabras
#donde las mas destacadas fueron: contenido, profe, dinámico, difícil, clases, explicación.
#• 1. ¿Qué podrías inferir sobre la percepción general de los cursos en función de esta nube de palabras?
#• 2. ¿Qué limitaciones tiene este tipo de visualización en un análisis de minería de texto más profundo?
#• 3. Menciona al menos una técnica complementaria que podría dar mayor profundidad a este análisis, y justifica su utilidad

##RESPUESTAS
#1. Podriamos inferir que los estudiantes valoran al profe y el contenido de la materia, tambien las caracteristicas de las clases que se pueden evidenciar que las clases son dinamicas pero a su vez dificiles
#2. Entre las limitaciones podemos tener falta de contexto, por ejemplo al mencional dificil no sabemos si es un dificil bueno (de desafio) o malo (de no se entiende), tambien la relacion de las palabras, no sabemos cual esta relacionada con cual, finalmente la falta de emociones, en una nube de palabras no podemos ver el reflejo de los sentimientos (positivo, negativo o neutral) asociado a los comentarios.
#3. Analisis de sentimiento, esto permitiría saber si las menciones a "profe", "difícil", "dinámico", etc., tienen una carga emocional positiva o negativa. Ayudaria a priorizar acciones, por ejemplo si "difícil" es un problema recurrente con sentimiento negativo, se podria mejorar el metodo de enseñanza

##PREGUNTA 3
# Empleando el dataset larazon.RData, realice la limpieza correspondiente y para las noticias vinculadas a marcha realice:
# • Análisis de Cluster jerárquico de los términos
# • Análisis de sentimiento

rm(list = ls())
library(dplyr)
library(tm)
library(wordcloud2)
library(syuzhet)
library(igraph)
library(visNetwork)
library(lubridate)
load("C:/Users/Josias/Desktop/7mo/Mineria II/larazon.Rdata")  
larazon<-bd
colnames(larazon)

#solo titulares que tienen marcha
larazon_marcha <- larazon %>% filter(grepl("marcha", titular, ignore.case = TRUE))
nrow(larazon_marcha)
#corpus
cp <- VCorpus(VectorSource(larazon_marcha$titular))
#limpieza
limpieza <- function(cp, extra=c("")) {
  cpl <- tm_map(cp, content_transformer(tolower))#mayusculas 
  cpl <- tm_map(cpl, removePunctuation) #puntuacion
  cpl <- tm_map(cpl, removeNumbers) #numeros
  cpl <- tm_map(cpl, removeWords, stopwords("es"))#esp 
  cpl <- tm_map(cpl, removeWords, stopwords("en"))#eng
  cpl <- tm_map(cpl, removeWords, extra)
  cpl <- tm_map(cpl, stripWhitespace)#espacios
  return(cpl)
}

cpl_larazon_marcha <- limpieza(cp)
#matriz de terminos
tdm <- TermDocumentMatrix(cpl_larazon_marcha)
tdm_m <- as.matrix(tdm)
bdw <- data.frame(word = rownames(tdm_m), freq = rowSums(tdm_m))#freq
wordcloud2(bdw, size = 0.5)#nube


##3.1. Cluster Jerarquico
aux1 <- removeSparseTerms(tdm, 0.975)#eliminamos terminos que no son muy frecuentes, que salen en menos del 2.5% de los docs
plot(hclust(dist(aux1), method = "complete"))#diagrama de jerarquias


##3.2. Analisis de sentimientos
sentim <- bdw %>% filter(freq >= 5) #agarramos solo palabras que se repitan mas de 5 veces
sentim1 <- get_nrc_sentiment(sentim$word, language = "spanish")

aux2 <- apply(sentim1, 2, sum)[1:8]
names(aux2) <- c("ira", "anticipación", "asco", "miedo", "alegría", "tristeza", "sorpresa", "confianza")
barplot(sort(aux2), horiz = TRUE, las = 1)

##PREGUNTA 4
#Utilice la base de datos de la encuestas a hogares 2023 para personas de 18 años o más y genere una base de datos similar a la de data(“Adult”) incluyendo las siguientes variables (omitir valores perdidos una vez seleccionadas las variables):
# • Incidencia de pobreza (p0)
# • Deciles de ingreso laboral (transformación sobre ylab)
# • Nivel educativo (niv_edu)
# • Autoidentificación indígena (s01a_09)
# • Migración reciente (s01b_11a)
# • Grupo Ocupacional ocupación principal (cob_op)

#Obtenga el lift de la regla:
#  {p0=Pobre, s01a_09=1. Pertenece} ⇒ {s01b_11a=2. En otro lugar del país}


rm(list=ls())
library(haven)
library(labelled)
library(dplyr)
library(arules)
library(arulesViz)

load("C:/Users/Josias/Desktop/7mo/Mineria II/eh23.Rdata")  
#convertimos la bd
bd <- eh23p %>% filter(s01a_03 >= 18) #mayores de 18
bd1 <- bd %>%  mutate(deciles = ntile(ylab, 10)) %>%  
  select(p0,deciles,niv_ed,s01a_09,s01b_11a,cob_op) #filtramos
bd2 <- bd1 %>% mutate(across(everything(), as.factor)) 
bd2 <- na.omit(bd2) #omitimos na
tr <- as(bd2, "transactions") #convertimos a transaccion

regla <- apriori(tr,parameter = list(supp = 0.001, conf = 0.001, minlen = 2), 
                            appearance = list(
                              lhs = c("p0=1", "s01a_09=1"),
                              rhs = c("s01b_11a=2"),
                              default = "none"))
inspect(regla) 
