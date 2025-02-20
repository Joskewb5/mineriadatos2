#20/02/2025
#Josias Milan Sirpa Pinto

rm(list = ls())
library(dplyr)
library(tm) #mineria de texto
library(udpipe) #etiquetado
library(hunspell) #ortografia
library(syunzhet) #analisis de sentimiento
library(stringr)
library(pdftools)
library(ggplot2)
library(wordcloud2)
library(ggwordcloud)
library(igraph)
library(visNetwork)

install.packages("visNetwork")

###################
options(stringsAsFactors = FALSE)
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
##################
load("C:/Users/Josias/Desktop/7mo/Mineria II/larazon.RData")
##################
topico<-bd %>% filter(str_detect(tolower(titular), "economía"))
##################
bd_corpus<-VCorpus((VectorSource(topico$titular)))
#################
#Corpus en carpeta
getReaders()#ver formatos 
dirpdf<-"C:/Users/Josias/Desktop/7mo/Mineria II/Pdf"
pdf_corpus <- Corpus(DirSource(
  dirpdf,, pattern = ".pdf"), readerControl = list(reader = readPDF))
# Limpieza de corpus
getTransformations()
bd_corpus %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(str_replace), "la paz", "lapaz") %>% 
  tm_map(removeWords, stopwords("es")) %>% 
  tm_map(removeWords, c("ejemplo"))

#funcion para la limpieza
limpieza<-function(cp, extra=c(""), cambio=c("lapaz"="la paz")){
  cp %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(str_replace_all), cambio) %>% 
    tm_map(removeWords, stopwords("es")) %>% 
    tm_map(removeWords, extra) %>% 
    tm_map(stripWhitespace) %>% 
    return()
  
}
bd_corpusd<- limpieza(bd_corpus,
                      extra = c("hola"),
                      cambio = c("'"="", "'"=""))
#################
bd_corpus[[1]]$content
bd_corpusd[[1]]$content

pdf_corpusd<-limpieza(pdf_corpus)
pdf_corpus[[1]]$content
pdf_corpus[[1]]$content

#armar el TDM o DTM
dtm_v<-DocumentTermMatrix(bd_corpusd)
tdm_v<-TermDocumentMatrix(bd_corpusd)
dtm_pdf<-DocumentTermMatrix(pdf_corpusd)
tdm_pdf<-TermDocumentMatrix(pdf_corpusd)
dim(dtm_pdf)
dim(tdm_pdf)
#analisis descriptivo: frecuencias
aux<-tdm_v %>% as.matrix() %>% rowSums() %>% data.frame(freq=.)
aux$words<-rownames(aux)

ggplot(aux %>% filter(freq>2), aes(freq, words)) + geom_point()
wordcloud2(aux[,c(2,1)])

#con ggplot
ggplot(aux, aes(label = words, size))