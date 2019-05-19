# Daniela Pérez
# Date: 07 de Septiembre de 2018

# Cambiar esto de acuerdo a donde se haya realizado la recoleccion de los
# datos fuente: 

# * Si los datos se recogieron en linux pero se estan analizando en 
# windows, la codificacion en el origen es "UTF-8" y en el destino "ISO8859-1".
# * Si los datos se recogen y analizan en linux origen y destino son ambos iguales
# a "UTF-8". 
# * Si los datos se recogen y analizan en windows origen y destino son ambos iguales
# a "ISO8859-1".

origen="UTF-8"
destino="ISO8859-1"

## Introducción

# Este documento resume los análisis realizados para los datos
# recolectados del archivo histórico de tweets, de cada usuario
# en particular. Se asume que los tweets ya fueron recogidos y están en
# diferentes archivos .csv identificados con los nombres de usuarios
# en la red social y la fecha correspondiente al muestreo.


## Preliminares

# Hay que cargar todas las librerías necesarias para el análisis.


library(rtweet)                       # Twitter API 
library(tidyverse)                    # Data manipulation
library(stringr)                      # String manipulation
library(knitr)                        # Report generation
library(tidytext)                     # text mining, Silge package
library(stopwords)                    # to clean up text
library(lubridate)                    # to ease date manipulation
library(RColorBrewer)                 # nice colors
library(wordcloud)                    # to make word clouds
library(gridExtra)                    # For graph related to
library(scales)                       # ...correlations


# Leer los tweets almacenados 

tweets_corpoelecinfo1<- read_csv(file = "datos_tweets_@CORPOELECinfo 17 de mayo.csv",
                                col_names = TRUE)
tweets_corpoelecinfo1 = tweets_corpoelecinfo1 %>% select(status_id,created_at,user_id,  screen_name,text,source,                
                                  is_quote, is_retweet,favorite_count,retweet_count)

tweets_corpoelecinfo2<- read_csv(file = "datos_tweets_@CORPOELECinfo 25 de agosto.csv",
                                 col_names = TRUE)

tweets_corpoelecmerida <- read_csv(file = "datos_tweets_@corpoelecmerida 29 de mayo.csv",
                                   col_names = TRUE)
tweets_corpoeleczulia <- read_csv(file= "datos_tweets_@CorpoelecZulia_27 de agosto.csv",
                                  col_names = TRUE)

tweets_lmottad <- read_csv(file = "datos_tweets_@LMOTTAD 27 de agosto.csv",
                              col_names = TRUE)




# Se unen todos los tweets en un único dataframe

tweetsA <- bind_rows(tweets_corpoelecinfo1, tweets_corpoelecinfo2, tweets_corpoelecmerida, 
                     tweets_corpoeleczulia,tweets_lmottad)
tweetsA %>% group_by(screen_name) %>% summarise(numero_tweets = n())
# Selección de variables: autor, fecha, texto, id:

tweetsA <- tweetsA %>% select(screen_name, created_at, status_id, text) %>%
  mutate(created_at = ymd_hms(created_at))

# Se muestra el número de tweets muestreados a lo largo del tiempo

ggplot(tweetsA, aes(x = created_at, fill = screen_name)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name, ncol = 1)

# Se seleccionan sólo los del 2018

tweetsA1 <- tweetsA %>% filter(year(created_at)==2018)

# Y se repite el gráfico anterior

ggplot(tweetsA1, aes(x = created_at, fill = screen_name)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name, ncol = 1)

# Tabla con número de tweets

tweetsA1 %>% group_by(screen_name) %>% summarise(numero_tweets = n())

# Se renombran las variables con nombres más prácticos

tweetsA1 <- tweetsA1 %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
head(tweetsA1)
tweetsA1$texto=iconv(tweetsA1$texto, origen,destino, sub="")

# Esto se hace para limpiar el texto de patrones no informativos

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de paginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}


# Ahora, se aplica la función de limpieza y tokenización a cada tweet
# con lo cual cada tweet se descompone en sus palabras y esto es lo que 
# se almacena a continuación:

tweetsA1 <- tweetsA1 %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))
tweetsA1 %>% select(texto_tokenizado) %>% head()
tweetsA1 %>% slice(1) %>% select(texto_tokenizado) %>% pull()

tweets_tidyA1 <- tweetsA1 %>% select(-texto) %>% unnest()
tweets_tidyA1 <- tweets_tidyA1 %>% rename(token = texto_tokenizado)
head(tweets_tidyA1)

# Este gráfico explora la distribucion temporal de los tweets para los
# diferentes usuarios

ggplot(tweets_tidyA1, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 week") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

# Otra forma de visualizarlo:

tweets_mes_anyo <- tweets_tidyA1 %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

# Ahora se estudia la frecuencia de palabras utilizada por cada usuario

tweets_tidyA1 %>% group_by(autor) %>% summarise(n = n())
tweets_tidyA1 %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

# Estas son frecuencia de palabras distintas por cada usuario:

tweets_tidyA1 %>% select(autor, token) %>% distinct() %>% group_by(autor) %>%
  summarise(palabras_distintas = n())
tweets_tidyA1 %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

# Notar los cambios de frecuencia entre palabras totales y únicas por usuario
# Longitud media de tweets por usuario

tweets_tidyA1 %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
group_by(autor) %>% summarise(media_longitud = mean(longitud), sd_longitud = sd(longitud))
tweets_tidyA1 %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()

# Palabras más utilizadas por usuario:

tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

# Como se ve, esto incluye un monton de palabras no interesantes: articulos,
# preposiciones, otras que es necesario quitar usando stopwords

# Esto es una lista de palabras a excluir del análisis

lista_stopwords <-stopwords("es")

# De nuevo, las palabras mas usadas pero excluyendo las no interesantes

tweets_tidyA1 <- tweets_tidyA1 %>% filter(!(token %in% lista_stopwords))
tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

# si se excluye el "rt" y otras que se cuelan aquí:

lista_stopwords <- c(lista_stopwords, "rt","regranned","from","envivo")

# Y se repite el gráfico anterior

tweets_tidyA1 <- tweets_tidyA1 %>% filter(!(token %in% lista_stopwords))
tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)


set.seed(123)

# Nubes de palabras, otra forma de visualizar lo anterior

wordcloud_custom <- function(grupo, df){
  print(grupo)
  png(paste(grupo,"png",sep="."))
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 250, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
  dev.off()
}

df_grouped <- tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest()
walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

#Correlación entre usuarios

tweets_spread <- tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ CORPOELECinfo + corpoelecmerida, method = "pearson", data = tweets_spread)
cor.test(~ CorpoelecZulia_ + CORPOELECinfo, method = "pearson", data = tweets_spread)

cor.test(~ CORPOELECinfo + LMOTTAD, method = "pearson", data = tweets_spread)
cor.test(~ corpoelecmerida + LMOTTAD, method = "pearson", data = tweets_spread)
cor.test(~ CorpoelecZulia_ + LMOTTAD, method = "pearson", data = tweets_spread)

cor.test(~ CorpoelecZulia_ + corpoelecmerida, method = "pearson", data = tweets_spread)


# Gráfico de comparación de frequencia de palabras usadas por cada usuario en
# referente a la cuenta @Corpoelecinfo

p1 <- ggplot(tweets_spread, aes(corpoelecmerida, CORPOELECinfo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(tweets_spread, aes(LMOTTAD,CORPOELECinfo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p3 <- ggplot(tweets_spread, aes(CorpoelecZulia_,CORPOELECinfo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


grid.arrange(p1,p3,p2, nrow = 1)


#Número de palabras comunes entre los usuarios

palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="CORPOELECinfo") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="corpoelecmerida") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre CORPOELECinfo y corpoelecmerida", palabras_comunes)

palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="CORPOELECinfo") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="CorpoelecZulia_") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre CORPOELECinfo y CorpoelecZulia_", palabras_comunes)



palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="CORPOELECinfo") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="LMOTTAD") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre CORPOELECinfo y LMOTTAD", palabras_comunes)

palabras_comunes <- dplyr::intersect(tweets_tidyA1 %>% filter(autor=="corpoelecmerida") %>%
                                       select(token), tweets_tidyA1 %>% filter(autor=="LMOTTAD") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre corpoelecmerida y LMOTTAD", palabras_comunes)

# Comparación de uso de las palabras:
# Pivotaje y despivotaje,

tweets_spread <- tweets_tidyA1 %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)

# Selección de los autores corpoelecmerida y CORPOELECinfo

tweets_unpivot1 <- tweets_unpivot %>% filter(autor %in% c("corpoelecmerida",
                                                         "CORPOELECinfo"))
# Se añade el total de palabras de cada autor

tweets_unpivot1 <- tweets_unpivot1 %>% left_join(tweets_tidyA1 %>%
                                                 group_by(autor) %>%
                                                 summarise(N = n()),
                                               by = "autor")
# Cálculo de odds y log of odds de cada palabra

tweets_logOdds1 <- tweets_unpivot1 %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds1 <- tweets_logOdds1 %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds1 <- tweets_logOdds1 %>% mutate(log_odds = log(corpoelecmerida/CORPOELECinfo),
                                            abs_log_odds = abs(log_odds))

# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de corpoelecmerida. Esto es así porque el ratio sea ha
# calculado como corpoelecmerida/CORPOELECinfo.

tweets_logOdds1 <- tweets_logOdds1 %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@corpoelecmerida",
                                   "@CORPOELECinfo"))
tweets_logOdds1 %>% arrange(desc(abs_log_odds)) %>% head()

#Gráficamente:

tweets_logOdds1 %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@corpoelecmerida / @CORPOELECinfo)") +
  coord_flip() +
  theme_bw()


# SelecciÓn de los autores corpoelecZulia_ y CORPOELECinfo

tweets_unpivot2 <- tweets_unpivot %>% filter(autor %in% c("CorpoelecZulia_",
                                                          "CORPOELECinfo"))
# Se añade el total de palabras de cada autor

tweets_unpivot2 <- tweets_unpivot2 %>% left_join(tweets_tidyA1 %>%
                                                   group_by(autor) %>%
                                                   summarise(N = n()),
                                                 by = "autor")
# Cálculo de odds y log of odds de cada palabra

tweets_logOdds2 <- tweets_unpivot2 %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds2 <- tweets_logOdds2 %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds2 <- tweets_logOdds2 %>% mutate(log_odds = log(CorpoelecZulia_/CORPOELECinfo),
                                              abs_log_odds = abs(log_odds))


tweets_logOdds2 <- tweets_logOdds2 %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@CorpoelecZulia_",
                                   "@CORPOELECinfo"))
tweets_logOdds2 %>% arrange(desc(abs_log_odds)) %>% head()

#Gráficamente:

tweets_logOdds2 %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@CorpoelecZulia_ / @CORPOELECinfo)") +
  coord_flip() +
  theme_bw()


# Selección de los autores LMOTTAD y CORPOELECinfo

tweets_unpivot3 <- tweets_unpivot %>% filter(autor %in% c("LMOTTAD",
                                                          "CORPOELECinfo"))
# Se añade el total de palabras de cada autor

tweets_unpivot3 <- tweets_unpivot3 %>% left_join(tweets_tidyA1 %>%
                                                   group_by(autor) %>%
                                                   summarise(N = n()),
                                                 by = "autor")
# Cálculo de odds y log of odds de cada palabra

tweets_logOdds3 <- tweets_unpivot3 %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds3 <- tweets_logOdds3 %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds3 <- tweets_logOdds3 %>% mutate(log_odds = log(LMOTTAD/CORPOELECinfo),
                                              abs_log_odds = abs(log_odds))
tweets_logOdds3 <- tweets_logOdds3 %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@LMOTTAD",
                                   "@CORPOELECinfo"))
tweets_logOdds3 %>% arrange(desc(abs_log_odds)) %>% head()

# Gráficamente:

tweets_logOdds3 %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@LMOTTAD / @CORPOELECinfo)") +
  coord_flip() +
  theme_bw()



# Relación entre palabras:

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de paginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}
bigramas <- tweetsA1 %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama

bigramas %>% count(bigrama, sort = TRUE)

# Separación de los bigramas

bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword

bigrams_separados <- bigrams_separados %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Unión de las palabras para formar de nuevo los bigramas

bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes

bigramas %>% count(bigrama, sort = TRUE) %>% print(n = 20)

#Gráficamente:

library(igraph)
library(ggraph)
graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 200) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)
plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")

ggraph(graph = graph) +
  geom_edge_link(colour = "gray70") +
  geom_node_text(aes(label = name), size = 4) +
  theme_bw()

