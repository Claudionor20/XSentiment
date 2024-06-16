# Importando pacotes
library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(stringi)
library(wordcloud)

# Lendo base
load("dados_rotulados.rda")

# Tirando colunas desnecessárias
dados <- dados|>
  dplyr::select(text,Polaridade)

# Lendo arquivo de stopwords
stopwords <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/stopwords_.txt",header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")

# Drope duplicados
stopwords <- unique(stopwords)


# Transformando em vetor
stopwords <- as.vector(stopwords$V1)

# Função de remoção de acentos
remove_acentos <- function(text) {
  text <- stri_trans_general(text, "latin-ascii")
  
  return(text)
}

# Criando dicionário de emojis
emojis <- c("❤️" = " amor ","💗" = " amor ","🥰" = " amor ","💕" = " amor ","💜" = " amor ","🫶🏾" = " amor ","♥" = " amor ","💓" = " amor ","👏🏻" = " parabéns ",
            "👏" = " parabéns ","🥹" = " amor ","😻" = " amor ","❤️‍🔥" = " amor ","😘" = " amor ","❣️" = " amor ","💟" = " amor ","🤢" = " nojo ",
            "💖" = " amor ","🤮" = " nojo ","🔥" = " amor ","🖤" = " amor ","🫶🏼" = " amor ","🤩" = " amor ","💞" = " amor ","💝" = " amor ","😊" = " amor ")

# Função de transformação de emojis
substitute_emojis <- function(text, emoji_dict) {
  for (emoji in names(emoji_dict)) {
    text <- stri_replace_all_fixed(text, emoji, emoji_dict[[emoji]], vectorize_all = FALSE)
  }
  return(text)
}

# Criando corpus para utilizar a biblioteca tm
corpus <- Corpus(VectorSource(dados$text))

# Aplicando transformações no corpus
corpus <- tm_map(corpus, removeWords, stopwords) # Removendo stopwords
corpus <- tm_map(corpus, content_transformer(substitute_emojis),emojis) # Substituindo emojis
corpus <- tm_map(corpus, content_transformer(remove_acentos)) # Removendo acentos
corpus <- tm_map(corpus, content_transformer(tolower)) # Transformando em minúsculo
corpus <- tm_map(corpus, removePunctuation) # Removendo pontuação
corpus <- tm_map(corpus, removeNumbers) # Removendo números
corpus <- tm_map(corpus, stripWhitespace) # Removendo espaços em branco
corpus <- tm_map(corpus, removeWords, "anitta") # Removendo palavra "anitta"


# Transformando em matriz de termos
dtm <- DocumentTermMatrix(corpus)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wordcloud(names(freq), freq, min.freq=20)
dtm <- as.matrix(dtm)

# Fazendo gráfico de palavras mais frequentes
dados|>
  filter(Polaridade == 1)|>
  count(text, sort = TRUE)|>
  slice(1:40)|>
  ggplot(aes(x = reorder(text,n), y = n))+
  geom_col()+
  coord_flip()+
  labs(title = "Palavras mais frequentes",
       x = "Palavras",
       y = "Frequência")+
  theme_minimal()




