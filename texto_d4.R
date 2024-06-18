# Importando pacotes
library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(stringi)
library(wordcloud)
library(textstem)



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
corpus <- tm_map(corpus, removeWords, "anitta") # Removendo palavra "anitta"
corpus <- tm_map(corpus, stripWhitespace) # Removendo espaços em branco

# Transformando corpus em dataframe
dados_filtrado <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

# Adicionado coluna de polaridade
dados_filtrado$Polaridade <- dados$Polaridade

# Separando stopwords
dados_filtrado <- tidytext::unnest_tokens(dados_filtrado, word, text)

# Trocando palavras repetidas
variacoes_a <- c("aaaaa", "aaaudjak", "aaaaah", "aaaaaaaaaa", "aaaaaa", "aaaaaaaaa", "aaaaaaa", "aaa", "aaaff", "aaaaaaaaaaaaaaaaaaaa","aaaaaaaaaaa", "aaaa")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_a, "empolgado", word))

variacoes_ah <- c("ahhhhhhhhhhhhhhhhhhhhhhhhhhh", "ahhh", "ahhhhh")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_ah, "ah", word))

variacoes_agora <- c("agoraaaaaaa")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_agora, "agora", word))

variacoes_anitta <- c("anittaaaaa", "anita", "aniraaaa", "aninha", "aninhaaaaa", "anira", "aniraaa", "anittaaaaaaa")

dados_filtrado <- dados_filtrado|>
  filter(!word %in% variacoes_anitta)

variacoes_caralho <- c("krl","krlh","caralho", "caralhoooo", "caralh", "caralhooooo", "carai", "caraaaaai", "caralhoo", "caralhoooooo", "caralhas")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_caralho, "caralho", word))

# Removendo variaçoes de risada

variacoes_risada <- c("kakakakakakkakakak","kakakakakakak","kakakaka","kjkk","kkkkkkkkkkkkkk","ksdjskskk","k","kakak","kkkkkkkkkkkkk","mkkkkkkk",
                      "kkkkkkkkkkk","kkkkkkkkmmm","kskskskkskskzkzkzkzm","mkk","kskkskskskkskskkskkskskks","kkl","kkakakakaka","kkkkkkkkkkkk","mlk",
                      "ldhkdhaksjskjkh","kksjdskkdkss","ksxbkdjxkdbdj","akakakakak","akaka","kkkj","lllllllkkkkkkkkk")

dados_filtrado <- dados_filtrado|>
  filter(!word %in% variacoes_risada)








# Transformando em csv
write.csv(dados_filtrado, "dados_filtrado.csv")

# Lendo dicionario de lematização
lema <- read.delim("https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-pt.txt")
names(lema) <- c("stem", "word")

# Função de lematização
dados_lem = dplyr::left_join(dados_filtrado, lema, 
                             by='word')

# transformando em csv
write.csv(dados_lem, "dados_lem.csv")


# Transformando em matriz de termos
dtm <- DocumentTermMatrix(corpus)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wordcloud(names(freq), freq, min.freq=20)
dtm <- as.matrix(dtm)


dados_token <- tidytext::unnest_tokens(dados_filtrado, word, text)
# Fazendo gráfico de palavras mais frequentes
dados_token|>
  filter(Polaridade == -1)|>
  count(word, sort = TRUE)|>
  slice(141:180)|>
  ggplot(aes(x = reorder(word,n), y = n))+
  geom_col()+
  coord_flip()+
  labs(title = "Palavras mais frequentes",
       x = "Palavras",
       y = "Frequência")+
  theme_minimal()




