# Importando pacotes
library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(stringi)
library(wordcloud)
library(textstem)
library(rtweet)



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
corpus <- tm_map(corpus, content_transformer(tolower)) # Transformando em minúsculo
corpus <- tm_map(corpus, removePunctuation) # Removendo pontuação
corpus <- tm_map(corpus, content_transformer(remove_acentos)) # Removendo acentos
corpus <- tm_map(corpus, removeNumbers) # Removendo números
corpus <- tm_map(corpus, removeWords, stopwords) # Removendo stopwords
corpus <- tm_map(corpus, content_transformer(substitute_emojis),emojis) # Substituindo emojis
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

variacoes_anitta <- c("anittaaaaa", "anita", "aniraaaa", "aninha", "aninhaaaaa", "anira", "aniraaa", "anittaaaaaaa","anitta")

dados_filtrado <- dados_filtrado|>
  filter(!word %in% variacoes_anitta)

variacoes_caralho <- c("crllll","crlh","crl","caalho","caaaralho","krl","krlh","caralho", "caralhoooo", "caralh", "caralhooooo", "carai", "caraaaaai", "caralhoo", "caralhoooooo", "caralhas")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_caralho, "caralho", word))

# Removendo variaçoes de risada

variacoes_risada <- c("kakkakakak","kakakakakakkakakakakakakakak","kakakakakakakakakakakakakakakakakakakakakak","kakakakakakkakakak","kakakakakakak","kakakaka","kjkk","kkkkkkkkkkkkkk","ksdjskskk","k","kakak","kkkkkkkkkkkkk","mkkkkkkk",
                      "kkkkkkkkkkk","kkkkkkkkmmm","kskskskkskskzkzkzkzm","mkk","kskkskskskkskskkskkskskks","kkl","kkakakakaka","kkkkkkkkkkkk","mlk",
                      "ldhkdhaksjskjkh","kksjdskkdkss","ksxbkdjxkdbdj","akakakakak","akaka","kkkj","lllllllkkkkkkkkk","kkkkkkkkkkkkkkkkkkkkkkk",
                      "hahhaha", "haha", "hahaha", "hahahaha", "haha", "hahahha","hahhahahhahah", "hahahahahha", "hahahahahahahha", "hahahahahhahaa", 
                      "hahahah", "hahahaha", "haha", "haushsshsshsushsuhsshssushuahsushsishsushauahaushuahsjsjaushsj", "ahahaha",
                      "hahahaha", "hahahaha", "hahahhahaha", "hahahahaha", "haha", "hahahhaahahahahhaa", "hahahahahaah", "hahahaha",
                      "kkakakakakak", "kkkkkkkcomotanka","kkkkkkkkkkkkkkk","kkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk")


dados_filtrado <- dados_filtrado|>
  filter(!word %in% variacoes_risada)


variacoes_amor <- c("amor", "amo","ama","amado","amooo","amamos","amada","amoooooo","amam","amoooo","am","amooooooo","amooooo","amaa","amar")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_amor, "amor", word))


variacoes_bolsonaro <- c("bolso", "bolsobosta", "bolsominion", "bolsonara", "bolsonario", "bolsonarista", "bolsonaristas", "bolsonaro", "bolsonaronoflow", "bolsonaropresidente", "bolsonaroreeleito", "bolsonaroreeleitoem", 
                          "bolsonarotemrazao", "bolsonet", "bolsoney", "bolsonitta","bozo","jairbolsonaro")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacoes_bolsonaro, "bolsonaro", word))

variacao_admirar <- c("admiravel","admira","admiracao")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_admirar, "admirar", word))

variacao_adoro <- c("adoro")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_adoro, "adorar", word))

variacao_agradecer <- c("agradecida", "agradecimentos","agradeco")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_agradecer, "agradecer", word))

variacao_arrasar <- c("arrasa","arrasouuu")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_arrasar, "arrasar", word))

variacao_arrepiar <- c("arrepia","arrepiadissimo","arrepiante","arrepiei", "arrepio")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_arrepiar, "arrepiar", word))

variacao_artista <- c("artist","artista","artistica","artistico")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_artista, "artistas", word))

variacao_assistir <- c("assisti", "assistia", "assistia")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_assistir, "assistir", word))

variacao_ludmila <- c("lud","ludmila","ludmilla","ludimilla")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_ludmila, "ludmila", word))

variacao_pablo <- c("pabllo", "pablo", "pablovitta")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_pablo, "pabllovittar", word))

variacao_pqp <- c("pqp", "pqppp", "pqpppp", "pqppppp")

dados_filtrado <- dados_filtrado|>
  filter(!word %in% variacao_pqp)

variacao_tudo <- c("tudoh", "tudoooo", "tudooooh", "tudooooo", "tudoooooooo", "tudooooooooooooooooooooo", "tudovei")

dados_filtrado <- dados_filtrado|>
  mutate(word = ifelse(word %in% variacao_tudo, "tudooo", word))

# Transformando em csv
write.csv(dados_filtrado, "dados_filtrado.csv")

# Lendo dicionario de lematização
lema <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/lemmatization-pt.txt",header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
names(lema) <- c("stem", "word")

# Função de lematização
dados_lem = dplyr::left_join(dados_filtrado, lema, 
                             by='word')
dados_lem$word = ifelse(is.na(dados_lem$stem),
                            dados_lem$word,dados_lem$stem)
dados_lem = dplyr::select(dados_lem,-stem)


#Contando a frequencia das palavras
dados_lem = group_by(dados_lem,word)
freq = arrange(summarize(dados_lem, frequencia = n()),
               desc(frequencia))
dados_lem = left_join(dados_lem,freq,'word')
dados_lem = ungroup(dados_lem)

# Removendo palavras com frequencia menor que 10
dados_lem = filter(dados_lem,frequencia >= 10)

# contando frequencia das palavras dentro do tweet
dados_freq = group_by(dados_lem,Polaridade,word)
freq_palavra = summarize(dados_freq,word,frequencia=n())
freq_palavra = unique(freq_palavra)
dados_freq = ungroup(dados_freq)
dados_freq = group_by(dados_lem,Polaridade)

total_palavras = summarize(dados_freq,TotalPalavras = n())
freq_palavra = left_join(freq_palavra,total_palavras,by='Polaridade')
freq_palavra = mutate(freq_palavra,FreqPercentual = frequencia/TotalPalavras)

dados_freq = select(freq_palavra,-c(frequencia,TotalPalavras))

# Matriz termo documento
matriz_ftd<-tidyr::pivot_wider(dados_freq, id_cols = c(Polaridade), names_from = word, values_from = FreqPercentual)

matriz_ftd<-replace(matriz_ftd,is.na(matriz_ftd), 0)

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




