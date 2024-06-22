library(dplyr)
library(tidytext)
library(stringi)
library(SMOTEWB)
library(text2vec)
library(tm)
library(MVar.pt)
load("dados_rotulados.rda")
# Separando os dados em treino e teste
set.seed(2021)
notreino <- createDataPartition(dados$Polaridade, p = 0.7, list = FALSE)
treino <- dados[notreino,]
teste <- dados[-notreino,]
treino <- treino |> select(text, Polaridade)

# Lendo arquivo de stopwords
stopwords <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/stopwords_.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
stopwords <- unique(stopwords$V1)

# Funções de limpeza de texto
remove_acentos <- function(text) stri_trans_general(text, "latin-ascii")
remove_numeros <- function(text) gsub("[0-9]", "", text)

# Dicionário de emojis
emojis <- c("❤️" = " amor ", "💗" = " amor ", "🥰" = " amor ", "💕" = " amor ", "💜" = " amor ", "🫶🏾" = " amor ", "♥" = " amor ", "💓" = " amor ", "👏🏻" = " parabéns ",
            "👏" = " parabéns ", "🥹" = " amor ", "😻" = " amor ", "❤️‍🔥" = " amor ", "😘" = " amor ", "❣️" = " amor ", "💟" = " amor ", "🤢" = " nojo ",
            "💖" = " amor ", "🤮" = " nojo ", "🔥" = " amor ", "🖤" = " amor ", "🫶🏼" = " amor ", "🤩" = " amor ", "💞" = " amor ", "💝" = " amor ", "😊" = " amor ")

# Substituir emojis
substitute_emojis <- function(text, emoji_dict) {
  for (emoji in names(emoji_dict)) {
    text <- stri_replace_all_fixed(text, emoji, emoji_dict[[emoji]], vectorize_all = FALSE)
  }
  text
}

# Aplicando as transformações no texto
treino <- treino |>
  mutate(RecordID = row_number(),
         text = substitute_emojis(text, emojis),
         text = remove_acentos(text),
         text = remove_numeros(text))

# Tokenização e remoção de stopwords
treino <- treino |> 
  unnest_tokens(word, text) |>
  filter(!word %in% stopwords)

# Normalização de variações de palavras
normalizacoes <- list(
  "empolgado" = c("aaaaa", "aaaudjak", "aaaaah", "aaaaaaaaaa", "aaaaaa", "aaaaaaaaa", "aaaaaaa", "aaa", "aaaff", "aaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaa", "aaaa"),
  "ah" = c("ahhhhhhhhhhhhhhhhhhhhhhhhhhh", "ahhh", "ahhhhh"),
  "agora" = c("agoraaaaaaa"),
  "caralho" = c("caralhu", "crllll", "crlh", "crl", "caalho", "caaaralho", "krl", "krlh", "caralho", "caralhoooo", "caralh", "caralhooooo", "carai", "caraaaaai", "caralhoo", "caralhoooooo", "caralhas"),
  "amor" = c("amor", "amo", "ama", "amado", "amooo", "amamos", "amada", "amoooooo", "amam", "amoooo", "am", "amooooooo", "amooooo", "amaa", "amar"),
  "bolsonaro" = c("imbroxavel", "bolso", "bolsobosta", "bolsominion", "bolsonara", "bolsonario", "bolsonarista", "bolsonaristas", "bolsonaro", "bolsonaronoflow", "bolsonaropresidente", "bolsonaroreeleito", "bolsonaroreeleitoem", "bolsonarotemrazao", "bolsonet", "bolsoney", "bolsonitta", "bozo", "jairbolsonaro", "jair"),
  "admirar" = c("admiravel", "admira", "admiracao"),
  "adorar" = c("adoro"),
  "agradecer" = c("agradecida", "agradecimentos", "agradeco"),
  "arrasar" = c("arrasa", "arrasouuu"),
  "arrepiar" = c("arrepia", "arrepiadissimo", "arrepiante", "arrepei", "arrepio"),
  "artistas" = c("artist", "artista", "artistica", "artistico"),
  "assistir" = c("assisti", "assistia", "assistia"),
  "ludmila" = c("lud", "ludmila", "ludmilla", "ludimilla"),
  "pabllovittar" = c("pabllo", "pablo", "pablovitta"),
  "tudooo" = c("tudoh", "tudoooo", "tudooooh", "tudooooo", "tudoooooooo", "tudooooooooooooooooooooo", "tudovei")
)

for (nova_palavra in names(normalizacoes)) {
  variacoes <- normalizacoes[[nova_palavra]]
  treino <- treino |> mutate(word = ifelse(word %in% variacoes, nova_palavra, word))
}

# Removendo palavras irrelevantes e risadas
variacoes_ignoradas <- c("anittaaaaa", "anita", "aniraaaa", "aninha", "aninhaaaaa", "anira", "aniraaa", "anittaaaaaaa", "anitta", "kakkakakak", "kakakakakakkakakakakakakakak", "kakakakakakakakakakakakakakakakakakakakakak", "kakakakakakkakakak", "kakakakakakak", "kakakaka", "kjkk", "kkkkkkkkkkkkkk", "ksdjskskk", "k", "kakak", "kkkkkkkkkkkkk", "mkkkkkkk", "kkkkkkkkkkk", "kkkkkkkkmmm", "kskskskkskskzkzkzkzm", "mkk", "kskkskskskkskskkskkskskks", "kkl", "kkakakakaka", "kkkkkkkkkkkk", "mlk", "ldhkdhaksjskjkh", "kksjdskkdkss", "ksxbkdjxkdbdj", "akakakakak", "akaka", "kkkj", "lllllllkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkk", "hahhaha", "haha", "hahaha", "hahahaha", "haha", "hahahha", "hahhahahhahah", "hahahahahha", "hahahahahahahha", "hahahahahhahaa", "hahahah", "hahahaha", "haha", "haushsshsshsushsuhsshssushuahsushsishsushauahaushuahsjsjaushsj", "ahahaha", "hahahaha", "hahahaha", "hahahhahaha", "hahahahaha", "haha", "hahahhaahahahahhaa", "hahahahahaah", "hahahaha", "kkakakakakak", "kkkkkkkcomotanka", "kkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk", "pqp", "pqppp", "pqpppp", "pqppppp")
treino <- treino |> filter(!word %in% variacoes_ignoradas)

# Lematização
lema <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/lema_claudio.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
names(lema) <- c("stem", "word")
dados_lem <- left_join(treino, lema, by = 'word') |>
  mutate(word = ifelse(is.na(stem), word, stem)) |>
  select(-stem)


#Contando a frequencia das palavras
dados_lem = group_by(dados_lem,word)
freq = arrange(summarize(dados_lem, frequencia = n()),
               desc(frequencia))
dados_lem = left_join(dados_lem,freq,'word')
dados_lem = ungroup(dados_lem)

# filtrando palavras com a frequencia minima
freq_min <- 15
dados_min<-dplyr::filter(dados_lem,
                         frequencia>=freq_min)
# Contagem de frequência das palavras
dados_freq <- dados_min |>
  count(RecordID, word, Polaridade) |> 
  group_by(RecordID) |> 
  mutate(TotalPalavras = sum(n), FreqPercentual = n / TotalPalavras) |>
  ungroup()


# Criando matriz termo documento
tdm <- dados_freq |>
  dcast(RecordID + Polaridade ~ word, value.var = "FreqPercentual", fill = 0)

# Removendo colunas desnecessárias e reordenando
colunas_desnecessarias <- c("V1", "l", "achar", "acreditar", "acontecer", "acompanhar", "antiga", "bbma", "chegar")
tdm <- tdm |> select(-any_of(colunas_desnecessarias))
tdm$Polaridade <- as.factor(ifelse(tdm$Polaridade == "1", 1, 0))

# Removendo coluna RecordID
tdm <- tdm|>
  select(-RecordID)

# Balanceamento dos dados
tdm <- as.data.frame(tdm)
tdm_bal <- SMOTEWB(
  x = tdm[, -1], 
  y = tdm$Polaridade, 
  K = 5
)

# Transformando em data frame final
tdm_final <- cbind(tdm_bal[[1]], Polaridade = tdm_bal[[2]])
rownames(tdm_final) <- NULL
tdm_final <- as.data.frame(tdm_final)

# Separar a matriz de termos e os rótulos de treinamento
terms_train <- tdm_final[, -ncol(tdm)]
labels_train <- tdm_final[, ncol(tdm)]

# Função para calcular o IDF a partir dos dados de treinamento
calculate_idf <- function(terms) {
  N <- nrow(terms)
  doc_freq <- colSums(terms > 0)
  idf <- log(N / (1 + doc_freq))
  return(idf)
}

# Função para calcular o TF-IDF
calculate_tfidf <- function(terms, idf) {
  tf <- as.matrix(terms)
  tfidf <- tf * idf
  return(tfidf)
}
# Calcular o IDF a partir dos dados de treinamento
idf_train <- calculate_idf(terms_train)

# Calcular o TF-IDF para os dados de treinamento
tfidf_matrix_train <- calculate_tfidf(terms_train, idf_train)

# Converter a matriz TF-IDF para uma matriz esparsa do tipo dgCMatrix
tfidf_sparse_matrix_train <- as(tfidf_matrix_train, "dgCMatrix")

