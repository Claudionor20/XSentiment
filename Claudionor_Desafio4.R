# Importando pacotes necessários
library(dplyr)
library(tidytext)
library(stringi)
library(SMOTEWB)
library(caret)
library(maditr)
library(xgboost)
library(data.table)


load("dados_rotulados.rda")

set.seed(2611)
notreino <- caret::createDataPartition(dados$Polaridade, p = 0.6, list = FALSE)
treino <- dados[notreino,]
teste <- dados[-notreino,]

preprocessamento <- function(dados) {
  # Separando os dados em treino e teste
  
  dados <- dados |> select(text, Polaridade)
  
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
  dados <- dados |>
    mutate(RecordID = row_number(),
           text = substitute_emojis(text, emojis),
           text = remove_acentos(text),
           text = remove_numeros(text))
  
  # Tokenização e remoção de stopwords
  dados <- dados |> 
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
    dados <- dados |> mutate(word = ifelse(word %in% variacoes, nova_palavra, word))
  }
  
  # Removendo palavras irrelevantes e risadas
  variacoes_ignoradas <- c("anittaaaaa", "anita", "aniraaaa", "aninha", "aninhaaaaa", "anira", "aniraaa", "anittaaaaaaa", "anitta", "kakkakakak", "kakakakakakkakakakakakakakak", "kakakakakakakakakakakakakakakakakakakakakak", "kakakakakakkakakak", "kakakakakakak", "kakakaka", "kjkk", "kkkkkkkkkkkkkk", "ksdjskskk", "k", "kakak", "kkkkkkkkkkkkk", "mkkkkkkk", "kkkkkkkkkkk", "kkkkkkkkmmm", "kskskskkskskzkzkzkzm", "mkk", "kskkskskskkskskkskkskskks", "kkl", "kkakakakaka", "kkkkkkkkkkkk", "mlk", "ldhkdhaksjskjkh", "kksjdskkdkss", "ksxbkdjxkdbdj", "akakakakak", "akaka", "kkkj", "lllllllkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkk", "hahhaha", "haha", "hahaha", "hahahaha", "haha", "hahahha", "hahhahahhahah", "hahahahahha", "hahahahahahahha", "hahahahahhahaa", "hahahah", "hahahaha", "haha", "haushsshsshsushsuhsshssushuahsushsishsushauahaushuahsjsjaushsj", "ahahaha", "hahahaha", "hahahaha", "hahahhahaha", "hahahahaha", "haha", "hahahhaahahahahhaa", "hahahahahaah", "hahahaha", "kkakakakakak", "kkkkkkkcomotanka", "kkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk", "pqp", "pqppp", "pqpppp", "pqppppp")
  dados <- dados |> filter(!word %in% variacoes_ignoradas)
  
  # Lematização
  lema <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/lemmatization-pt_vClaudionor.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  names(lema) <- c("stem", "word")
  dados_lem <- left_join(dados, lema, by = 'word') |>
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
  
  # Botando polaridade na ultima coluna
  tdm <- tdm %>%
    select(-Polaridade) %>%
    bind_cols(tdm %>% select(Polaridade))
  
  # Balanceamento dos dados
  tdm <- as.data.frame(tdm)
  tdm_bal <- SMOTEWB(
    x = tdm[, -ncol(tdm)], 
    y = tdm$Polaridade, 
    K = 5
  )
  
  # Transformando em data frame final
  tdm_final <- cbind(tdm_bal[[1]], Polaridade = tdm_bal[[2]])
  rownames(tdm_final) <- NULL
  tdm_final <- as.data.frame(tdm_final)
  
  tdm_final$Polaridade <- as.factor(tdm_final$Polaridade)
  # mudar levels de 1 e 2 para 0 e 1
  levels(tdm_final$Polaridade) <- c(0,1)
  
  # Separar a matriz de termos e os rótulos de treinamento
  terms_train <- tdm_final[, -ncol(tdm_final)]
  labels_train <- tdm_final[, ncol(tdm_final)]
  
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
  
  tfidf_matrix_train <- cbind(tfidf_matrix_train, Polaridade = tdm_final$Polaridade)
  
  return(tfidf_matrix_train)
}

preprocessamento_teste <- function(dados) {
  # Separando os dados em treino e teste
  
  dados <- dados |> select(text, Polaridade)
  
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
  dados <- dados |>
    mutate(RecordID = row_number(),
           text = substitute_emojis(text, emojis),
           text = remove_acentos(text),
           text = remove_numeros(text))
  
  # Tokenização e remoção de stopwords
  dados <- dados |> 
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
    dados <- dados |> mutate(word = ifelse(word %in% variacoes, nova_palavra, word))
  }
  
  # Removendo palavras irrelevantes e risadas
  variacoes_ignoradas <- c("anittaaaaa", "anita", "aniraaaa", "aninha", "aninhaaaaa", "anira", "aniraaa", "anittaaaaaaa", "anitta", "kakkakakak", "kakakakakakkakakakakakakakak", "kakakakakakakakakakakakakakakakakakakakakak", "kakakakakakkakakak", "kakakakakakak", "kakakaka", "kjkk", "kkkkkkkkkkkkkk", "ksdjskskk", "k", "kakak", "kkkkkkkkkkkkk", "mkkkkkkk", "kkkkkkkkkkk", "kkkkkkkkmmm", "kskskskkskskzkzkzkzm", "mkk", "kskkskskskkskskkskkskskks", "kkl", "kkakakakaka", "kkkkkkkkkkkk", "mlk", "ldhkdhaksjskjkh", "kksjdskkdkss", "ksxbkdjxkdbdj", "akakakakak", "akaka", "kkkj", "lllllllkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkk", "hahhaha", "haha", "hahaha", "hahahaha", "haha", "hahahha", "hahhahahhahah", "hahahahahha", "hahahahahahahha", "hahahahahhahaa", "hahahah", "hahahaha", "haha", "haushsshsshsushsuhsshssushuahsushsishsushauahaushuahsjsjaushsj", "ahahaha", "hahahaha", "hahahaha", "hahahhahaha", "hahahahaha", "haha", "hahahhaahahahahhaa", "hahahahahaah", "hahahaha", "kkakakakakak", "kkkkkkkcomotanka", "kkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkk", "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk", "pqp", "pqppp", "pqpppp", "pqppppp")
  dados <- dados |> filter(!word %in% variacoes_ignoradas)
  
  # Lematização
  lema <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/lemmatization-pt_vClaudionor.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  names(lema) <- c("stem", "word")
  dados_lem <- left_join(dados, lema, by = 'word') |>
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
  
  # Botando polaridade na ultima coluna
  tdm <- tdm %>%
    select(-Polaridade) %>%
    bind_cols(tdm %>% select(Polaridade))
  
  # Separar a matriz de termos e os rótulos de treinamento
  terms_train <- tdm[, -ncol(tdm)]
  labels_train <- tdm[, ncol(tdm)]
  
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
  
  # voltando com a coluna de polaridade
  tfidf_matrix_train <- cbind(tfidf_matrix_train, Polaridade = tdm$Polaridade)
  
  
}


# Aplicando a função de preprocessamento
treino <- preprocessamento(treino)

teste <- preprocessamento_teste(teste)

treino <- as.data.frame(treino)
teste <- as.data.frame(teste)


# Verificando colunas exclusivas no conjunto de treino
colunas_exclusivas <- setdiff(colnames(treino), colnames(teste))

# Adicionando colunas ausentes no conjunto de teste e preenchendo com zeros
for (col in colunas_exclusivas) {
  if(!col %in% colnames(teste)){
  teste[[col]] = 0
  }
}

colunas_treino <- colnames(treino)

teste1 <- teste[,colunas_treino]

teste <- teste1

teste$Polaridade <- ifelse(teste$Polaridade == "1",0,1)
treino$Polaridade <- ifelse(treino$Polaridade == "1",0,1)

levels(teste$Polaridade) <- c(0,1)
levels(treino$Polaridade) <- c(0,1)

########## Treinando o modelo ##############


# Definir a grade de hiperparâmetros
param_grid <- expand.grid(
  eta = c(0.3,0.1,0.3,0.5),
  max_depth = c(3, 6, 9,12),
  nrounds = c(1000,2000),
  early_stopping_rounds = c(10,12)
)


results <- list()


# Definir o número de folds para a validação cruzada
k <- 10
folds <- caret::createFolds(treino$Polaridade, k = k, list = TRUE)

# Função para calcular a média das métricas de cada combinação de hiperparâmetros
calc_media_metrics <- function(metrics_list) {
  media_metrics <- sapply(metrics_list, mean)
  return(media_metrics)
}


# Loop sobre todas as combinações de hiperparâmetros
for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # Inicializar listas para armazenar métricas de cada fold
  metrics_list <- list(accuracy = numeric(k),
                       specificity = numeric(k),
                       sensitivity = numeric(k),
                       npv = numeric(k))
  
  for (fold_index in 1:k) {
    fold <- folds[[fold_index]]
    
    # Separar dados de treino e validação
    treino_fold <- treino[-fold,]
    dtrain <- xgb.DMatrix(data = as.matrix(treino_fold[,-ncol(treino)]), label = as.matrix(as.factor(treino_fold$Polaridade)))
    
    validacao_fold <- treino[fold,]
    dvalid <- xgb.DMatrix(data = as.matrix(validacao_fold[,-ncol(treino)]), label = as.matrix(as.factor(validacao_fold$Polaridade)))
    
    # Treinar o modelo com o fold de treino
    set.seed(2611)
    train_labels <- getinfo(dtrain, "label")
    xgbm_model <- xgboost(data = dtrain,
                          gamma=0, eta=params$eta, max_depth=params$max_depth,
                          nrounds = params$nrounds, 
                          early_stopping_rounds=params$early_stopping_rounds,
                          objective = "binary:hinge",
                          verbose = 0,
                          lambda = 1
                          )
    
    # Predição na validação
    predicao_validacao <- predict(xgbm_model, newdata = dvalid)
    
    # Fazendo matriz de confusão
    cm <- confusionMatrix(as.factor(predicao_validacao), as.factor(validacao_fold$Polaridade))
    
    
    # Armazenar métricas do fold
    metrics_list$accuracy[fold_index] <- cm$overall['Accuracy']
    metrics_list$specificity[fold_index] <- cm$byClass['Specificity']
    metrics_list$sensitivity[fold_index] <- cm$byClass['Sensitivity']
    metrics_list$npv[fold_index] <- cm$byClass['Neg Pred Value']
  }
  
  # Calcular médias das métricas
  media_metrics <- calc_media_metrics(metrics_list)
  
  # Armazenar os resultados
  results[[i]] <- list(
    params = params,
    accuracy = media_metrics['accuracy'],
    specificity = media_metrics['specificity'],
    sensitivity = media_metrics['sensitivity'],
    negative_predictive_value = media_metrics['npv'],
    model = xgbm_model,
    confusion_matrix = cm
  )
  
  
}


melhores_modelos <- results[sapply(results, function(x) all(x$accuracy > 0.80 & x$specificity > 0.80 & x$sensitivity > 0.815))]

# Selecionar o melhor modelo
best_model <- melhores_modelos[[2]]$model

levels(teste$Polaridade)
levels(treino$Polaridade)

# Predição na base de teste
dtest <- xgb.DMatrix(data = as.matrix(teste[,-ncol(teste)]), label = as.matrix(as.factor(teste$Polaridade)))
predicao_teste <- predict(best_model, newdata = dtest)



# Matriz de confusão
cm_teste <- confusionMatrix(as.factor(predicao_teste), as.factor(teste$Polaridade))

cm_teste

