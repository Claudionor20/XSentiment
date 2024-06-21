# Importando pacotes necessários
library(dplyr)
library(tidytext)
library(stringi)
library(SMOTEWB)

load("dados_rotulados.rda")

set.seed(2024)
notreino <- createDataPartition(dados$Polaridade, p = 0.7, list = FALSE)
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
  lema <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/lema_claudio.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
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
  
  # Balanceamento dos dados
  tdm <- as.data.frame(tdm)
  tdm_bal <- SMOTEWB(
    x = tdm[, -1], 
    y = tdm$Polaridade, 
    K = 5,
    over = 200,                 # taxa de over-sampling
    under = 100 
  )
  
  # Transformando em data frame final
  tdm_final <- cbind(tdm_bal[[1]], Polaridade = tdm_bal[[2]])
  rownames(tdm_final) <- NULL
  tdm_final <- as.data.frame(tdm_final)
  
  tdm_final$Polaridade <- as.factor(tdm_final$Polaridade)
  # mudar levels de 1 e 2 para 0 e 1
  levels(tdm_final$Polaridade) <- c(0,1)
  
  return(tdm_final)
}


# Aplicando a função de preprocessamento
treino <- preprocessamento(treino)
teste <- preprocessamento(teste)

# Aplicando as mesmas colunas da base treino na base teste, ou seja, se tiver uma coluna na base treino que não tem na base teste, essa coluna será adicionada na base teste e preenchida com 0
colunas_treino <- colnames(treino)
colunas_teste <- colnames(teste)
colunas_faltantes <- setdiff(colunas_treino, colunas_teste)
for (coluna in colunas_faltantes) {
  teste[[coluna]] <- 0
}

# Deixando as colunas na mesma ordem
teste <- teste[colunas_treino]





########## Treinando o modelo ##############

otimizador <- function(predicao, resposta) {
  require(ROCR)
  
  # Criar o objeto de previsão
  pred <- prediction(predicao, resposta)
  
  # Calcular a performance
  perf <- performance(pred, "tpr", "fpr")
  
  # Calcular a diferença entre TPR e FPR
  tpr <- unlist(perf@y.values)
  fpr <- unlist(perf@x.values)
  funcao <- tpr - fpr
  
  # Encontrar o ponto de corte ótimo
  optimal_idx <- which.max(funcao)
  ponto_otimo <- perf@alpha.values[[1]][optimal_idx]
  
  # Plotar a curva ROC
  plot(perf, colorize = TRUE, lwd = 2, main = "Curva ROC")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  points(fpr[optimal_idx], tpr[optimal_idx], pch = 19, col = "red")
  text(fpr[optimal_idx], tpr[optimal_idx], labels = round(ponto_otimo, 2), pos = 4, col = "red")
  
  return(ponto_otimo)
}


# Definir a grade de hiperparâmetros
param_grid <- expand.grid(
  eta = c(0.3,0.1,0.3,0.5),
  max_depth = c(3, 6, 9),
  nrounds = c(1000),
  early_stopping_rounds = c(10)
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
    set.seed(29062003)
    xgbm_model <- xgboost(data = dtrain,
                          gamma=0, eta=params$eta, max_depth=params$max_depth,
                          nrounds = params$nrounds, 
                          early_stopping_rounds=params$early_stopping_rounds,
                          objective = "binary:hinge",
                          verbose = 0,
                          subsample = 0.8,
                          lambda = 1,
                          colsample_bytree = 0.8)
    
    # Predição na validação
    predicao_validacao <- predict(xgbm_model, newdata = dvalid)
    
    # Encontrar o ponto de corte ótimo
    ponto_otimo <- otimizador(predicao_validacao, validacao_fold$Polaridade)
    
    # Classificar as predições
    classificacao <- ifelse(predicao_validacao >= ponto_otimo, 1, 0)
    
    # Matriz de confusão
    #cm <- confusionMatrix(as.factor(classificacao), as.factor(validacao_fold$Polaridade))
    
    cm <- confusionMatrix(as.factor(classificacao),as.factor(validacao_fold$Polaridade))
    
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
    ponto_otimo = ponto_otimo,
    model = xgbm_model,
    confusion_matrix = cm
  )
  
  
}



melhores_modelos <- results[sapply(results, function(x) all(x$accuracy > 0.75 & x$specificity > 0.75 & x$sensitivity > 0.70))]

# Selecionar o melhor modelo
best_model <- melhores_modelos[[1]]$model


# Predição na base de teste
dtest <- xgb.DMatrix(data = as.matrix(teste[,-ncol(teste)]), label = as.matrix(as.factor(teste$Polaridade)))
predicao_teste <- predict(best_model, newdata = dtest)

# Encontrar o ponto de corte ótimo
ponto_otimo <- otimizador(predicao_teste, teste$Polaridade)

# Classificar as predições
classificacao_teste <- ifelse(predicao_teste >= ponto_otimo, 1, 0)

# Matriz de confusão
cm_teste <- confusionMatrix(as.factor(classificacao_teste), as.factor(teste$Polaridade))

cm_teste
