# Importando pacotes
library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(stringi)
library(wordcloud)
library(textstem)
library(rtweet)
library(caret)
library(lexiconPT)
library(maditr)
library(xgboost)

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

# função para remover numeros
remove_numeros <- function(text) {
  text <- gsub("[0-9]", "", text)
  
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

#criando coluna identificador

dados = mutate(dados, RecordID = c(1:length(dados$text)))
dados = select(dados,c(RecordID,everything()))

# Removendo emojis
dados <- dados|>
  mutate(text = substitute_emojis(text, emojis))

# Separando stopwords
dados <- tidytext::unnest_tokens(dados, word, text)

# Removendo acentos
dados <- dados|>
  mutate(word = remove_acentos(word))


# Removendo stopwords
dados_filtrado <- dados|>
  filter(!word %in% stopwords)

# Removendo numeros
dados_filtrado <- dados_filtrado|>
  mutate(word = remove_numeros(word))



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

variacoes_caralho <- c("caralhu","crllll","crlh","crl","caalho","caaaralho","krl","krlh","caralho", "caralhoooo", "caralh", "caralhooooo", "carai", "caraaaaai", "caralhoo", "caralhoooooo", "caralhas")

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


variacoes_bolsonaro <- c("imbroxavel","bolso", "bolsobosta", "bolsominion", "bolsonara", "bolsonario", "bolsonarista", "bolsonaristas", "bolsonaro", "bolsonaronoflow", "bolsonaropresidente", "bolsonaroreeleito", "bolsonaroreeleitoem", 
                          "bolsonarotemrazao", "bolsonet", "bolsoney", "bolsonitta","bozo","jairbolsonaro","jair")

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
lema <- read.delim("https://raw.githubusercontent.com/Claudionor20/XSentiment/main/lema_claudio.txt",header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
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

# filtrando palavras com a frequencia minima
freq_min <- 15
dados_min<-dplyr::filter(dados_lem,
                         frequencia>=freq_min)


# contando frequencia das palavras dentro do tweet
dados_freq = group_by(dados_min,RecordID,word,Polaridade)
freq_palavra = summarize(dados_freq,word,frequencia=n())
freq_palavra = unique(freq_palavra)
dados_freq = ungroup(dados_freq)
dados_freq = group_by(dados_lem,RecordID)

total_palavras = summarize(dados_freq,TotalPalavras = n())
freq_palavra = left_join(freq_palavra,total_palavras,by='RecordID')
freq_palavra = mutate(freq_palavra,FreqPercentual = frequencia/TotalPalavras)

dados_freq = select(freq_palavra,-c(frequencia,TotalPalavras))

# Criando matriz termo documento com polaridade
tdm = dcast(dados_freq, RecordID + Polaridade ~ word, value.var = "FreqPercentual")

tdm<-replace(tdm,is.na(tdm), 0)

##Realizar analise de sentimento
#carregando dicionario lexico
oplexicon = lexiconPT::oplexicon_v3.0
names(oplexicon) = c('word','Classe','Polaridade Palavra','Polaridade Revisada')

#Vamos usar o dados_lem, pois possui o tweet
#prox do seu formato inicial (sem cortes por freq)
dados_sent = dplyr::left_join(dados_lem,oplexicon,by='word')
dados_sent$`Polaridade Palavra` = replace(dados_sent$`Polaridade Palavra`,is.na(dados_sent$`Polaridade Palavra`), 0)
dados_sent = dplyr::group_by(dados_sent, RecordID)
scores = dplyr::summarize(dados_sent,score=sum(`Polaridade Palavra`))


#termo-documento por frequncia (Bag of Words)
matriz_ftd = dplyr::left_join(tdm,scores,by='RecordID')
matriz_ftd = select(matriz_ftd,c(Polaridade,everything()))
# Tirando recordeID
matriz_ftd = select(matriz_ftd,-RecordID)

# Transformar coluna polaridade em 0 e 1
matriz_ftd$Polaridade = ifelse(matriz_ftd$Polaridade == "-1", 0, 1)
# Separando em treino e teste

set.seed(123)
matriz_ftd$Polaridade = as.factor(matriz_ftd$Polaridade)
notreino <- createDataPartition(matriz_ftd$Polaridade, p = 0.7, list = FALSE)
treino <- matriz_ftd[notreino,]
teste <- matriz_ftd[-notreino,]

sapply(treino, class)


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
  eta = c(0.3),
  max_depth = c(6),
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
    dtrain <- xgb.DMatrix(data = as.matrix(treino_fold[,-length(treino_fold)]), label = as.matrix(as.factor(treino_fold$Polaridade)))
    
    validacao_fold <- treino[fold,]
    dvalid <- xgb.DMatrix(data = as.matrix(validacao_fold[,-length(validacao_fold)]), label = as.matrix(as.factor(validacao_fold$Polaridade)))
    
    # Treinar o modelo com o fold de treino
    set.seed(5415)
    xgbm_model <- xgboost(data = dtrain,
                          gamma=0, eta=params$eta, max_depth=params$max_depth,
                          nrounds = params$nrounds, 
                          early_stopping_rounds=params$early_stopping_rounds,
                          objective = "binary:hinge",
                          verbose = 0)
    
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


