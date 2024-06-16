library(xgboost)
library(caret)
library(kernlab)
library(dplyr)
library(progress)

data("spam")




# Transformação da coluna type
spam$type_binario <- ifelse(spam$type == "spam", 0, 1)

# Remover a coluna type original se necessário
spam <- spam[, -which(names(spam) == "type")]

spam <- spam|>
  dplyr::mutate(type_binario = factor(type_binario, levels = c("0","1")))
  
notreino <- createDataPartition(spam$type, p=0.75, list=F)


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


treino1<- spam[notreino,]
teste1 <- spam[-notreino,]


# Definir o número de folds para a validação cruzada
k <- 10
folds <- caret::createFolds(treino1$type_binario, k = k, list = TRUE)

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
    treino_fold <- treino1[-fold,]
    dtrain <- xgb.DMatrix(data = as.matrix(treino_fold[,-length(treino_fold)]), label = as.matrix(as.factor(treino_fold$type_binario)))

    validacao_fold <- treino1[fold,]
    dvalid <- xgb.DMatrix(data = as.matrix(validacao_fold[,-length(validacao_fold)]), label = as.matrix(as.factor(validacao_fold$type_binario)))

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
    ponto_otimo <- otimizador(predicao_validacao, validacao_fold$type_binario)
    
    # Classificar as predições
    classificacao <- ifelse(predicao_validacao >= ponto_otimo, 1, 0)
    
    # Matriz de confusão
    #cm <- confusionMatrix(as.factor(classificacao), as.factor(validacao_fold$type_binario))
    cm <- confusionMatrix(as.factor(classificacao),as.factor(validacao_fold$type_binario))
    
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



# Pegue os modelos com as melhores métricas gerais

melhores_modelos <- results[sapply(results, function(x) all(x$accuracy > 0.80 & x$specificity > 0.65 & x$sensitivity > 0.65 & x$negative_predictive_value > 0.27))]

# Escolhendo melhor modelo 

melhor_modelo <- melhores_modelos[[1]]

# Fazendo predições na base de teste

dvalid1 <- xgb.DMatrix(data = as.matrix(teste1[,-length(teste1)]), label = as.matrix(as.factor(teste1$type_binario)))
predicao_teste <- predict(melhor_modelo$model, newdata = dvalid1)

# utilizando o ponto de corte ótimo

classificacao_teste <- ifelse(predicao_teste >= melhor_modelo$ponto_otimo, 1, 0)

# Matriz de confusão

cm_teste <- confusionMatrix(as.factor(classificacao_teste),as.factor(teste1$type_binario))


