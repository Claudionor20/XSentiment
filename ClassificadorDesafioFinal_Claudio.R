Classificador <- function(data){
  
  # Verificando se todos pacotes necessários estão instalados
  pacotes <- c("dplyr", "tidytext",  "stringi", "SMOTEWB","caret","maditr","xgboost","data.table")
  for (pacote in pacotes) {
    if (!requireNamespace(pacote, quietly = TRUE)) {
      install.packages(pacote)
    }
  }
  
  lapply(pacotes, library, character.only = TRUE)
  
  load("Claudionor_DesafioFinal.rda")
  
  data1 <- preprocessamento_teste(data)
  
  # Verificando colunas exclusivas no conjunto de treino
  colunas_exclusivas <- setdiff(colunas_treino, colnames(data1))
  
  # Adicionando colunas ausentes no conjunto de teste e preenchendo com zeros
  for (col in colunas_exclusivas) {
    if(!col %in% colnames(data1)){
      data1[[col]] = 0
    }
  }
  
  data2 <- data1[,colunas_treino]
  
  data2$Polaridade <- ifelse(data2$Polaridade == "1",0,1)
  
  data3 <- xgb.DMatrix(data = as.matrix(data2[,-ncol(data2)]), label = as.matrix(as.factor(data2$Polaridade)))
  

  # Fazendo predicao
  pred <- predict(best_model, newdata = data3)
  
  # Matriz de confusão
  
  data2 <- data2|>
    mutate(Polaridade = factor(Polaridade, levels = c(0,1)))

  cm <- confusionMatrix(as.factor(pred), as.factor(data2$Polaridade)) # Matriz de confusão
  
  return(cm)
}


load("dados_rotulados.rda")



Classificador(dados[1:400,])
