#Carregar Pacotes
{ library(dplyr)
  library(stringr)
  library(rtweet)
  library(readxl)
  library(caret)
  library(tidyverse)}

#Carregar bibliotecas
  ##stop words
  {stop_words2 = read.delim(file="https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  names(stop_words2) = c('Palavras')
    ### removendo espaços em branco da lista importada
  for (i in 1:dim(stop_words2)[1]){
    stop_words2[i,1]=trimws(stop_words2[i,1])}
    }
  
  ##lematizacao
  lemma_dic <- read.delim(file = "https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE, encoding = 'UTF-8')
  names(lemma_dic) <- c("stem", "Palavras")

  ##sentimentos
  oplexicon = lexiconPT::oplexicon_v3.0

#### CRIAÇÃO DE FUNÇÕES BÁSICAS #### 
  
# função para remover acentos
# tirar_acentos <- function(x) iconv(x, to = "ASCII//TRANSLIT")
  tirar_acentos <- function(str,pattern="all") {
    # Rotinas e funções úteis V 1.0
    # rm.accent - REMOVE ACENTOS DE PALAVRAS
    # Função que tira todos os acentos e pontuações de um vetor de strings.
    # Parâmetros:
    # str - vetor de strings que terão seus acentos retirados.
    # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
    #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
    #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
    #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
    if(!is.character(str))
      str <- as.character(str)
    
    pattern <- unique(pattern)
    
    if(any(pattern=="Ç"))
      pattern[pattern=="Ç"] <- "ç"
    
    symbols <- c(
      acute = "áéíóúÁÉÍÓÚýÝ",
      grave = "àèìòùÀÈÌÒÙ",
      circunflex = "âêîôûÂÊÎÔÛ",
      tilde = "ãõÃÕñÑ",
      umlaut = "äëïöüÄËÏÖÜÿ",
      cedil = "çÇ"
    )
    
    nudeSymbols <- c(
      acute = "aeiouAEIOUyY",
      grave = "aeiouAEIOU",
      circunflex = "aeiouAEIOU",
      tilde = "aoAOnN",
      umlaut = "aeiouAEIOUy",
      cedil = "cC"
    )
    
    accentTypes <- c("´","`","^","~","¨","ç")
    
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
      return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    
    for(i in which(accentTypes%in%pattern))
      str <- chartr(symbols[i],nudeSymbols[i], str)
    
    return(str)
  }
  
  # LER BASE DE TWEETS ROTULADOS
  load("dados_rotulados_Anita.rda")
  
  # SELECIONAR VARIAVEIS DE INTERESSE
  dados = data.frame(dados) %>% select(c(text,Polaridade))
  #TIRAR ACENTOS DA BASE DE TEXTO E DAS BIBLIOTECAS(OPCIONAL)
  {dados$text = tirar_acentos(dados$text)
  oplexicon$term <- tirar_acentos(oplexicon$term)
  stop_words2$Palavras <- tirar_acentos(stop_words2$Palavras)
  lemma_dic$stem <-tirar_acentos(lemma_dic$stem)
  lemma_dic$Palavras <-tirar_acentos(lemma_dic$Palavras)
  }
  
  # TRATAMENTOS DE TEXTO (problemas na escrita - pode aumentar)
    #Tudo em minusculo
    {dados$text = str_to_lower(dados$text)
    #Letras duplicadas
    dados$text = str_replace_all(dados$text,'aaaa','a')
    dados$text = str_replace_all(dados$text,'aaa','a')
    dados$text = str_replace_all(dados$text,'aa','a')
    dados$text = str_replace_all(dados$text,'eeee','e')
    dados$text = str_replace_all(dados$text,'eee','e')
    dados$text = str_replace_all(dados$text,'ee','e')
    dados$text = str_replace_all(dados$text,'iiii','i')
    dados$text = str_replace_all(dados$text,'iii','i')
    dados$text = str_replace_all(dados$text,'ii','i')
    dados$text = str_replace_all(dados$text,'oooo','o')
    dados$text = str_replace_all(dados$text,'ooo','o')
    dados$text = str_replace_all(dados$text,'oo','o')
    dados$text = str_replace_all(dados$text,'uuuu','u')
    dados$text = str_replace_all(dados$text,'uuu','u')
    dados$text = str_replace_all(dados$text,'uu','u')
    #Consertar Anita
    dados$text = str_replace_all(dados$text,'@anitta','Anitta')
    dados$text = str_replace_all(dados$text,'anita','Anitta')
    dados$text = str_replace_all(dados$text,'anira','Anitta')
    #Remover pontuacao
    dados$text = str_replace_all(dados$text,"[[:punct:]]", "")
    #Remover Numeros
    dados$text = str_replace_all(dados$text,'0','')
    dados$text = str_replace_all(dados$text,'1','')
    dados$text = str_replace_all(dados$text,'2','')
    dados$text = str_replace_all(dados$text,'3','')
    dados$text = str_replace_all(dados$text,'4','')
    dados$text = str_replace_all(dados$text,'5','')
    dados$text = str_replace_all(dados$text,'6','')
    dados$text = str_replace_all(dados$text,'7','')
    dados$text = str_replace_all(dados$text,'8','')
    dados$text = str_replace_all(dados$text,'9','')
    }
  
  #criando coluna identificador
  
  dados = mutate(dados, RecordID = c(1:length(dados$text)))
  dados = select(dados,c(RecordID,everything()))
  
  # CRIAÇÃO DO BAG OF WORDS
  {
    dados = mutate(dados, Palavras = strsplit(as.character(text)," ")) %>% unnest(Palavras)
    dados$Palavras = str_replace_all(dados$Palavras,' ','')
    dados$Palavras = str_replace_all(dados$Palavras,' ','')
    dados$Palavras = str_replace_all(dados$Palavras,'
','')
    dados$Palavras =  str_squish(dados$Palavras)
    dados = na.exclude(dados)
    dados = filter(dados,Palavras!=' ')
    dados = filter(dados,Palavras!='')
    dados = filter(dados,Palavras!=' ')
    dados = filter(dados,Palavras!='')
     
    }
  
  # REMOVENDO STOP WORDS
  dados = anti_join(dados, stop_words2, by='Palavras')
  
  # LEMATIZANDO OS TERMOS
  {dados_lematizados = left_join(dados, lemma_dic, by='Palavras')
    dados_lematizados$Palavras = ifelse(is.na(dados_lematizados$stem),dados_lematizados$Palavras,dados_lematizados$stem)
    dados_lematizados = select(dados_lematizados,-stem)}
  
  # Contando a frequencia das palavras
  {dados_lematizados = group_by(dados_lematizados,Palavras)
  freq = summarize(dados_lematizados, frequencia = n())
  freq = arrange(freq,desc(frequencia))
  dados_lematizados = left_join(dados_lematizados,freq,'Palavras')
  min_freq = 8 #Voce decide a freq minima
  dados_lematizados = ungroup(dados_lematizados)
  }
  
  # filtrando palavras com a frequencia minima
  {dados_lematizados = filter(dados_lematizados,frequencia>=min_freq)
    dados_lematizados = select(dados_lematizados,-frequencia)}
  
  # contando frequencia das palavras dentro do tweet
  {dados_lematizados = dados_lematizados %>% group_by(RecordID,Palavras,Polaridade)
  freq_palavra = summarize(dados_lematizados,Palavras,Polaridade,frequencia=n())
  freq_palavra = unique(freq_palavra)
  dados_lematizados = ungroup(dados_lematizados)
  dados_lematizados = group_by(dados_lematizados,RecordID)
    
  total_palavras = summarize(dados_lematizados,TotalPalavras = n())
  freq_palavra = left_join(freq_palavra,total_palavras,by='RecordID')
  freq_palavra = mutate(freq_palavra,FreqPercentual = frequencia/TotalPalavras)
    
  dados_lematizados = select(freq_palavra,-c(frequencia,TotalPalavras))
  }
  
  
  # transformando na matriz termo-documento
  
  teste_bow = pivot_wider(dados_lematizados, id_cols = c(RecordID,Polaridade), names_from = Palavras, values_from = FreqPercentual)
  
  bow = teste_bow %>% replace(is.na(.), 0)
  
  # calculando scores basicos com o lexico rotulado
  
  {names(oplexicon) = c('Palavras','Classe','Polaridade Palavra','Polaridade Revisada')
    oplexicon = select(oplexicon,c(Palavras,`Polaridade Palavra`))
    
    dados_lematizados = left_join(dados_lematizados,oplexicon,by='Palavras')
    dados_lematizados = dados_lematizados %>% replace(is.na(.), 0)
    dados_lematizados = group_by(dados_lematizados, RecordID)
    scores = summarize(dados_lematizados,score=sum(Polaridade))
    ungroup(dados_lematizados)
  }
  
  # incluindo coluna do score no Bag Of Words
  
  bow = left_join(bow,scores,by='RecordID')
  bow = select(bow,c(Polaridade,score,everything()))
  
  # removendo coluna de RecordID
  bow = ungroup(bow)
  bow = select(bow,-RecordID)
  
  
  
  # removendo objetos que não serão mais utilizados
    {rm('freq')
    rm('freq_palavra')
    rm('teste_bow')
    rm('stop_words2')
    rm('total_palavras')
    rm('lemma_dic')
    rm('oplexicon')
    rm('scores')}
  