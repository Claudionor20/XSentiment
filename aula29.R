#Carregar bnco de dados
load(tweets_nik.rda)
dados<-dplyr::select(tweets_nik,text)

#criando coluna identificador
dados = mutate(dados, RecordID = c(1:length(dados$text)))


#Tokenizacao dos textos
#primeiro transformamos o texto em vetor de palavras
dados_tk<- tidytext::unnest_tokens(dados,output=Palavras,input=text)

#Remover espaços
dados_tk$Palavras =  stringr::str_squish(dados_tk$Palavras)

#Stop words
#Carregar biblioteca de stop words
stop_words = read.delim(file="https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
names(stop_words) = c('Palavras')

#Ha um problema: ela possui espaco nas palavras.
#Removendo espaços em branco da lista importada
for (i in 1:dim(stop_words)[1]){
  stop_words[i,1]=trimws(stop_words[i,1])}

#Removendo stop words  
dados_sw = dplyr::anti_join(dados_tk, stop_words,by="Palavras")

#Contando palavras
head(dplyr::count(dados_sw,Palavras, sort = TRUE))

#lematizacao
#carregando dicionario
lemma_dic <- read.delim(file = "https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE, encoding = 'UTF-8')
names(lemma_dic) <- c("stem", "Palavras")

#lematizando (pode haver aumento de palavras)
dados_lem = dplyr::left_join(dados_sw, lemma_dic, 
                                     by='Palavras')
dados_lem$Palavras = ifelse(is.na(dados_lem$stem),
                                    dados_lem$Palavras,dados_lem$stem)
dados_lem = dplyr::select(dados_lem,-stem)

#Contando a frequencia das palavras
dados_lem = group_by(dados_lem,Palavras)
freq = arrange(summarize(dados_lem, frequencia = n()),
               desc(frequencia))
dados_lem = left_join(dados_lem,freq,'Palavras')
dados_lem = ungroup(dados_lem)

#Visualizando nuvens de palavras
library(dplyr)
dados_lem %>%
count(Palavras) %>%
with(wordcloud::wordcloud(Palavras, n, max.words = 50))
  
# filtrando palavras com a frequencia minima
freq_min <- 10
dados_min<-dplyr::filter(dados_lem,
                        frequencia>=freq_min)

# contando frequencia das palavras dentro do tweet
dados_freq = group_by(dados_min,RecordID,Palavras)
freq_palavra = summarize(dados_freq,Palavras,frequencia=n())
freq_palavra = unique(freq_palavra)
dados_freq = ungroup(dados_freq)
dados_freq = group_by(dados_lem,RecordID)
  
total_palavras = summarize(dados_freq,TotalPalavras = n())
freq_palavra = left_join(freq_palavra,total_palavras,by='RecordID')
freq_palavra = mutate(freq_palavra,FreqPercentual = frequencia/TotalPalavras)
  
dados_freq = select(freq_palavra,-c(frequencia,TotalPalavras))



#transformando na matriz termo-documento

matriz_ftd<-tidyr::pivot_wider(dados_freq, id_cols = c(RecordID), names_from = Palavras, values_from = FreqPercentual)

matriz_ftd<-replace(matriz_ftd,is.na(matriz_ftd), 0)

##Realizar analise de sentimento
#carregando dicionario lexico
oplexicon = lexiconPT::oplexicon_v3.0
names(oplexicon) = c('Palavras','Classe','Polaridade Palavra','Polaridade Revisada')

#Vamos usar o dados_lem, pois possui o tweet
#prox do seu formato inicial (sem cortes por freq)
dados_sent = dplyr::left_join(dados_lem,oplexicon,by='Palavras')
dados_sent$`Polaridade Palavra` = replace(dados_sent$`Polaridade Palavra`,is.na(dados_sent$`Polaridade Palavra`), 0)
dados_sent = dplyr::group_by(dados_sent, RecordID)
scores = dplyr::summarize(dados_sent,score=sum(`Polaridade Palavra`))

#Incluindo coluna do score na matrix
#termo-documento por frequncia (Bag of Words)
matriz_ftd = dplyr::left_join(matriz_ftd,scores,by='RecordID')
matriz_ftd = select(matriz_ftd,c(Polaridade,score,everything()))






