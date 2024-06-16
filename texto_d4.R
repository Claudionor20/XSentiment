# Importando pacotes
library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)


# Lendo base
load("dados_rotulados.rda")

# Tirando colunas desnecessárias
dados <- dados|>
  dplyr::select(text,Polaridade)

corpous <- VCorpus(VectorSource(dados$text))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('portuguese'))
corpus <- tm_map(corpus, stripWhitespace)

# Fazendo tokenização
dados <- tidytext::unnest_tokens(dados, text, text)

# Lendo arquivo de stopwords
stopwords <- read.delim("https://raw.githubusercontent.com/thiagoscouto/stopwords_ptbr/master/stopwords_ptbr.txt",header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
names(stopwords) <- "text"

# Acrescentando stopwords 
novas_stopwords <- c("tá","tô","eh","né","oq","viu","in","ah","agr","vc", "vcs", "tb", "tmb", "tbm", "blz", "pq", "q", "qq", "kd", "blz", "blza", 
                     "bora", "mano", "mto", "muito", "aff", "poxa", "po", "tipo", "aí", "véi", 
                     "fala", "galera", "pessoal", "curti", "curte", "post", "posts", "segue", "seguindo",
                     "segue-me", "vou", "vai", "tá", "to", "tô", "d+", "tudibom", "msg", "msgs", 
                     "lol", "kkk", "rs", "rss", "rsrs", "rsrsrs", "n", "nao", "não", "eh", "ehh", 
                     "uhul", "uhull", "oba", "eai", "eae", "e aí", "foda", "fod@", "fodao", 
                     "vc", "vcs", "pf", "pff", "pls", "qnd", "qdo", "flw", "vlw", "valeu", "beleza", 
                     "ok", "okay", "nunca", "sempre", "hoje", "amanha", "ontem", "hj", "msm", "mimimi","br")

# Verificando diferenças
unicas_stopwords <- setdiff(novas_stopwords, stopwords$text)
novas_stopwords1 <- data.frame(text = unicas_stopwords)


# Acrescentando stopwords
stopwords <- rbind(stopwords, novas_stopwords1)
                   
# Removendo stopwords
dados <- anti_join(dados,stopwords, by = "text")

# Removendo a palavra anitta (Tem em todas)
dados <- dados|>
  dplyr::filter(text != "anitta")

# Fazendo gráfico de palavras mais frequentes
dados|>
  filter(Polaridade == 1)|>
  count(text, sort = TRUE)|>
  slice(1:40)|>
  ggplot(aes(x = reorder(text,n), y = n))+
  geom_col()+
  coord_flip()+
  labs(title = "Palavras mais frequentes",
       x = "Palavras",
       y = "Frequência")+
  theme_minimal()




