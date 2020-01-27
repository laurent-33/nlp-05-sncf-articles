library(koRpus)
library(data.table)
#install.koRpus.lang(lang=c("en","fr"))
library(koRpus.lang.en)
library(koRpus.lang.fr)
library(tm)
library(dplyr)
# library(remotes)
# remotes::install_github("colinfay/rfeel")
library(rfeel)
library(ggplot2)
library(plotly)
library(proustr)

data <- fread("panorama-presse-sncf.csv", encoding="UTF-8")

# Récupération des articles en français
data <- data %>%
  subset(Langue == 'French') %>% 
  subset(!is.na(FullText)) %>% 
  subset(SupportType == 'Article de Presse') %>% 
  subset(Country = 'FRANCE')

# Suppressoin des colonnes avec que des NA ou une seule valeur
na_columns = c("Langue", "SupportType", "Country",
               "TweetType", "NLA_ObjectID", "RUBRIQUE_Parent_ID",
               "Nombre_de_Tweet", "Media", "Nombre_d_Abonnements",
               "TweetId", "RUBRIQUE_Parent", "href", "TypeArticle",
               "Code_client", "Client", "Nombre_de_Follower")

summary(data)

data <- select(data, -na_columns)

# Récupération du text
fulltext <- data[5, "FullText"][[1]]

set.kRp.env(TT.cmd="manual", TT.options=list(path="C://TreeTagger",
                                             tokenizer = "utf8-tokenize.perl",
                                             params = "french-utf8.par",
                                             abbrev = "french-abbreviations", preset = "fr"), lang = "fr")

# Analyse de l'article
result <- fulltext %>%
  treetag(format = "obj", stopwords=stopwords("fr")) %>%
  kRp.filter.wclass("nonpunct") %>%
  kRp.filter.wclass("stopword")
  
# Extraction des lemmes
result <- result@TT.res$lemma

# Suppression des mots non reconnus et des nombres
result <- result %>%
  data.frame(word = .) %>%
  filter(., word != "<unknown>", word != "@card@")

words_number <- nrow(result)
result
        
# Calcul du score de polarité
polarity <- merge(result, rfeel("polarity")) %>%
  select(polarity) %>%
  table()
  
polarity <- round((polarity["positive"] - polarity["negative"]) / words_number
                  , 1)
polarity

# Calcul des scores émotionnels
feelings <- merge(result, rfeel("score")) %>%
  table()

feelings <- colSums(feelings) / words_number
feelings <- as.data.frame(t(feelings))
feelings$neutral <- 1 - sum(feelings)
feelings

percent <- feelings / sum(feelings)
percent

pie(as.table(t(percent)), labels = names(percent), col=hcl.colors(ncol(percent)))

df_rfeel_polarity <- rfeel("polarity")
df_rfeel_score <- rfeel("score")

data_nlp <- data[,c("FullText", "HitTitle", "Keywords")]

data_nlp$anger <- NA
data_nlp$disgust <- NA
data_nlp$fear <- NA
data_nlp$joy <- NA
data_nlp$sadness <- NA
data_nlp$surprise <- NA
data_nlp$polarity <- NA

# analyse de 100 articles
for (i in 1:10){
  tagged_article <- treetag(data_nlp[i, "FullText"][[1]], format="obj")
  tagged_article <- kRp.filter.wclass(tagged_article, "stopword")
  
  lemma_article <- tagged_article@TT.res$lemma
  df_lemma_article <- as.data.frame(lemma_article)%>% 
    filter(., lemma_article !="@card@") %>% 
    filter(., lemma_article !="<unknown>")
  
  colnames(df_lemma_article)<-"word"
  count_positive <- sum((merge(df_lemma_article, df_rfeel_polarity))$polarity == "positive")
  count_negative <- sum((merge(df_lemma_article, df_rfeel_polarity))$polarity == "negative")
  total <- nrow(df_lemma_article)
  
  data_nlp$polarity[i] <- (count_positive - count_negative) / total
  data_nlp[i,4] <- (round((table((merge(df_lemma_article, df_rfeel_score))$sentiment)/total)*100, 2))["anger"]
  data_nlp[i,5] <- (round((table((merge(df_lemma_article, df_rfeel_score))$sentiment)/total)*100, 2))["disgust"]
  data_nlp[i,6] <- (round((table((merge(df_lemma_article, df_rfeel_score))$sentiment)/total)*100, 2))["fear"]
  data_nlp[i,7] <- (round((table((merge(df_lemma_article, df_rfeel_score))$sentiment)/total)*100, 2))["joy"]
  data_nlp[i,8] <- (round((table((merge(df_lemma_article, df_rfeel_score))$sentiment)/total)*100, 2))["sadness"]
  data_nlp[i,9] <- (round((table((merge(df_lemma_article, df_rfeel_score))$sentiment)/total)*100, 2))["surprise"]
}

data_analysed <- data_nlp
data_analysed$anger[is.na(data_analysed$anger)] <- 0
data_analysed$disgust[is.na(data_analysed$disgust)] <- 0
data_analysed$fear[is.na(data_analysed$fear)] <- 0
data_analysed$joy[is.na(data_analysed$joy)] <- 0
data_analysed$sadness[is.na(data_analysed$sadness)] <- 0
data_analysed$surprise[is.na(data_analysed$surprise)] <- 0

summary(data_analysed)

# Rendu visuel
y <- c(data_nlp$HitTitle[1], data_nlp$HitTitle[2], data_nlp$HitTitle[3], data_nlp$HitTitle[4], data_nlp$HitTitle[5], data_nlp$HitTitle[6])
x1 <- c(data_nlp$anger[1], data_nlp$disgust[1], data_nlp$fear[1], data_nlp$joy[1], data_nlp$sadness[1], data_nlp$surprise[1] )
x2 <- c(data_nlp$anger[2], data_nlp$disgust[2], data_nlp$fear[2], data_nlp$joy[2], data_nlp$sadness[2], data_nlp$surprise[2] )
x3 <- c(data_nlp$anger[3], data_nlp$disgust[3], data_nlp$fear[3], data_nlp$joy[3], data_nlp$sadness[3], data_nlp$surprise[3] )
x4 <- c(data_nlp$anger[4], data_nlp$disgust[4], data_nlp$fear[4], data_nlp$joy[4], data_nlp$sadness[4], data_nlp$surprise[4] )
x5 <- c(data_nlp$anger[7], data_nlp$disgust[7], data_nlp$fear[7], data_nlp$joy[7], data_nlp$sadness[7], data_nlp$surprise[7] )

data <- data.frame(y, x1, x2, x3, x4, x5)

top_labels <- c('anger', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'polarity')

p <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
             marker = list(color = 'rgba(38, 24, 74, 0.8)',
                           line = list(color = 'rgb(248, 248, 249)', width = 2))) %>%
  add_trace(x = ~x2, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
  add_trace(x = ~x3, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
  add_trace(x = ~x4, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
  add_trace(x = ~x5, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
  layout(xaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
         margin = list(l = 120, r = 10, t = 140, b = 80),
         showlegend = FALSE) %>%
  # labeling the y-axis
  add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right') %>%
  # labeling the percentages of each bar (x_axis)
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 / 2, y = y,
                  text = paste(data[,"x1"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 / 2, y = y,
                  text = paste(data[,"x2"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 / 2, y = y,
                  text = paste(data[,"x3"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 / 2, y = y,
                  text = paste(data[,"x4"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                  text = paste(data[,"x5"], '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  # labeling the first Likert scale (on the top)
  add_annotations(xref = 'x', yref = 'paper',
                  x = c(21 / 2, 21 + 30 / 2, 21 + 30 + 21 / 2, 21 + 30 + 21 + 16 / 2,
                        21 + 30 + 21 + 16 + 12 / 2),
                  y = 1.15,
                  text = top_labels,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE)

p

