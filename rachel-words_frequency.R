data <- panorama.presse.sncf


library(koRpus)
library("koRpus.lang.fr")


set.kRp.env(TT.cmd="manual", lang="fr", TT.options=list(path="C:/TreeTagger",
                                                        preset="fr"))


data$FullText[5]

article5 = data$FullText[5]
summary(treetag(file = article5, format = "obj"))


library(udpipe)
article5 <- udpipe(x = article5,
                     object = "french")
article5 <- article5$lemma
article5


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


docs <- Corpus(VectorSource(article5))
#consulter le contenu du doc
#inspect(docs)


#Transformation du text : Remplacer “/”, “@” et “|” avec un espace
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Nettoyage du texte
# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("french"))
# Supprimer votre propre liste de mots non désirés
docs <- tm_map(docs, removeWords, c("être", "afin")) 
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


# Construire la matrice des mots : table contenant la fréquence des mots
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


# Générer le nuage de mots
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Explorer les mots fréquents ainsi que leurs associations
findFreqTerms(dtm, lowfreq = 5)

findAssocs(dtm, terms = "SNCF", corlimit = 0.3)

#Dessiner la fréquence des mots
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
