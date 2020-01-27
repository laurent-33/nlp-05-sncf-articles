library(koRpus)
library(data.table)
#install.koRpus.lang(lang=c("en","fr"))
library(koRpus.lang.en)
library(koRpus.lang.fr)
library(tm)
library(dplyr)
library()

available.koRpus.lang()

set.kRp.env(TT.cmd="manual", TT.options=list(path="C://TreeTagger",
                                             tokenizer = "utf8-tokenize.perl",
                                             params = "french-utf8.par",
                                             abbrev = "french-abbreviations", preset = "fr"), lang = "fr")

treetag("c'est la fête", format="obj")

data <- read.csv("panorama-presse-sncf.csv",
                 encoding="UTF-8",
                 sep=";",
                 comment.char="#",
                 stringsAsFactors=FALSE)

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

data <- select(data, -na_columns)

fulltext <- data[5, "FullText"]

result <- treetag(fulltext, format = "obj", stopwords=stopwords("fr"))
result <- kRp.filter.wclass(result, "nonpunct")
result <- kRp.filter.wclass(result, "stopword")
result

summary(result)
class(result)

result <- table(result[, "lemma"])

result <- result %>%
  as.data.frame() %>%
  arrange(desc(Freq))

result <- result[which(result$Var1 != "@card@" & result$Var1 != "<unknown>"),]
barplot(result[1:10,2], names.arg = result[1:10,1])

