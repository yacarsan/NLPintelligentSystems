library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library("readr")
library("utf8")
library(udpipe)
library(tidyverse)
library(tibble)
library(dplyr)

#data <- readlines('War.txt',header = TRUE)
urlWar <- "https://raw.githubusercontent.com/yacarsan/NLPintelligentSystems/main/War.txt"
urlLove <- "https://raw.githubusercontent.com/yacarsan/NLPintelligentSystems/main/Love.txt"
urlHappiness <- "https://raw.githubusercontent.com/yacarsan/NLPintelligentSystems/main/Happiness.txt"

#Feeling analysis of all the datasets
plotUrl(urlWar,"Feelings analysis of War poems")
plotUrl(urlLove,"Feelings analysis of Love poems")
plotUrl(urlHappiness,"Feelings analysis of Hapiness poems")

warDataset <- readLines(urlWar, encoding="UTF-8")

warDataset[!utf8_valid(warDataset)]
Encoding(warDataset) <- "UTF-8"
warDataset <- iconv(warDataset, "UTF-8", "UTF-8",sub='')

model <- udpipe_download_model(language = "english-ewt") #Alternative: "english-gum", "english-lines", "english-partut"
udmodel_war <- udpipe_load_model(file = model$file_model)
anno <- udpipe_annotate(udmodel_war,x = warDataset)
df <- as.data.frame(anno)
df
analisis <- as_tibble(anno)
analisis

analisis %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col() +
  coord_flip()

analisis %>%
  filter(upos == "NOUN") %>%
  count(token, sort = T) %>%
  mutate(token = reorder(token, n)) %>%
  top_n(30) %>%
  ggplot(aes(token, n)) +
  geom_col(fill = "darkred") +
  coord_flip()

analisis %>%
  filter(upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "darkred") +
  coord_flip()

#Function is charge of the feeling analysis of the datasets
plotUrl <- function(url,title){
  # Read the dataset
  dataset <- readLines(url, encoding="UTF-8")
  
  #test if utf8 is valid and delete the values that aren�t valid
  dataset[!utf8_valid(dataset)]
  Encoding(dataset) <- "UTF-8"
  dataset <- iconv(dataset, "UTF-8", "UTF-8",sub='')
  
  feelings_df <-get_nrc_sentiment(dataset,lang="english")
  
  barplot(
    colSums(prop.table(feelings_df[, 1:8])),
    space = 0.20,
    horiz = FALSE,
    las = 1,
    cex.names = 0.6,
    col = brewer.pal(n = 10, name = "Set3"),
    main = title,
    xlab="emotions", ylab = NULL)
  return(feelings_df)
}