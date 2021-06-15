### In this script we investigate the relevance of the corpus #############

setwd("C:/Users/stank/Documents/Jaar 4 uni/Thesis in philosophy")

source("Code/Packages.R")

# Will take around 10 seconds
source("Code/CorpusPreparation.R")


FamousWords = Corpus %>%
  count(word, sort = TRUE)
FamousWords

# Now removing the stop words

NewStopWords = bind_rows(stop_words, 
                         tibble(word = c("o??", "??ame", "??o", "the??e", "??uch", "tho??e")))

CleanFamousWords = Corpus %>% 
  anti_join(NewStopWords) %>%
  count(word, sort = TRUE)
CleanFamousWords

# The word force used a lot, excluding the stop words. 

set.seed(4)
CleanFamousWords %>%
  with(wordcloud(word, n, max.words = 30, scale = c(4,.5)))


WordsCountedPerAuthor = Corpus %>%
  anti_join(NewStopWords) %>%
  count(author, word, sort = TRUE)
WordsCountedPerAuthor




# Lets consider the tf_idf 
# aka the term frequency-inverse document frequency


CorpusCount = Corpus %>%
  count(author, word, sort = TRUE)

Corpustfidf = CorpusCount %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf))
Corpustfidf


# Lets check the idf of force
Corpustfidf %>% 
  filter(word == "force")
# This indicates that all documents contain the word force

Corpustfidf %>% filter(word == "gravity")
# The same does not hold for the gravity. 

CorpusAfter1687 = Corpus %>%
  filter(year > 1687)
  
CorpusCount = CorpusAfter1687 %>% 
  count(author, word, sort = TRUE)

Corpustfidf = CorpusCount %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf))
Corpustfidf %>% filter(word == "gravity")
#But we do find it in all works after 1687



