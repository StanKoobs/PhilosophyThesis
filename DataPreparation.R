### IN this script we load in the data ####################################

rm(list = ls())

setwd("C:/Users/stank/Documents/Jaar 4 uni/Thesis in philosophy")

source("Code/Packages.R")

tokenizeDocument <- function(document) {
  name = as.character(rlang::enexpr(document))
  document = document %>%
    unnest_tokens(word, text)
  document = document %>% 
    mutate(author = name, year = str_sub(name, -4, -1))
  return(document)
}


LeGrand1694 <- tibble(text = read_file("NewCorpus/1694LeGrand.txt"))
Corpus <- tokenizeDocument(LeGrand1694)
Ditton1705 <- tibble(text = read_file("NewCorpus/1705Ditton.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Ditton1705))
Stirling1717 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1717Stirling.txt")))
Greene1727 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1727Greene.txt")))
Pemberton1728 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1728Pemberton.txt")))
Worster1730 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1730Worster.txt")))
Banks1733 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1733Banks.txt")))
Horsley1743 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1743Horsley.txt")))
Wlyde1743 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1743Wlyde.txt")))
Robertson1745 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1745Robertson.txt")))
Knight1748 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1748Knight.txt")))
Rutherforth1748 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1748Rutherforth.txt")))
Emerson1754 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1754Emerson.txt")))
Wilson1754 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1754Wilson.txt")))
Rowning1758 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1758Rowning.txt")))
Jones1762 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1762Jones.txt")))
Ferguson1772 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1772Ferguson.txt")))
Arden1774 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1774Arden.txt")))
Hamilton1774 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1774Hamilton.txt")))
Lovett1774 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1774Lovett.txt")))
Maclaurin1775 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1775Maclaurin.txt")))
Atwood1776 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1776Atwood.txt")))
Cullen1777 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1777Cullen.txt")))
Atkinson1784 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1784Atkinson.txt")))
Robinson1784 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1784Robinson.txt")))
Fenning1786 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1786Fenning.txt")))
Peart1789 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1789Peart.txt")))
Adams1794 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1794Adams.txt")))
Telescope1794 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1794Telescope.txt")))
Walker1795 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1795Walker.txt")))
Anderson1798 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1798Anderson.txt")))
Gregory1798Vol2 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1798GregoryVol2.txt")))
Gregory1798Vol3 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1798GregoryVol3.txt")))
Wood1803 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1803Wood.txt")))
Robison1804 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1804Robison.txt")))
Young1807 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1807Young.txt")))
Playfair1812 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1812Playfair.txt")))
Robison1822Vol1 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1822RobisonVol1.txt")))
Robison1822Vol2 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1822RobisonVol2.txt")))
Robison1822Vol3 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1822RobisonVol3.txt")))
Robison1822Vol4 <- tokenizeDocument(tibble(text = read_file("NewCorpus/1822RobisonVol4.txt")))



Corpus = LeGrand1694 %>% mutate(author = "LeGrand1694")

inner_join(Ditton1705, Corpus)



for (doc in mget(ls())) {
  doc = tokenizeDocument(doc)
}


x = 2
tokenizeDocument(x)


tibDitton1705 = tibble(text = Ditton1705)

tokenizedDitton = Ditton1705 %>% 
  unnest_tokens(word, text)


#data("stop_words")

#tokenizedDitton = tokenizedDitton %>%
#  anti_join(stop_words)

tokenizedDitton %>%
  count(word, sort = TRUE)


tokenizedDitton %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 30))


get_sentiments("nrc")


nrc_joy = get_sentiments("nrc") %>%
  filter(sentiment == "joy")


tokenizedDitton %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)



library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")


hoi = function(x) {
  print(as.character(rlang::enexpr(x)))
}

doei =3
hoi(doei)
hoi(3)

