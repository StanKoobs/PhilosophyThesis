### In this script we load in the corpus as single words ##################

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

Digby1644 <- tibble(text = read_file("NewCorpus/1644Digby.txt"))
Corpus <- tokenizeDocument(Digby1644)

Comenius1651 <- tibble(text = read_file("NewCorpus/1651Comenius.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Comenius1651))

#Boyle1661 <- tibble(text = read_file("NewCorpus/1661Boyle.txt"))
#Corpus <- bind_rows(Corpus, tokenizeDocument(Boyle1661))

Boyle1663 <- tibble(text = read_file("NewCorpus/1663Boyle.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Boyle1663))

Boyle1666 <- tibble(text = read_file("NewCorpus/1666Boyle.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Boyle1666))

Cavendish1666 <- tibble(text = read_file("NewCorpus/1666Cavendish.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Cavendish1666))

Cavendish1668 <- tibble(text = read_file("NewCorpus/1668Cavendish.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Cavendish1668))

LeGrand1694 <- tibble(text = read_file("NewCorpus/1694LeGrand.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(LeGrand1694))

Ditton1705 <- tibble(text = read_file("NewCorpus/1705Ditton.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Ditton1705))

Stirling1717 <- tibble(text = read_file("NewCorpus/1717Stirling.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Stirling1717))

Greene1727 <- tibble(text = read_file("NewCorpus/1727Greene.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Greene1727))

Pemberton1728 <- tibble(text = read_file("NewCorpus/1728Pemberton.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Pemberton1728))

Worster1730 <- tibble(text = read_file("NewCorpus/1730Worster.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Worster1730))

Banks1733 <- tibble(text = read_file("NewCorpus/1733Banks.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Banks1733))

Desaguliers1734 <- tibble(text = read_file("NewCorpus/1734Desaguliers.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Desaguliers1734))

Horsley1743 <- tibble(text = read_file("NewCorpus/1743Horsley.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Horsley1743))

Wlyde1743 <- tibble(text = read_file("NewCorpus/1743Wlyde.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Wlyde1743))

Robertson1745 <- tibble(text = read_file("NewCorpus/1745Robertson.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Robertson1745))

Knight1748 <- tibble(text = read_file("NewCorpus/1748Knight.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Knight1748))

Rutherforth1748 <- tibble(text = read_file("NewCorpus/1748Rutherforth.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Rutherforth1748))

Emerson1754 <- tibble(text = read_file("NewCorpus/1754Emerson.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Emerson1754))

Wilson1754 <- tibble(text = read_file("NewCorpus/1754Wilson.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Wilson1754))

Rowning1758 <- tibble(text = read_file("NewCorpus/1758Rowning.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Rowning1758))

Jones1762 <- tibble(text = read_file("NewCorpus/1762Jones.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Jones1762))

Ferguson1772 <- tibble(text = read_file("NewCorpus/1772Ferguson.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Ferguson1772))

Hamilton1774 <- tibble(text = read_file("NewCorpus/1774Hamilton.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Hamilton1774))

Lovett1774 <- tibble(text = read_file("NewCorpus/1774Lovett.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Lovett1774))

Maclaurin1775 <- tibble(text = read_file("NewCorpus/1775Maclaurin.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Maclaurin1775))

Atwood1776 <- tibble(text = read_file("NewCorpus/1776Atwood.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Atwood1776))

Cullen1777 <- tibble(text = read_file("NewCorpus/1777Cullen.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Cullen1777))

Atkinson1784 <- tibble(text = read_file("NewCorpus/1784Atkinson.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Atkinson1784))

Robinson1784 <- tibble(text = read_file("NewCorpus/1784Robinson.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Robinson1784))

Fenning1786 <- tibble(text = read_file("NewCorpus/1786Fenning.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Fenning1786))

Peart1789 <- tibble(text = read_file("NewCorpus/1789Peart.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Peart1789))

Adams1794 <- tibble(text = read_file("NewCorpus/1794Adams.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Adams1794))

Telescope1794 <- tibble(text = read_file("NewCorpus/1794Telescope.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Telescope1794))

Walker1795 <- tibble(text = read_file("NewCorpus/1795Walker.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Walker1795))

#Anderson1798 <- tibble(text = read_file("NewCorpus/1798Anderson.txt"))
#Corpus <- bind_rows(Corpus, tokenizeDocument(Anderson1798))

#Gregory1798 <- tibble(text = read_file("NewCorpus/1798GregoryVol2.txt"))
#Gregory1798complete <- tokenizeDocument(Gregory1798)

#Gregory1798 <- tibble(text = read_file("NewCorpus/1798GregoryVol3.txt"))
#Gregory1798complete <- bind_rows(Gregory1798complete, tokenizeDocument(Gregory1798))
#Corpus <- bind_rows(Corpus, Gregory1798complete)

Wood1803 <- tibble(text = read_file("NewCorpus/1803Wood.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Wood1803))

Robison1804 <- tibble(text = read_file("NewCorpus/1804Robison.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Robison1804))

Young1807 <- tibble(text = read_file("NewCorpus/1807Young.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Young1807))

Playfair1812 <- tibble(text = read_file("NewCorpus/1812Playfair.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Playfair1812))

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol1.txt"))
Robison1822complete <- tokenizeDocument(Robison1822)

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol2.txt"))
Robison1822complete <- bind_rows(Robison1822complete, tokenizeDocument(Robison1822))

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol3.txt"))
Robison1822complete <- bind_rows(Robison1822complete, tokenizeDocument(Robison1822))

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol4.txt"))
Robison1822complete <- bind_rows(Robison1822complete, tokenizeDocument(Robison1822))
Corpus <- bind_rows(Corpus, Robison1822complete)
