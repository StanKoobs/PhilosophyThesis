### In this script we load in the Corpusngramas ngrams ##################

setwd("C:/Users/stank/Documents/Jaar 4 uni/Thesis in philosophy")

source("Code/Packages.R")

tokenizeDocument <- function(document, ngramSize) {
  name = as.character(rlang::enexpr(document))
  document = document %>%
    unnest_tokens(ngram, text, token = "ngrams", n = ngramSize)
  document = document %>% 
    mutate(author = name, year = str_sub(name, -4, -1))
  return(document)
}

Digby1644 <- tibble(text = read_file("NewCorpus/1644Digby.txt"))
Corpus <- tokenizeDocument(Digby1644, n)

Comenius1651 <- tibble(text = read_file("NewCorpus/1651Comenius.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Comenius1651, n))

Boyle1661 <- tibble(text = read_file("NewCorpus/1661Boyle.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Boyle1661, n))

Boyle1663 <- tibble(text = read_file("NewCorpus/1663Boyle.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Boyle1663, n))

Boyle1666 <- tibble(text = read_file("NewCorpus/1666Boyle.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Boyle1666, n))

Cavendish1666 <- tibble(text = read_file("NewCorpus/1666Cavendish.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Cavendish1666, n))

Cavendish1668 <- tibble(text = read_file("NewCorpus/1668Cavendish.txt"))
Corpus <- bind_rows(Corpus, tokenizeDocument(Cavendish1668, n))

LeGrand1694 <- tibble(text = read_file("NewCorpus/1694LeGrand.txt"))
Corpusngram <- bind_rows(Corpus, tokenizeDocument(LeGrand1694, n))

Ditton1705 <- tibble(text = read_file("NewCorpus/1705Ditton.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Ditton1705, n))

Stirling1717 <- tibble(text = read_file("NewCorpus/1717Stirling.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Stirling1717, n))

Greene1727 <- tibble(text = read_file("NewCorpus/1727Greene.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Greene1727, n))

Pemberton1728 <- tibble(text = read_file("NewCorpus/1728Pemberton.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Pemberton1728, n))

Worster1730 <- tibble(text = read_file("NewCorpus/1730Worster.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Worster1730, n))

Banks1733 <- tibble(text = read_file("NewCorpus/1733Banks.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Banks1733, n))

Horsley1743 <- tibble(text = read_file("NewCorpus/1743Horsley.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Horsley1743, n))

Wlyde1743 <- tibble(text = read_file("NewCorpus/1743Wlyde.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Wlyde1743, n))

Robertson1745 <- tibble(text = read_file("NewCorpus/1745Robertson.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Robertson1745, n))

Knight1748 <- tibble(text = read_file("NewCorpus/1748Knight.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Knight1748, n))

Rutherforth1748 <- tibble(text = read_file("NewCorpus/1748Rutherforth.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Rutherforth1748, n))

Emerson1754 <- tibble(text = read_file("NewCorpus/1754Emerson.txt"))
Corpusngram<- bind_rows(Corpusngram, tokenizeDocument(Emerson1754, n))

Wilson1754 <- tibble(text = read_file("NewCorpus/1754Wilson.txt"))
Corpusngram<- bind_rows(Corpusngram, tokenizeDocument(Wilson1754, n))

Rowning1758 <- tibble(text = read_file("NewCorpus/1758Rowning.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Rowning1758, n))

Jones1762 <- tibble(text = read_file("NewCorpus/1762Jones.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Jones1762, n))

Ferguson1772 <- tibble(text = read_file("NewCorpus/1772Ferguson.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Ferguson1772, n))

Hamilton1774 <- tibble(text = read_file("NewCorpus/1774Hamilton.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Hamilton1774, n))

Lovett1774 <- tibble(text = read_file("NewCorpus/1774Lovett.txt"))
Corpusngram<- bind_rows(Corpusngram, tokenizeDocument(Lovett1774, n))

Maclaurin1775 <- tibble(text = read_file("NewCorpus/1775Maclaurin.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Maclaurin1775, n))

Atwood1776 <- tibble(text = read_file("NewCorpus/1776Atwood.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Atwood1776, n))

Cullen1777 <- tibble(text = read_file("NewCorpus/1777Cullen.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Cullen1777, n))

Atkinson1784 <- tibble(text = read_file("NewCorpus/1784Atkinson.txt"))
Corpusngram<- bind_rows(Corpusngram, tokenizeDocument(Atkinson1784, n))

Robinson1784 <- tibble(text = read_file("NewCorpus/1784Robinson.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Robinson1784, n))

Fenning1786 <- tibble(text = read_file("NewCorpus/1786Fenning.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Fenning1786, n))

Peart1789 <- tibble(text = read_file("NewCorpus/1789Peart.txt"))
Corpusngram<- bind_rows(Corpusngram, tokenizeDocument(Peart1789, n))

Adams1794 <- tibble(text = read_file("NewCorpus/1794Adams.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Adams1794, n))

Telescope1794 <- tibble(text = read_file("NewCorpus/1794Telescope.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Telescope1794, n))

Walker1795 <- tibble(text = read_file("NewCorpus/1795Walker.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Walker1795, n))

Anderson1798 <- tibble(text = read_file("NewCorpus/1798Anderson.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Anderson1798, n))

Gregory1798 <- tibble(text = read_file("NewCorpus/1798GregoryVol2.txt"))
Gregory1798complete <- tokenizeDocument(Gregory1798, n)

Gregory1798 <- tibble(text = read_file("NewCorpus/1798GregoryVol3.txt"))
Gregory1798complete <- bind_rows(Gregory1798complete, tokenizeDocument(Gregory1798, n))
Corpusngram <- bind_rows(Corpusngram, Gregory1798complete)

Wood1803 <- tibble(text = read_file("NewCorpus/1803Wood.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Wood1803, n))

Robison1804 <- tibble(text = read_file("NewCorpus/1804Robison.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Robison1804, n))

Young1807 <- tibble(text = read_file("NewCorpus/1807Young.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Young1807, n))

Playfair1812 <- tibble(text = read_file("NewCorpus/1812Playfair.txt"))
Corpusngram <- bind_rows(Corpusngram, tokenizeDocument(Playfair1812, n))

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol1.txt"))
Robison1822complete <- tokenizeDocument(Robison1822, n)

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol2.txt"))
Robison1822complete <- bind_rows(Robison1822complete, tokenizeDocument(Robison1822, n))

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol3.txt"))
Robison1822complete <- bind_rows(Robison1822complete, tokenizeDocument(Robison1822, n))

Robison1822 <- tibble(text = read_file("NewCorpus/1822RobisonVol4.txt"))
Robison1822complete <- bind_rows(Robison1822complete, tokenizeDocument(Robison1822, n))
Corpusngram <- bind_rows(Corpusngram, Robison1822complete)


