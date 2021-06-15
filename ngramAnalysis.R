### In this script, we perform the ngram analysis #########################

setwd("C:/Users/stank/Documents/Jaar 4 uni/Thesis in philosophy")

rm(list = ls())

source("Code/Packages.R")
source("Code/Thesisggtheme.R")


# Set the size of the ngrams
n = 2

# Will take around 15 second
source("Code/ngramPreparation.R")

Countngram = Corpusngram %>%
  count(ngram, sort = TRUE)
Countngram

# These words are not interesting, we should remove the stop words

NewStopWords = bind_rows(stop_words, 
                         tibble(word = c("o??", "??ame", "??o", "the??e", "??uch", "tho??e")))

# Might take a minute
ngramSeperated = Corpusngram %>%
  separate(ngram, c("word1", "word2"), sep = " ")
ngramFilter = ngramSeperated %>%
  filter(!word1 %in% NewStopWords$word) %>%
  filter(!word2 %in% NewStopWords$word)

Countngram = ngramFilter %>%
  count(word1, word2, sort = TRUE)
Countngram

# Lets check force

set.seed(2)
NetworkGraph = Countngram %>%
  filter(word1 == "force" | word2 == "force" | word1 == "forces" | word2 == "forces") %>%
  filter(n > 40) %>% 
  graph_from_data_frame()

a = grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(NetworkGraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), size = 6.5, vjust = 1, hjust = 0.5)




# Lets check the development of this over time


# Before 1687
ngramSeperatedBefore1687 = Corpusngram %>% 
  filter(year < 1687) %>%
  separate(ngram, c("word1", "word2"), sep = " ")

ngramFilterBefore1687 = ngramSeperatedBefore1687 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word2 == "force")


TotalForceB1687 = ngramFilterBefore1687 %>% 
  count(author, word2)

NewtonianForceB1687 =  ngramFilterBefore1687 %>%
  filter(word1 == "absolute" | 
           word1 == "contractive" | 
           word1 == "accelerating" | 
           word1 == "centripetal" | 
           word1 == "gravitating") %>%
  count(author, word2)


NonNewtonianForceB1687 =  ngramFilterBefore1687 %>%
  filter(word1 == "external" | 
           word1 == "disturbing" |
           word1 == "di??turbing") %>%
  count(author, word2)


NewtonianForceB1687
#No author before 1687 used these concepts

NonNewtonianForceB1687
#Boyle used Non-Newtonian concepts



frequseBefore = tibble(author = TotalForceB1687$author,
                 frequse = rep(0, nrow(TotalForceB1687)))

for (i in 1:nrow(TotalForceB1687)) {
  if (frequseBefore[i, 1] %in% NonNewtonianForceB1687$author) {
    idx = which(NonNewtonianForceB1687$author == frequseBefore$author[i])
    frequseBefore[i, 2] = -(NonNewtonianForceB1687[idx, "n"] / TotalForceB1687[i, "n"])
  }
}

frequseBefore
#Seems to work


### After 1687

ngramSeperatedAfter1687 = Corpusngram %>% 
  filter(year > 1687) %>%
  separate(ngram, c("word1", "word2"), sep = " ")

ngramFilterAfter1687 = ngramSeperatedAfter1687 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word2 == "force")


TotalForceA1687 = ngramFilterAfter1687 %>%
  count(author, word2, sort = TRUE)

NewtonianForceA1687 = ngramFilterAfter1687 %>%
  filter(word1 == "absolute" | 
           word1 == "contractive" | 
           word1 == "accelerating" | 
           word1 == "centripetal" | 
           word1 == "gravitating") %>%
  group_by(author) %>%
  count(word2)


NonNewtonianForceA1687 =  ngramFilterAfter1687 %>%
  filter(word1 == "external" |
          word1 == "disturbing" |
           word1 == "di??turbing") %>%
  count(author, word2)

frequseAfter = tibble(author = TotalForceA1687$author,
                 frequse = rep(0, nrow(TotalForceA1687)))

for (i in 1:nrow(TotalForceA1687)) {
  if (frequseAfter[i, 1] %in% NewtonianForceA1687$author) {
    idx = which(NewtonianForceA1687$author == frequseAfter$author[i])
    frequseAfter[i, 2] = (NewtonianForceA1687[idx, "n"] / TotalForceA1687[i, "n"])
  }
  if (frequseAfter[i, 1] %in% NonNewtonianForceA1687$author) {
    idx = which(NonNewtonianForceA1687$author == frequseAfter$author[i])
    frequseAfter[i, 2] = frequseAfter[i, 2] - 
      (NonNewtonianForceA1687[idx, "n"] / TotalForceA1687[i, "n"])
  }
  
}




NewtonianForceOverTime = 
  tibble(year = rep(0, nrow(TotalForceB1687) + nrow(frequseAfter)),
         frequentUse = rep(0, nrow(TotalForceB1687) + nrow(frequseAfter)))

for (i in 1:nrow(TotalForceB1687)) {
  NewtonianForceOverTime[i, 1] = 
    as.integer(str_sub(TotalForceB1687[i, 1], -4, -1))
  NewtonianForceOverTime[i, 2] = 
    frequseBefore[i, 2]
}

for (i in 1:nrow(frequseAfter)) {
  NewtonianForceOverTime[i + nrow(TotalForceB1687), 1] = 
    as.integer(str_sub(frequseAfter[i, 1], -4, -1))
  NewtonianForceOverTime[i + nrow(TotalForceB1687), 2] = 
    frequseAfter[i, 2]
}


summary(lm(frequentUse ~ year, data = NewtonianForceOverTime[8:43,]))


ggplot(NewtonianForceOverTime[8:43,], aes(x = year, y = frequentUse)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  ThesisggTheme() +
  labs(x = 'Year', y = 'Term frequency', 
       title = 'Newtonian concept of force over time') +
  geom_vline(xintercept = 1687, linetype = 'dashed', color = 'red', size = 1.5) +
  geom_curve(aes(x = 1670, y = 0.5, xend = 1686, yend = 0.49), color = 'grey20',
             angle = 90, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_textbox(aes(x = 1665, y = 0.51), label = 'Publishing of \nthe Principa', 
               color = 'grey20', size = 7) +
  theme(plot.title = element_text('sigmar', color = 'grey10', size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16, color = 'grey10'),
        axis.title.y = element_text(size = 16, color = 'grey10')) +
  coord_cartesian(xlim = c(1653, 1825), clip = 'off')

NewtonianForceOverTime = NewtonianForceOverTime %>%
  arrange(year)


AbsoluteResultsOverTime = tibble(absolute = 1:nrow(NewtonianForceOverTime),
                                 frequentUse = NewtonianForceOverTime$frequentUse)

ggplot(AbsoluteResultsOverTime, aes(x = absolute, y = frequentUse)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ThesisggTheme() +
  labs(x = "", y = 'Term frequency', 
       title = 'Absolute results of the Newtonian concept of force over time') +
  geom_vline(xintercept = 7.5, linetype = 'dashed', color = 'red', size = 1.5) +
  geom_curve(aes(x = 5, y = 0.43, xend = 7.4, yend = 0.38), color = 'grey20',
             angle = 90, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_textbox(aes(x = 3, y = 0.55), label = 'Publishing of \nthe Principa', 
               color = 'grey20', size = 7) +
  theme(plot.title = element_text('sigmar', color = 'grey10', size = 20),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16, color = 'grey10'),
        axis.title.y = element_text(size = 16, color = 'grey10')) 
  


### Circle diagram

AfterPrincia = NewtonianForceOverTime[8:43,]

dfcirkel = tibble(View = c("Newtonian", "Neutral", "Non-newtonian"),
                  Amount =  c(sum(AfterPrincia$frequentUse > 0),
                  NonNewtonian = sum(AfterPrincia$frequentUse == 0),
                  Neutral = sum(AfterPrincia$frequentUse < 0)))


cirkeldiagram = ggplot(dfcirkel, aes(x = "", y = Amount, fill = View)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = paste0(Amount)), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  ggtitle("Pie chart of the number of authors per philosophical view")
cirkeldiagram


