##### Test case ngram

setwd("C:/Users/stank/Documents/Jaar 4 uni/Thesis in philosophy")

rm(list = ls())

source("Code/Packages.R")
source("Code/Thesisggtheme.R")


# Set the size of the ngrams
n = 2

# Will take around 15 second
source("Code/ngramPreparation.R")


# Before 1687
ngramSeperatedBefore1687 = Corpusngram %>% 
  filter(year < 1687) %>%
  separate(ngram, c("word1", "word2"), sep = " ")

ngramFilterBefore1687 = ngramSeperatedBefore1687 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word2 == "philosophy")


TotalForceB1687 = ngramFilterBefore1687 %>% 
  count(author, word2)

NewtonianForceBefore1687 =  ngramFilterBefore1687 %>%
  filter(word1 == "experimental") %>%
  count(author, word2)


NewtonianForceBefore1687


frequseBefore = tibble(author = TotalForceB1687$author,
                 frequse = rep(0, nrow(TotalForceB1687)))

for (i in 1:nrow(TotalForceB1687)) {
  if (frequseBefore[i, 1] %in% NewtonianForceBefore1687$author) {
    idx = which(NewtonianForceBefore1687$author == frequseBefore$author[i])
    frequseBefore[i, 2] = (NewtonianForceBefore1687[idx, "n"] / TotalForceB1687[i, "n"])
  }
}




# After 1687

ngramSeperatedAfter1687 = Corpusngram %>% 
  filter(year > 1687) %>%
  separate(ngram, c("word1", "word2"), sep = " ")

ngramFilterAfter1687 = ngramSeperatedAfter1687 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word2 == "philosophy")


TotalForceA1687 = ngramFilterAfter1687 %>%
  count(author, word2, sort = TRUE)

NewtonianForceA1687 = ngramFilterAfter1687 %>%
  filter(word1 == "experimental") %>%
  group_by(author) %>%
  count(word2)

frequse = tibble(author = TotalForceA1687$author,
                 frequse = rep(0, nrow(TotalForceA1687)))

for (i in 1:nrow(TotalForceA1687)) {
  if (frequse[i, 1] %in% NewtonianForceA1687$author) {
    idx = which(NewtonianForceA1687$author == frequse$author[i])
    frequse[i, 2] = (NewtonianForceA1687[idx, "n"] / TotalForceA1687[i, "n"])
  }
}




NewtonianForceOverTime = 
  tibble(year = rep(0, nrow(TotalForceB1687) + nrow(frequse)),
         frequentUse = rep(0, nrow(TotalForceB1687) + nrow(frequse)))

for (i in 1:nrow(TotalForceB1687)) {
  NewtonianForceOverTime[i, 1] = 
    as.integer(str_sub(frequseBefore[i, 1], -4, -1))
  NewtonianForceOverTime[i, 2] = 
    frequseBefore[i, 2]
}

for (i in 1:nrow(frequse)) {
  NewtonianForceOverTime[i + nrow(TotalForceB1687), 1] = 
    as.integer(str_sub(frequse[i, 1], -4, -1))
  NewtonianForceOverTime[i + nrow(TotalForceB1687), 2] = 
    frequse[i, 2]
}


summary(lm(frequentUse ~ year, data = NewtonianForceOverTime))


ggplot(NewtonianForceOverTime, aes(x = year, y = frequentUse)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  ThesisggTheme() +
  labs(x = 'Year', y = 'Term frequency', 
       title = 'Empiricist concept of philosophy over time') +
  geom_vline(xintercept = 1687, linetype = 'dashed', color = 'red', size = 1.5) +
  geom_curve(aes(x = 1670, y = 0.8, xend = 1686, yend = 0.79), color = 'grey20',
             angle = 90, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_textbox(aes(x = 1665, y = 0.89), label = 'Publishing of \nthe Principa', 
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
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())
