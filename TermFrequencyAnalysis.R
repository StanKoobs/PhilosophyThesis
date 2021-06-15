### In this script we analyze the term frequency over time ################

setwd("C:/Users/stank/Documents/Jaar 4 uni/Thesis in philosophy")

source("Code/Packages.R")
source("Code/Thesisggtheme.R")

# Will take around 10 seconds
source("Code/CorpusPreparation.R")

CorpusCount = Corpus %>%
  count(author, word, sort = TRUE)

Corpustfidf = CorpusCount %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf))
Corpustfidf

# What word do you want to investigate?
wordtoinvestigate = "experiments"

corpustf = Corpustfidf %>% 
  filter(word == wordtoinvestigate)%>%
  arrange(desc(tf))
corpustf

WordOverTime = tibble(year = rep(0, nrow(corpustf)), 
                       tf = rep(1, nrow(corpustf)))

for (i in 1:nrow(corpustf)) {
  WordOverTime[i, 1] = as.integer(str_sub(corpustf[i, 1], -4, -1))
  WordOverTime[i, 2] = corpustf[i, "tf"]
}


summary(lm(tf ~ year, data = WordOverTime))

Forcetfplot = ggplot(WordOverTime, aes(x = year, y = tf)) +
  geom_point(size = 1.7) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  ThesisggTheme() +
  labs(x = 'Year', y = 'Term frequency', 
       title = 'The use of "experiments" over time') +
  geom_vline(xintercept = 1687, linetype = 'dashed', color = 'red', size = 1.5) +
  geom_curve(aes(x = 1670, y = 0.0049, xend = 1686, yend = 0.0047), color = 'grey20',
             angle = 90, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_textbox(aes(x = 1665, y = 0.0055), label = 'Publishing of \nthe Principa', 
            color = 'grey20', size = 7) +
  theme(plot.title = element_text('sigmar', color = 'grey10', size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16, color = 'grey10'),
        axis.title.y = element_text(size = 16, color = 'grey10')) +
  coord_cartesian(xlim = c(1653, 1825), ylim = c(0, 0.006), clip = 'off')

Forcetfplot


plot_grid(Forcetfplot, Gravitytfplot)



# Absolute results

WordOverTime = WordOverTime %>%
  arrange(year)

AbsoluteResultsOverTime = tibble(absolute = 1:nrow(WordOverTime),
                                 frequentUse = WordOverTime$tf)


ggplot(AbsoluteResultsOverTime, aes(x = absolute, y = frequentUse)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ThesisggTheme() +
  labs(x = "", y = 'Term frequency', 
       title = 'Absolute results of the Newtonian concept of force over time') +
  geom_vline(xintercept = 7.5, linetype = 'dashed', color = 'red', size = 1.5) +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())


### Test of statistical difference

wordToTest = "force"

Corpusbefore1687 = Corpus %>%
  filter(year < 1687) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(word, author, n) %>%
  filter(word == wordToTest)

tfBefore1687 = Corpusbefore1687$tf
tfBefore1687


Corpusafter1687 = Corpus %>%
  filter(year > 1687) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(word, author, n) %>%
  filter(word == wordToTest)

tfafter1687 = Corpusafter1687$tf
tfafter1687


t.test(tfBefore1687, tfafter1687)
