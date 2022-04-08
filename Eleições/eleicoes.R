#Hi, I'm Jober, this code is part of my Data Science portfolio.
#You can check out my analysis at joberconde.medium.com
#My email is joberconde@gmail.com
library(rtweet)
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(stopwords)
library(stylo)
library(lexiconPT)
library(ggbreak)
library(wordcloud)
library(syuzhet)

setwd('~/Desktop/twitter') # set working directory

####################getting data###################################

sampleText = get_timeline('lulaoficial', # getting the last 3200 tweets
                         n = 3200, # starts twitter API
                         parse = T)

####################cleaning data##################################

sampleText$strippedText = gsub('http.*','',  sampleText$text) # removes links
sampleText$strippedText = gsub('https.*','', sampleText$strippedText) #removes links
sampleText$strippedText = gsub('[^[:graph:]]', ' ', sampleText$strippedText) # removes graphic characters like emoticons 
sampleText$strippedText = gsub('[[:cntrl:]]', '', sampleText$strippedText) # removes control characters
sampleText$strippedText = gsub('\\w*[0-9]+\\w*\\s*', '', sampleText$strippedText) # removes numbers


sampleTextClean = sampleText %>% # separates words and eliminates ponctuation
  select(strippedText) %>%
  unnest_tokens(word, strippedText)

customStopWords = stopwords('pt') # defines stopwords

customStopWords = rbind(customStopWords,'é', 'y', 'la','el','en', # adds more stopwords
                        'lula','lulaoficial', 'equipelula', 'ricardo', # lula
                        'stuckert', 'the', 'foto', 'ricardostuckert')

customStopWords = rbind(customStopWords,'é', 'r', 'govbr', 'tarcisiogdf', # bolsonaro
                        'secomvc', 'jair', 'govbrazil', 'bolsonaro',
                        'mdregional_br','jairbolsonaro')

cleanedTweetWords = as.data.frame(delete.stop.words(unlist(sampleTextClean),# deletes stopwords from the sample
                                     stop.words = customStopWords))[[1]]

#################plots the top 15 words#####################################

  as.data.frame(cbind('word' = cleanedTweetWords)) %>%
  count(word,
        sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word,
                        n)) %>%
  ggplot(aes(x = word,
             y = n,
             fill = word)) +
  geom_col(show.legend = F) +
  xlab(NULL) +
  coord_flip() +
  labs(x = 'Palavra',
       y = 'Contagem',
       title = "Contagem de palavras únicas - @jairbolsonaro") +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold'))

ggsave('top15.pdf', # exporting plot
       width = 7,
       height = 3.5,
       units = "in")

#########################tweets frequency##################################

sampleText %>%
  select(created_at) %>%
  as.data.frame(row.names = F) %>%
  format(format = '%Y-%m-%d') %>%
  unlist(use.names = F) %>%
  sort() %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(x = as.Date(.), y = Freq)) +
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_labels = '%Y-%b')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = 'bold')) +
  labs(x = 'Data',
       y = 'Tweets', 
       title = "Frequência de tweets - @jairbolsonaro")

ggsave('freq.pdf', # exporting plot
       width = 7,
       height = 3.5,
       units = "in")

sampleText %>% # finding the peak
  select(created_at) %>%
  as.data.frame(row.names = F) %>%
  format(format = '%Y-%m-%d') %>%
  unlist(use.names = F) %>%
  sort() %>%
  table() %>%
  as.data.frame() %>%
  top_n(1, Freq)

#########################correlation between words#########################

sampleText %>% # preparing text
  select(strippedText) %>%
  unnest_tokens(pairedWords, strippedText, token = 'ngrams', n = 2) %>%
  separate(pairedWords, c('word1', 'word2'), sep = ' ') %>%
  filter(!word1 %in% customStopWords) %>% # filters stopwords
  filter(!word2 %in% customStopWords) %>%
  drop_na() %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>% # plots
  ggraph(layout = 'fr') +
  geom_node_point(color = 'darkslategray4',
                  size = 3) +
  geom_node_text(aes(label = name),
                 vjust = 1.8,
                 size = 3) +
  labs(x = '',
       y = '', 
       title = "Palavras correlacionadas - @jairbolsonaro") +
  theme(plot.title = element_text(face = 'bold'))

ggsave('corr.pdf', # exporting plot
       width = 7,
       height = 3.5,
       units = "in")

#################sentiment analysis###################################

oplex = oplexicon_v3.0 # copies the sentimental lexicon
sentilex = sentiLex_lem_PT02

sampleToken = sampleText %>% # creates tokens
unnest_tokens(output = 'term',
              input = 'text')

sampleSentiments = reduce(.x = list(sampleToken, # binds data
                              oplex,
                              select(sentilex,
                                     term,
                                     lex_polarity = polarity)),
  .f = inner_join,
  by = 'term') %>%
  group_by(status_id) %>% # groups by tweet id
  summarise(tweetOplex = sum(polarity), # gets results by tweet
            tweetSentilex = sum(lex_polarity)) %>%
  ungroup()

sampleSentimentsByDate = inner_join(sampleText, # binds data
                                sampleSentiments,
                                by = 'status_id') %>%
  filter(tweetOplex != 0) %>%
  count(sentiment = if_else(tweetOplex < 0, # daily positive and negative tweets count
                            'negativo',
                            'positivo'),
        date = as.Date(created_at)) %>%
  as.data.frame() %>%
  pivot_wider(id_cols = 'date',
              names_from = 'sentiment',
              values_from = 'n') %>%
  mutate(sentimento = positivo - negativo) # calculates polarity

sampleSentimentsByDate[is.na(sampleSentimentsByDate)] = 0

ggplot(sampleSentimentsByDate, aes(x = date, # plots
                             y = sentimento,
                             fill = ..y.. >= 0.5)) +
  geom_bar(stat='identity') +
  scale_x_date(date_labels='%b %y',
               date_breaks  ='1 month') +
  geom_hline(yintercept=0,
             linetype='dashed', 
             color = 'gray',
             size = 0.1) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(name = 'Sentimento',
                      labels = c('Negativo',
                                 'Positivo',
                                 '')) +
  labs(x = 'Data',
       y = 'Escala de Sentimentos', 
       title = 'Análise de sentimentos - @jairbolsonaro') +
  theme(plot.title = element_text(face = 'bold')) #+
  scale_y_break(c(10,35)) # it needs to be changed when necessary

ggsave('sent.pdf', # exporting plot
       width = 7,
       height = 3.5,
       units = "in")

######################word cloud#######################################

myList = cleanedTweetWords %>% # organize data
  unlist() %>%
  table() %>%
  as.data.frame()

set.seed(1234) # for reproducibility

pdf('cloud.pdf', # opens device
    width = 7,
    height = 3.5)

wordcloud(words = myList$., # creates the cloud
          freq = myList$Freq,
          min.freq = 20,
          max.words=200,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8, 'Dark2'))

dev.off() # finishes device
