#Ryan Carriere

install.packages('quanteda') 
library(quanteda)
install.packages('rpart')
install.packages('rpart.plot')
library(pROC)
library(rpart)
library(rpart.plot)
install.packages("caret")
library(caret)

setwd('C:/Users/grays/Desktop/BusinessAnalytics')
textRC <- read.csv('MOVIEDATA.csv',stringsAsFactors = F)
str(textRC)


#create corpus
myCorpusRC <- corpus(textRC$Synopsis)
summary(myCorpusRC)

# Create a dfm (UNITGRAM) data future matrix
myDfmRC <- dfm(myCorpusRC)
topfeatures(myDfmRC)



# Remove stop words and perform stemming
library(stopwords)
myDfmRC <- dfm(myCorpusRC,
               remove_punc = T,
               remove = c(stopwords("english")),
               stem = T)
dim(myDfmRC)
topfeatures(myDfmRC,25)


#remove unwanted words or symbols in this case
stopwords1 <-c("film","movi","one","like","time","play","charact","make","s","get","stori","scene","even","show","can","just","good","two","much","take","love","life","way","come","work")

myDfmRC <- dfm_remove(myDfmRC,stopwords1)
topfeatures(myDfmRC,25)

#stopwords2
stopwords2 <-c("film", "movi", "play", "even", "just", "go", "get", "like", "time", "make", "charact", "scene", "show", "1","2", "year", "come", "may","john")

myDfmRC <- dfm_remove(myDfmRC,stopwords2)
topfeatures(myDfmRC,25)


#create a wordcloud

tstat_freqRC <- textstat_frequency(myDfmRC)
head(tstat_freqRC)
dim(myDfmRC)

textplot_wordcloud(myDfmRC,max_words=150)


#delete infrequent words
myDfmRC<- dfm_trim(myDfmRC,min_termfreq=15, min_docfreq=5)
dim(myDfmRC)

#school
term_schoolRC <- textstat_simil(myDfmRC,
                                selection="school",
                                margin="feature",
                                method="correlation")
as.list(term_schoolRC,n=8)


#Movie91
term_movieRC <- textstat_simil(myDfmRC,
                                selection="batman & robin",
                                margin="feature",
                                method="correlation")
as.list(term_movieRC,n=8)


#----------------------
# TOPIC MODELING 

library(topicmodels)
library(tidytext)


#explore LDA_VEM topic model with 4 topics
myLdaRC <- LDA(myDfmRC,k=4,control=list(seed=50)) 
myLdaRC


# Term-topic probabilities
myLda_tdRC <- tidy(myLdaRC)
myLda_tdRC


# Visulize most common terms in each topic
library(ggplot2)
library(dplyr)

top_terms <- myLda_tdRC %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# View topic 8 terms in each topic
Lda_termRC<-as.matrix(terms(myLdaRC,8))
View(Lda_termRC)
