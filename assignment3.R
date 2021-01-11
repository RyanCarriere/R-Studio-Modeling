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
textRC <- read.csv('gastext.csv',stringsAsFactors = F)
str(textRC)
#change to factor
textRC$Target <- factor(textRC$Target)
textRC$Service_flag <- factor(textRC$Service_flag)
textRC$CustType_flag <- factor(textRC$CustType_flag)
textRC$Contact_flag <- factor(textRC$Contact_flag)
textRC$new_flag <- factor(textRC$new_flag)
textRC$Choice_flag <- factor(textRC$Choice_flag)
textRC$Comp_card_flag <- factor(textRC$Comp_card_flag)
textRC$AcctType_flag <- factor(textRC$AcctType_flag)
textRC$Contact_Flag2 <- factor(textRC$Contact_Flag2)
textRC$HQ_flag <- factor(textRC$HQ_flag)
textRC$Multi_flag <- factor(textRC$Multi_flag)
textRC$NewCust_Flag <- factor(textRC$NewCust_Flag)
cols.dont.want <- c("Cust_ID")
textRC <- textRC[, ! names(textRC) %in% cols.dont.want, drop = F]

#OR - Sample code to convert multiple columns into factors: df[,3:13]<-lapply(df[,3:13],factor)
str(textRC)

#create corpus
myCorpusRC <- corpus(textRC$Comment)
summary(myCorpusRC)

# Create a dfm (UNITGRAM) data future matrix
myDfmRC <- dfm(myCorpusRC)
topfeatures(myDfmRC)

#INITIAL ANALYSIS
#frequency analysis
tstat_freqRC <- textstat_frequency(myDfmRC)
head(tstat_freqRC, 20)
#visualize most frequent terms
library(ggplot2)
myDfmRC %>%
  textstat_frequency(n = 20) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()
textplot_wordcloud(myDfmRC,max_words=100)
#-----------------------
#preprocessing
# Remove stop words and perform stemming
library(stopwords)
myDfmRC <- dfm(myCorpusRC,
               remove_punc = T,
               remove = c(stopwords("english")),
               stem = T)
dim(myDfmRC)
topfeatures(myDfmRC,50)

#remove unwanted words or symbols in this case
stopwords1 <-c('t','')
myDfmRC <- dfm_remove(myDfmRC,stopwords1)
topfeatures(myDfmRC,30)

stopwords2 <-c('get','use', '2','per','1','don','anymor', 'take', 'can', 'cant','use','get')
myDfmRC <- dfm_remove(myDfmRC,stopwords2)
topfeatures(myDfmRC,50)
dim(myDfmRC)

#frequency analysis
tstat_freqRC <- textstat_frequency(myDfmRC)
head(tstat_freqRC)
dim(myDfmRC)
#remove infrequent terms #significantly reduced dimentionality
myDfmRC<- dfm_trim(myDfmRC,min_termfreq=2, min_docfreq=2)
dim(myDfmRC)
textplot_wordcloud(myDfmRC,max_words=100)
#---------------------
#how terms are related
#price
term_priceRC <- textstat_simil(myDfmRC,
                               selection="price",
                               margin="feature",
                               method="correlation")
as.list(term_priceRC,n=5)
#service
term_serviceRC <- textstat_simil(myDfmRC,
                                 selection="servic",
                                 margin="feature",
                                 method="correlation")
as.list(term_serviceRC,n=5)


# TOPIC MODELING
library(topicmodels)
library(tidytext)

#remove shower and point
stopwords3 <-c('shower','point')
myDfmRC <- dfm_remove(myDfmRC,stopwords3)
topfeatures(myDfmRC,30)
dim(myDfmRC)

#remove all 0 rows
myDfmRC <- as.matrix(myDfmRC)
myDfmRC <-myDfmRC[which(rowSums(myDfmRC)>0),]
myDfmRC <- as.dfm(myDfmRC)
dim(myDfmRC)

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

# Document-topic probabilities
ap_documentsRC <- tidy(myLdaRC, matrix = "gamma")
ap_documentsRC

# View document-topic probabilities in a table
#how each topic is related
Lda_documentRC<-as.data.frame(myLdaRC@gamma)

#RUN MODELS
# Prepare the corpus by adding the ID and Target columns
textRC[,3:15]<-lapply(textRC[,3:15],factor)
cols.dont.want <- c("Comments")
nctextRC <- textRC[, ! names(textRC) %in% cols.dont.want, drop = F]
nctextRC <- textRC[-c(1)]
str(textRC)
str(nctextRC)

#train/valid sets made
trainIndexRC <- createDataPartition(nctextRC$Target,
                                    p=0.7,
                                    list=FALSE,
                                    times=1)
df1.trainRC <- nctextRC[-trainIndexRC,]
df1.validRC <- nctextRC[trainIndexRC,]

#DEC TREE MODEL (All only numeric/no comments)
Tree1.RC <- train(Target~.,
                  data = df1.trainRC,
                  method = 'rpart',
                  na.action = na.pass)
Tree1.RC
prp(Tree1.RC$finalModel,type=2,extra=106)

#simple dec model analysis
pred_tree1.RC <- predict(Tree1.RC,
                         newdata = df1.validRC,
                         na.action = na.pass)
confusionMatrix(pred_tree1.RC,df1.validRC$Target)

#DECISION TREE MODEL (with comments)
myDfmRC <- dfm(myCorpusRC,
               remove_punc=T,
               remove=c(stopwords('english'),stopwords1,stopwords2),
               stem=T)
myDfmRC <- dfm_trim(myDfmRC,min_termfreq=4, min_docfreq=2)
dim(myDfmRC)
myDfm_tfidfRC <- dfm_tfidf(myDfmRC)

#wvd for dimention reduction
library(quanteda.textmodels)

#combine
modelSvdRC <- textmodel_lsa(myDfm_tfidfRC, nd=8)
modelSvdRC <- as.data.frame(modelSvdRC$docs)
df2RC <- cbind(nctextRC,modelSvdRC)
trainIndex2RC <- createDataPartition(df2RC$Target,
                                     p=0.7,
                                     list=FALSE,
                                     time=1)
df2.trainRC <- df2RC[trainIndex2RC,]
df2.validRC <- df2RC[-trainIndex2RC,]
Tree2.RC <- train(Target~.,
                  data=df2.trainRC,
                  method='rpart',
                  na.action=na.pass)
Tree2.RC
prp(Tree2.RC$finalModel,type=2,extra=106)
pred_tree2.RC <- predict(Tree2.RC,
                         newdata = df2.validRC,
                         na.action = na.pass)
confusionMatrix(pred_tree2.RC, df2.validRC$Target)