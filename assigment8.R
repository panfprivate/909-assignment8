##install required packages
install.packages("tm")
install.packages("tm.corpus.Reuters21578")
install.packages("lda")
install.packages("topicmodels")
install.packages("SnowballC")
require(tm.corpus.Reuters21578)
require(lda)
require(topicmodels)
require(SnowballC)
require(proxy)

data(Reuters21578)
rt <- Reuters21578

##Separate data set
tra_query <- "LEWISSPLIT == 'TRAIN'"
test_query <- "LEWISSPLIT == 'TEST'"
tra_rt <- tm_filter(rt,FUN = sFilter, tra_query)
test_rt <- tm_filter(rt,FUN = sFilter, test_query)

##sets with TOPICS values of YES, NO, and BYPASS
yes<-tm_filter(rt, FUN=sFilter, "TOPICS == 'YES'")
no<-tm_filter(rt, FUN=sFilter, "TOPICS == 'NO'")
bypass<-tm_filter(rt, FUN=sFilter, "TOPICS == 'BYPASS'")

##Pre-processing
rt<-tm_map(rt,removeNumbers)
rt<-tm_map(rt,removePunctuation)
rt<-tm_map(rt,tolower)
rt<-tm_map(rt,removeWords,stopwords("english"))
rt<-tm_map(rt,removeWords,c("and","for","from","its","said","reuter","said.","that","the","was","will","with"))
rt<-tm_map(rt,stemDocument)
rt<-tm_map(rt,stripWhitespace)

##get document-term matrix for the whole data
dtm<-DocumentTermMatrix(rt)
##remove sparse terms
dtm2<-removeSparseTerms(dtm,0.99)

##find the sum of words in each document
rowTotals <- apply(dtm2,1,sum)
##pick the document with more than one word
dtm3 <- dtm2[rowTotals>0,]  

##several LDA models with different algorithms
vem<-LDA(dtm3,method="VEM",control=list(alpha=0.1),k=10) ##VEM
gib<-LDA(dtm3,method="Gibbs",control=list(alpha=0.1),k=10) ##Gibbsg
ctm<-CTM(dtm3,k=10) ##CTM

##indicates the most likely terms for each topcis
vem_term <- terms(vem)
gib_term <- terms(gib)
##indicates the most likely topics for each document
vem_topic <- topics(vem)
gib_topic <- topics(gib)

##Classification
class=c("earn","acq","money-fx","grain","crude",
        "trade","interest","ship","wheat","corn")
##­NaiveBayes(dtm[class]~.,dtm)

##Clustering
dist_dtm <- dist(scale(dtm3)) ##HAC
hcluster <-hclust(dist_dtm,method="average")

km <- kmeans(norm.tdm,10) ##K-means
