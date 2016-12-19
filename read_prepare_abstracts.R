rm(list=ls())

library(ggplot2)
library(tm)
library(lsa)
library(ExPosition)
library(reshape2)
library(dplyr)

hbr <- read.csv('./data/HBR Citations_correct_abstracts.csv', strip.white=TRUE, as.is=TRUE)


########################################### 
# Starter code from: 
# https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/forums/t/2396/very-first-steps-in-r

# confirm that there's always at most one of
# ABSTRACT or AUTHOR.SUPPLIED.ABSTRACT
all(hbr$ABSTRACT=="" | hbr$AUTHOR.SUPPLIED.ABSTRACT=="")

# and put it in place
hbr$abstract <- ifelse(hbr$ABSTRACT!="",hbr$ABSTRACT,hbr$AUTHOR.SUPPLIED.ABSTRACT)
hbr$abstract_type <- ifelse(hbr$ABSTRACT!="","HBR",
                            ifelse(hbr$AUTHOR.SUPPLIED.ABSTRACT!="","author",
                                   "none"))
hbr$ABSTRACT. <- NULL
hbr$AUTHOR.SUPPLIED.ABSTRACT. <- NULL

# fix up the dates
# without this you incorrectly get results like year 2068, etc...
hbr$dm <- substr(hbr$SYSTEM..PUB.DATE,1,5)
hbr$y <- substr(hbr$SYSTEM..PUB.DATE,7,9)
hbr$dmY <- ifelse(as.numeric(hbr$y)>20,
                  paste(hbr$dm,"-19",hbr$y,sep=""),
                  paste(hbr$dm,"-20",hbr$y,sep=""))
hbr$date <- as.Date(hbr$dmY,format="%d-%b-%Y")
hbr$dm <- NULL
hbr$y <- NULL
hbr$dmY <-NULL
hbr$SYSTEM..PUB.DATE <- NULL

###########################################

# compute article frequency grouped by year

article_freq <- aggregate(hbr$ARTICLE.TITLE, by = list(year = as.factor(format(hbr$date, "%Y"))), FUN = length)

# visualize
ggplot(data=article_freq, aes(x=year, y=x)) + geom_point()

# drop records of 1st and last year ?

hbr <- hbr[-which(format(hbr$date, "%Y") %in% c(1922,2012)),]

# Group records from before 1975 into 5-year units ?

# Text-mining.. 

# we will need author names so we delete them from abstracts
# Author_names <- c(hbr$AUTHOR.1.LAST.NAME, hbr$AUTHOR.2.LAST.NAME,hbr$AUTHOR.3.LAST.NAME)
# Author_names<-Author_names[-which(Author_names=="")]
# Author_names<-tolower(Author_names)
# Author_names<-gsub('[^[:alnum:]///]', '', Author_names)
# Author_names<-unique(Author_names)

docs = Corpus(VectorSource(hbr$abstract))

tospace <- (function(x, pattern) gsub(pattern, " ", x))

docs = tm_map(docs, content_transformer(tolower))                       # convert all chars to lower-case
docs = tm_map(docs, content_transformer(tospace), '(f|ht)tp\\S+\\s*')   # remove web adresses
docs = tm_map(docs, content_transformer(tospace), "[/\\]")              # convert all useless slashes to spaces.
docs = tm_map(docs, content_transformer(removeNumbers))                 # remove numbers
docs = tm_map(docs, content_transformer(tospace), "[^[:alnum:]///' ]")  # convert all non-alphabets to spaces
docs = tm_map(docs, content_transformer(tospace), "[']")                # convert ' to spaces
docs = tm_map(docs, content_transformer(removeWords), stopwords_en)             # remove junk words
to_delete_words <- c("article","discussed", "author")
docs = tm_map(docs, content_transformer(removeWords), to_delete_words)             # remove junk words
# docs = tm_map(docs, content_transformer(removeWords), Author_names)             # remove author names from abstrac
#docs = tm_map(docs, stemDocument)                             # stem document
docs = tm_map(docs, content_transformer(stripWhitespace))     # stripe white spaces

dtm <- DocumentTermMatrix(docs) # or TermDocumentMatrix
dtm <- removeSparseTerms(dtm,0.999) # 

dtm.mat <- as.matrix(dtm)

words.list <- colnames(dtm.mat)
term.freq <- colSums(dtm.mat) # term frequency

# Frequency plot
term.freq.df <- data.frame(term=names(term.freq), freq=term.freq) # for ggplot
ggplot(data=term.freq.df, aes(log10(freq))) + geom_histogram(bins = 30) +
  labs(title = "Term Frequency", x= "Log_10 (frequency)", y="Count")


# we will use 4-year running interval

years.list <- as.numeric(unique(format(hbr$date, '%Y')))
years.grp <- 
  cut(as.numeric(format(hbr$date, '%Y')), 
    breaks=seq.int(1923, 2011, 4),
    include.lowest=T, ordered_result = F)
levels(years.grp) <- gsub(',', '-', substr(levels(years.grp), 2,10)) # better names

# We want to know word freq. by year

year_nominal <- makeNominalData(data.frame(as.factor(format(hbr$date, '%Y'))))
colnames(year_nominal) <- substr(colnames(year_nominal), 35,40)

# We want to know word freq by 'year group'
# year_nominal <- makeNominalData(data.frame(years.grp))
# colnames(year_nominal) <- levels(years.grp)

# Now we have:
# dtm           ==> 12697 (articles)  x 18376  (words)
# year_nominal  ==> 12697 (articles)  x 89    (years)

# we want: 
# word_year     ==> 4673  (words) x 89 (years) # gives counts of each word per year!

word_year <- t(dtm.mat) %*% year_nominal 

# head(word_year)

# No need for this when using the 4-year window
yrs.sorted <- order(as.numeric(colnames(word_year)))
word_year <- word_year[,yrs.sorted]
#word_year[word_year==0] <- NA       # we convert all zeros to NAs 
                                    # so that we get rid of multiple 
                                    # "zero" records in next step

# popular word per year

pop.cnt <- apply(word_year, 2, function(x) {return( max(x) )})                                   # return maximum "count"
pop.wrd <- as.character(apply(word_year, 2, function(x) {return( names(which(x==max(x)))[1] )})) # return word

pop_word_year <- data.frame(year=colnames(word_year), word=pop.wrd,count=pop.cnt)


# using reshape2::melt() function to convert from matrix to long format
# word_year.long <- melt(word_year, 
#                        value.name = "count", 
#                        variable.name = "year", 
#                        na.rm = TRUE)
# names(word_year.long) <- c("terms", "year", "count")
# 
# head(word_year.long) # nice format; but too many records!

res.ca <- epCA(t(word_year), graphs = F)

# First, we give a wight to each word according to its "temporal" importance
weight <- data.frame(word = row.names(res.ca$ExPosition.Data$fj) , 
                     wt   = as.vector(res.ca$ExPosition.Data$dj))
top25 <- weight$wt > quantile(weight$wt, .75)

# then visualize those "selected" ones..

df1<-
  data.frame(res.ca$ExPosition.Data$fi) %>% 
  select(num_range("X", 1:4)) %>%
  mutate(year= row.names(res.ca$ExPosition.Data$fi)) #%>%
#  filter(year < 1950)
df2<-
  data.frame(res.ca$ExPosition.Data$fj[top25,]) %>% 
  select(num_range("X", 1:4)) %>%
  mutate(word= row.names(res.ca$ExPosition.Data$fj)[top25]) %>%
  mutate(wt  = as.vector(res.ca$ExPosition.Data$dj)[top25])

g1<-ggplot(data=df1, aes(x=X1, y=X2, label=year)) + geom_text(color="red") + geom_path()
g2<-g1+geom_text(data=df2, aes(x=X1, y=X2, label=word, size=wt), check_overlap = T)
g2<- g2 + labs(title="Correspondence Analysis of HBR corpus over 90 years",
               x="Component 1", y="Component 2") + theme_void() + theme(legend.position="none")
g2

clus<-hclust(dist(res.ca$ExPosition.Data$fj[top25,1:5]), method = "ward.D2")
memb <- cutree(clus, k = 4)

g1<-ggplot(data=df1, aes(x=X1, y=X2, label=year)) +  geom_text(color="blue", size=10, check_overlap = T)# + geom_path()
g2<-g1+geom_text(data=df2, aes(x=X1, y=X2, label=word, size=wt), check_overlap = T, color=memb)
g2<- g2 + labs(title="Term map of HBR corpus over 90 years",
               x="Component 1", y="Component 2") + theme_void() + theme(legend.position="none")
g2

ggsave('./term_map.png', g2)

