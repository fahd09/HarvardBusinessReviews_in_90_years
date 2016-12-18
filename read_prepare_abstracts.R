

library(ggplot2)
library(tm)
library(lsa)
library(ExPosition)
library(reshape2)


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

docs = Corpus(VectorSource(hbr$abstract))

tospace <- (function(x, pattern) gsub(pattern, " ", x))

docs = tm_map(docs, content_transformer(tolower))                       # convert all chars to lower-case
docs = tm_map(docs, content_transformer(tospace), '(f|ht)tp\\S+\\s*')   # remove web adresses
docs = tm_map(docs, content_transformer(tospace), "[/\\]")              # convert all useless slashes to spaces.
docs = tm_map(docs, content_transformer(removeNumbers))                 # remove numbers
docs = tm_map(docs, content_transformer(tospace), "[^[:alnum:]///' ]")  # convert all non-alphabets to spaces
docs = tm_map(docs, content_transformer(tospace), "[']")                # convert ' to spaces
docs = tm_map(docs, content_transformer(removeWords), stopwords_en)             # remove junk words

#docs = tm_map(docs, stemDocument)                             # stem document
docs = tm_map(docs, content_transformer(stripWhitespace))     # stripe white spaces

dtm <- DocumentTermMatrix(docs) # or TermDocumentMatrix
dtm <- removeSparseTerms(dtm,0.9987) # remove words that only appear in *10900*(1-0.9987)* studies. i.e. 15 studies

dtm.mat <- as.matrix(dtm)

words.list <- colnames(dtm.mat)
term.freq <- colSums(dtm.mat) # term frequency

# Frequency plot
term.freq.df <- data.frame(term=names(term.freq), freq=term.freq) # for ggplot
ggplot(data=term.freq.df, aes(log10(freq))) + geom_histogram(bins = 30) +
  labs(title = "Term Frequency", x= "Log_10 (frequency)", y="Count")

# We want to know word freq. by year
year_nominal <- makeNominalData(data.frame(as.factor(format(hbr$date, '%Y'))))
colnames(year_nominal) <- substr(colnames(year_nominal), 35,40)

# Now we have:
# dtm           ==> 12697 (articles)  x 4673  (words)
# year_nominal  ==> 12697 (articles)  x 89    (years)

# we want: 
# word_year     ==> 4673  (words) x 89 (years) # gives counts of each word per year!

word_year <- t(dtm.mat) %*% year_nominal

head(word_year)

yrs.sorted <- order(as.numeric(colnames(word_year)))
word_year <- word_year[,yrs.sorted]
word_year[word_year==0] <- NA       # we convert all zeros to NAs 
                                    # so that we get rid of multiple 
                                    # "zero" records in next step

# using reshape2::melt() function to convert from matrix to long format
word_year.long <- melt(word_year, value.name = "count", variable.name = "year", na.rm = TRUE)
names(word_year.long) <- c("terms", "year", "count")

head(word_year.long)

#





