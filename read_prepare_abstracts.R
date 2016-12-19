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

# hbr <- hbr[-which(format(hbr$date, "%Y") %in% c(1922,2012)),]

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
docs = tm_map(docs, content_transformer(tospace), "[/\\]")              # convert all useless slashes to spaces.
docs = tm_map(docs, content_transformer(removeNumbers))                 # remove numbers
docs = tm_map(docs, content_transformer(tospace), "[^[:alnum:]///' ]")  # convert all non-alphabets to spaces
docs = tm_map(docs, content_transformer(tospace), "[']")                # convert ' to spaces
docs = tm_map(docs, content_transformer(removeWords), stopwords('en'))  # stopwords_en)             # remove junk words

# should we delete those words? planning government international
to_delete_words <- strsplit('furthermore post september women january march june july november december october april august february hbr accounting period planning government international discussed companies company author authors article articles experience businessmen describe describes examine examines method details analysts analyst methods noted mentioned journal harvard regarding refers job excerpt benefits relations plans focuses act related pertaining recent able form techniques hard little basic michael types specific day comments manage manages manager managers means makes provides argue argues instead pay pays david current professor rate rates ability look looks common self robert john provide level line example a about above across after again against all almost alone along already also although always among an and another any anybody anyone anything anywhere are area areas around as ask asked asking asks at away b back backed backing backs be became because become becomes been before began behind being beings best better between big both but by c came can cannot case cases certain certainly clear clearly come could d did differ different differently do does done down downed downing downs during e each early either end ended ending ends enough even evenly ever every everybody everyone everything everywhere f face faces far felt few find finds first for four from full fully further furthered furthering furthers g gave general generally get gets give given gives go going goods got group grouped grouping groups h had has have having he her here herself high higher highest him himself his how however i if in interest interests into is it its itself j just k keep keeps kind knew know known knows l large largely last later latest least less let lets like likely long longer longest m made make making man many may me member members men might more most mostly mr mrs much must my myself n necessary need needed needing needs never new newer newest next no nobody non noone not nothing now nowhere number numbers o of off often old older oldest on once one only open opened opening opens or order ordered ordering orders other others our out over p part parted parting parts per perhaps place places point pointed pointing points possible present presented presenting presents problem problems put puts q quite r rather really right room rooms s said same saw say says second seconds see seem seemed seeming seems sees several shall she should show showed showing shows side sides since small smaller smallest so some somebody someone something somewhere state states still such sure t take taken than that the their them then there therefore these they thing things think thinks this those though thought thoughts three through thus to today together too took toward turn turned turning turns two u under until up upon us use used uses v very w want wanted wanting wants was way ways we well wells went were what when where whether which while who whole whose why will with within without work worked working works would x y year years yet you your yours z rather number part don say suggests however among reports ways small get found issues says just articles made large review needs better used become different top like business can management new issue managers will presented one editor discusses response letter also work people corporate market executives many authors may time product states employees make industry economic must world united research include organization well presents study marketing customers executive two use products value change need system performance sales way financial problems first process years making costs book public cost growth customer several three long often high approach case important including best help leadership topics based reviews control capital social even markets good service systems problem take american much price', ' ')[[1]] # add more words
docs = tm_map(docs, content_transformer(removeWords), to_delete_words)             # remove junk words
#docs = tm_map(docs, stemDocument)                             # stem document
docs = tm_map(docs, content_transformer(stripWhitespace))     # stripe white spaces

dtm <- DocumentTermMatrix(docs)     
dtm <- removeSparseTerms(dtm,0.995) # get rid of very infrequent words
                                    # 12699 - 12699 * .995 ~ 64 words
                                    # so words with < 64 freq will be removed

dtm.mat <- as.matrix(dtm)

words.list <- colnames(dtm.mat)
term.freq <- colSums(dtm.mat) # term frequency

# look at the summary
summary(term.freq)


# Frequency plot
term.freq.df <- data.frame(term=names(term.freq), freq=term.freq) # for ggplot
ggplot(data=term.freq.df, aes(log10(freq))) + geom_histogram(bins = 30) +
  labs(title = "Term Frequency", x= "Log_10 (frequency)", y="Count")


# we will use 4-year running interval

# years.list <- as.numeric(unique(format(hbr$date, '%Y')))
# years.grp <- 
#   cut(as.numeric(format(hbr$date, '%Y')), 
#     breaks=seq.int(1923, 2011, 4),
#     include.lowest=T, ordered_result = F)
# levels(years.grp) <- gsub(',', '-', substr(levels(years.grp), 2,10)) # better names

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

# examine pop_word_year


# using reshape2::melt() function to convert from matrix to long format
# word_year.long <- melt(word_year, 
#                        value.name = "count", 
#                        variable.name = "year", 
#                        na.rm = TRUE)
# names(word_year.long) <- c("terms", "year", "count")
# 
# head(word_year.long) # nice format; but too many records!

# Use ExPosition package for Correspondence Analysis
res.ca <- epCA(t(word_year), graphs = F)

# First, we give a wight to each word according to its "temporal" importance
weight <- data.frame(word = row.names(res.ca$ExPosition.Data$fj),  wt = as.vector(res.ca$ExPosition.Data$dj))
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
  mutate(wt  = 3+as.vector(res.ca$ExPosition.Data$dj)[top25])

g1<-ggplot(data=df1, aes(x=X1, y=X2, label=year)) + geom_text(color="red") + geom_path()
g2<-g1+geom_text(data=df2, aes(x=X1, y=X2, label=word, size=wt), check_overlap = T)
g2<- g2 + labs(title="Correspondence Analysis of HBR corpus over 90 years",
               x="Component 1", y="Component 2") + theme_void() + theme(legend.position="none")
g2

clus<-hclust(dist(res.ca$ExPosition.Data$fj[top25,1:5]), method = "ward.D2")
memb <- cutree(clus, k = 6)

g1<-ggplot(data=df1, aes(x=X1, y=X2, label=year)) +  geom_text(size=2, check_overlap = T)# + geom_path()
g2<-g1+geom_text(data=df2, aes(x=X1, y=X2, label=word, size=2+wt), check_overlap = T, color=memb)
g2<- g1 + labs(title="Term map of HBR corpus over 90 years",
               x="Component 1", y="Component 2") + theme_void() + theme(legend.position="none")
g2

# ggplot(data=df2, aes(x=X1, y=X2, label=word, size=4+wt, color=factor(memb))) +  geom_text(check_overlap = F)# + geom_path()

ggsave('./term_map.png', g2)

# Nice map


