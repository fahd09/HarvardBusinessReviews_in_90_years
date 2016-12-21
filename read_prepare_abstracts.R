rm(list=ls())

library(ggplot2)
library(tm)
library(RWeka) 
library(lsa)
library(ExPosition)
library(reshape2)
library(dplyr)

hbr <- read.csv('./data/HBR Citations_correct_abstracts.csv', strip.white=TRUE, as.is=TRUE)


# Will use later for N-gram analysis
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)



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

# Which parts of corpus we want to keep ?
unique(hbr$DOCUMENT.TYPE)
table(factor(hbr$DOCUMENT.TYPE=="Article"))

###########################################
# compute article frequency grouped by year

article_freq <- aggregate(hbr$ARTICLE.TITLE, by = list(year = as.factor(format(hbr$date, "%Y"))), FUN = length)

# visualize
ggplot(data=article_freq, aes(x=year, y=x)) + geom_point()

# drop records of 1st and last year ?
# hbr <- hbr[-which(format(hbr$date, "%Y") %in% c(1922,2012)),]
# Group records from before 1975 into 5-year units ?

# Text-mining.. 

docs = Corpus(VectorSource(hbr$abstract[hbr$DOCUMENT.TYPE=="Article"]))
tospace <- (function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, content_transformer(tolower))                       # convert all chars to lower-case
docs = tm_map(docs, content_transformer(tospace), "[/\\]")              # convert all useless slashes to spaces.
docs = tm_map(docs, content_transformer(removeNumbers))                 # remove numbers
docs = tm_map(docs, content_transformer(tospace), "[^[:alnum:]///' ]")  # convert all non-alphabets to spaces
docs = tm_map(docs, content_transformer(tospace), "[']")                # convert ' to spaces

docs = tm_map(docs, content_transformer(removeWords), stopwords_en)             # remove junk words
to_delete_words <- strsplit('models plan federal development investment organizations program books own furthermore post september women january march june july november december october april august february hbr accounting period planning government international discussed companies company author authors article articles experience businessmen describe describes examine examines method details analysts analyst methods noted mentioned journal harvard regarding refers job excerpt benefits relations plans focuses act related pertaining recent able form techniques hard little basic michael types specific day comments manage manages manager managers means makes provides argue argues instead pay pays david current professor rate rates ability look looks common self robert john provide level line example a about above across after again against all almost alone along already also although always among an and another any anybody anyone anything anywhere are area areas around as ask asked asking asks at away b back backed backing backs be became because become becomes been before began behind being beings best better between big both but by c came can cannot case cases certain certainly clear clearly come could d did differ different differently do does done down downed downing downs during e each early either end ended ending ends enough even evenly ever every everybody everyone everything everywhere f face faces far felt few find finds first for four from full fully further furthered furthering furthers g gave general generally get gets give given gives go going goods got group grouped grouping groups h had has have having he her here herself high higher highest him himself his how however i if in interest interests into is it its itself j just k keep keeps kind knew know known knows l large largely last later latest least less let lets like likely long longer longest m made make making man many may me member members men might more most mostly mr mrs much must my myself n necessary need needed needing needs never new newer newest next no nobody non noone not nothing now nowhere number numbers o of off often old older oldest on once one only open opened opening opens or order ordered ordering orders other others our out over p part parted parting parts per perhaps place places point pointed pointing points possible present presented presenting presents problem problems put puts q quite r rather really right room rooms s said same saw say says second seconds see seem seemed seeming seems sees several shall she should show showed showing shows side sides since small smaller smallest so some somebody someone something somewhere state states still such sure t take taken than that the their them then there therefore these they thing things think thinks this those though thought thoughts three through thus to today together too took toward turn turned turning turns two u under until up upon us use used uses v very w want wanted wanting wants was way ways we well wells went were what when where whether which while who whole whose why will with within without work worked working works would x y year years yet you your yours z rather number part don say suggests however among reports ways small get found issues says just articles made large review needs better used become different top like business can management new issue managers will presented one editor discusses response letter also work people corporate market executives many authors may time product states employees make industry economic must world united research include organization well presents study marketing customers executive two use products value change need system performance sales way financial problems first process years making costs book public cost growth customer several three long often high approach case important including best help leadership topics based reviews control capital social even markets good service systems problem take american much price', ' ')[[1]] # add more words
docs = tm_map(docs, content_transformer(removeWords), to_delete_words)             # remove junk words

#docs = tm_map(docs, stemDocument)             # remove junk words
docs = tm_map(docs, content_transformer(stripWhitespace))     # stripe white spaces

stopwrds_hbr <- scan('./toDelete/stopwords_hbr.txt', what = "", sep = ',')
stopwrds_hbr <- unlist(lapply(stopwrds_hbr, strsplit, split=','))

dtm.raw.2gram <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer,
                                                   stopwords = stopwrds_hbr))     
dtm.raw.1gram <- DocumentTermMatrix(docs, control = list(stopwords = stopwrds_hbr))     

dtm.2gram <- removeSparseTerms(dtm.raw.2gram,0.999) 
dtm.1gram <- removeSparseTerms(dtm.raw.1gram,0.999) 

# get rid of very infrequent words
# 12699 - 12699 * .995 ~ 64 words
# so words with < 64 freq will be removed

dtm.mat.1 <- as.matrix(dtm.2gram)
dtm.mat.2 <- as.matrix(dtm.1gram)

dtm.mat <- cbind(dtm.mat.1, dtm.mat.2)

rm(list=c("dtm.mat.1","dtm.mat.2","dtm.2gram","dtm.1gram"))

words.list <- colnames(dtm.mat)
term.freq <- colSums(dtm.mat) # term frequency

# look at the summary
summary(term.freq)


# Frequency plot
term.freq.df <- data.frame(term=names(term.freq), freq=term.freq) # for ggplot
ggplot(data=term.freq.df, aes(log10(freq))) + geom_histogram(bins = 30) +
  labs(title = "Term Frequency", x= "Log_10 (frequency)", y="Count")


################## Option 1
# We want to know word freq. by year

# temp_df <- data.frame(year=as.factor(format(hbr$date[hbr$DOCUMENT.TYPE=="Article"], '%Y')))
# year_nominal <- makeNominalData(temp_df)
# colnames(year_nominal) <- substr(colnames(year_nominal), 6,10)
# word_year <- t(dtm.mat) %*% year_nominal 

################## Option 2
# we will use 4-year running interval

years.list <- as.numeric(unique(format(hbr$date[hbr$DOCUMENT.TYPE=="Article"], '%Y')))
years.grp <- 
  cut(as.numeric(format(hbr$date[hbr$DOCUMENT.TYPE=="Article"], '%Y')), 
    breaks=16,#31,#seq.int(1922, 2012, 3),
    include.lowest=T, ordered_result = F)
levels(years.grp) <- gsub(',', '-', substr(levels(years.grp), 2,10)) # better names
year_nominal <- makeNominalData(data.frame(years.grp))
word_year <- t(dtm.mat) %*% year_nominal 
colnames(word_year) <- substr(colnames(word_year), 16,20)
word_year<-word_year[,order(as.numeric(colnames(word_year)))]

# Now we have:
# dtm           ==> 12697 (articles)  x 18376  (words)
# year_nominal  ==> 12697 (articles)  x 89    (years)

# we want: 
# word_year     ==> 4673  (words) x 89 (years) # gives counts of each word per year!


# head(word_year)

# No need for this when using the 4-year window
# yrs.sorted <- order(as.numeric(colnames(word_year)))
# word_year <- word_year[,yrs.sorted]
#word_year[word_year==0] <- NA       # we convert all zeros to NAs 
# so that we get rid of multiple 
# "zero" records in next step

# popular word per year
num<-4
pop.cnt <- apply(word_year, 2, function(x) {return( sort(x, decreasing = T)[1:num] )})                                   # return maximum "count"
pop.wrd <- apply(word_year, 2, function(x) {return( names(x[order(x, decreasing = T)[1:num]]) )})                                   # return maximum "count"
#pop.wrd <- apply(word_year, 2, function(x) {return( list(x[order(x, decreasing = T)[1:5]]) )})                                   # return maximum "count"

long1 <- melt(pop.cnt, value.name = "count", varnames = c("order","year"))
long2 <- melt(pop.wrd, value.name = "word", varnames = c("order","year"))

pop_word_year <- cbind(long2, count=long1$count)

#pop_word_year <- data.frame(year=as.numeric(colnames(word_year)), word=pop.wrd,count=pop.cnt)

# examine pop_word_year
word_year[grep('emotional', row.names(word_year)),]

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
#top25 <- weight$wt > quantile(weight$wt, .55)

# then visualize those "selected" ones..

df1<-
  data.frame(res.ca$ExPosition.Data$fi) %>% 
  select(num_range("X", 1:4)) %>%
  mutate(year= as.numeric(row.names(res.ca$ExPosition.Data$fi))) #%>%
df1 <- df1[order(df1$year),]
#  filter(year < 1950)
df2<-
  data.frame(res.ca$ExPosition.Data$fj) %>% 
  select(num_range("X", 1:4)) %>%
  mutate(word= row.names(res.ca$ExPosition.Data$fj)) %>%
  mutate(wt  = as.vector(res.ca$ExPosition.Data$dj),
         top = factor(ifelse(wt>quantile(wt,.95), 1, 0))) %>%
  filter(top ==1)

# df2$marker <- ifelse(df2$word %in% c("newspapers", "amazon", "morgan", "war ii", 
#                                      "depression era","facebook","innovation",
#                                      "win win","venture capitalists",
#                                      "trial error","texas instruments",
#                                      "cold war","civil rights",
#                                      "boston consulting","health insurance",
#                                      "medical care","silicon valley",
#                                      "start ups", "breakthroughs",
#                                      "canada","marriage","propaganda","railroad",
#                                      "railways","recessions","robots",
#                                      "terrorist","semiconductor",
#                                      "civilization", "workers union"), 5, 3)
df2$marker <- ifelse(df2$word %in% as.character( unique(pop_word_year$word) ), 5, 3)

#df2 <- subset(df2, top==1)

g1<-ggplot(data=df1, aes(x=X1, y=X2, label=year)) + geom_text(color="red") + geom_path()
g2<-g1+geom_text(data=df2, aes(x=X1, y=X2, label=word, size=marker), check_overlap = F)
g3<- g2 + labs(title="Correspondence Analysis of HBR corpus over 90 years",
               x="Component 1", y="Component 2") + theme_void() + theme(legend.position="none")
g3

#clus<-hclust(dist(df2[,1:2]) )#, method = "ward.D2")
clus<-hclust(dist(df2[,1:2]), method = "ward.D2")
memb <- factor(cutree(clus, k = 5))


library(ggrepel)

g1<-ggplot(data=df1, aes(x=X1, y=X2, label=year)) + #geom_path() +
  geom_label_repel(
    aes(x=X1, y=X2, label = factor(year)),
    fill = "white", fontface = 'bold', color = 'black',
    box.padding = unit(0.05, "lines")
  )
g2<-g1+geom_text(data=df2, aes(x=X1, y=X2, label=word, color=memb, size=2), check_overlap = F)
g3<- g2 + labs(title="Vocabolary Map of Harvard Business Revew Corpus\nfrom 1922 to 2012",
               x="Component 1", y="Component 2") + theme_void() + 
  theme(legend.position="none") + scale_color_brewer(palette = "Set1")
g3 

ggsave('./term_map_clean2.png', g3)
#ggsave('./term_map_full.png', g3)


# Nice map