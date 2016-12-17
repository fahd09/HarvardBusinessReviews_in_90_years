
library(ggplot2)

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

# Group records from before 1975 into 5-year units ?
