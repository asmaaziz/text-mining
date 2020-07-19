library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

##data load
#mydata <-read.csv(file = "C:/Users/asma/useful.csv", header = FALSE, sep = " ")
#FORWARD SLASH
mydata <- readLines("C:/Users/asma/useful stuff.txt") #FORWARD SLASH

#Number of non-empty comments
print (sprintf("Total non-empty comments = %d",length(mydata[mydata!=""])))

## text
## text <- readLines(file.choose()) 

##load the data as a document

docs <- Corpus(VectorSource(mydata))

## clean up data

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

##Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("also","got","ever","just")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#generate word cloud

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 1)


findAssocs(dtm, terms = "objections", corlimit = 0.2)

#Making barplot
ylim <- c(0, 1.1*max(d$freq))
bp <- barplot(d[1:10,]$freq, ylim = ylim,
              main ="Frequent Words in Useful Experience: Q2 & Q3", names.arg = d[1:10, ]$word,
              ylab = "Word Frequencies", col = "orchid4")
text(x = bp, y = d[1:10,]$freq, label = d[1:10,]$freq, pos = 3, cex = 0.9, col = "red4", font = 2, family = "sans")
## Add x-axis labels 
## axis(1, at=bp, labels=d[1:10,]$word, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

dim(d)
str(d)
dir(d)
summary(d)


