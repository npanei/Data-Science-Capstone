# data science capstone
# CSV table preparations
# npanei

# this code produces a lookup table for the text prediction algorithm
# the lookup table is stored as an array of integers
# these integers are actually the rank of the most common words in the corpus
# the most common word, "the", is stored as 1, for example

# this code also produces an index which the app will incorporate
# to translate from text to index number and back

# these outputs will be stored in two CSV files
# which will be uploaded to github and read directly by URL by the Shiny app

library(Hmisc)

# define common corrupted text to clean the corpus

corrupt <- read.table("Data Science Capstone/final/en_US/en_US.news.txt",skip=7,nrow=1,
                      sep="\n",quote="",stringsAsFactors=FALSE)
corrupt <- substr(as.character(corrupt),start=4,stop=6)
corrupt_a <- substr(corrupt,start=1,stop=1)

# function definitions

corpusread <- function(readlist,p) {
    # read the corpus as a long array of character strings
    read1 <- read.table(readlist[1],sep="\n",quote="",stringsAsFactors=FALSE)
    read1 <- apply(read1,1,as.character)
    read2 <- read.table(readlist[2],sep="\n",quote="",stringsAsFactors=FALSE)
    read2 <- apply(read2,1,as.character)
    read3 <- read.table(readlist[3],sep="\n",quote="",stringsAsFactors=FALSE)
    read3 <- apply(read3,1,as.character)
    
    r <- rbinom(length(c(read1,read2,read3)),size=1,prob=p) # take sample
    tolower(c(read1,read2,read3))[r==1] # return sample, reduced to lowercase
}

nullclip <- function(k) { # delete empty or corrupt "words"
    k[k != "" & k != corrupt_a]
}

wordsplit <- function(j) { # split each line of the corpus into individual words
    j <- strsplit(j,split="[^[:alnum:]']")
    lapply(j,nullclip)
}

# create index of the most common words in a large sample
# corpusread is embedded - stops the large sample from remaining in memory
commontable <- function(readlist,p) {
    w <- corpusread(readlist,p)
    w <- gsub(corrupt,"'",w)
    w <- wordsplit(w)
    t <- table(unlist(w))
    sort(t[t>100],decreasing=TRUE)
}

numsplit <- function(k,index) { # translate a line of words into their index numbers
    
    x <- c()
    i <- 1
    for (j in k) {
        
        if (length(index[index[,2]==j,1])==1) {
            x[i] <- index[index[,2]==j,1]
        } else {
            x[i] <- 0 # unindexed words are translated to "0"
        }
        i <- i + 1
    }
    x
}

# this function saves memory by using both numsplit and wordsplit line by line
# using one then the other on the entire corpus would be burdensome
numtranslate <- function(k,index) { 
    numsplit(wordsplit(k)[[1]],index)
}

# generate lookup table
lookup1 <- function(corpus,indexcount) {
    x <- c()
    for (i in indexcount) {
        y <- c()
        for (j in 1:length(corpus)) {
            if (length(corpus[[j]])==0) {
                next
            }
            flag <- 0
            # after one common word is detected, save the following word
            for (k in 1:length(corpus[[j]])) {
                if (corpus[[j]][k]==i & flag==0) {
                    flag <- 1
                } else if (flag==1) {
                    flag <- 0
                    y[length(y)+1] <- corpus[[j]][k]
                }
            }
        }
        z <- names(sort(table(y),decreasing=TRUE))

        mark <- 1
        while (z[mark] %in% c("0","400","552","559","589")) { # profanity filter
            mark <- mark + 1
        }
        # the below check for use in testing; lookup table had no NAs in practice
        if (length(z)==0) { 
            x[i] <- NA
        } else {
            x[i] <- as.numeric(z[mark])
        }
        # x <- as.numeric(z[mark]) # delete the above 5 lines and instead use this line
        # for use with argument indexcount=0 to compute most common word after a "0" word
    }
    x
}

##############

# main code

# create index of most common words based on a 50% sample of the corpus
# the below takes about 6 minutes to compile; the whole corpus would take much longer
set.seed(215)
US_complete_index <- commontable(c("Data Science Capstone/final/en_US/en_US.twitter.txt",
                                   "Data Science Capstone/final/en_US/en_US.blogs.txt",
                                   "Data Science Capstone/final/en_US/en_US.news.txt"),0.5)

write.csv(names(US_complete_index),file="Data Science Capstone/full_index.csv")

# take a smaller sample for the lookup table, 1%
set.seed(350)
US_complete_sample <- corpusread(c("Data Science Capstone/final/en_US/en_US.twitter.txt",
                                   "Data Science Capstone/final/en_US/en_US.blogs.txt",
                                   "Data Science Capstone/final/en_US/en_US.news.txt"),0.01)

# the "corrupt" string defined earlier replaces apostrophes in the corpus
# it occurs in hundreds of thousands of instances
US_complete_sample <- gsub(corrupt,"'",US_complete_sample)

# translate sample 
# only the 2000 most common words are used in practice
US_num <- lapply(US_complete_sample,numtranslate,index=US_complete_index[1:2000,])

# create lookup table; takes about 6 minutes to compile
US_lookup1 <- lookup1(US_num,1:2000)

# lookup1(US_num,0) returns 3
# that is, the most common word after an uncategorized "0" word is "and"
# used slightly modified code to get to this, see lookup1 function definition

write.csv(US_lookup1,file="Data Science Capstone/lookup_table_1.csv")