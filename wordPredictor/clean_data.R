#' ---
#' title: "Task 1: Getting and Cleaning the Data"
#' author: "Fernanda Sanchez"
#' date: "`r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#' ---
#' 
rm(list = ls())
suppressPackageStartupMessages({
  library(downloader)
  library(tm)
  library(knitr)
  library(tidyverse)
  library(dplyr)
  library(dtplyr)
  library(data.table)
  library(ggthemes)
  library(wordcloud)
  library(ngram)
})  
start <- Sys.time()

#' Download the data
# url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# download(url, dest="Coursera-SwiftKey.zip", mode="wb") 
# unzip ("Cousera-SwiftKey.zip")

#' English Repository Files
blogs_file   <- "./final/en_US/en_US.blogs.txt"
news_file    <- "./final/en_US/en_US.news.txt"
twitter_file <- "./final/en_US/en_US.twitter.txt"  

#' File Sizes (Mb)
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

#' Read the data files
blogs   <- read_lines(blogs_file)
news    <- read_lines(news_file)
twitter <- read_lines(twitter_file) 

#' Number of Lines per file
blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

#' Distibution of characters per line, by file
blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
        names = c("blogs", "news", "twitter"),
        ylab = "log(Number of Characters)", xlab = "File Name") 
title("Distributions of Characters per Line")

#' Total characters per file
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

#' Total words per file
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(news, sep = " ")

#' Create summary of repo stats
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
# Percentages
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
kable(repo_summary)


#' ## Sample the data and save the sample
#' 
#' Compute sample sizes in terms of lines
sample_pct = 0.01
set.seed(1001)

blogs_size   <- blogs_lines * sample_pct
news_size    <- news_lines * sample_pct
twitter_size <- twitter_lines * sample_pct

#' Create samples
blogs_sample   <- sample(blogs, blogs_size)
news_sample    <- sample(news, news_size)
twitter_sample <- sample(twitter, twitter_size)
repo_sample    <- c(blogs_sample, news_sample, twitter_sample)

#' Save sample
writeLines(repo_sample, "./final/en_US/en_US.repo_sample.txt")
saveRDS(repo_sample, file = "./final/en_US/repo_sample.rds" )

#' ## Clean the sample data
#'
#' Use `tm` to create and clean the corpus
clean_sample <- Corpus(VectorSource(repo_sample))
print(as.character(clean_sample[[1]]))

#' Remove URL's  
#' Source: [R and Data Mining]("http://www.rdatamining.com/books/rdm/faq/removeurlsfromtext")
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean_sample <- tm_map(clean_sample, content_transformer(removeURL))

# Remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
clean_sample <- tm_map(clean_sample, content_transformer(removeNumPunct))

#' Transform sample to all lower case
clean_sample <- tm_map(clean_sample, content_transformer(tolower))

#' Create profanity filter  
#' Source: [List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words]("List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/en")
#profanity <- read.table("./data/final/en_US/profanity.txt", header = FALSE, sep ="\n")

#' Remove profanity  
#clean_sample <- tm_map(clean_sample, removeWords, profanity[,1])

#' Remove stopwords  
clean_sample <- tm_map(clean_sample, removeWords, stopwords("english"))
clean_sample <- tm_map(clean_sample, removeWords, stopwords("SMART"))
print(as.character(clean_sample[[1]]))

#' Remove Whitespace  
clean_sample <- tm_map(clean_sample, stripWhitespace)
print(as.character(clean_sample[[1]]))

#' Save clean corpus  
saveRDS(clean_sample, file = "./data/final/en_US/clean_sample.rds" )

#' ## Initial Exploratory Data Analysis  
#' 
#' Convert to document term matrix
docterm_corpus <- DocumentTermMatrix(clean_sample)
dim(docterm_corpus)

new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse = 0.993)
dim(new_docterm_corpus)

#' Find frequent terms
colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)

#' Most frequent and least frequent words
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words

#' Plot most frequent terms
ggplot(doc_features[count>5000],aes(name, count)) +
  geom_bar(stat = "identity",fill='lightblue',color='black') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_economist() + scale_color_economist() 

#' Create word cloud
wordcloud(names(colS), colS, min.freq = 500, 
          colors = brewer.pal(6, 'Dark2'), random.order = FALSE)  

wordcloud(names(colS), colS, min.freq = 2000, 
          colors = brewer.pal(6, 'Dark2'), random.order = FALSE)  

end <- Sys.time()
(ellapsed <- end - start)