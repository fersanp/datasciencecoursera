#' ---
#' title: "Task 2: Exploratory Data Analysis"
#' author: "Fernanda Sanchez"
#' date: "`r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#' ---
#'
#' ## Introduction
#' This script uses the tidy data principles applied to text mining, as outlined in
#' [Text Mining with R: A Tidy Approach](http://tidytextmining.com/).  
#' 

#+ startup, echo = FALSE 
rm(list = ls())
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})
start_time <- Sys.time()

#' ## Data Loading and Summarizing
#+ DataLoading

#' English Repository Files
blogs_file   <- "./final/en_US/en_US.blogs.txt"
news_file    <- "./final/en_US/en_US.news.txt"
twitter_file <- "./final/en_US/en_US.twitter.txt"  

#' File Sizes (Mb)
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

#' Read the data files
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

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
title("Characters per Line")

#' Total characters per file
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

#' Total words per file
blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(twitter, sep = " ")

#' Create summary of repo stats
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
kable(repo_summary)

#saveRDS(repo_summary, "./clean_repos/repo_summary.rds")

#' Read the data files into dataframes
blogs   <- data.frame(text = blogs)
news    <- data.frame(text = news)
twitter <- data.frame(text = twitter)

#' ## Data Sampling and Cleaning
#+ DataSampling
set.seed(123)
sample_pct <- 0.1

blogs_size   <- blogs_lines * sample_pct
news_size    <- news_lines * sample_pct
twitter_size <- twitter_lines * sample_pct

#' Create samples
blogs_sample   <- blogs[sample(blogs_lines, blogs_size, replace = FALSE, prob = NULL),]
news_sample    <- news[sample(news_lines, news_size, replace = FALSE, prob = NULL),]
twitter_sample <- twitter[sample(twitter_lines, twitter_size, replace = FALSE, prob = NULL),]
repo_sample    <- c(blogs_sample, news_sample, twitter_sample)

#' Convert the samples into dataframes
blogs_sample <- data.frame(text = blogs_sample)
news_sample  <- data.frame(text = news_sample)
twitter_sample <- data.frame(text = twitter_sample)

#' Save sample
#writeLines(repo_sample, "./final/en_US/en_US.repo_sample.txt")
#saveRDS(repo_sample, file = "./final/en_US/repo_sample.rds" )


#' Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)


#' Create filters: stopwords, non-alphanumeric's, url's, repeated letters(+3x)
#+ DataCleaning
data("stop_words")
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
#' and ngrams.
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII"))
clean_sample <-  na.omit(clean_sample)

#' Create tidy dataframe for repo sample
#' Separate the text into words and then Remove stop words 
tidy_repo <- clean_sample %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#' ## Most frequent words and word distributions
#' Word counts: Number of unique words in tidy repo
(repo_count <- tidy_repo %>%
    summarise(keys = n_distinct(word)))

#' Number of words to attain 50% and 90% coverage of all words in repo
cover_50 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_50)

#' Plot most frequent terms
top_10 <- cover_50[order(-cover_50$n),]
top_10 <- top_10[1:10,]
ggplot(top_10, aes(reorder(word, -n), n)) +
  geom_bar(stat = "identity",color='gray') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("word") +
  ylab("frequency") 
ggsave("www/monograms.png")  


cover_90 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(cover_90)

#' ## Word distributions  
#' Word distribution
cover_90 %>%
  top_n(20, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#' Word distribution by source
freq <- tidy_repo %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))

freq %>%
  filter(proportion > 0.002) %>% 
  mutate(word = reorder(word, proportion)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  facet_grid(~source, scales = "free")

#' Word cloud
cover_90 %>%
  with(wordcloud(word, n, max.words = 50, 
                 colors = brewer.pal(6, 'Dark2'), random.order = FALSE))

#saveRDS(tidy_repo, "./clean_repos/tidy_repo.rds")
#saveRDS(cover_90, "./clean_repos/cover_90.rds")
rm(tidy_repo, cover_50, cover_90)

#' ## Bigrams  
#' Create bigrams by source using `unnest_tokens`

#clean_sample %>% remove_empty("rows")

bigram_repo <- clean_sample  %>%
  select_if(~ !is.list(.)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Number of bigrams to attain 90% coverage of all bigrams in repo
bigram_cover_90 <- bigram_repo %>%
  count(bigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(bigram_cover_90)

#' Bigram distribution
bigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(bigram = reorder(bigram, proportion)) %>%
  ggplot(aes(bigram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

bigram_cover <- bigram_repo %>%
  count(bigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n)) 
#saveRDS(bigram_cover_90, "./clean_repos/bigram_cover_90.rds")

#' ## Trigrams    
#' Create Trigrams by source using `unnest_tokens`
#+ trigrams

trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Number of trigrams to attain 90% coverage of all trigrams in repo
trigram_cover_90 <- trigram_repo %>%
  count(trigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(trigram_cover_90)

#' trigram distribution
trigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(trigram = reorder(trigram, proportion)) %>%
  ggplot(aes(trigram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

trigram_cover <- trigram_repo %>%
  count(trigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n)) 
#saveRDS(trigram_cover_90, "./clean_repos/trigram_cover_90.rds")

#' ## Quadgrams  
#' Create quadgrams by source using `unnest_tokens`
#+ quadgrams

quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

#' Number of quadgrams to attain 90% coverage of all quadgrams in repo
quadgram_cover_90 <- quadgram_repo %>%
  count(quadgram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(quadgram_cover_90)

#' quadgram distribution
quadgram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(quadgram = reorder(quadgram, proportion)) %>%
  ggplot(aes(quadgram, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

quadgrams_separated <- quadgram_cover_90 %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quadgrams_separated

quadgram_cover <- quadgram_repo %>%
  count(quadgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n)) 

#' Quintgrams

quintgram_repo <- clean_sample  %>%
  unnest_tokens(quintgram, text, token = "ngrams", n = 5)

quintgram_cover <- quintgram_repo %>%
  count(quintgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  


#' Sextgrams
sextgram_repo <- clean_sample  %>%
  unnest_tokens(sextgram, text, token = "ngrams", n = 6)

sextgram_cover <- sextgram_repo %>%
  count(sextgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("sextgram_repo"))

#' ## What does the distribution on ngrams look like?
#+ DistyPlot
disty <- data_frame(ngram = c(rep("bigrams",  nrow(bigram_cover)),
                              rep("trigrams",  nrow(trigram_cover)),
                              rep("quadgrams", nrow(quadgram_cover)),
                              rep("quintgrams", nrow(quintgram_cover)),
                              rep("sextgrams",  nrow(sextgram_cover))),
                    number = c(bigram_cover$n,  trigram_cover$n, 
                               quadgram_cover$n, quintgram_cover$n,
                               sextgram_cover$n))
disty
disty$ngram <- as.factor(disty$ngram)
ggplot(data = disty, aes(y = number, x = reorder(ngram, -number))) +
  geom_boxplot() + scale_y_log10() +
  xlab("ngram")
ggsave("./www/ngrams.png")

sextgram_cover %>%
  top_n(10, n) %>%
  mutate(sextgram = reorder(sextgram, desc(n))) %>%
  ggplot(aes(sextgram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("word") +
  ylab("frequency") 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Sextgrams") 
ggsave("./www/sextgrams.png")

quintgram_cover %>%
  top_n(10, n) %>%
  mutate(quintgram = reorder(quintgram, desc(n))) %>%
  ggplot(aes(quintgram, n)) +
  geom_col() +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Quintgrams")
ggsave("./www/quintgrams.png")

quadgram_cover %>%
  top_n(10, n) %>%
  mutate(quadgram = reorder(quadgram, desc(n))) %>%
  ggplot(aes(quadgram, n)) +
  geom_col() +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Quadgrams")
ggsave("./www/quadgrams.png")

trigram_cover %>%
  top_n(10, n) %>%
  mutate(trigram = reorder(trigram, desc(n))) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Trigrams")
ggsave("./www/trigrams.png")

bigram_cover %>%
  top_n(10, n) %>%
  mutate(bigram = reorder(bigram, desc(n))) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Bigrams")
ggsave("./www/bigrams.png")

#' ## Separate words
#+ NgramWords 
bi_words <- bigram_cover %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bi_words

tri_words <- trigram_cover %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
tri_words

quad_words <- quadgram_cover %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words

quint_words <- quintgram_cover %>%
  separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
quint_words

sext_words <- sextgram_cover %>%
  separate(sextgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
sext_words

#' Save data for the Shiny App
saveRDS(bi_words, "./bi_words_fast.rds")
saveRDS(tri_words, "./tri_words_fast.rds")
saveRDS(quad_words,"./quad_words_fast.rds")
saveRDS(quint_words,"./quint_words_fast.rds")
saveRDS(sext_words,"./sext_words_fast.rds")
#saveRDS(quadgram_cover_90, "./quadgram_cover_90.rds")

end <- Sys.time()

(run_time <- end - start_time)