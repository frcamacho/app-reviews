# install itunesr directly from CRAN:
#install.packages("itunesr")

library(itunesr)
library(tidyverse)
library(tidytext)
library(ggsci)
library(widyr)
library(ggpubr)
library(topicmodels)
retreive_reviews<- function(appID, country){
  reviews_data <- data.frame()
  for (i in 1:10){
    data <- getReviews(appID,country,i)
    reviews_data <- rbind(reviews_data,data)
  }
  reviews_data$Date <- as.Date(reviews_data$Date)
  reviews_data$Rating <- as.numeric(reviews_data$Rating) 
  return(reviews_data)
  
}

bravo_reviews <- retreive_reviews(383925190, 'us')
rownames(bravo_reviews) <- seq(length=nrow(bravo_reviews)) 
bravo_reviews$doc <- rownames(bravo_reviews)
########
#NLP analysis 

bigrams_df <- bravo_reviews %>% unnest_tokens(word, Review, token = "ngrams", n = 2)

# we need to separated the bigrams into two columns to filter stop words  
bigrams_separated <- bigrams_df %>%
  separate(word, c("word1", "word2"), sep = " ")
#filter stop words from bigrams created in two columns 
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "^\\d"))  %>%
  filter(!str_detect(word2, "^\\d"))  %>%
  filter(!is.na(word1) & !is.na(word2)) 

#Now let's united these filtered bigrams into one column
bigrams_united <- bigrams_filtered %>%
  unite(word, word1, word2, sep = " ")  %>% mutate(bigram = gsub("[][]|[^[:ascii:]]", "", word, perl=T))


#######################################################

create_desc_plts<-function(df){
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
  
  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  
  
  plt_1 <- df %>% count(bigram, sort = TRUE) %>% top_n(.,25)%>%mutate(bigram = reorder(bigram,n)) %>% 
    ggplot(., aes(bigram, n)) + geom_col() + xlab(NULL) + coord_flip()+ theme_bw() + ggtitle("Top 25 n-gram counts in all documents")
  plt_2 <- df %>% group_by(Rating) %>% count(bigram, sort = TRUE) %>% top_n(.,5) %>% 
    ggplot(., aes(reorder_within(bigram,n,Rating), n)) + geom_col() + xlab(NULL) + coord_flip()+ theme_bw()+ scale_x_reordered() +facet_wrap(~Rating, scales = "free") + 
    ggtitle("Top 5 n-grams counts per Rating") 
  plt1_2<-ggarrange(plt_1, plt_2, ncol = 2, nrow = 1)
  return(plt1_2)
}


# create descriptive plots for unigram counts and metadata 
create_desc_plts(bigrams_united)


# create descriptive plots for unigram counts and metadata 
#create_desc_plts(unigram_df)
create_desc_plts(bigrams_united)

format_data_LDA <- function(df){
  
  df_counts <-df %>% group_by(doc) %>% count(doc,bigram, sort =  TRUE) %>% ungroup()
  
  total_words_count_df <- df_counts %>% group_by(doc) %>%
    summarize(total = sum(n))%>% ungroup()
  
  combined_counts_df <- left_join(df_counts, total_words_count_df) 
  
  
  " tf-idf is to find the important words for the content of each document by decreasing 
  the weight for commonly used words and increasing the weight for words that are not used 
  very much in a collection or corpus of documents"
  
  tf_idf_df <- combined_counts_df %>%
    bind_tf_idf(bigram, doc, n)
  
  return(tf_idf_df)
}

#unigram_tf_idf <-format_data_LDA(unigram_df)
bigram_tf_idf <-format_data_LDA(bigrams_united)

bigram_tf_idf %>% ggplot(.,aes(tf_idf)) + geom_histogram(binwidth = .5) + theme_bw()

bigram_tf_idf %>% arrange(desc(tf_idf)) %>% filter(!duplicated(bigram)) %>% top_n(25, tf_idf) %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%ggplot(aes(bigram, tf_idf))+ geom_col() + 
  labs(x=NULL, y = "tf-idf") + coord_flip() + theme_bw()

##################################################

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

bigram_tf_idf %>% inner_join(., bigrams_united, by = c("bigram", "doc")) %>% 
  distinct() %>% group_by(Rating) %>% top_n(10, tf_idf) %>%
ggplot(., aes(reorder_within(bigram,tf_idf,Rating), tf_idf)) + geom_col() + xlab(NULL) + coord_flip()+ theme_bw()+ scale_x_reordered() +facet_wrap(~Rating, scales = "free") + 
  ggtitle("Top 5 n-grams counts per Rating") 


##################################################

############################################################

# convert tidytext format to dtm format 
bigram_tf_idf_dtm<-bigram_tf_idf  %>% filter(tf_idf >=1.5) %>%select(doc, bigram, n)%>%
  cast_dtm(.,doc, bigram, n)


bigram_lda <- LDA(bigram_tf_idf_dtm, k = 2, control = list(seed =83181))
bigram_lda_topics <- tidy(bigram_lda, matrix = "beta")

bigram_lda_top_terms <- bigram_lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

bigram_lda_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "fixed") +
  coord_flip()+theme_bw() + scale_fill_npg()
