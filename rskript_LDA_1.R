#Topic Models 1: with all words

# 1. Load Packages & Data

library(quanteda)
library(tidyverse)
library(lubridate)
library(rtweet)
library(tidytext)
library(readxl)


daddit_2019_excel <- read.csv("subm_daddit_2019.csv")
daddit_2020_excel <- read.csv("subm_daddit_2020.csv")
mommit_2019_excel <- read.csv("subm_mommit_2019.csv")
mommit_2020_excel <- read.csv("subm_mommit_2020.csv")

# 2. Data preparation

daddit_2019 <- as_tibble(daddit_2019_excel)
daddit_2020 <- as_tibble(daddit_2020_excel)
mommit_2019 <- as_tibble(mommit_2019_excel)
mommit_2020 <- as_tibble(mommit_2020_excel)

# 2.1 Select and filter relevant variables
daddit_2019 <- daddit_2019 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

daddit_2020 <- daddit_2020 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

mommit_2019 <- mommit_2019 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

mommit_2020 <- mommit_2020 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

# 2.2 Cleaning Data
daddit_2019$selftext <- gsub("\\s(https:\\S+)", "", daddit_2019$selftext) # removes all URLs in format " https.... "
daddit_2019$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", daddit_2019$selftext)
daddit_2019$selftext <- gsub("^http.*", "", daddit_2019$selftext) # removes all URLs
daddit_2019$selftext <- gsub("&amp;", "and", daddit_2019$selftext) ## removes ampersand (&)
daddit_2019$selftext <- gsub("?", "", daddit_2019$selftext) # this needs to be removed due to an error in the data
daddit_2019$selftext <- gsub("[[:digit:]]", "", daddit_2019$selftext) #remove digits
daddit_2019$selftext <- gsub("[[:punct:]]", "", daddit_2019$selftext) #remove punctuations
daddit_2019$selftext <- gsub("@\\w+", "", daddit_2019$selftext)

daddit_2020$selftext <- gsub("\\s(https:\\S+)", "", daddit_2020$selftext) 
daddit_2020$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("^http.*", "", daddit_2020$selftext) 
daddit_2020$selftext <- gsub("&amp;", "and", daddit_2020$selftext) 
daddit_2020$selftext <- gsub("?", "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("[[:digit:]]", "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("[[:punct:]]", "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("@\\w+", "", daddit_2020$selftext)

mommit_2019$selftext <- gsub("\\s(https:\\S+)", "", mommit_2019$selftext) 
mommit_2019$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("^http.*", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("&amp;", "and", mommit_2019$selftext) 
mommit_2019$selftext <- gsub("?", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("[[:digit:]]", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("[[:punct:]]", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("@\\w+", "", mommit_2019$selftext)

mommit_2020$selftext <- gsub("\\s(https:\\S+)", "", mommit_2020$selftext) 
mommit_2020$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("^http.*", "", mommit_2020$selftext) 
mommit_2020$selftext <- gsub("&amp;", "and", mommit_2020$selftext) 
mommit_2020$selftext <- gsub("?", "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("[[:digit:]]", "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("[[:punct:]]", "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("@\\w+", "", mommit_2020$selftext)

#copy of seltext
daddit_2019$selftext.1 <- daddit_2019$selftext
daddit_2020$selftext.1 <- daddit_2020$selftext
mommit_2019$selftext.1 <- mommit_2019$selftext
mommit_2020$selftext.1 <- mommit_2020$selftext

# 2.3 Unnest tokens

tidy_daddit_2019 <- daddit_2019 %>% 
  unnest_tokens(word, selftext.1, token = "words")
tidy_daddit_2020 <- daddit_2020 %>% 
  unnest_tokens(word, selftext.1, token = "words")
tidy_mommit_2019 <- mommit_2019 %>% 
  unnest_tokens(word, selftext.1, token = "words")
tidy_mommit_2020 <- mommit_2020 %>% 
  unnest_tokens(word, selftext.1, token = "words")

# 2.4 Remove Stopwords 

data(stop_words)
  
tidy_daddit_2019 <- tidy_daddit_2019 %>%
  anti_join(stop_words)

tidy_daddit_2020 <-  tidy_daddit_2020 %>% 
  anti_join(stop_words)

tidy_mommit_2019 <-  tidy_mommit_2019 %>% 
  anti_join(stop_words)

tidy_mommit_2020 <-  tidy_mommit_2020 %>% 
  anti_join(stop_words)

# 2.5 Remove remaining stopwords (with the help of self-created stopwords list)

library(readxl)
stop_words_eigen <- read_excel("stop_words_eigen.xlsx")

tidy_daddit_2019 <- tidy_daddit_2019 %>%
  anti_join(stop_words_eigen)

tidy_daddit_2020 <- tidy_daddit_2020 %>%
  anti_join(stop_words_eigen)

tidy_mommit_2019 <- tidy_mommit_2019 %>%
  anti_join(stop_words_eigen)

tidy_mommit_2020 <- tidy_mommit_2020 %>%
  anti_join(stop_words_eigen)

#2.6 Lemmatization 

library(textstem)

#daddit 19
lemma_d19 <- tidy_daddit_2019$word %>%
  lemmatize_strings()
lemma_d19 <- as_tibble(lemma_d19)
tidy_daddit_2019 <- bind_cols(tidy_daddit_2019, lemma_d19) #add lemmatized words to dataframe

#daddit 20
lemma_d20 <- tidy_daddit_2020$word %>%
  lemmatize_strings()
lemma_d20 <- as_tibble(lemma_d20)
tidy_daddit_2020 <- bind_cols(tidy_daddit_2020, lemma_d20)

#mommit 19
lemma_m19 <- tidy_mommit_2019$word %>%
  lemmatize_strings()
lemma_m19 <- as_tibble(lemma_m19)
tidy_mommit_2019 <- bind_cols(tidy_mommit_2019, lemma_m19)

#mommit 20
lemma_m20 <- tidy_mommit_2020$word %>%
  lemmatize_strings()
lemma_m20 <- as_tibble(lemma_m20)
tidy_mommit_2020 <- bind_cols(tidy_mommit_2020, lemma_m20)


# 3. LDA 1 (including all words)

# 3.1 Create DTM 
#DTM daddit 2019
DTM_d19 <- tidy_daddit_2019 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# DTM daddit 2020
DTM_d20 <- tidy_daddit_2020 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# DTM mommit 2019
DTM_m19 <- tidy_mommit_2019 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# DTM mommit 2020 
DTM_m20 <- tidy_mommit_2020 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# 3.2 Topicmodels 1
library(topicmodels)
K <- 25 #per LDA 25 Topics

#daddit 19
topicModel.d19 <- LDA(DTM_d19, 
                      K, 
                      method="Gibbs", 
                      control=list(iter = 500, 
                                   verbose = 25,
                                   seed = 1)) #Set Seed so that result will be reproducible

tmResult_d19 <- posterior(topicModel.d19)
beta_d19 <- tmResult_d19$terms
glimpse(beta_d19)            
terms(topicModel.d19, 10)

#daddit 20
topicModel.d20 <- LDA(DTM_d20, 
                      K, 
                      method="Gibbs", 
                      control=list(iter = 500, 
                                   verbose = 25,
                                   seed = 2))

tmResult_d20 <- posterior(topicModel.d20)
beta_d20 <- tmResult_d20$terms
glimpse(beta_d20)            
terms(topicModel.d20, 10)


#mommit 19
topicModel.m19 <- LDA(DTM_m19, 
                      K, 
                      method="Gibbs", 
                      control=list(iter = 500, 
                                   verbose = 25,
                                   seed = 3))

tmResult_m19 <- posterior(topicModel.m19)
beta_m19 <- tmResult_m19$terms
glimpse(beta_m19)            
terms(topicModel.m19, 10)


#mommit 20
topicModel.m20 <- LDA(DTM_m20, 
                      K, 
                      method="Gibbs", 
                      control=list(iter = 500, 
                                   verbose = 25,
                                   seed = 4))

tmResult_m20 <- posterior(topicModel.m20)
beta_m20 <- tmResult_m20$terms
glimpse(beta_m20)            
terms(topicModel.m20, 10)