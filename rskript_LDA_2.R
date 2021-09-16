# Topic Model 2: Filter Wordlist
setwd("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt")

# 1. Load Packages and Data

library(quanteda)
library(tidyverse)
library(lubridate)
library(rtweet)
library(tidytext)
library(readxl)

daddit_2019_excel <- read_excel("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt/Daten_Reddit/daddit_2019_excel.xlsx")
daddit_2020_excel <- read_excel("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt/Daten_Reddit/daddit_2020_excel.xlsx")
mommit_2019_excel <- read_excel("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt/Daten_Reddit/mommit_2019_excel.xlsx")
mommit_2020_excel <- read_excel("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt/Daten_Reddit/mommit_2020_excel.xlsx")


# 2. Data preparation
daddit_2019 <- as_tibble(daddit_2019_excel)
daddit_2020 <- as_tibble(daddit_2020_excel)
mommit_2019 <- as_tibble(mommit_2019_excel)
mommit_2020 <- as_tibble(mommit_2020_excel)

# 2.1 Filter and select relevant Variables
daddit_2019 <- daddit_2019 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

mommit_2019 <- mommit_2019 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

daddit_2020 <- daddit_2020 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

mommit_2020 <- mommit_2020 %>% 
  select(c(created_utc, selftext, author, id, subreddit)) %>%
  filter(selftext != "NA",
         selftext != "removed",
         selftext != "deleted")

# 2.2 cleaning data
daddit_2019$selftext <- gsub("\\s(https:\\S+)", "", daddit_2019$selftext) # removes all URLs in format " https.... "
daddit_2019$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", daddit_2019$selftext)
daddit_2019$selftext <- gsub("^http.*", "", daddit_2019$selftext) # removes all URLs
daddit_2019$selftext <- gsub("&amp;", "and", daddit_2019$selftext) ## removes ampersand (&)
daddit_2019$selftext <- gsub("â", "", daddit_2019$selftext) # this needs to be removed due to an error in the data
daddit_2019$selftext <- gsub("[[:digit:]]", "", daddit_2019$selftext) #remove digits
daddit_2019$selftext <- gsub("[[:punct:]]", "", daddit_2019$selftext) #remove punctuations
daddit_2019$selftext <- gsub("@\\w+", "", daddit_2019$selftext)

daddit_2020$selftext <- gsub("\\s(https:\\S+)", "", daddit_2020$selftext) 
daddit_2020$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("^http.*", "", daddit_2020$selftext) 
daddit_2020$selftext <- gsub("&amp;", "and", daddit_2020$selftext) 
daddit_2020$selftext <- gsub("â", "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("[[:digit:]]", "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("[[:punct:]]", "", daddit_2020$selftext)
daddit_2020$selftext <- gsub("@\\w+", "", daddit_2020$selftext)

mommit_2019$selftext <- gsub("\\s(https:\\S+)", "", mommit_2019$selftext) 
mommit_2019$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("^http.*", "", mommit_2019$selftext) 
mommit_2019$selftext <- gsub("&amp;", "and", mommit_2019$selftext) 
mommit_2019$selftext <- gsub("â", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("[[:digit:]]", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("[[:punct:]]", "", mommit_2019$selftext)
mommit_2019$selftext <- gsub("@\\w+", "", mommit_2019$selftext)

mommit_2020$selftext <- gsub("\\s(https:\\S+)", "", mommit_2020$selftext) 
mommit_2020$selftext <- gsub(pattern = " ?(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("^http.*", "", mommit_2020$selftext) 
mommit_2020$selftext <- gsub("&amp;", "and", mommit_2020$selftext) 
mommit_2020$selftext <- gsub("â", "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("[[:digit:]]", "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("[[:punct:]]", "", mommit_2020$selftext)
mommit_2020$selftext <- gsub("@\\w+", "", mommit_2020$selftext)


#Copy of seltext

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

# 2.4 Remove stopwords 

data(stop_words)

tidy_daddit_2019 <- tidy_daddit_2019 %>%
  anti_join(stop_words)

tidy_daddit_2020 <-  tidy_daddit_2020 %>% 
  anti_join(stop_words)

tidy_mommit_2019 <-  tidy_mommit_2019 %>% 
  anti_join(stop_words)

tidy_mommit_2020 <-  tidy_mommit_2020 %>% 
  anti_join(stop_words)

#2.5 Remove remaining stopwords (with the help of self-created stopword list)

stop_words_eigen <- read_excel("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt/stop_words_eigen.xlsx")

tidy_daddit_2019 <- tidy_daddit_2019 %>%
  anti_join(stop_words_eigen)

tidy_daddit_2020 <- tidy_daddit_2020 %>%
  anti_join(stop_words_eigen)

tidy_mommit_2019 <- tidy_mommit_2019 %>%
  anti_join(stop_words_eigen)

tidy_mommit_2020 <- tidy_mommit_2020 %>%
  anti_join(stop_words_eigen)

# 2.6 Filter new words (explanation in text)

filter_words <- read_excel("C:/Users/ellah/OneDrive/Dokumente/Studium/Semester 5/Emprisches Fopra/Reddit_Projekt/filter_words.xlsx")

tidy_daddit_2019_2 <- tidy_daddit_2019 %>%
  anti_join(filter_words)

tidy_daddit_2020_2 <- tidy_daddit_2020 %>%
  anti_join(filter_words)

tidy_mommit_2019_2 <- tidy_mommit_2019 %>%
  anti_join(filter_words)

tidy_mommit_2020_2 <- tidy_mommit_2020 %>%
  anti_join(filter_words)

# 2.7 Lemmatization

library(textstem)

#daddit 19
lemma_d19_2 <- tidy_daddit_2019_2$word %>%
  lemmatize_strings()
lemma_d19_2 <- as_tibble(lemma_d19_2)
tidy_daddit_2019_2 <- bind_cols(tidy_daddit_2019_2, lemma_d19_2)

#daddit 20
lemma_d20_2 <- tidy_daddit_2020_2$word %>%
  lemmatize_strings()
lemma_d20_2 <- as_tibble(lemma_d20_2)
tidy_daddit_2020_2 <- bind_cols(tidy_daddit_2020_2, lemma_d20_2)

#mommit 19
lemma_m19_2 <- tidy_mommit_2019_2$word %>%
  lemmatize_strings()
lemma_m19_2 <- as_tibble(lemma_m19_2)
tidy_mommit_2019_2 <- bind_cols(tidy_mommit_2019_2, lemma_m19_2)

#mommit 20
lemma_m20_2 <- tidy_mommit_2020_2$word %>%
  lemmatize_strings()
lemma_m20_2 <- as_tibble(lemma_m20_2)
tidy_mommit_2020_2 <- bind_cols(tidy_mommit_2020_2, lemma_m20_2)

# 3. LDA 2 (filtered)
# 3.1 creating DTM

#DTM daddit 2019_2
DTM_d19_2 <- tidy_daddit_2019_2 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# DTM daddit 2020
DTM_d20_2 <- tidy_daddit_2020_2 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# DTM mommit 2019_2
DTM_m19_2 <- tidy_mommit_2019_2 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# DTM mommit 2020 
DTM_m20_2 <- tidy_mommit_2020_2 %>%
  count(id, value) %>% 
  cast_dfm(id, value, n)

# 3.2 Topicmodels 2
K <- 17

#daddit 19
topicModel.d19_2 <- LDA(DTM_d19_2, 
                        K, 
                        method="Gibbs", 
                        control=list(iter = 500, 
                                     verbose = 25,
                                     seed = 5)) #set seed to make results reproducible

tmResult_d19_2 <- posterior(topicModel.d19_2)
beta_d19_2 <- tmResult_d19_2$terms
glimpse(beta_d19_2)            
terms(topicModel.d19_2, 10)

#daddit 20
topicModel.d20_2 <- LDA(DTM_d20_2, 
                        K, 
                        method="Gibbs", 
                        control=list(iter = 500, 
                                     verbose = 25,
                                     seed = 6))

tmResult_d20_2 <- posterior(topicModel.d20_2)
beta_d20_2 <- tmResult_d20_2$terms
glimpse(beta_d20_2)            
terms(topicModel.d20_2, 10)

#mommit 19
topicModel.m19_2 <- LDA(DTM_m19_2, 
                        K, 
                        method="Gibbs", 
                        control=list(iter = 500, 
                                     verbose = 25,
                                     seed = 7))

tmResult_m19_2 <- posterior(topicModel.m19_2)
beta_m19_2 <- tmResult_m19_2$terms
glimpse(beta_m19_2)            
terms(topicModel.m19_2, 10)

#mommit 20
topicModel.m20_2 <- LDA(DTM_m20_2, 
                        K, 
                        method="Gibbs", 
                        control=list(iter = 500, 
                                     verbose = 25,
                                     seed = 8))

tmResult_m20_2 <- posterior(topicModel.m20_2)
beta_m20_2 <- tmResult_m20_2$terms
glimpse(beta_m20_2)            
terms(topicModel.m20_2, 10)
