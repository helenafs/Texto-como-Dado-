#aula: https://www.youtube.com/watch?v=ECwPKWnmmc4&ab_channel=WiDSSalvador 

library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library (tidyverse)

df <- read.csv("C:\\Users\\helen\\Documents\\Mestrado\\1_Monografia\\Texto-como-Dado-\\live coding wids\\base_reduzida.csv")

df <-  df %>% 
  filter(match_word == 1)

rownames(df) <- df$id_trabalho

require(tidytext)
require(tm)
require(stringr)
require(SnowballC)

df$traducao <- tolower(df$traducao) #minúscula 
df$traducao <- str_replace_all(df$traducao,"[[:punct:]]", " ") #retira a pontuação e coloca espaço vazio 
df$traducao <- str_replace_all(df$traducao,"[[:digit:]]", " ") #retira os dígitos e coloca espaço vazio 
df$traducao <- removeWords(df$traducao, stop_words$word) #retira as stopwords
df$traducao <- wordStem(df$traducao, "english") #stemming das palavras em inglês 
void_words <- c('data','based','study','proposed','results','work','using','used',
                'also','present','method','paper','different','approach','presented',
                'studies','however','obtained','brazilian','applied','many','among',
                'considered','propose','evaluate','objective','considering','several',
                'research','possible','important','methodology','methods','however',' data',
                'main','data','test') #palavras vazias identificadas previamente 
df$traducao <- removeWords(df$traducao, void_words) #remoção das palavras vazias 
df$traducao <- str_squish(df$traducao) #remoção dos espaços vazios do começo e do final da string e também reduz os espaços vazios dentro da string 

(text <- VectorSource(df$traducao)) #cria um vetor 
text_gram <- Corpus(text) 
text_bigram <- VCorpus(text)

dtm_gram <- DocumentTermMatrix(text_gram,control=list(wordLengths=c(4,15)))
tidy_gram<-tidy(dtm_gram)


############
install.packages('RWeka')
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
## dtm_bigram <- DocumentTermMatrix(text_bigram,control=list(wordLengths=c(4,15), tokenize=BigramTokenizer))

## tidy_bigram <- tidy(dtm_bigram)

## full_bag_words <- bind_rows(tidy_bigram,tidy_gram)

## words <- full_bag_words %>% bind_tf_idf(term,document,count) %>%   arrange(document,desc(tf_idf)) %>% group_by(document) %>%   mutate(id=row_number()) %>%   filter(id<20)

## words_dtm <- words %>% select(document,term,tf_idf) %>% cast_dtm(document,term,tf_idf)

## words_dtm_tibble <- tidy(words_dtm)
## words_dtm_matrix <- as.matrix(words_dtm)
## words_dtm_matrix <- t(words_dtm_matrix)
## norm <- sqrt(colSums(words_dtm_matrix^2))
## words_dtm_matrix <- scale(words_dtm_matrix,center=FALSE,scale=norm)
## words_dtm_matrix <- t(words_dtm_matrix)
## words_dtm_matrix_tidy <- tidytext::tidy(words_dtm_matrix)
## new_words_matrix <- as.matrix(words_dtm_matrix)
## rank_matrix <- new_words_matrix%*%t(new_words_matrix)
## rank_row <- colSums(rank_matrix)

## rank_row_tibble <- as_tibble(rank_row)
## names_rank_row <- rownames(rank_row_tibble)
## names_rank_row_2 <- str_sub(names_rank_row,1,-5)
## words_dtm <- cbind(score=rank_row)
## words_dtm2 <- cbind(words_dtm,names_rank_row)
## words_dtm2_tibble <- as_tibble(words_dtm2)

## df <- df %>% mutate(id_trabalho = c(1:nrow(df)))

## id <- as.character(df$id_trabalho)

## recommendend_presentations <- df %>% mutate(id_trabalho=id) %>%   inner_join(words_dtm2_tibble, by=c('id_trabalho'='names_rank_row')) %>%  arrange(desc(score)) %>% filter(score>0)
