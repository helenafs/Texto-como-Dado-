require(stringr)
require(readr)
require(dplyr)
require(tidyr)

df <-  read_csv("Mestrado/Dissertacao/base_de_dados/base_final.csv")

df$keyword <- str_replace_all(df$keyword,"\n", " ")
df$keyword <- str_replace_all(df$keyword,"[[:digit:]]", " ")
df$keyword <- tolower(df$keyword)

require(tidytext)
texto_un <- df %>%
  select(keyword,id_trabalho) %>% 
  unnest_tokens(output = "words", input = keyword)

texto_un %>% 
  group_by(words) %>% 
  tally() %>% 
  arrange(-n)

x <- str_match_all(df$keyword, "distribution")

n <- nrow(df)
df <- df %>% 
  mutate(match_word = rep("n",n))

for(i in 1:n){
  if(is.na(x[[i]][1]) == FALSE){
    df$match_word[i] <- 1
  }else{
    df$match_word[i] <- 0
  }
}