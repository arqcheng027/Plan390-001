library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)

Speech = read.csv("presidential_speeches_sample.csv")
tokens = unnest_tokens(Speech, word, content)
head(tokens)
stop_words
tokens = anti_join(tokens, stop_words, by="word")
tokens = mutate(tokens, lemma=lemmatize_words(word))
View(head(tokens))
wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)
model = LDA(word_mtx, 25, control=list(seed=42))
beta = tidy(model, matrix="beta")
View(beta)
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10
ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()

## Create date variable
all(str_detect(Speech$document, "[[:digit:]]{4}"))  
Speech[,c("Year")] = str_match(Speech$document, "[[:digit:]]{4}")
byyear = group_by(Speech, Year) %>% 
  summarize(
    job = sum(str_count(content, "job")), 
    war= sum(str_count(content, "war"))
  )

#plot for job, war
ggplot(byyear, aes(x= Year)) +
  geom_point(aes(y=job, color = "job")) +
  geom_point(aes(y=war, color = "war"))
