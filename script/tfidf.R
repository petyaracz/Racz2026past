setwd('~/Github/Racz2026past/')

# -- head -- #

library(tidyverse)
library(tidytext)

# -- read -- #

c = read_tsv('dat/omagyar_dictionary.tsv.gz')
g = read_tsv('~/Github/Racz2025Bible/dat/gospels.gz')
d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- sanity check -- #

d |> 
  count(translation,book,chapter,verse,word) |> 
  filter(n > 1) 
# welp

# -- lemmatise -- #

c2 = c |> 
  rename(word = norm) |> 
  filter(!is.na(lemma)) |> 
  arrange(-omagyar_freq) |> 
  group_by(word) |> 
  slice(1) |> 
  ungroup() |> 
  select(word,lemma)
  
gl = g |> 
  filter(
    translation %in% d$translation,
    type == 'normalised'
         ) |> 
  unnest_tokens(word,text,drop = F) |> 
  left_join(c2)

# -- get context windows -- #

# ...

# issue: verbs have no unique id-s either in d or c. but sequential numbering should do it

# -- tfidf -- #

tfidf = gl |> 
  mutate(doc_id = glue::glue('{translation}:{book}, {chapter}:{verse}')) |> 
  count(doc_id, word) |> 
  bind_tf_idf(word, doc_id, n)
