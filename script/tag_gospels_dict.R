# use the gospel list
# combine with omagyar dict

# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(tidytext)
library(glue)

# -- read -- #

# par bible
b = read_tsv('~/Github/Racz2025Bible/dat/gospels.gz')
# omagyar dictionary
o = read_tsv('dat/omagyar_dictionary.tsv.gz')


# -- unnest -- #

b = b |> 
  filter(type == 'normalised')

d = b |>
  unnest_tokens(word, text, drop = F)

# -- disambiguate -- #

o2 = o |> 
  filter(
    !is.na(lemma),
    str_detect(tag, '^V')
    ) |> 
  group_by(norm) |> 
  arrange(-omagyar_freq) |> 
  slice(1) |> 
  ungroup() |> 
  rename(word = norm)

# -- combine -- #

d2 = d |> 
  left_join(o2)

# -- check -- #

d2 |> 
  count(is.na(tag))

b |> 
  sample_n(1) |> 
  inner_join(d2) |> 
  select(word,tag,lemma)

# yeah sure whatever

# -- lemma freq -- #

word_freq = d2 |> 
  count(word, name = 'freq') |> 
  mutate(log_freq = log10(freq))
  
lemma_freq = d2 |> 
  count(lemma, name = 'lemma_freq') |> 
  mutate(log_lemma_freq = log10(lemma_freq))

d3 = d2 |> 
  left_join(word_freq) |> 
  left_join(lemma_freq) |> 
  filter(!is.na(class))

# -- write -- #

write_tsv(
  d3, 'dat/gospel_and_dict.tsv.gz'
)

