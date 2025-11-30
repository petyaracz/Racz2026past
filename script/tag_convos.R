# use the torteneti maganeleti korpusz matches
# combine with omagyar dict

# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(tidytext)
library(glue)

# -- read -- #

# tmk verb hits (C~V.*)
s = read_tsv('dat/tmk_verb_norm.txt', col_types = cols(.default = "c"))

# omagyar dictionary
o = read_tsv('dat/omagyar_dictionary.tsv.gz')

# -- disambiguate -- #

# omagyar
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

# -- tidy up tmk -- #

l = s |> 
  unnest_tokens(word, hit, drop = F) |> 
  mutate(next_word = lead(word)) |> 
  inner_join(o2) |> 
  filter(!is.na(verb_past_class) | next_word %in% c('volt','vala'))


# -- lemma freq, preverbs -- #

word_freq = l |> 
  count(word, name = 'freq') |> 
  mutate(log_freq = log10(freq))

lemma_freq = l |> 
  count(lemma, name = 'lemma_freq') |> 
  mutate(log_lemma_freq = log10(lemma_freq))

# -- volt, vala -- #

d1 = l |> 
  mutate(
    class_subtype = ifelse(
      next_word %in% c('volt','vala'), 'complex', 'other'
    )
  ) |> 
  left_join(word_freq) |> 
  left_join(lemma_freq)

d2 = d1 |> 
  mutate(
    verb_past_class_complex = case_when(
      !is.na(verb_past_class) & class_subtype == 'complex' ~ 'complex imperfective',
      verb_present & class_subtype == 'complex' ~ 'complex perfective',
      verb_past_class == 'Ipf' & class_subtype == 'other' ~ 'imperfective',
      verb_past_class == 'Past' & class_subtype == 'other' ~ 'perfective'
    )
  ) |> 
  filter(!is.na(verb_past_class_complex))

# -- more checks -- #

d2[is.na(d2$lemma),]
d2[is.na(d2$person),]
d2[is.na(d2$number),]
unique(d2[!d2$prefix,]$lemma)
unique(d2[d2$motion_verb,]$lemma)
unique(d2[d2$modal_verb,]$lemma)
unique(d2[d2$communication_verb,]$lemma)
# unique(d2[d2$verb_past_class_complex == 'past_complex',]$word)
# unique(d2[d2$verb_past_class_complex == 'past',]$word)
# unique(d2[d2$verb_past_class_complex == 'perf_complex',]$word)
# unique(d2[d2$verb_past_class_complex == 'ipf',]$word)

# ohoho
d2 = d2 |> filter(!word %in% c('érettünk'), !is.na(person), !is.na(number))

# -- write -- #

d2 |> 
  write_tsv('dat/tmk_and_dict.tsv.gz')
