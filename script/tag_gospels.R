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

# -- combine -- #

d2 = d |> 
  left_join(o2)

# -- check -- #

d2 |> 
  count(is.na(tag))

b |> 
  sample_n(1) |> 
  inner_join(d2) |> 
  select(verb_past_class,word,tag,lemma)

# yeah sure whatever

# -- lemma freq, preverbs -- #

word_freq = d2 |> 
  count(word, name = 'freq') |> 
  mutate(log_freq = log10(freq))
  
lemma_freq = d2 |> 
  count(lemma, name = 'lemma_freq') |> 
  mutate(log_lemma_freq = log10(lemma_freq))

# -- volt, vala -- #

d3 = d2 |> 
  mutate(
    next_word = lead(word),
    class_subtype = ifelse(
      next_word %in% c('volt','vala'), 'complex', 'other'
    )
         ) |> 
  left_join(word_freq) |> 
  left_join(lemma_freq)

# -- the four classes -- #

# not a thing: ipf + volt/vala (verb_past_class == Ipf, class_subtype == 'complex')
# present + volt/vala (verb_past_class == Past, class_subtype == 'complex')
# past + volt/vala (verb_past_class == Past, class_subtype == 'complex')
# ipf (verb_past_class == Ipf, class_subtype == 'complex')
# past (verb_past_class == Past, class_subtype == 'complex')

# d3 |> 
#   filter(class_subtype == 'complex',!is.na(tag)) |> 
#   distinct(word,next_word,tag,freq) |> 
#   arrange(-freq) |> 
#   group_by(tag) |> 
#   slice(1) |> 
#   select(-freq) |> 
#   googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/1Wn98bJANCA-Tv5rgKgu-BaTcv7A_eIv6FBAAAHbSizQ/edit?usp=sharing', 'Sheet1')

# d3 |> 
#   count(class_subtype,verb_past_class,verb_present,tag) |> View()

d4 = d3 |> 
  mutate(
    verb_past_class_complex = case_when(
      !is.na(verb_past_class) & class_subtype == 'complex' ~ 'past_complex',
      verb_present & class_subtype == 'complex' ~ 'perf_complex',
      verb_past_class == 'Ipf' & class_subtype == 'other' ~ 'ipf',
      verb_past_class == 'Past' & class_subtype == 'other' ~ 'past'
    )
  ) |> 
  filter(!is.na(verb_past_class_complex))

# -- more checks -- #

d4[is.na(d4$lemma),]
d4[is.na(d4$person),]
d4[is.na(d4$number),]
unique(d4[!d4$prefix,]$lemma)
unique(d4[d4$motion_verb,]$lemma)
unique(d4[d4$modal_verb,]$lemma)
unique(d4[d4$communication_verb,]$lemma)
unique(d4[d4$verb_past_class_complex == 'past_complex',]$word)
unique(d4[d4$verb_past_class_complex == 'past',]$word)
unique(d4[d4$verb_past_class_complex == 'perf_complex',]$word)
unique(d4[d4$verb_past_class_complex == 'ipf',]$word)

# ohoho
d4 = d4 |> filter(!word %in% c('érettünk'))

# d3 |> 
#   distinct(lemma,word,class,lemma_freq,freq) |> 
#   group_by(lemma,class) |> 
#   arrange(-freq) |> 
#   mutate(words = paste(word, collapse = ', ')) |> 
#   arrange(lemma) |> 
#   distinct(lemma,class,words) |> 
#   pivot_wider(names_from = class, values_from = words) |> 
#   googlesheets4::write_sheet('https://docs.google.com/spreadsheets/d/1aHORd9t4nkJeZ5pSVX6gri14Hr5ObCQMK75OyKVBUoc/edit?usp=sharing', 'checks')
  
# -- write -- #

write_tsv(
  d4, 'dat/gospel_and_dict.tsv.gz'
)
