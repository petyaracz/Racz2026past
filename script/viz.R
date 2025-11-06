# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(ggthemes)
library(patchwork)

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- set factors -- #

d2 = d |> 
  mutate(
    log_lemma_freq_scaled = scales::rescale(log_lemma_freq),
    lemma_length = scales::rescale(nchar(lemma)),
    number = fct_relevel(number, 'S'),
    person = fct_relevel(as.factor(person), '3','2','1'),
    prefix = ifelse(prefix, 'prefixed verb', 'other verb') |> 
      fct_relevel('other verb'),
    modal_verb = ifelse(modal_verb, 'modal verb', 'other verb') |> 
      fct_relevel('other verb'),
    communication_verb = ifelse(communication_verb, 'communicative verb', 'other verb') |> 
      fct_relevel('other verb'),
    motion_verb = ifelse(motion_verb, 'motion verb', 'other verb') |> 
      fct_relevel('other verb'),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

# -- counts -- #

my_counts = d2 |> 
  count(lemma,log_lemma_freq_scaled,class,translation,year,lemma_length,prefix,motion_verb,modal_verb,communication_verb,number,person) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

my_counts_l = d2 |> 
  count(lemma,log_lemma_freq_scaled,class,translation,year,lemma_length,prefix,motion_verb,modal_verb,communication_verb) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

# -- viz -- #

keep_lemma = my_counts_l |> 
  filter(!lemma %in% c('befogad','betölt','elölkel','meghirhedik')) |> 
  select(lemma,translation,p) |> 
  pivot_wider(names_from = translation, values_from = p) |> 
  mutate(
    has_na = if_any(everything(), is.na) |>
           ifelse("yes", "no")
    ) |> 
  filter(has_na == 'no') |> 
  pull(lemma)

my_counts_l |> 
  filter(lemma %in% keep_lemma) |> 
  ggplot(aes(translation,p,group = lemma)) +
  geom_line()


