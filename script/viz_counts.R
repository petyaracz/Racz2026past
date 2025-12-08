# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(ggthemes)

# -- read -- #

d = read_tsv('dat/gospel_and_dict.tsv.gz')
tmk = read_tsv('dat/tmk_and_dict.tsv.gz')

# -- counts -- #

c1 = d |> 
  count(lemma,lemma_freq,verb_past_class_complex) |> 
  mutate(source = 'Gospels')

c2 = tmk |> 
  count(lemma,verb_past_class_complex) |> 
  mutate(source = 'informal language')

counts = bind_rows(c1,c2)

implemma = c1 |> 
  pivot_wider(names_from = verb_past_class_complex, values_from = n, values_fill = 0) |> 
  mutate(
    sum = `complex imperfective` + `complex perfective` + imperfective + perfective,
    imppercent = imperfective/sum
  ) |> 
  select(lemma,imppercent)

# counts |> 
#   filter(lemma_freq > 250) |> 
#   distinct(lemma) |> 
#   write_tsv('dat/lemma250.tsv')
keep = read_tsv('dat/lemma250translated.tsv')

counts |> 
  inner_join(keep) |>  
  left_join(implemma) |> 
  mutate(
    freq = sum(n),
    percent = n/freq,
    .by = c(lemma,source,translation)
  ) |> 
  mutate(
    lemmatranslation = glue::glue('{lemma} ({translation})') |> 
      fct_reorder(imppercent)
         ) |> 
  ggplot(aes(lemmatranslation,percent,fill = verb_past_class_complex)) +
  geom_col() +
  facet_wrap( ~ source) +
  theme_few() +
  scale_fill_colorblind() +
  coord_flip() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(fill = 'category')

ggsave('viz/verbsfreq250gospel.png', dpi = 'print', width = 5, height = 8)

d |> 
  select(-translation) |> 
  inner_join(keep) |> 
  count(lemma,translation,lemma_freq,verb_past_class_complex,year) |> 
  mutate(
    sum = sum(n),
    percent = n/sum,
    .by = c(lemma,translation,lemma_freq,year)
  ) |> 
  mutate(
    lemmatranslation = glue::glue('{lemma} ({translation})') |> 
      fct_reorder(-lemma_freq)
  ) |> 
  ggplot(aes(year,percent,colour = verb_past_class_complex)) +
  geom_line() +
  facet_wrap( ~ lemmatranslation) +
  theme_few() +
  scale_colour_colorblind() +
  labs(colour = 'category') +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank()
  )

ggsave('viz/verbsfreq250gospelchange.png', dpi = 'print', width = 13, height = 6)
