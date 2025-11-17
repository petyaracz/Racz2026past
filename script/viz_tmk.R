# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(ggthemes)
library(patchwork)

# -- read -- #

d = read_tsv('dat/tmk_and_dict_and_pred.tsv.gz')

# -- comp -- #

predictors = c(
'log_lemma_freq_scaled',
'number',
'person',
'prefix',
'motion_verb',
'communication_verb',
'modal_verb',
'lemma_length'
)

my_preds = d |> 
  summarise(
    pred_ipf = mean(`P(Y = ipf)`),
    pred_past = mean(`P(Y = past)`),
    pred_past_complex = mean(`P(Y = past_complex)`),
    pred_perf_complex = mean(`P(Y = perf_complex)`),
    .by = number
  ) |> 
  pivot_longer(-number)

p1 = d |> 
  ggplot(aes(number,fill = verb_past_class_complex)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  scale_fill_colorblind() +
  coord_flip()

my_preds |> 
  ggplot(aes(number,value,colour = name)) +
  geom_point() +
  theme_bw() +
  scale_colour_colorblind() +
  coord_flip()

# or something